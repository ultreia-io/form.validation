# Internal helpers ---------------------------------------------------------

# Plan identity and path checks ---------------------------------------------------------

#' Hash a batch artifact
#' @param path Path to an existing file.
#' @return A SHA-256 hexadecimal string computed from file bytes.
#' @keywords internal
batch_file_hash <- function(path) digest::digest(file = path, algo = "sha256")

#' Compute the identity of a frozen batch plan
#' @param plan Verified local `validation_batch_plan`.
#' @return A SHA-256 string for the plan contents.
#' @details Excludes the stored ID and local directory. Callback definitions, including argument defaults, are normalized without bytecode or source references so runtime compilation and source metadata do not change the identity.
#' @keywords internal
batch_plan_id <- function(plan) {
  # Strip bytecode and mutable source references from callback definitions.
  for (name in c("reader", "prepare_data")) {
    callback <- plan[[name]]
    if (is.function(callback)) {
      attributes <- attributes(callback)
      attributes$srcref <- NULL
      callback <- eval(parse(text = deparse(callback, control = "all"), keep.source = FALSE)[[1L]],
        envir = environment(callback))
      attributes(callback) <- attributes
      plan[[name]] <- callback
    }
  }
  digest::digest(unclass(plan)[setdiff(names(plan), c("id", "directory"))], algo = "sha256")
}

#' Validate relative batch paths
#' @param paths Character vector of nonempty relative paths using forward slashes.
#' @return `NULL` on success; otherwise an error.
#' @details Rejects absolute paths, backslashes, trailing slashes and empty, dot or parent components.
#' @keywords internal
check_batch_paths <- function(paths) {
  if (!is.character(paths) || anyNA(paths) || any(!nzchar(paths)) ||
      any(grepl("^[/~]|^[A-Za-z]:|\\\\|/$", paths)) ||
      any(vapply(strsplit(paths, "/", fixed = TRUE), function(parts)
        any(parts %in% c("", ".", "..")), logical(1))))
    stop("Expected safe relative paths", call. = FALSE)
}

# State, resources, locking and logging ---------------------------------------------------------

#' Replace a batch state file atomically
#' @param value R object to serialize.
#' @param path Destination RDS path. Parent directories are created.
#' @param compress Compression setting passed to `saveRDS()`.
#' @return The destination path, invisibly.
#' @details Writes a temporary file in the destination directory, then renames it. A failed write leaves the previous state intact.
#' @keywords internal
write_batch_state <- function(value, path, compress = TRUE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(".state-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(value, temporary, compress = compress)
  if (!file.rename(temporary, path)) stop("Cannot save batch state: ", path, call. = FALSE)
  invisible(path)
}

#' Read a completed stage of the current batch
#' @param plan Verified local `validation_batch_plan`.
#' @param name Stage basename, such as `validation` or `reports`, without `.rds`.
#' @return The saved stage state.
#' @details Rejects missing, incomplete or mismatched states. Reads only trusted local RDS artifacts.
#' @keywords internal
read_batch_state <- function(plan, name) {
  path <- file.path(plan$directory, paste0(name, ".rds"))
  if (!file.exists(path)) stop("Run the ", name, " stage first", call. = FALSE)
  state <- readRDS(path)
  if (!identical(state$plan_id, plan$id) || !isTRUE(state$complete))
    stop("The ", name, " stage is incomplete or belongs to another plan", call. = FALSE)
  state
}

#' Load a checksum-verified resource bundle
#' @param plan Verified local `validation_batch_plan`.
#' @param key Name in the plan resource records.
#' @return The deserialized resource bundle.
#' @details Verifies file existence and SHA-256 before reading trusted local RDS data.
#' @keywords internal
read_batch_resources <- function(plan, key) {
  resource <- plan$resources[[key]]
  path <- file.path(plan$directory, resource$file)
  if (!file.exists(path) || !identical(batch_file_hash(path), resource$sha256))
    stop("Frozen validation resources changed: ", key, call. = FALSE)
  readRDS(path)
}

#' Acquire an exclusive local batch lock
#' @param plan Verified local `validation_batch_plan`.
#' @return The lock directory path; the caller must remove it on exit.
#' @details Records host and process ID. Reclaims a lock only when its owner is a dead local process; rejects other existing owners.
#' @keywords internal
lock_validation_batch <- function(plan) {
  lock <- file.path(plan$directory, ".pipeline-lock")
  owner_path <- file.path(lock, "owner.rds")
  if (dir.exists(lock)) {
    owner <- tryCatch(readRDS(owner_path), error = function(error) NULL)
    local <- !is.null(owner) && identical(owner$host, Sys.info()[["nodename"]])
    alive <- local && isTRUE(tryCatch(tools::pskill(owner$pid, signal = 0L), error = function(error) TRUE))
    if (!local || alive) stop("Another stage owns the batch lock: ", lock, call. = FALSE)
    unlink(lock, recursive = TRUE)
  }
  if (!dir.create(lock, showWarnings = FALSE)) stop("Cannot lock batch: ", lock, call. = FALSE)
  saveRDS(list(pid = Sys.getpid(), host = Sys.info()[["nodename"]]), owner_path)
  lock
}

#' Format the current batch timestamp
#' @return Current UTC time as `YYYY-MM-DDTHH:MM:SSZ`.
#' @keywords internal
batch_time <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

#' Create a timestamped batch logger
#' @param directory Existing batch directory.
#' @param filename Log filename within that directory.
#' @return A function accepting `text`, appending timestamped lines to the file and emitting them through `message()`.
#' @keywords internal
batch_log <- function(directory, filename) {
  function(text) {
    line <- paste(batch_time(), text)
    cat(line, "\n", file = file.path(directory, filename), append = TRUE)
    message(line)
  }
}

# Input validation and parallel workers ---------------------------------------------------------

#' Process the pending inputs for one resource bundle
#' @param selected Pending input rows sharing a resource bundle.
#' @param bundle Frozen rules and references for these inputs.
#' @param plan Verified batch plan.
#' @param state Current validation stage state.
#' @param state_path Path to the stage state file.
#' @param log Batch logger.
#' @param workers Maximum concurrent validation processes.
#' @param cluster A socket cluster on Windows; `NULL` for sequential or forked execution.
#' @return Updated validation stage state.
#' @keywords internal
validate_batch_inputs <- function(selected, bundle, plan, state, state_path, log, workers, cluster = NULL) {
  save_outcome <- function(input, outcome) {
    state$outcomes[[input]] <<- outcome
    write_batch_state(state, state_path)
    log(paste(toupper(outcome$status), input, sprintf("elapsed=%.3fs", outcome$elapsed), outcome$message))
  }
  if (workers == 1L) {
    for (i in seq_len(nrow(selected))) {
      file <- selected[i, ]
      log(paste("START", file$relative))
      save_outcome(file$relative, validate_batch_input(file, bundle, plan, log))
    }
    return(state)
  }
  if (!is.null(cluster)) {
    for (first in seq.int(1L, nrow(selected), by = workers)) {
      rows <- seq.int(first, min(first + workers - 1L, nrow(selected)))
      files <- lapply(rows, function(i) selected[i, ])
      for (file in files) log(paste("START", file$relative))
      finished <- parallel::parLapplyLB(cluster, files, validate_batch_socket_input,
        bundle = bundle, plan = plan)
      for (i in seq_along(files)) {
        for (line in finished[[i]]$lines) log(line)
        save_outcome(files[[i]]$relative, finished[[i]]$outcome)
      }
    }
    return(state)
  }
  running <- list()
  on.exit({
    for (job in running) try(tools::pskill(job$process$pid), silent = TRUE)
    if (length(running)) parallel::mccollect(lapply(running, `[[`, "process"))
  }, add = TRUE)
  next_input <- 1L
  while (next_input <= nrow(selected) || length(running)) {
    while (next_input <= nrow(selected) && length(running) < workers) {
      file <- selected[next_input, ]
      log(paste("START", file$relative))
      process <- parallel::mcparallel(validate_batch_input(file, bundle, plan, log), silent = TRUE)
      running[[as.character(process$pid)]] <- list(process = process,
        input = file$relative, started = proc.time()[["elapsed"]])
      next_input <- next_input + 1L
    }
    finished <- parallel::mccollect(lapply(running, `[[`, "process"), wait = FALSE, timeout = 1)
    if (is.null(finished)) next
    for (pid in names(finished)) {
      task <- running[[pid]]
      outcome <- finished[[pid]]
      if (is.null(outcome) || inherits(outcome, "try-error"))
        outcome <- list(status = "execution-failed", message = "Validation worker stopped unexpectedly",
          elapsed = proc.time()[["elapsed"]] - task$started, completed_at = batch_time())
      running[[pid]] <- NULL
      save_outcome(task$input, outcome)
    }
  }
  state
}

#' Validate and save one planned input
#' @param file One row from the batch plan's input table.
#' @param bundle Frozen rules and references for the input's job.
#' @param plan Verified batch plan.
#' @param log Batch logger.
#' @return A small outcome record; the typed result is saved separately.
#' @keywords internal
validate_batch_input <- function(file, bundle, plan, log) {
  started <- proc.time()[["elapsed"]]
  outcome <- tryCatch({
    if (!identical(batch_file_hash(file$path), file$sha256)) stop("Input changed after planning")
    job <- plan$jobs[match(file$job, plan$jobs$id), ]
    rules <- bundle$plans[[job$plan]]
    result <- validate_workbook(file$path, rules, bundle$references, file$relative,
      config = plan$config, reader = plan$reader, prepare_data = plan$prepare_data, log = log)
    if (!identical(batch_file_hash(file$path), file$sha256)) stop("Input changed during validation")
    artifact <- list(plan_id = plan$id, input = file$relative, input_sha256 = file$sha256,
      validated_at = batch_time(), model = rules$model, result = result)
    path <- file.path(plan$directory, file$result)
    write_batch_state(artifact, path, compress = FALSE)
    list(status = result$status, result = file$result, result_sha256 = batch_file_hash(path))
  }, error = function(error) list(status = "execution-failed", message = conditionMessage(error)))
  outcome$elapsed <- proc.time()[["elapsed"]] - started
  outcome$completed_at <- batch_time()
  outcome
}

#' Validate one input in a socket worker
#' @inheritParams validate_batch_input
#' @return The outcome and its timing log lines.
#' @keywords internal
validate_batch_socket_input <- function(file, bundle, plan) {
  lines <- character()
  worker_log <- function(text) lines <<- c(lines, text)
  outcome <- validate_batch_input(file, bundle, plan, worker_log)
  list(outcome = outcome, lines = lines)
}

# Individual report rendering ---------------------------------------------------------

#' Render one saved result into the batch's staging directory
#' @param file One row from the batch plan's input table.
#' @param plan Verified batch plan.
#' @param validation Completed validation stage state.
#' @param stage Staging directory for all individual reports.
#' @param report_filename HTML basename.
#' @param title,author,stylesheet,logo Report presentation options.
#' @param reference_context_columns Public input columns shown in evidence tables.
#' @return Status and elapsed rendering time, or an error message.
#' @keywords internal
render_batch_input_report <- function(file, plan, validation, stage, report_filename,
                                      title, author, stylesheet, logo, reference_context_columns) {
  started <- proc.time()[["elapsed"]]
  tryCatch({
    outcome <- validation$outcomes[[file$relative]]
    path <- file.path(plan$directory, file$result)
    if (is.null(outcome$result_sha256) || !identical(batch_file_hash(path), outcome$result_sha256))
      stop("Saved validation result changed")
    saved <- readRDS(path)
    if (!identical(saved$plan_id, plan$id) || !identical(saved$input, file$relative) ||
        !identical(saved$input_sha256, file$sha256))
      stop("Saved result differs from the plan")
    job <- plan$jobs[match(file$job, plan$jobs$id), ]
    context <- as.list(file[reference_context_columns])
    report <- validation_report_data(saved$result, saved$model,
      reference_provenance = plan$resources[[job$resource]]$references, context = context)
    depth <- length(strsplit(file$relative, "/", fixed = TRUE)[[1L]])
    write_validation_report(report, file.path(stage, file$relative, report_filename),
      title = if ("title" %in% names(file)) file$title else paste(title, "-", file$relative),
      author = author, assets_directory = paste0(strrep("../", depth), "report-assets"),
      stylesheet = stylesheet, logo = logo)
    list(status = saved$result$status, elapsed = proc.time()[["elapsed"]] - started)
  }, error = function(error) list(error = conditionMessage(error)))
}

# Publication metadata and navigation ---------------------------------------------------------

#' Export public batch navigation metadata
#' @param plan Verified local `validation_batch_plan`.
#' @param path Destination JSON file.
#' @param reports Completed report state, or `NULL` before reporting.
#' @return The result of `jsonlite::write_json()`, invisibly.
#' @details Writes selected input metadata and jobs. Report state adds statuses and encoded links. Private paths, callbacks and resources are excluded; job metadata must already be public.
#' @keywords internal
write_batch_plan_json <- function(plan, path, reports = NULL) {
  columns <- unique(c("relative", plan$group_columns, plan$identity_columns, "model_version"))
  files <- plan$files[intersect(columns, names(plan$files))]
  names(files)[1L] <- "input"
  if (!is.null(reports)) {
    files$status <- vapply(files$input, function(input) reports$outcomes[[input]]$status, character(1))
    files$report <- vapply(file.path(files$input, reports$report_filename), batch_url, character(1))
  }
  jsonlite::write_json(list(schema_version = 1L, plan_id = plan$id, batch = basename(plan$directory),
    created_at = plan$created_at, group_columns = as.list(plan$group_columns),
    model_versions = as.list(plan$model_versions), jobs = plan$jobs, files = files),
    path, auto_unbox = TRUE, pretty = TRUE)
}

#' Validate an HTML report filename
#' @param filename Single filename without a directory.
#' @return `NULL` on success; otherwise an error.
#' @details Requires an alphanumeric first character, a `.html` suffix and only letters, digits, underscores, dots or hyphens.
#' @keywords internal
check_batch_report_filename <- function(filename) {
  if (!is.character(filename) || length(filename) != 1L || is.na(filename) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*\\.html$", filename))
    stop("Expected a plain .html report filename", call. = FALSE)
}

#' Encode a relative report URL
#' @param path One slash-separated relative path.
#' @return A URL-encoded string preserving directory separators.
#' @keywords internal
batch_url <- function(path) {
  paste(vapply(strsplit(path, "/", fixed = TRUE)[[1L]], utils::URLencode,
    character(1), reserved = TRUE), collapse = "/")
}

#' Add ancestor navigation to a publication copy
#' @param path HTML file to update in place.
#' @param relative Report directory relative to the publication root.
#' @param report_filename HTML filename used by ancestor aggregates.
#' @param current_label Current-page label, or `NULL` to use the final path component.
#' @param parent_filename HTML filename used by the immediate parent.
#' @return `NULL`, invisibly, after writing the updated HTML.
#' @details Replaces existing parent or breadcrumb navigation. Links are encoded, labels escaped and the current page remains unlinked.
#' @keywords internal
add_batch_breadcrumbs <- function(path, relative, report_filename, current_label = NULL, parent_filename = report_filename) {
  parts <- strsplit(relative, "/", fixed = TRUE)[[1L]]
  depth <- length(parts)
  labels <- c("All reports", utils::head(parts, -1L))
  links <- vapply(seq_along(labels), function(i) paste(render_report_template("breadcrumb-link.html", list(
    href = batch_url(paste0(strrep("../", depth - i + 1L), if (i == length(labels)) parent_filename else report_filename)), label = labels[i])), collapse = "\n"), character(1))
  navigation <- paste(render_report_template("breadcrumbs.html", list(filename = if (is.null(current_label)) parts[depth] else current_label)), collapse = "\n")
  navigation <- sub("<!-- breadcrumb-items -->", paste(links, collapse = "\n"), navigation, fixed = TRUE)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  html <- gsub('<nav (?:class="report-parent"|id="breadcrumbs"|aria-label="Breadcrumb")[^>]*>.*?</nav>', "", html, perl = TRUE)
  anchor <- '<div class="report-navigation">'
  if (!grepl(anchor, html, fixed = TRUE)) stop("Missing report navigation element: ", path)
  html <- sub(anchor, paste0(anchor, "\n", navigation), html, fixed = TRUE)
  writeLines(html, path, useBytes = TRUE)
}
