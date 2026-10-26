#' Validate every input in a saved batch plan
#' @inheritParams read_validation_batch_plan
#' @param resume Reuse completed, checksum-verified results after an interruption.
#' @param workers Number of forms to validate concurrently. The default is one.
#'   Parallel validation uses forked processes on Unix and socket workers on Windows.
#' @return Validation stage state, invisibly.
#' @details Reads only planned inputs and frozen resources. Each form saves its
#'   typed result and definition independently. The parent checkpoints outcomes
#'   as workers finish on Unix, or after each small worker group on Windows.
#'   No report tables, CSV files or HTML pages are generated. Reader failures are
#'   ordinary failed validation results; infrastructure failures keep the stage
#'   incomplete and can be retried. Result RDS files are uncompressed for fast reuse.
#' @export
validate_batch <- function(plan, resume = TRUE, workers = 1L) {
  if (length(workers) != 1L || !is.numeric(workers) || is.na(workers) ||
      !is.finite(workers) || workers < 1 || workers > .Machine$integer.max ||
      workers != as.integer(workers))
    stop("workers must be a positive whole number", call. = FALSE)
  workers <- as.integer(workers)
  plan <- read_validation_batch_plan(plan)
  lock <- lock_validation_batch(plan)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  state_path <- file.path(plan$directory, "validation.rds")
  state <- if (file.exists(state_path)) readRDS(state_path) else
    list(schema_version = 1L, plan_id = plan$id, started_at = batch_time(),
      complete = FALSE, outcomes = list())
  if (!identical(state$plan_id, plan$id)) stop("Validation state belongs to another plan", call. = FALSE)
  if (!isTRUE(resume) && length(state$outcomes)) stop("Validation has already started; use resume = TRUE", call. = FALSE)
  done <- vapply(plan$files$relative, function(input) {
    outcome <- state$outcomes[[input]]
    if (is.null(outcome$result_sha256)) return(FALSE)
    path <- file.path(plan$directory, outcome$result)
    if (!file.exists(path) || !identical(batch_file_hash(path), outcome$result_sha256))
      stop("Saved validation result is missing or changed: ", input, call. = FALSE)
    TRUE
  }, logical(1))
  if (isTRUE(state$complete) && all(done)) return(invisible(state))
  pending <- plan$files[!done, , drop = FALSE]
  for (i in seq_len(nrow(pending))) {
    file <- pending[i, ]
    if (!file.exists(file$path) || !identical(batch_file_hash(file$path), file$sha256))
      stop("Planned input is missing or changed; create a new plan: ", file$relative, call. = FALSE)
  }
  log <- batch_log(plan$directory, "validation.log")
  started <- proc.time()[["elapsed"]]
  state$complete <- FALSE
  write_batch_state(state, state_path)
  log(sprintf("VALIDATE %d planned inputs; %d already completed; workers=%d",
    nrow(plan$files), sum(done), workers))
  cluster <- if (workers > 1L && .Platform$OS.type == "windows")
    parallel::makePSOCKcluster(workers) else NULL
  if (!is.null(cluster)) on.exit(parallel::stopCluster(cluster), add = TRUE)
  for (resource in names(plan$resources)) {
    ids <- plan$jobs$id[plan$jobs$resource == resource]
    selected <- pending[pending$job %in% ids, , drop = FALSE]
    if (!nrow(selected)) next
    bundle <- read_batch_resources(plan, resource)
    state <- validate_batch_inputs(selected, bundle, plan, state, state_path, log, workers, cluster)
    rm(bundle)
  }
  state$complete <- all(vapply(plan$files$relative,
    function(input) !is.null(state$outcomes[[input]]$result_sha256), logical(1)))
  state$finished_at <- batch_time()
  state$jobs <- plan$jobs
  state$jobs$elapsed <- vapply(seq_len(nrow(plan$jobs)), function(i) {
    selected <- plan$files$relative[plan$files$job == plan$jobs$id[i]]
    sum(vapply(selected, function(input) state$outcomes[[input]]$elapsed, numeric(1)))
  }, numeric(1))
  write_batch_state(state, state_path)
  log(sprintf("FINISHED validation; complete=%s; invocation=%.3fs", state$complete, proc.time()[["elapsed"]] - started))
  if (!state$complete) stop("Validation could not finish; rerun validate with the same plan: ", plan$directory, call. = FALSE)
  invisible(state)
}

# Each worker writes only its own result. The parent alone updates validation.rds.
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

#' Generate individual reports from completed validation results
#' @inheritParams read_validation_batch_plan
#' @param report_filename HTML basename, ending in `.html`.
#' @inheritParams write_validation_report
#' @param overwrite Rebuild the complete reports directory from saved results.
#' @param workers Number of forms to render concurrently. The default is one.
#' @return The reports directory, invisibly.
#' @details Requires every planned input to have a saved result. Reads no input
#'   workbook or reference CSV. Uses the form saved with each result, so rendering
#'   does not depend on changes to installed form definitions.
#'   Reports are staged together and replace the previous report tree only after
#'   every form has rendered successfully. The batch plan and results stay intact.
#' @export
write_validation_batch_reports <- function(plan, report_filename = "index.html", overwrite = FALSE,
                                            title = "Validation report", author = NULL,
                                            stylesheet = NULL, logo = NULL, workers = 1L) {
  if (length(workers) != 1L || !is.numeric(workers) || is.na(workers) ||
      !is.finite(workers) || workers < 1 || workers > .Machine$integer.max ||
      workers != as.integer(workers))
    stop("workers must be a positive whole number", call. = FALSE)
  workers <- as.integer(workers)
  plan <- read_validation_batch_plan(plan)
  check_batch_report_filename(report_filename)
  lock <- lock_validation_batch(plan)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  validation <- read_batch_state(plan, "validation")
  output <- file.path(plan$directory, "reports")
  if (file.exists(output) && !isTRUE(overwrite)) stop("Reports already exist; use overwrite = TRUE", call. = FALSE)
  stage <- tempfile(".reports-", tmpdir = plan$directory)
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  log <- batch_log(plan$directory, "report.log")
  started <- proc.time()[["elapsed"]]
  outcomes <- list()
  files <- lapply(seq_len(nrow(plan$files)), function(i) plan$files[i, ])
  render <- function(file) render_batch_input_report(file, plan, validation, stage,
    report_filename, title, author, stylesheet, logo)
  if (workers == 1L) {
    rendered <- lapply(files, render)
  } else if (.Platform$OS.type == "windows") {
    cluster <- parallel::makePSOCKcluster(workers)
    on.exit(parallel::stopCluster(cluster), add = TRUE)
    rendered <- parallel::parLapplyLB(cluster, files, render)
  } else {
    rendered <- parallel::mclapply(files, render, mc.cores = workers, mc.preschedule = FALSE)
  }
  for (i in seq_along(files)) {
    file <- files[[i]]
    result <- rendered[[i]]
    if (inherits(result, "try-error") || !is.null(result$error))
      stop("Cannot render report for ", file$relative, ": ",
        if (inherits(result, "try-error")) as.character(result) else result$error, call. = FALSE)
    outcomes[[file$relative]] <- result
    log(sprintf("REPORTED %s elapsed=%.3fs", file$relative, result$elapsed))
  }
  paths <- sort(list.files(stage, recursive = TRUE, all.files = TRUE, no.. = TRUE))
  artifacts <- stats::setNames(vapply(file.path(stage, paths), batch_file_hash, character(1)), paths)
  state <- list(schema_version = 1L, plan_id = plan$id, complete = TRUE,
    validation_sha256 = batch_file_hash(file.path(plan$directory, "validation.rds")),
    report_filename = report_filename, finished_at = batch_time(),
    elapsed = proc.time()[["elapsed"]] - started, outcomes = outcomes, artifacts = artifacts)
  backup <- tempfile(".reports-previous-", tmpdir = plan$directory)
  if (dir.exists(output) && !file.rename(output, backup)) stop("Cannot replace reports directory", call. = FALSE)
  if (!file.rename(stage, output)) {
    if (dir.exists(backup)) file.rename(backup, output)
    stop("Cannot finalize reports", call. = FALSE)
  }
  write_batch_state(state, file.path(plan$directory, "reports.rds"))
  unlink(backup, recursive = TRUE)
  log(sprintf("FINISHED reports; %d forms; elapsed=%.3fs", nrow(plan$files), state$elapsed))
  invisible(output)
}

#' Render one saved result into the batch's staging directory
#' @param file One row from the batch plan's input table.
#' @param plan Verified batch plan.
#' @param validation Completed validation stage state.
#' @param stage Staging directory for all individual reports.
#' @param report_filename HTML basename.
#' @param title,author,stylesheet,logo Report presentation options.
#' @return Status and elapsed rendering time, or an error message.
#' @keywords internal
render_batch_input_report <- function(file, plan, validation, stage, report_filename,
                                      title, author, stylesheet, logo) {
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
    context <- as.list(file[unique(c(plan$group_columns, plan$identity_columns))])
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
