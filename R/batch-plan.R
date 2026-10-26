#' Freeze an explicit inventory for staged batch validation
#'
#' A batch plan describes work to execute. A [`validation_plan()`][form.validation::validation_plan] describes the
#' rules for one form; several such rule plans may belong to a batch.
#' @param inputs Data frame with `path`, unique relative `relative` paths and
#'   `job` identifiers. Additional grouping columns and `title` are optional.
#' @param jobs Data frame with unique `id`, `resource` bundle key and `plan` rule
#'   plan key. Additional columns are public metadata. Jobs without inputs remain.
#' @param resources Named list of bundles. Each has named `plans` containing
#'   `validation_plan` objects and `references` already prepared by the caller.
#'   A references `provenance` attribute may identify snapshots for evidence.
#' @param directory New local batch directory, outside the input directories.
#' @param group_columns Ordered input columns defining publication directories.
#'   Each relative path must consist of these components followed by a filename.
#' @param identity_columns Input columns distinguishing aggregate findings.
#' @param model_versions Model versions to display, including those without inputs.
#' @param metadata Private caller metadata, retained only in `plan.rds`.
#' @param packages Optional consumer package names to record with engine versions.
#' @inheritParams validate_workbook
#' @return A `validation_batch_plan`, also saved in `plan.rds`.
#' @details Does no directory discovery. Freezes resources, reader, preparation
#'   policy and input hashes. Later stages use only this inventory.
#'   Public `plan.json` contains relative paths and explicitly selected grouping
#'   metadata; local paths, resources, callbacks and private metadata are excluded.
#'   Extra job columns must contain only public metadata.
#' @export
validation_batch_plan <- function(inputs, jobs, resources, directory,
                                  group_columns = character(), identity_columns = group_columns,
                                  model_versions = character(), metadata = list(), packages = character(),
                                  config = list(), reader = read_validation_workbook,
                                  prepare_data = prepare_validation_data) {
  required <- unique(c("path", "relative", "job", group_columns, identity_columns))
  if (!is.data.frame(inputs) || !all(required %in% names(inputs)))
    stop("Missing batch input columns", call. = FALSE)
  if (!is.data.frame(jobs) || !all(c("id", "resource", "plan") %in% names(jobs)))
    stop("Missing batch job columns", call. = FALSE)
  for (column in c("id", "resource", "plan")) {
    if (!is.character(jobs[[column]]) || anyNA(jobs[[column]]) || any(!nzchar(jobs[[column]])))
      stop("Invalid job column: ", column, call. = FALSE)
  }
  if (anyDuplicated(jobs$id) || anyNA(inputs$job) || !all(inputs$job %in% jobs$id))
    stop("Each input must identify one unique job", call. = FALSE)
  check_batch_paths(inputs$relative)
  if (anyDuplicated(inputs$relative)) stop("Duplicate input path", call. = FALSE)
  for (column in unique(c(group_columns, identity_columns))) {
    if (!is.character(inputs[[column]]) || anyNA(inputs[[column]]))
      stop("Grouping and identity columns must contain text: ", column, call. = FALSE)
  }
  for (i in seq_len(nrow(inputs))) {
    parts <- strsplit(inputs$relative[i], "/", fixed = TRUE)[[1L]]
    groups <- unlist(inputs[i, group_columns, drop = FALSE], use.names = FALSE)
    if (length(parts) != length(group_columns) + 1L ||
        !identical(unname(utils::head(parts, -1L)), unname(as.character(groups))))
      stop("Input path differs from grouping metadata: ", inputs$relative[i], call. = FALSE)
    if (parts[1L] %in% c("report-assets", "reference-evidence", "code-list-evidence", "registry-evidence", "conversion-evidence", "logs"))
      stop("Input path uses a reserved publication directory", call. = FALSE)
  }
  named_list(resources, "resources")
  if (!all(jobs$resource %in% names(resources))) stop("Missing resource bundle", call. = FALSE)
  for (i in seq_len(nrow(jobs))) {
    bundle <- resources[[jobs$resource[i]]]
    if (!inherits(bundle$plans[[jobs$plan[i]]], "validation_plan"))
      stop("Missing rule plan for job: ", jobs$id[i], call. = FALSE)
  }
  jobs$model_version <- vapply(seq_len(nrow(jobs)), function(i) {
    rules <- resources[[jobs$resource[i]]]$plans[[jobs$plan[i]]]
    form.model::form_metadata(rules$model)$form$version
  }, character(1))
  inputs$model_version <- jobs$model_version[match(inputs$job, jobs$id)]
  model_versions <- unique(c(model_versions, jobs$model_version))
  if (!is.function(reader) || (!is.null(prepare_data) && !is.function(prepare_data)))
    stop("Supply a reader and an optional preparation function", call. = FALSE)
  config <- execution_config(config)
  text_value(directory, "directory")
  if (file.exists(directory)) stop("Batch directory already exists: ", directory, call. = FALSE)
  inputs$path <- vapply(inputs$path, normalizePath, character(1), mustWork = TRUE)
  if (any(dir.exists(inputs$path))) stop("Batch inputs must be files", call. = FALSE)
  dir.create(dirname(directory), recursive = TRUE, showWarnings = FALSE)
  directory <- file.path(normalizePath(dirname(directory), mustWork = TRUE), basename(directory))
  if (any(vapply(dirname(inputs$path), function(path)
      startsWith(paste0(directory, "/"), paste0(path, "/")), logical(1))))
    stop("Batch output must be outside input directories", call. = FALSE)
  stage <- tempfile(".plan-", tmpdir = dirname(directory))
  dir.create(file.path(stage, "resources"), recursive = TRUE)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  inputs$sha256 <- vapply(inputs$path, batch_file_hash, character(1))
  inputs$result <- file.path("validation", "results", inputs$relative, "result.rds")
  records <- list()
  for (i in seq_along(resources)) {
    key <- names(resources)[i]
    relative <- file.path("resources", paste0(i, ".rds"))
    saveRDS(resources[[i]], file.path(stage, relative))
    records[[key]] <- list(file = relative, sha256 = batch_file_hash(file.path(stage, relative)),
      references = attr(resources[[i]]$references, "provenance"))
  }
  jobs$forms <- vapply(jobs$id, function(id) sum(inputs$job == id), integer(1))
  packages <- unique(c("form.validation", "form.model", packages))
  versions <- stats::setNames(vapply(packages, function(package)
    as.character(utils::packageVersion(package)), character(1)), packages)
  plan <- structure(list(schema_version = 1L, created_at = batch_time(), directory = directory,
    files = inputs, jobs = jobs, resources = records, group_columns = group_columns,
    identity_columns = identity_columns, model_versions = model_versions, metadata = metadata,
    packages = versions, config = config, reader = reader, prepare_data = prepare_data),
    class = "validation_batch_plan")
  plan$id <- batch_plan_id(plan)
  saveRDS(plan, file.path(stage, "plan.rds"))
  write_batch_plan_json(plan, file.path(stage, "plan.json"))
  if (!file.rename(stage, directory)) stop("Cannot finalize batch plan: ", directory, call. = FALSE)
  plan
}

#' Read a saved validation batch plan
#' @param plan A `validation_batch_plan`, its directory or its local `plan.rds` file.
#' @return The verified plan, with its current local directory.
#' @details RDS files are local application artifacts; read only plans you trust.
#'   Loading does not scan inputs or reopen reference sources.
#' @export
read_validation_batch_plan <- function(plan) {
  if (is.character(plan) && length(plan) == 1L && !is.na(plan)) {
    path <- if (dir.exists(plan)) file.path(plan, "plan.rds") else plan
    path <- normalizePath(path, mustWork = TRUE)
    plan <- readRDS(path)
    plan$directory <- dirname(path)
  }
  if (!inherits(plan, "validation_batch_plan") || !identical(plan$schema_version, 1L) ||
      !identical(plan$id, batch_plan_id(plan))) stop("Invalid or modified batch plan", call. = FALSE)
  if (!dir.exists(plan$directory)) stop("Batch directory is missing", call. = FALSE)
  plan
}

#' Hash a batch artifact
#' @param path Path to an existing file.
#' @return A SHA-256 hexadecimal string computed from file bytes.
#' @keywords internal
batch_file_hash <- function(path) digest::digest(file = path, algo = "sha256")
#' Format the current batch timestamp
#' @return Current UTC time as `YYYY-MM-DDTHH:MM:SSZ`.
#' @keywords internal
batch_time <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
#' Compute the identity of a frozen batch plan
#' @param plan Verified local `validation_batch_plan`.
#' @return A SHA-256 string for the plan contents.
#' @details Excludes the stored ID and local directory. Callback bodies are normalized to source form so runtime byte compilation does not change the identity.
#' @keywords internal
batch_plan_id <- function(plan) {
  # R may compile callbacks after execution; bytecode is not a change of policy.
  # Hash their source bodies so an unchanged plan survives reuse in one session.
  for (name in c("reader", "prepare_data")) {
    if (is.function(plan[[name]])) body(plan[[name]]) <- body(plan[[name]])
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

# State updates replace a complete file; interrupted writes leave the prior state readable.
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

#' Encode a relative report URL
#' @param path One slash-separated relative path.
#' @return A URL-encoded string preserving directory separators.
#' @keywords internal
batch_url <- function(path) {
  paste(vapply(strsplit(path, "/", fixed = TRUE)[[1L]], utils::URLencode,
    character(1), reserved = TRUE), collapse = "/")
}

# Public navigation data contains only the explicit public columns.
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

# A dead local process may leave a lock after a session crash.
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
