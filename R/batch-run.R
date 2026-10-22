#' Validate every input in a saved batch plan
#' @inheritParams read_validation_batch_plan
#' @param resume Reuse completed, checksum-verified results after an interruption.
#' @return Validation stage state, invisibly.
#' @details Reads only planned inputs and frozen resources. Saves each complete
#'   typed result and its form definition before moving to the next input.
#'   No report tables, CSV files or HTML pages are generated. Reader failures are
#'   ordinary failed validation results; infrastructure failures keep the stage
#'   incomplete and can be retried. Result RDS files are uncompressed for fast reuse.
#' @export
validate_batch <- function(plan, resume = TRUE) {
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
  log(sprintf("VALIDATE %d planned inputs; %d already completed", nrow(plan$files), sum(done)))
  for (resource in names(plan$resources)) {
    ids <- plan$jobs$id[plan$jobs$resource == resource]
    selected <- pending[pending$job %in% ids, , drop = FALSE]
    if (!nrow(selected)) next
    bundle <- read_batch_resources(plan, resource)
    for (i in seq_len(nrow(selected))) {
      file <- selected[i, ]
      file_started <- proc.time()[["elapsed"]]
      log(paste("START", file$relative))
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
      outcome$elapsed <- proc.time()[["elapsed"]] - file_started
      outcome$completed_at <- batch_time()
      state$outcomes[[file$relative]] <- outcome
      write_batch_state(state, state_path)
      log(paste(toupper(outcome$status), file$relative, sprintf("elapsed=%.3fs", outcome$elapsed), outcome$message))
    }
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

#' Generate individual reports from completed validation results
#' @inheritParams read_validation_batch_plan
#' @param report_filename HTML basename, ending in `.html`.
#' @inheritParams write_validation_report
#' @param overwrite Rebuild the complete reports directory from saved results.
#' @return The reports directory, invisibly.
#' @details Requires every planned input to have a saved result. Reads no input
#'   workbook or reference CSV. Uses the form saved with each result, so rendering
#'   does not depend on changes to installed form definitions.
#'   Reports are staged together and replace the previous report tree only after
#'   every form has rendered successfully. The batch plan and results stay intact.
#' @export
write_validation_batch_reports <- function(plan, report_filename = "index.html", overwrite = FALSE,
                                            title = "Validation report", author = NULL,
                                            stylesheet = NULL, logo = NULL) {
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
  for (i in seq_len(nrow(plan$files))) {
    file <- plan$files[i, ]
    file_started <- proc.time()[["elapsed"]]
    outcome <- validation$outcomes[[file$relative]]
    path <- file.path(plan$directory, file$result)
    if (is.null(outcome$result_sha256) || !identical(batch_file_hash(path), outcome$result_sha256))
      stop("Saved validation result changed: ", file$relative, call. = FALSE)
    saved <- readRDS(path)
    if (!identical(saved$plan_id, plan$id) || !identical(saved$input, file$relative) ||
        !identical(saved$input_sha256, file$sha256))
      stop("Saved result differs from the plan: ", file$relative, call. = FALSE)
    log(paste("REPORT", file$relative))
    report <- validation_report_data(saved$result, saved$model)
    depth <- length(strsplit(file$relative, "/", fixed = TRUE)[[1L]])
    write_validation_report(report, file.path(stage, file$relative, report_filename),
      title = if ("title" %in% names(file)) file$title else paste(title, "-", file$relative),
      author = author,
      assets_directory = paste0(strrep("../", depth), "report-assets"),
      stylesheet = stylesheet, logo = logo)
    outcomes[[file$relative]] <- list(status = saved$result$status,
      elapsed = proc.time()[["elapsed"]] - file_started)
    log(sprintf("REPORTED %s elapsed=%.3fs", file$relative, outcomes[[file$relative]]$elapsed))
    rm(saved, report)
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
