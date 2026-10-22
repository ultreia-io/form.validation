#' Read and validate a workbook without generating reports
#' @param path Input workbook path, left unchanged.
#' @param plan A rule plan from [validation_plan()] or [read_validation_rules()].
#' @param references Explicit reference snapshots, reusable across inputs.
#' @param source Diagnostic input label; defaults to `path`.
#' @param reader Function accepting `path` and `timed`, returning worksheet grids.
#' @param prepare_data Function accepting `data` and `model`, called after structure
#'   passes. Defaults to [prepare_validation_data()]; NULL leaves grids unchanged.
#' @param log Function receiving timing lines; defaults to [message()].
#' @inheritParams validate_data
#' @return A typed `validation_result`. Reader failures are saved as failed results.
#' @details No CSV or HTML is written. Reading, preparation and validation have
#'   separate timings. Configuration is supplied explicitly by the caller.
#' @export
validate_workbook <- function(path, plan, references = list(), source = path, config = list(),
                              reader = read_validation_workbook,
                              prepare_data = prepare_validation_data, log = message) {
  if (!inherits(plan, "validation_plan")) stop("Supply a validation_plan", call. = FALSE)
  if (!is.function(reader) || !is.function(log) ||
      (!is.null(prepare_data) && !is.function(prepare_data)))
    stop("Invalid reader, preparation function or logger", call. = FALSE)
  conversion_elapsed <- 0
  timed <- function(stage, expression) {
    started <- proc.time()[["elapsed"]]
    on.exit({
      elapsed <- proc.time()[["elapsed"]] - started
      if (stage == "conversion") conversion_elapsed <<- elapsed
      if (stage == "validation") elapsed <- elapsed - conversion_elapsed
      log(sprintf("TIMING %s %s=%.3fs", source, stage, elapsed))
    })
    force(expression)
  }
  read <- tryCatch(timed("read", withCallingHandlers(list(data = reader(path, timed)),
    warning = function(w) stop(conditionMessage(w), call. = FALSE))),
                   error = function(e) list(error = conditionMessage(e)))
  if (!is.null(read$error)) {
    log(paste("TIMING", source, "validation=skipped (read failed)"))
    result <- workbook_read_failure(plan, source, gsub(path, source, read$error, fixed = TRUE))
  } else {
    result <- timed("validation", validate_data(plan, read$data, references,
      source = source, config = config,
      prepare_data = if (is.null(prepare_data)) NULL else
        function(data) timed("conversion", prepare_data(data, plan$model))))
    if (!result$structure_passed) log(paste("TIMING", source, "conversion=skipped (structure did not pass)"))
  }
  result
}

#' Represent an unreadable input without executing its validation rules
#' @param plan Validation plan whose rules are recorded as skipped.
#' @param path Input label retained in the result; may be relative to the input root.
#' @param message Reader failure explanation.
#' @return A failed `validation_result` with an `input/read` execution record,
#'   empty findings, and a closed structure gate.
#' @keywords internal
workbook_read_failure <- function(plan, path, message) {
  execution <- do.call(rbind, lapply(plan$rules, function(rule) data.frame(
    rule_id = rule$id, stage = rule$stage, severity = rule$severity,
    sheet = if (is.null(rule$sheet)) NA_character_ else rule$sheet,
    field = if (is.null(rule$field)) NA_character_ else rule$field,
    status = "skipped", message = "Workbook could not be read", stringsAsFactors = FALSE)))
  execution <- rbind(data.frame(rule_id = "input/read", stage = "structure", severity = "error",
    sheet = NA_character_, field = NA_character_, status = "failed", message = message), execution)
  findings <- cbind(data.frame(rule_id = character(), severity = character(), source = character()),
    validation_finding("unused")[FALSE, ])
  structure(list(status = "failed", valid = NA, complete = FALSE, structure_passed = FALSE,
    findings = findings, execution = execution, model = model.form::form_metadata(plan$model),
    source = path, rules_version = plan$rules_version,
    metadata = plan$metadata), class = "validation_result")
}
