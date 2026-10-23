#' Execute a validation plan against supplied worksheet data
#'
#' Run structural checks first. Field and business checks run only when the
#' complete structure stage passes. No workbook readers, network lookups, or
#' report rendering are invoked. Data, references, and configuration are caller-owned.
#' @param plan A plan from [`validation_plan()`][form.validation::validation_plan] or [`read_validation_rules()`][form.validation::read_validation_rules].
#'   Optional caller-owned `plan$metadata` is retained in the result unchanged.
#' @param data Named list of data frames or two-dimensional matrices, keyed by
#'   exact workbook sheet names. Retain all physical preamble rows.
#'   Column names and row names have no validation meaning. Use list columns or
#'   list matrices when a column contains mixed types; no implicit coercion is performed.
#' @param references Named list of snapshots. Each snapshot has `values` (a vector
#'   or list of string codes) and may include a caller-owned `version` label.
#'   Optional `aliases` map alternate codes to canonical `values`; both are accepted.
#'   Alias targets must exist in `values`; submitted values are not rewritten.
#'   Missing snapshots make affected rules incomplete; malformed snapshot values
#'   make them fail execution. Use [`prepare_validation_references()`][form.validation::prepare_validation_references] to reuse
#'   checked code-list keys across submissions.
#' @param source One diagnostic label identifying the input file or submission.
#' @param config Named list with optional `blank_strings` (default `FALSE`),
#'   `allow_extra_sheets` (default `TRUE`), and `custom` (default empty list).
#'   `blank_strings = TRUE` treats `""` as absent; whitespace is never trimmed.
#'   `custom` carries options for callbacks. Unknown configuration keys are errors.
#' @param prepare_data Optional function taking `data` and returning prepared grids.
#'   Called once after structure passes, before any content check. It must preserve
#'   sheet names, order and dimensions. Errors or warnings produce a failed
#'   `data/prepare` execution record and skip content. Successful preparation adds
#'   no rule record. The default `NULL` leaves supplied values unchanged.
#' @return A `validation_result` list containing:
#'   \describe{
#'     \item{status}{`"valid"`, `"invalid"`, `"incomplete"`, or `"failed"`.
#'       Failed execution takes precedence over incomplete execution and findings.}
#'     \item{valid}{`FALSE` if any error finding exists; otherwise `NA` when
#'       execution is unfinished, or `TRUE` when all applicable checks completed.}
#'     \item{complete}{Whether execution finished without incomplete or failed rules.}
#'     \item{findings}{Data frame with `rule_id`, `severity`, `source`, `message`,
#'       `sheet`, `field`, `row`, `column`, and list columns `value` and `expected`.
#'       Coordinates are one-based physical locations; `NA` means not applicable.}
#'     \item{execution}{Data frame with `rule_id`, `stage`, `severity`, `sheet`, `field`,
#'       `status`, and `message`.
#'       Status is `completed`, `skipped`, `incomplete`, or `failed`.}
#'     \item{model}{Identity, versions, and provenance from form.model's public API.}
#'     \item{source}{Input label, also retained for submissions without findings.}
#'     \item{structure_passed}{Whether the structure gate opened for content checks.}
#'     \item{rules_version}{Version from YAML rule files, or `NA` for direct plans.}
#'     \item{metadata}{Caller metadata from the plan, or `NULL` when absent.}
#'   }
#' @details Every declared sheet is mandatory. Its absence is a validation error,
#'   while extra sheets are ignored by default. Empty tables have no records to
#'   check. No minimum record count or preamble extent is inferred from `start_row`.
#'   Cell layouts without `row_count` require enough rows for their last declared cell;
#'   extra rows are allowed. An explicit `row_count` still requires an exact extent.
#'
#' Mandatory checks require a filled, non-null value. `NaN` is a type error.
#' Empty strings are values unless configured otherwise.
#' Conditional requiredness demands a filled non-null value. Nulls are ignored
#' by value constraints. Use mandatory or conditional requiredness to reject
#' empty values.
#'
#' Type failures suppress dependent value checks for that cell. Warnings and
#' errors from a rule mark execution failed. All structural checks finish, then
#' any structural error or unfinished check gates all content. Within content,
#' other checks continue, including business callbacks. Warning and
#' info findings do not make `valid` false, but incomplete execution never passes.
#' @section Errors:
#' Invalid API arguments raise errors before execution. Rule errors, warnings,
#' malformed callback results, and missing prerequisites are returned in execution
#' records rather than being confused with submitted-data validation failures.
#' @examples
#' model <- form.model::read_form_model("examples/survey-form.yaml", "form.model")
#' plan <- validation_plan(model)
#' data <- list(Metadata = matrix(vector("list", 15), 5, 3),
#'              Observations = matrix(vector("list", 66), 6, 11))
#' data$Metadata[[2, 3]] <- "001"
#' data$Metadata[[3, 3]] <- "2026-10-03"
#' data$Observations[5, ] <- as.list(c(
#'   "STATION", "COUNT", "LENGTH", "UNIT", "ACTIVE", "OBSERVED_AT", "LATITUDE",
#'   "LONGITUDE", "OBSERVATION_ID", "QUALITY", "OBSERVER"))
#' data$Observations[6, ] <- list(
#'   "001", 2L, NULL, NULL, TRUE, "2026-10-03T12:00:00Z",
#'   -20.5, 55.5, "OBS-001", 1L, "ALICE")
#' refs <- list(
#'   observers = list(values = "ALICE"),
#'   stations = list(values = c("001", "002"), version = "1"))
#' result <- validate_data(plan, data, refs, source = "sample.xlsx")
#' result
#' result$findings
#' @export
validate_data <- function(plan, data, references = list(), source = "<data>", config = list(), prepare_data = NULL) {
  if (!inherits(plan, "validation_plan") || !is.list(plan$rules) || !length(plan$rules)) {
    stop("plan must come from validation_plan()", call. = FALSE)
  }
  if (!is.null(prepare_data) && !is.function(prepare_data)) stop("prepare_data must be a function or NULL", call. = FALSE)
  named_list(data, "data")
  for (name in names(data)) {
    grid <- data[[name]]
    if (!(is.data.frame(grid) || is.matrix(grid))) {
      stop("Each supplied sheet must be a data frame or matrix: ", name, call. = FALSE)
    }
  }
  named_list(references, "references")
  text_value(source, "source")
  config <- execution_config(config)
  context <- list(model = plan$model, sheets = form.model::form_sheets(plan$model),
                  fields = form.model::form_fields(plan$model), data = data,
                  references = references, config = config,
                  cache = new.env(parent = emptyenv()))
  field_values <- function(id) {
    text_value(id, "field ID")
    field <- context$fields[[id]]
    if (is.null(field)) stop("Unknown field: ", id, call. = FALSE)
    c(list(sheet = field$sheet, field = id, type = field$type), prepared_field(field, context))
  }
  rows <- list()
  records <- list()
  structure_passed <- TRUE
  preparation_failed <- FALSE
  rules <- plan$rules
  if (!is.null(prepare_data)) {
    if (any(vapply(rules, function(rule) rule$id == "data/prepare", logical(1)))) {
      stop("data/prepare is reserved for data preparation", call. = FALSE)
    }
    structural <- vapply(rules, function(rule) rule$stage == "structure", logical(1))
    rules <- append(rules, list(list(id = "data/prepare", kind = "prepare",
      stage = "fields", severity = "error")), after = sum(structural))
  }
  for (rule in rules) {
    gated <- !identical(rule$stage, "structure") && (!structure_passed || preparation_failed)
    absent <- !identical(rule$kind, "custom") && !is.null(rule$sheet) &&
      rule$kind != "presence" && !rule$sheet %in% names(data)
    outcome <- if (gated) {
      list(status = "skipped", message = if (preparation_failed) "Data preparation failed" else
        "Structure validation did not pass", findings = NULL)
    } else if (absent) {
      list(status = "skipped", message = "Sheet is absent; see its presence check", findings = NULL)
    } else {
      tryCatch(withCallingHandlers({
        found <- if (identical(rule$kind, "prepare")) {
          prepared <- prepare_data(data)
          named_list(prepared, "prepared data")
          if (!identical(names(prepared), names(data)) || any(vapply(names(data), function(name) {
            grid <- prepared[[name]]
            !(is.data.frame(grid) || is.matrix(grid)) || !identical(dim(grid), dim(data[[name]]))
          }, logical(1)))) stop("Data preparation must preserve sheet names, order and dimensions", call. = FALSE)
          data <- prepared
          context$data <- prepared
          context$cache <- new.env(parent = emptyenv())
          NULL
        } else if (identical(rule$kind, "custom")) {
          rule_config <- config
          rule_config$rule <- rule$parameters
          rule_config$field_values <- field_values
          rule$check(model = plan$model, data = data, references = references, config = rule_config)
        } else run_builtin(rule, context)
        list(status = "completed", message = "", findings = check_findings(found))
      }, warning = function(w) stop(conditionMessage(w), call. = FALSE)),
      validation_incomplete = function(e) list(status = "incomplete", message = conditionMessage(e),
        sheet = e$sheet, field = e$field, findings = NULL),
      error = function(e) list(status = "failed", message = conditionMessage(e), findings = NULL))
    }
    if (identical(rule$kind, "prepare")) {
      if (outcome$status %in% c("completed", "skipped")) next
      preparation_failed <- TRUE
    }
    if (identical(rule$stage, "structure") &&
        (outcome$status %in% c("incomplete", "failed") ||
         (!is.null(outcome$findings) && nrow(outcome$findings) && rule$severity == "error"))) {
      structure_passed <- FALSE
    }
    sheet <- if (is.null(outcome$sheet)) rule$sheet else outcome$sheet
    field <- if (is.null(outcome$field)) rule$field else outcome$field
    records[[length(records) + 1L]] <- data.frame(rule_id = rule$id, stage = rule$stage, severity = rule$severity,
                                                sheet = if (is.null(sheet)) NA_character_ else sheet,
                                                field = if (is.null(field)) NA_character_ else field,
                                                status = outcome$status,
                                                message = outcome$message, stringsAsFactors = FALSE)
    if (!is.null(outcome$findings) && nrow(outcome$findings)) {
      n <- nrow(outcome$findings)
      rows[[length(rows) + 1L]] <- cbind(data.frame(rule_id = rep(rule$id, n),
        severity = rep(rule$severity, n), source = rep(source, n), stringsAsFactors = FALSE), outcome$findings)
    }
  }
  execution <- do.call(rbind, records)
  rownames(execution) <- NULL
  findings <- if (length(rows)) do.call(rbind, rows) else cbind(
    data.frame(rule_id = character(), severity = character(), source = character(), stringsAsFactors = FALSE),
    bind_findings(list()))
  rownames(findings) <- NULL
  complete <- all(execution$status %in% c("completed", "skipped"))
  has_errors <- any(findings$severity == "error")
  valid <- if (has_errors) FALSE else if (!complete) NA else TRUE
  status <- if (any(execution$status == "failed")) "failed" else if (!complete) "incomplete" else if (has_errors) "invalid" else "valid"
  structure(list(status = status, valid = valid, complete = complete,
                 structure_passed = structure_passed, findings = findings,
                 execution = execution, model = form.model::form_metadata(plan$model),
                 source = source, rules_version = plan$rules_version,
                 metadata = plan$metadata, reference_provenance = attr(references, "provenance")), class = "validation_result")
}

#' Normalize explicit execution options
#' @param config Named list of supported options.
#' @return A list with both Boolean policies and a custom options list.
#' @keywords internal
execution_config <- function(config) {
  named_list(config, "config")
  result <- list(blank_strings = FALSE, allow_extra_sheets = TRUE, custom = list())
  if (length(setdiff(names(config), names(result)))) stop("Unsupported configuration keys", call. = FALSE)
  for (key in names(config)) result[key] <- config[key]
  for (key in c("blank_strings", "allow_extra_sheets")) {
    if (!is.logical(result[[key]]) || length(result[[key]]) != 1L || is.na(result[[key]])) {
      stop(key, " must be TRUE or FALSE", call. = FALSE)
    }
  }
  named_list(result$custom, "config$custom")
  result
}

#' Print a validation result summary
#' @param x A result from [`validate_data()`][form.validation::validate_data].
#' @param ... Additional arguments, currently ignored.
#' @return `x`, invisibly. Writes the status and finding/execution counts.
#' @examples
#' model <- form.model::read_form_model("examples/survey-form.yaml", "form.model")
#' print(validate_data(validation_plan(model), list()))
#' @export
print.validation_result <- function(x, ...) {
  cat("<validation_result> ", x$status, "\n", sep = "")
  cat(nrow(x$findings), "findings;", sum(x$execution$status == "completed"), "completed;",
      sum(x$execution$status == "incomplete"), "incomplete;", sum(x$execution$status == "failed"), "failed\n")
  invisible(x)
}
