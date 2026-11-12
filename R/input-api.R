# Public operations ---------------------------------------------------------

#' Read stored Excel values into worksheet grids
#'
#' Read XLSX input. Reading does not validate, convert declared field
#' types, or change the input file. [`validate_data()`][form.validation::validate_data] also accepts caller-supplied
#' grids directly.
#' @param path Path to an XLSX workbook.
#' @param timed Optional function with arguments `stage` and `expression` that
#'   evaluates and returns `expression`. Called for `read.sheets` and
#'   `read.worksheets`; the default simply evaluates each expression.
#' @param trim_ws Let readxl trim leading and trailing whitespace in text cells?
#'   Defaults to FALSE; native cell types and internal whitespace are retained.
#' @return Named list of data frames in workbook sheet order. Columns contain
#'   lists preserving individual cell types, anchored at physical cell A1.
#' @details Uses readxl and cellranger. Leading blank
#'   rows and columns and textual leading zeros are retained. Text whitespace
#'   is preserved unless `trim_ws = TRUE` is supplied.
#'   Dimensions follow cells containing data, ignoring formatting-only extent.
#'   Formula caches are read without recalculation. Excel errors and formulas
#'   without cached values become missing values. Reader warnings are errors.
#'   No form, reference source, or domain-specific configuration is required.
#' @export
read_workbook_data <- function(path, timed = function(stage, expression) force(expression), trim_ws = FALSE) {
  text_value(path, "path")
  if (!is.function(timed)) stop("timed must be a function", call. = FALSE)
  if (!is.logical(trim_ws) || length(trim_ws) != 1L || is.na(trim_ws))
    stop("trim_ws must be TRUE or FALSE", call. = FALSE)
  sheets <- timed("read.sheets", readxl::excel_sheets(path))
  data <- timed("read.worksheets", stats::setNames(lapply(sheets, function(sheet) {
    withCallingHandlers(as.data.frame(readxl::read_xlsx(path, sheet = sheet,
      range = cellranger::cell_limits(c(1, 1), c(NA, NA)), col_names = FALSE,
      col_types = "list", trim_ws = trim_ws, na = character(), .name_repair = "minimal")),
      warning = function(w) stop(conditionMessage(w), call. = FALSE))
  }), sheets))
  data
}

#' Explicitly prepare field values using declared form types
#'
#' Optional value conversion for callers that accept textual representations of
#' supplied values. No workbook reading or validation is performed.
#' @param data Named worksheet grids, as for [`validate_data()`][form.validation::validate_data].
#' @param model Checked form model from form.model.
#' @param na_strings Text tokens interpreted as missing, or a function receiving
#'   an effective field definition and returning its tokens. Defaults to none.
#' @param na_ignore_case Match missing-text tokens without case distinctions.
#'   Defaults to FALSE. Matching uses complete values, without trimming.
#' @return Prepared grids with the same sheet order and dimensions. Matrices
#'   become data frames; converted columns contain lists of scalar values.
#' @details Nonmissing scalar values in string fields become character. Midnight
#'   POSIXct values in date fields become Date in UTC; nonmidnight timestamps are
#'   retained so type validation can report them. Numeric fields accept decimal
#'   text with an optional sign and exponent, without separators or whitespace.
#'   Integer conversion requires a whole value within the exact integer range
#'   of a double. Boolean text accepts TRUE/T/YES/Y and FALSE/F/NO/N, ignoring case;
#'   numeric values and numeric text are never interpreted as booleans.
#'   Coordinate text uses numeric conversion; coordinate bounds remain validation
#'   checks. Invalid or ambiguous values remain unchanged for validation.
#'   ISO datetimes with Z or an explicit UTC offset are normalized to UTC,
#'   adding zero seconds when only minutes are supplied. Calendar dates and
#'   clock components must round-trip exactly. Dates already use YYYY-MM-DD.
#'   Fractional seconds are retained. Date fields also accept day-first D/M/YYYY
#'   and year-first YYYY.M.D, with optional leading zeros on day and month.
#'   Four-digit years and valid calendar dates are required; month-first dates,
#'   placeholders, partial dates and missing zones are not inferred.
#'   No trimming is performed here.
#'   Table preambles and undeclared cells are not converted.
#'   This policy is never enabled implicitly. Use
#'   `validate_data(plan, data, prepare_data = function(data)
#'   prepare_validation_data(data, plan$model))` to run it after structure passes.
#' @export
prepare_validation_data <- function(data, model, na_strings = character(), na_ignore_case = FALSE) {
  if (!is.logical(na_ignore_case) || length(na_ignore_case) != 1L || is.na(na_ignore_case))
    stop("na_ignore_case must be TRUE or FALSE", call. = FALSE)
  if (!is.function(na_strings) && (!is.character(na_strings) || anyNA(na_strings)))
    stop("na_strings must supply character tokens without missing values", call. = FALSE)
  named_list(data, "data")
  for (name in names(data)) {
    grid <- data[[name]]
    if (!(is.data.frame(grid) || is.matrix(grid))) {
      stop("Each supplied sheet must be a data frame or matrix: ", name, call. = FALSE)
    }
    if (is.matrix(grid)) data[[name]] <- as.data.frame(grid)
  }
  layouts <- form.model::form_sheets(model)
  for (field in form.model::form_fields(model)) {
    if (!field$sheet %in% names(data)) next
    grid <- data[[field$sheet]]
    if (field$column > ncol(grid)) next
    layout <- layouts[[field$sheet]]$layout
    rows <- if (layout$kind == "cells") field$row else which(seq_len(nrow(grid)) >= layout$start_row)
    rows <- rows[rows <= nrow(grid)]
    if (!length(rows)) next
    tokens <- if (is.function(na_strings)) na_strings(field) else na_strings
    if (!is.character(tokens) || anyNA(tokens))
      stop("Missing-value tokens must be character values without NA: ", field$id, call. = FALSE)
    if (na_ignore_case) tokens <- toupper(tokens)
    # Assign each column once instead of repeatedly copying it for every cell.
    column <- as.list(grid[[field$column]])
    column[rows] <- lapply(column[rows], function(value) {
      if (is.character(value) && length(value) == 1L && !is.na(value) &&
          (if (na_ignore_case) toupper(value) else value) %in% tokens) return(NA_character_)
      coerce_validation_value(value, field$type)
    })
    grid[[field$column]] <- column
    data[[field$sheet]] <- grid
  }
  data
}

#' Read and validate a workbook without generating reports
#' @param path Input workbook path, left unchanged.
#' @param plan A rule plan from [`create_validation_plan()`][form.validation::create_validation_plan] or [`read_validation_plan_from_rules()`][form.validation::read_validation_plan_from_rules].
#' @param references Explicit reference snapshots, reusable across inputs.
#' @param source Diagnostic input label; defaults to `path`.
#' @param reader Function accepting `path` and `timed`, returning worksheet grids.
#' @param prepare_data Function accepting `data` and `model`, called after structure
#'   passes. Defaults to [`prepare_validation_data()`][form.validation::prepare_validation_data]; NULL leaves grids unchanged.
#' @param log Function receiving timing lines; defaults to [message()].
#' @inheritParams validate_data
#' @return A typed `validation_result`. Reader failures are saved as failed results.
#' @details No CSV or HTML is written. Reading, preparation and validation have
#'   separate timings. Configuration is supplied explicitly by the caller.
#' @export
validate_workbook <- function(path, plan, references = list(), source = path, config = list(),
                              reader = read_workbook_data,
                              prepare_data = prepare_validation_data, log = message) {
  if (!inherits(plan, "validation_plan")) stop("Supply a validation_plan", call. = FALSE)
  if (!is.function(reader) || !is.function(log) ||
      (!is.null(prepare_data) && !is.function(prepare_data)))
    stop("Invalid reader, preparation function or logger", call. = FALSE)
  conversion_elapsed <- 0
  # Time and log a workbook processing stage
  # @param stage Stage label used in the timing log.
  # @param expression Delayed expression to evaluate.
  # @return The expression result; elapsed time is logged even when evaluation fails.
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
