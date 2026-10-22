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
read_validation_workbook <- function(path, timed = function(stage, expression) force(expression), trim_ws = FALSE) {
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
