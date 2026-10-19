#' Read stored Excel values into worksheet grids
#'
#' Optional XLSX input adapter. Reading does not validate, convert declared field
#' types, or change the input file. [validate_data()] also accepts caller-supplied
#' grids without this adapter or its optional dependencies.
#' @param path Path to an XLSX workbook.
#' @param timed Optional function with arguments `stage` and `expression` that
#'   evaluates and returns `expression`. Called for `read.sheets` and
#'   `read.worksheets`; the default simply evaluates each expression.
#' @return Named list of data frames in workbook sheet order. Columns contain
#'   lists preserving individual cell types, anchored at physical cell A1.
#' @details Requires the optional packages readxl and cellranger. Leading blank
#'   rows and columns, whitespace, and textual leading zeros are retained.
#'   Dimensions follow cells containing data, ignoring formatting-only extent.
#'   Formula caches are read without recalculation. Excel errors and formulas
#'   without cached values become missing values. Reader warnings are errors.
#'   No form, reference source, or domain-specific configuration is required.
#' @export
read_validation_workbook <- function(path, timed = function(stage, expression) force(expression)) {
  text_value(path, "path")
  if (!is.function(timed)) stop("timed must be a function", call. = FALSE)
  for (package in c("readxl", "cellranger")) {
    if (!requireNamespace(package, quietly = TRUE)) stop("Excel reading requires ", package, call. = FALSE)
  }
  sheets <- timed("read.sheets", readxl::excel_sheets(path))
  data <- timed("read.worksheets", stats::setNames(lapply(sheets, function(sheet) {
    withCallingHandlers(as.data.frame(readxl::read_xlsx(path, sheet = sheet,
      range = cellranger::cell_limits(c(1, 1), c(NA, NA)), col_names = FALSE,
      col_types = "list", trim_ws = FALSE, na = character(), .name_repair = "minimal")),
      warning = function(w) stop(conditionMessage(w), call. = FALSE))
  }), sheets))
  data
}
