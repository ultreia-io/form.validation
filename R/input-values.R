#' Explicitly prepare field values using declared form types
#'
#' Optional value conversion for callers that accept textual representations of
#' supplied values. No workbook reading or validation is performed.
#' @param data Named worksheet grids, as for [validate_data()].
#' @param model Checked form model from model.form.
#' @return Prepared grids with the same sheet order and dimensions. Matrices
#'   become data frames; converted columns contain lists of scalar values.
#' @details Nonmissing scalar values in string fields become character. Midnight
#'   POSIXct values in date fields become Date in UTC; nonmidnight timestamps are
#'   retained so type validation can report them. All other values, including
#'   blanks, nulls, whitespace, and textual leading zeros, are preserved.
#'   Table preambles and undeclared cells are not converted.
#'   This policy is never enabled implicitly. Use
#'   `validate_data(plan, data, prepare_data = function(data)
#'   prepare_validation_data(data, plan$model))` to run it after structure passes.
#' @export
prepare_validation_data <- function(data, model) {
  named_list(data, "data")
  for (name in names(data)) {
    grid <- data[[name]]
    if (!(is.data.frame(grid) || is.matrix(grid))) {
      stop("Each supplied sheet must be a data frame or matrix: ", name, call. = FALSE)
    }
    if (is.matrix(grid)) data[[name]] <- as.data.frame(grid)
  }
  layouts <- model.form::form_sheets(model)
  for (field in model.form::form_fields(model)) {
    if (!field$type %in% c("string", "date") || !field$sheet %in% names(data)) next
    grid <- data[[field$sheet]]
    if (field$column > ncol(grid)) next
    layout <- layouts[[field$sheet]]$layout
    rows <- if (layout$kind == "cells") field$row else which(seq_len(nrow(grid)) >= layout$start_row)
    rows <- rows[rows <= nrow(grid)]
    if (!length(rows)) next
    # Assign each column once instead of repeatedly copying it for every cell.
    column <- as.list(grid[[field$column]])
    column[rows] <- lapply(column[rows], function(value) {
      if (field$type == "string" && length(value) == 1L && !is.na(value)) {
        return(as.character(value))
      }
      if (field$type == "date" && inherits(value, "POSIXct") &&
          !is.na(value) && as.numeric(value) %% 86400 == 0) {
        return(as.Date(value, tz = "UTC"))
      }
      value
    })
    grid[[field$column]] <- column
    data[[field$sheet]] <- grid
  }
  data
}

#' Extract field values and coordinates
#' @param field Effective field definition obtained through model.form.
#' @param sheet Sheet definition.
#' @param data Named worksheet grids.
#' @param config Normalized execution configuration.
#' @return Physical coordinates, original values and a presence mask.
#' @keywords internal
field_cells <- function(field, sheet, data, config) {
  grid <- data[[field$sheet]]
  rows <- if (sheet$layout$kind == "cells") field$row else {
    if (nrow(grid) < sheet$layout$start_row) integer() else seq.int(sheet$layout$start_row, nrow(grid))
  }
  present <- rows <= nrow(grid) & field$column <= ncol(grid)
  original <- rep(list(NULL), length(rows))
  if (any(present)) {
    column <- if (is.data.frame(grid)) grid[[field$column]] else grid[, field$column]
    original[present] <- unname(as.list(column[rows[present]]))
  }
  if (config$blank_strings) {
    blank <- vapply(original, function(x) is.character(x) && length(x) == 1L && !is.na(x) && x == "", logical(1))
    present <- present & !blank
  }
  list(column = field$column, rows = rows, original = original, present = present)
}
