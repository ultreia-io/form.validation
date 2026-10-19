#' Create one finding for a custom rule
#'
#' Describe one validation failure. The runner adds the rule ID, severity, and
#' source label; callbacks cannot override those fields. Combine findings with
#' `rbind()`, or return `NULL` when a rule finds no problem.
#' @param message One nonempty explanation of the problem.
#' @param sheet Workbook sheet name, or `NA_character_` for a whole-input finding.
#' @param field Model field ID, or `NA_character_` when not applicable.
#' @param row,column One-based physical coordinates, or `NA_integer_`.
#' @param value,expected Relevant actual and expected values. Stored unchanged
#'   in list columns, including `NULL`, dates, and vectors.
#' @return A one-row data frame with `message`, `sheet`, `field`, `row`, `column`,
#'   `value`, and `expected`. It is the return contract for custom rule callbacks.
#' @examples
#' validation_finding("Count exceeds capacity", sheet = "Observations",
#'                    field = "count", row = 2, column = 3,
#'                    value = 12L, expected = 10L)
#' @export
validation_finding <- function(message, sheet = NA_character_, field = NA_character_,
                               row = NA_integer_, column = NA_integer_,
                               value = NULL, expected = NULL) {
  text_value(message, "message")
  for (name in c("sheet", "field")) {
    x <- get(name)
    if (!is.character(x) || length(x) != 1L || (!is.na(x) && !nzchar(x))) {
      stop(name, " must be a name or NA_character_", call. = FALSE)
    }
  }
  for (name in c("row", "column")) {
    x <- get(name)
    if (!is.numeric(x) || length(x) != 1L ||
        (!is.na(x) && (!is.finite(x) || x < 1 || x != floor(x) || x > .Machine$integer.max))) {
      stop(name, " must be a positive integer or NA_integer_", call. = FALSE)
    }
  }
  result <- data.frame(message = message, sheet = sheet, field = field,
                       row = as.integer(row), column = as.integer(column),
                       stringsAsFactors = FALSE)
  result$value <- I(list(value))
  result$expected <- I(list(expected))
  result
}

#' Create a batch of findings without constructing individual rows
#' @param message Character messages, one or one per finding.
#' @param row Physical row numbers; their length determines the number of findings.
#' @param sheet,field Character names or `NA`, one or one per finding.
#' @param column Physical column numbers or `NA`, one or one per finding.
#' @param value,expected Lists of original values, one or one per finding.
#'   Wrap a shared vector or structured expectation in `list(...)`.
#' @return A finding data frame with the same columns as [validation_finding()].
#'   Empty `row` produces an empty finding table. Values and order are preserved.
#' @examples
#' validation_findings("Unknown code", row = c(6L, 9L), sheet = "Observations",
#'   field = "station", column = 1L, value = list("X", "Y"), expected = list("stations"))
#' @export
validation_findings <- function(message, row, sheet = NA_character_, field = NA_character_,
                                column = NA_integer_, value = list(NULL), expected = list(NULL)) {
  n <- length(row)
  recycle <- function(x) {
    if (length(x) == n) return(unname(x))
    if (length(x) == 1L) return(rep(x, n))
    stop("Finding columns must have length one or length(row)", call. = FALSE)
  }
  if (!is.list(value) || !is.list(expected) || is.data.frame(value) || is.data.frame(expected)) {
    stop("value and expected must be lists", call. = FALSE)
  }
  result <- data.frame(message = recycle(message), sheet = recycle(sheet), field = recycle(field),
                       row = row, column = recycle(column), stringsAsFactors = FALSE)
  result$value <- I(recycle(value))
  result$expected <- I(recycle(expected))
  check_findings(result)
}

#' Create an empty finding table
#' @return Zero-row finding data frame, with list columns for values and expectations.
#' @keywords internal
empty_findings <- function() {
  structure(list(message = character(), sheet = character(), field = character(),
    row = integer(), column = integer(), value = I(list()), expected = I(list())),
    class = "data.frame", row.names = integer())
}

#' Signal that a rule lacks information needed to finish
#'
#' Use inside a custom rule when its prerequisites are unavailable. The runner
#' records incomplete execution, rather than treating unverified data as valid.
#' Ordinary errors and warnings are recorded as failed execution instead.
#' @param message One nonempty explanation of the missing prerequisite.
#' @return Does not return; signals a `validation_incomplete` error condition.
#' @examples
#' tryCatch(validation_incomplete("Reference snapshot unavailable"),
#'          validation_incomplete = function(e) conditionMessage(e))
#' @export
validation_incomplete <- function(message) {
  text_value(message, "message")
  stop(structure(list(message = message, call = NULL),
                 class = c("validation_incomplete", "error", "condition")))
}

#' Check one nonempty character argument
#' @param x Value to check.
#' @param name Argument name for the error message.
#' @return Invisibly `NULL`, or an argument error.
#' @keywords internal
text_value <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(name, " must be one nonempty string", call. = FALSE)
  }
  invisible(NULL)
}

#' Check a uniquely named list argument
#' @param x List to check; empty lists are allowed.
#' @param name Argument name for errors.
#' @return Invisibly `NULL`, or an argument error.
#' @keywords internal
named_list <- function(x, name) {
  if (!is.list(x) || is.data.frame(x) || (length(x) &&
      (is.null(names(x)) || anyNA(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x))))) {
    stop(name, " must be a list with unique nonempty names", call. = FALSE)
  }
  invisible(NULL)
}

#' Combine finding tables
#' @param rows List of finding data frames, possibly empty.
#' @return A finding data frame, with list columns preserved.
#' @keywords internal
bind_findings <- function(rows) {
  if (!length(rows)) return(empty_findings())
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

#' Verify a custom callback's finding contract
#' @param x Callback return value: `NULL` or a finding data frame.
#' @return A normalized finding data frame, or an error caught by the runner.
#' @keywords internal
check_findings <- function(x) {
  if (is.null(x)) return(empty_findings())
  columns <- c("message", "sheet", "field", "row", "column", "value", "expected")
  if (!is.data.frame(x) || !identical(names(x), columns) ||
      !is.list(x$value) || !is.list(x$expected)) {
    stop("Custom rules must return NULL or rows from validation_finding()", call. = FALSE)
  }
  if (!nrow(x)) return(empty_findings())
  if (any(vapply(x[columns[1:5]], function(column) !is.null(dim(column)), logical(1)))) {
    stop("Finding metadata must be vectors", call. = FALSE)
  }
  if (!is.character(x$message) || anyNA(x$message) || any(!nzchar(x$message))) {
    stop("message must contain nonempty strings", call. = FALSE)
  }
  for (name in c("sheet", "field")) {
    values <- x[[name]]
    if (!is.character(values) || any(!is.na(values) & !nzchar(values))) {
      stop(name, " must contain names or NA_character_", call. = FALSE)
    }
  }
  for (name in c("row", "column")) {
    values <- x[[name]]
    if (!is.numeric(values) || any(!is.na(values) &
        (!is.finite(values) | values < 1 | values != floor(values) | values > .Machine$integer.max))) {
      stop(name, " must contain positive integers or NA", call. = FALSE)
    }
    x[[name]] <- as.integer(values)
  }
  x$value <- I(unname(x$value))
  x$expected <- I(unname(x$expected))
  class(x) <- "data.frame"
  rownames(x) <- NULL
  x
}
