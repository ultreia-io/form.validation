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

#' Coerce one scalar using an explicit declared-type policy
#' @param value Submitted cell value.
#' @param type Declared form field type.
#' @return A converted scalar, or the original value when conversion is unsafe
#'   or outside the supported policy. Missing values remain unchanged.
#' @keywords internal
coerce_validation_value <- function(value, type) {
  if (length(value) != 1L || !is.atomic(value) || is.na(value)) return(value)
  if (type == "string") return(as.character(value))
  if (type == "date" && inherits(value, "POSIXct") &&
      is.finite(as.numeric(value)) && as.numeric(value) %% 86400 == 0)
    return(as.Date(value, tz = "UTC"))
  if (!is.character(value) || is.object(value)) return(value)
  if (type == "date") return(coerce_calendar_date(value))
  if (type == "datetime") return(coerce_iso_datetime(value))
  if (type == "boolean") {
    text <- toupper(value)
    if (text %in% c("TRUE", "T", "YES", "Y")) return(TRUE)
    if (text %in% c("FALSE", "F", "NO", "N")) return(FALSE)
    return(value)
  }
  if (type %in% c("number", "integer", "latitude", "longitude") &&
      grepl("^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][+-]?[0-9]+)?$", value)) {
    number <- suppressWarnings(as.numeric(value))
    if (!is.finite(number)) return(value)
    # Do not turn a nonzero value below double precision into zero.
    mantissa <- sub("[eE].*$", "", value)
    if (number == 0 && grepl("[1-9]", mantissa)) return(value)
    if (type == "integer" && (number != trunc(number) || abs(number) > 2^53 - 1))
      return(value)
    return(number)
  }
  value
}

#' Normalize explicitly ordered calendar-date text
#' @param value One nonmissing character value.
#' @return Canonical YYYY-MM-DD text, or the original unsupported/invalid value.
#' @details Slash dates are always day-first; dotted dates are always year-first.
#'   Day and month require one or two digits; year requires exactly four digits.
#'   The canonical date must pass the model's strict calendar validation.
#' @keywords internal
coerce_calendar_date <- function(value) {
  if (grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", value)) {
    parts <- as.integer(strsplit(value, "/", fixed = TRUE)[[1L]])
    parts <- parts[c(3L, 2L, 1L)]
  } else if (grepl("^[0-9]{4}[.][0-9]{1,2}[.][0-9]{1,2}$", value)) {
    parts <- as.integer(strsplit(value, ".", fixed = TRUE)[[1L]])
  } else return(value)
  canonical <- sprintf("%04d-%02d-%02d", parts[1L], parts[2L], parts[3L])
  if (form.model::form_value_matches_type(canonical, "date")) canonical else value
}

#' Normalize an unambiguous ISO datetime to a UTC ISO string
#' @param value One nonmissing character value.
#' @return Canonical UTC text, a UTC POSIXct for fractional seconds, or the
#'   original value if unsupported or invalid.
#' @details Requires minute or second precision and Z or a numeric HH:MM offset.
#'   Offset magnitude cannot exceed 14 hours. The unknown offset -00:00 is
#'   preserved as invalid. Calendar and clock checks precede offset arithmetic.
#' @keywords internal
coerce_iso_datetime <- function(value) {
  pattern <- "^([0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2})(:[0-9]{2}([.][0-9]+)?)?(Z|[+-][0-9]{2}:[0-9]{2})$"
  parts <- regmatches(value, regexec(pattern, value))[[1]]
  if (!length(parts)) return(value)
  local <- paste0(parts[2], if (nzchar(parts[3])) sub("[.].*$", "", parts[3]) else ":00", "Z")
  if (!form.model::form_value_matches_type(local, "datetime")) return(value)
  zone <- parts[5]
  offset <- 0
  if (zone != "Z") {
    hours <- as.integer(substr(zone, 2, 3))
    minutes <- as.integer(substr(zone, 5, 6))
    if (zone == "-00:00" || hours > 14L || minutes > 59L ||
        (hours == 14L && minutes != 0L)) return(value)
    offset <- (hours * 60 + minutes) * 60 * if (startsWith(zone, "-")) -1 else 1
  }
  parsed <- as.POSIXct(strptime(local, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) - offset
  if (nzchar(parts[4])) return(parsed + as.numeric(paste0("0", parts[4])))
  canonical <- format(parsed, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  if (form.model::form_value_matches_type(canonical, "datetime")) canonical else value
}

#' Extract field values and coordinates
#' @param field Effective field definition obtained through form.model.
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
