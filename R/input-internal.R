# Internal helpers ----------------------------------------------------------

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
    create_validation_finding("unused")[FALSE, ])
  structure(list(status = "failed", valid = NA, complete = FALSE, structure_passed = FALSE,
    findings = findings, execution = execution, model = form.model::form_metadata(plan$model),
    source = path, rules_version = plan$rules_version,
    metadata = plan$metadata), class = "validation_result")
}
