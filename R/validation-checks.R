#' Execute a generated workbook or sheet check
#' @param rule Generated descriptor.
#' @param context Validation inputs and cached field data.
#' @return Finding rows or `NULL`. Operational errors are handled by the runner.
#' @keywords internal
run_builtin <- function(rule, context) {
  data <- context$data
  config <- context$config
  sheets <- context$sheets
  findings <- list()
  if (rule$kind == "sheets") {
    expected <- names(sheets)
    if (!config$allow_extra_sheets) {
      for (name in setdiff(names(data), expected)) {
        findings[[length(findings) + 1L]] <- validation_finding("Unexpected sheet", sheet = name,
                                                              value = name, expected = expected)
      }
    }
    actual <- as.character(names(data)[names(data) %in% expected])
    order <- expected[expected %in% names(data)]
    if (!identical(unname(actual), unname(order))) {
      findings[[length(findings) + 1L]] <- validation_finding("Sheets are out of definition order",
                                                            value = actual, expected = order)
    }
    return(bind_findings(findings))
  }
  sheet <- sheets[[rule$sheet]]
  grid <- data[[rule$sheet]]
  if (rule$kind == "presence") {
    if (is.null(grid)) return(validation_finding("Mandatory sheet is absent",
      sheet = rule$sheet, value = FALSE, expected = TRUE))
    return(NULL)
  }
  if (!is.null(rule$field)) return(run_field(rule, context))
  if (rule$kind %in% c("primary_key_present", "primary_key_unique")) {
    return(run_primary_key(rule, context))
  }
  layout <- sheet$layout
  if (rule$kind == "row_count") {
    count <- max(0L, nrow(grid) - layout$start_row + 1L)
    return(validation_finding("Number of data rows in the table.",
      sheet = rule$sheet, value = count))
  }
  if (rule$kind == "empty_rows") {
    if (nrow(grid) < layout$start_row) return(NULL)
    rows <- seq.int(layout$start_row, nrow(grid))
    columns <- unique(vapply(form.model::form_fields(context$model, rule$sheet),
      function(field) as.integer(field$column), integer(1)))
    occupied <- rep(FALSE, length(rows))
    for (column in columns[columns <= ncol(grid)]) {
      values <- if (is.data.frame(grid)) grid[[column]] else grid[, column]
      occupied <- occupied | vapply(as.list(values[rows]), function(value) {
        length(value) > 0L && !all(is.na(value)) && !identical(value, "")
      }, logical(1))
    }
    filled <- which(occupied)
    if (length(filled) < 2L) return(NULL)
    gaps <- which(!occupied & seq_along(rows) > min(filled) & seq_along(rows) < max(filled))
    if (!length(gaps)) return(NULL)
    return(validation_findings("Empty row between populated rows", row = rows[gaps], sheet = rule$sheet))
  }
  if (rule$kind == "layout") {
    for (key in c("row_count", "column_count")) {
      actual <- if (key == "row_count") nrow(grid) else ncol(grid)
      if (!is.null(layout[[key]]) && actual != layout[[key]]) {
        dimension <- if (key == "row_count") "row" else "column"
        message <- sprintf("Expected %s count: %s; found: %s", dimension, layout[[key]], actual)
        findings[[length(findings) + 1L]] <- validation_finding(message,
          sheet = rule$sheet, value = actual, expected = layout[[key]])
      }
    }
    if (layout$kind == "cells" && is.null(layout$row_count)) {
      minimum <- minimum_cell_rows(context$model, rule$sheet)
      if (nrow(grid) < minimum) {
        message <- sprintf("Expected at least %s rows for declared cells; found: %s", minimum, nrow(grid))
        findings[[length(findings) + 1L]] <- validation_finding(message,
          sheet = rule$sheet, value = nrow(grid), expected = minimum)
      }
    }
    return(bind_findings(findings))
  }
  stop("Unsupported generated rule kind: ", rule$kind, call. = FALSE)
}

#' Execute an explicit primary-key rule
#' @param rule Generated descriptor with ordered key fields and local field names.
#' @param context Validation inputs and a cache cleared after preparation.
#' @return One finding per affected physical row, or an empty finding table.
#' @details Presence uses the configured missing-value policy. Uniqueness compares
#'   complete, correctly typed keys using native data-frame duplicate detection,
#'   without concatenating components. Every row of a duplicate group is reported.
#' @keywords internal
run_primary_key <- function(rule, context) {
  cache_key <- paste0("primary_key/", rule$sheet)
  if (!exists(cache_key, context$cache, inherits = FALSE)) {
    cells <- lapply(rule$key_fields, prepared_field, context = context)
    names(cells) <- unlist(rule$value, use.names = FALSE)
    context$cache[[cache_key]] <- list(cells = cells, rows = cells[[1L]]$rows,
      complete = Reduce(`&`, lapply(cells, `[[`, "filled")),
      typed = Reduce(`&`, lapply(cells, `[[`, "typed")))
  }
  key <- context$cache[[cache_key]]
  if (rule$kind == "primary_key_present") {
    bad <- which(!key$complete)
    message <- "Primary key contains missing values."
  } else {
    eligible <- which(key$complete & key$typed)
    columns <- lapply(key$cells, function(cells) {
      cells$values[match(eligible, cells$eligible)]
    })
    keys <- as.data.frame(columns, optional = TRUE, stringsAsFactors = FALSE)
    bad <- eligible[duplicated(keys) | duplicated(keys, fromLast = TRUE)]
    message <- "Primary key is not unique."
  }
  values <- lapply(bad, function(i) lapply(key$cells, function(cells) cells$original[[i]]))
  validation_findings(message, row = key$rows[bad], sheet = rule$sheet,
    value = values, expected = list(list(primary_key = unlist(rule$value, use.names = FALSE))))
}

#' Prepare code-list keys for reuse across submissions
#' @param references Named list of reference snapshots accepted by [`validate_data()`][form.validation::validate_data].
#' @param ids Character vector identifying code-list snapshots to prepare.
#'   Defaults to all supplied names. Exclude custom registry snapshots.
#' @return The supplied list with a private attribute containing prepared keys.
#'   Snapshot values, aliases, versions, and existing attributes are preserved.
#' @details Prepare once after loading a batch's reference snapshots, then pass
#'   the result to each [`validate_data()`][form.validation::validate_data] call. No data is read or rewritten.
#'   Keys include canonical codes and validated alias names. Missing or malformed
#'   snapshots are recorded, but signal incomplete or failed execution only when
#'   an eligible reference rule runs after the structure gate. Unused references
#'   do not fail a submission. Changed snapshots are detected and prepared anew
#'   by the engine; call this function again to share the updated preparation.
#' @examples
#' refs <- prepare_validation_references(list(stations = list(
#'   values = c("001", "002"), aliases = c(old = "001"))))
#' refs$stations$values
#' @export
prepare_validation_references <- function(references, ids = names(references)) {
  named_list(references, "references")
  if (is.null(ids)) ids <- character()
  if (!is.character(ids) || anyNA(ids) || any(!nzchar(ids))) {
    stop("ids must contain nonempty reference names", call. = FALSE)
  }
  ids <- unique(ids)
  prepared <- lapply(ids, function(id) {
    snapshot <- references[[id]]
    outcome <- tryCatch(list(keys = snapshot_keys(id, snapshot)),
                        error = function(e) list(error = e))
    c(list(snapshot = snapshot), outcome)
  })
  attr(references, "validation_keys") <- stats::setNames(prepared, ids)
  references
}

#' Get cached reference keys
#' @param id Reference identifier.
#' @param context Execution inputs and run-local cache.
#' @return Character keys, or an incomplete/failed execution condition.
#' @keywords internal
reference_keys <- function(id, context) {
  key <- paste0("reference/", id)
  if (exists(key, context$cache, inherits = FALSE)) return(context$cache[[key]])
  snapshot <- context$references[[id]]
  prepared <- attr(context$references, "validation_keys", exact = TRUE)[[id]]
  if (!is.null(prepared) && identical(prepared$snapshot, snapshot)) {
    if (!is.null(prepared$error)) stop(prepared$error)
    keys <- prepared$keys
  } else keys <- snapshot_keys(id, snapshot)
  context$cache[[key]] <- keys
  keys
}

#' Check a code list and extract its keys
#' @param id Reference identifier used in diagnostics.
#' @param snapshot Supplied list with `values` and optional `version` and `aliases`.
#' @return Character codes and accepted alias names. A missing snapshot signals
#'   incomplete execution; malformed values or alias targets raise an error.
#' @keywords internal
snapshot_keys <- function(id, snapshot) {
  if (is.null(snapshot)) validation_incomplete(paste("Missing reference snapshot:", id))
  if (!is.list(snapshot) || is.null(names(snapshot)) || anyDuplicated(names(snapshot)) ||
      any(!names(snapshot) %in% c("values", "version", "aliases")) || !"values" %in% names(snapshot)) {
    stop("Reference ", id, " must contain values and optional version and aliases", call. = FALSE)
  }
  values <- snapshot$values
  if (!(is.atomic(values) || is.list(values)) || is.data.frame(values) || !is.null(dim(values))) {
    stop("Reference values must be a vector or list of scalar keys: ", id, call. = FALSE)
  }
  keys <- lapply(seq_along(values), function(i) values[[i]])
  if (any(!vapply(keys, value_type, logical(1), type = "string"))) {
    stop("Reference keys must be strings: ", id, call. = FALSE)
  }
  keys <- vapply(keys, identity, character(1))
  aliases <- snapshot$aliases
  if (!is.null(aliases)) {
    if (!(is.character(aliases) || is.list(aliases)) || !is.null(dim(aliases)) ||
        (length(aliases) && (is.null(names(aliases)) || anyNA(names(aliases)) || any(!nzchar(names(aliases))))) ||
        any(!vapply(aliases, value_type, logical(1), type = "string"))) {
      stop("Reference aliases must map non-empty names to string codes: ", id, call. = FALSE)
    }
    targets <- vapply(as.list(aliases), identity, character(1))
    if (any(!targets %in% keys)) stop("Reference alias target is absent from values: ", id, call. = FALSE)
    keys <- unique(c(keys, names(aliases)))
  }
  keys
}

#' Execute a generated field check
#' @param rule Field rule descriptor.
#' @param context Explicit execution inputs.
#' @return Finding rows in physical row order, or an empty finding data frame.
#' @details Incorrectly typed cells are reported by their type check, without
#'   cascading bound, pattern, code-list, registry, or uniqueness errors.
#' @keywords internal
run_field <- function(rule, context) {
  field <- context$fields[[rule$field]]
  cells <- prepared_field(field, context)
  kind <- rule$kind
  value <- rule$value
  eligible <- cells$eligible
  values <- cells$values
  expected <- NULL
  if (kind == "mandatory") {
    bad <- which(!cells$filled)
    problem <- "Mandatory value is empty"; expected <- "filled"
  } else if (kind == "type") {
    bad <- which(cells$present & !cells$null & !cells$typed)
    problem <- switch(value,
      latitude = "Expected latitude: a finite number between -90 and 90 decimal degrees (inclusive)",
      longitude = "Expected longitude: a finite number between -180 and 180 decimal degrees (inclusive)",
      paste("Expected", value))
    expected <- value
  } else if (kind == "required_field") {
    dependency <- context$fields[[paste(field$sheet, value, sep = ".")]]
    bad <- which(prepared_field(dependency, context)$filled & !cells$filled)
    problem <- "Value is required by a filled companion field"; expected <- value
  } else if (kind == "enum") {
    allowed <- unlist(lapply(value, comparison_value, type = field$type), use.names = FALSE)
    bad <- eligible[!values %in% allowed]
    problem <- paste("Value is not in enum; allowed values:", paste(unlist(value), collapse = ", ")); expected <- value
  } else if (kind == "minimum") {
    bad <- eligible[values < value]
    problem <- "Value is below minimum"; expected <- value
  } else if (kind == "maximum") {
    bad <- eligible[values > value]
    problem <- "Value is above maximum"; expected <- value
  } else if (kind == "maximum_field") {
    companion <- context$fields[[paste(field$sheet, value, sep = ".")]]
    upper <- prepared_field(companion, context)
    rows <- intersect(eligible, upper$eligible)
    bad <- rows[values[match(rows, eligible)] > upper$values[match(rows, upper$eligible)]]
    problem <- paste("Value exceeds", value); expected <- value
  } else if (kind == "pattern") {
    bad <- eligible[!grepl(value, values, perl = TRUE)]
    problem <- "Value does not match pattern"; expected <- value
  } else if (kind %in% c("reference", "code_list")) {
    keys <- if (length(eligible)) reference_keys(value, context) else character()
    bad <- eligible[!values %in% keys]
    problem <- paste("Value not found in code list", value); expected <- value
  } else if (kind == "registry") {
    if (!length(eligible)) return(empty_findings())
    id <- if (is.null(value$column)) value$id else paste(value$id, value$column, sep = ".")
    snapshot <- context$references[[id]]
    if (is.null(snapshot)) validation_incomplete(paste("Missing registry snapshot:", id))
    if (!is.list(snapshot) || is.null(snapshot$values) ||
        !(is.atomic(snapshot$values) || is.list(snapshot$values)))
      stop("Registry snapshot needs values: ", id, call. = FALSE)
    aliases <- if (is.null(snapshot$aliases)) character() else snapshot$aliases
    index <- NULL
    for (candidate in snapshot$indexes) {
      if (identical(candidate$type, field$type) &&
          identical(candidate$matching, value$matching) &&
          identical(candidate$aliases, aliases)) {
        index <- candidate
        break
      }
    }
    if (is.null(index)) {
      cache_key <- paste0("registry/", rule$id)
      if (exists(cache_key, context$cache, inherits = FALSE)) index <- context$cache[[cache_key]]
      else {
        index <- registry_index(snapshot$values, field$type, value$matching, aliases)
        context$cache[[cache_key]] <- index
      }
    }
    counts <- registry_match_counts(index, cells$original[eligible])
    selected <- which(counts != 1L)
    bad <- eligible[selected]
    messages <- ifelse(counts[selected] == 0L,
      paste("Value not found in registry", id),
      paste("Value has", counts[selected], "matches in registry", paste0(id, "; expected exactly one")))
    return(validation_findings(messages, row = cells$rows[bad], sheet = field$sheet,
      field = field$id, column = cells$column, value = cells$original[bad],
      expected = list(list(reference = id, matches = 1L))))
  } else if (kind == "unique") {
    bad <- eligible[duplicated(values)]
    problem <- "Duplicate field value"; expected <- "unique"
  } else stop("Unsupported field check: ", kind, call. = FALSE)
  validation_findings(problem, row = cells$rows[bad], sheet = field$sheet, field = field$id,
                      column = cells$column, value = cells$original[bad], expected = list(expected))
}


#' Get the last declared cell row
#' @param model Checked form model.
#' @param sheet Name of a sheet with a cell layout.
#' @return Highest declared row as an integer, or zero when no cells are declared.
#' @keywords internal
minimum_cell_rows <- function(model, sheet) {
  fields <- form.model::form_fields(model, sheet)
  as.integer(max(c(0L, vapply(fields, `[[`, numeric(1), "row"))))
}

#' Identify explicit null cells
#' @param x One cell value.
#' @return `TRUE` for `NULL` or a scalar `NA`; `NaN` is an invalid number, not null.
#' @keywords internal
null_value <- function(x) {
  is.null(x) || (is.atomic(x) && length(x) == 1L && is.na(x) && !is.nan(x))
}

#' Check a submitted value's type
#' @param x One non-null cell value.
#' @param type Model field type.
#' @return Whether the value matches, without coercion.
#' @details Dates accept ISO strings or whole-day `Date` values. Datetimes accept
#'   UTC ISO strings or finite `POSIXct` values, including fractional seconds. Other classed values fail.
#'   Other values use `form.model::form_value_matches_type()`.
#' @keywords internal
value_type <- function(x, type) {
  if (length(x) != 1L || !is.atomic(x) || is.na(x)) return(FALSE)
  if (inherits(x, "Date")) return(type == "date" && is.finite(unclass(x)) && unclass(x) == floor(unclass(x)))
  if (inherits(x, "POSIXct")) return(type == "datetime" && is.finite(unclass(x)))
  form.model::form_value_matches_type(unname(x), type)
}

#' Normalize a typed value for comparisons
#' @param x A scalar already accepted by [`value_type()`][form.validation::value_type].
#' @param type Declared type.
#' @return A comparison scalar; dates become ISO strings and datetimes UTC seconds.
#' @keywords internal
comparison_value <- function(x, type) {
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (type == "datetime") {
    if (inherits(x, "POSIXct")) return(as.numeric(x))
    return(as.numeric(as.POSIXct(strptime(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))))
  }
  unname(x)
}

#' Compare two compatible declared values
#' @param x,y Scalars, including explicit nulls.
#' @param type Declared type for temporal normalization.
#' @return Logical equality. Numeric comparison is exact, without rounding tolerance.
#' @keywords internal
equal_value <- function(x, y, type) {
  if (null_value(x) || null_value(y)) return(null_value(x) && null_value(y))
  isTRUE(comparison_value(x, type) == comparison_value(y, type))
}

#' Cache field values for one validation run
#' @param field Effective field definition.
#' @param context Explicit execution inputs and a run-local cache.
#' @return Original values, physical coordinates, and filled/type masks.
#' @keywords internal
prepared_field <- function(field, context) {
  key <- paste0("field/", field$id)
  if (exists(key, context$cache, inherits = FALSE)) return(context$cache[[key]])
  cells <- field_cells(field, context$sheets[[field$sheet]], context$data, context$config)
  cells$null <- vapply(cells$original, null_value, logical(1))
  cells$filled <- cells$present & !cells$null
  cells$typed <- rep(FALSE, length(cells$rows))
  cells$typed[cells$filled] <- vapply(cells$original[cells$filled], value_type, logical(1), type = field$type)
  cells$eligible <- which(cells$typed)
  cells$values <- unlist(lapply(cells$original[cells$eligible], comparison_value, type = field$type), use.names = FALSE)
  context$cache[[key]] <- cells
  cells
}
