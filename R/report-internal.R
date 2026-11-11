# Internal helpers ---------------------------------------------------------

# Result tables and formatting ---------------------------------------------------------

#' Prepare shared CSV and report tables
#' @param result A validation result.
#' @return Summary and detail data frames, with values formatted as text.
#' @keywords internal
validation_result_tables <- function(result) {
  findings <- result$findings
  execution <- result$execution
  summary <- data.frame(input = result$source, model_version = result$model$form$version,
    rules_version = result$rules_version, status = result$status,
    structure_passed = result$structure_passed, generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    errors = sum(findings$severity == "error"), warnings = sum(findings$severity == "warning"), infos = sum(findings$severity == "info"),
    incomplete = sum(execution$status == "incomplete"), failed = sum(execution$status == "failed"),
    stringsAsFactors = FALSE)
  details <- data.frame(rule_id = findings$rule_id, kind = rep("validation", nrow(findings)),
    severity = findings$severity, sheet = findings$sheet, field = findings$field,
    row = findings$row, column = findings$column,
    value = vapply(findings$value, csv_value, character(1)),
    expected = vapply(findings$expected, csv_value, character(1)),
    message = findings$message, stringsAsFactors = FALSE)
  problems <- execution[execution$status %in% c("incomplete", "failed"), , drop = FALSE]
  if (nrow(problems)) {
    details <- rbind(details, data.frame(rule_id = problems$rule_id, kind = problems$status,
      severity = problems$severity, sheet = problems$sheet, field = problems$field,
      row = NA_integer_, column = NA_integer_, value = "", expected = "",
      message = problems$message, stringsAsFactors = FALSE))
  }
  details$stage <- report_stage_labels(execution$stage[match(details$rule_id, execution$rule_id)])
  details$sheet_order <- rep(NA_integer_, nrow(details))
  details$layout <- rep(NA_character_, nrow(details))
  details$field_label <- rep(NA_character_, nrow(details))
  list(summary = summary, details = details)
}

#' Format a value for CSV
#' @param value Original finding value or expectation.
#' @return One string. Scalar strings are unchanged; compound values use YAML.
#' @keywords internal
csv_value <- function(value) {
  if (is.null(value)) return("null")
  if (inherits(value, "Date")) value <- format(value, "%Y-%m-%d")
  if (inherits(value, "POSIXct")) value <- format(value, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  if (is.atomic(value) && length(value) == 1L) {
    if (is.nan(value)) return("NaN")
    if (is.na(value)) return("NA")
    if (is.logical(value)) return(if (value) "true" else "false")
    if (is.numeric(value)) return(format(value, digits = 17, trim = TRUE))
    return(as.character(value))
  }
  trimws(yaml::as.yaml(value))
}

#' Translate execution stages into report labels
#' @param stage Character stage codes: `structure`, `fields`, or `business`.
#' @return Unnamed labels in input order; unknown or missing codes return `NA`.
#' @keywords internal
report_stage_labels <- function(stage) {
  unname(c(structure = "Structure", fields = "Field", business = "Business")[stage])
}

#' Prepare counts for HTML cards
#' @param counts Named list or one-row data frame of numeric counts.
#' @return Scalar template values for formatted counts, raw values and empty states.
#' @keywords internal
report_count_values <- function(counts) {
  values <- list()
  for (key in names(counts)) {
    count <- as.numeric(counts[[key]])
    values[[paste0(key, "_count")]] <- format(count, big.mark = ",", scientific = FALSE, trim = TRUE)
    values[[paste0(key, "_value")]] <- format(count, scientific = FALSE, trim = TRUE)
    values[[paste0(key, "_empty")]] <- if (count == 0) "true" else "false"
  }
  values
}

# CSV output ---------------------------------------------------------

#' Write a report table as UTF-8 CSV with a BOM
#' @param data Report table.
#' @param path Destination CSV file.
#' @return No return value, called for file output.
#' @keywords internal
write_report_csv <- function(data, path) {
  connection <- file(path, open = "wt", encoding = "UTF-8")
  on.exit(close(connection))
  cat("\ufeff", file = connection)
  utils::write.csv(data, connection, row.names = FALSE, na = "")
}

# Templates and assets ---------------------------------------------------------

#' Render an installed HTML template
#' @param name Filename under the installed `report` directory.
#' @param values Named list of scalar character values or `NULL`.
#' @return Character vector of rendered HTML lines.
#' @details Escapes HTML characters. A NULL placeholder removes its complete line. Unknown placeholders and invalid values fail; inserted text is never processed again.
#' @keywords internal
render_report_template <- function(name, values) {
  template <- readLines(system.file("report", name, package = "form.validation", mustWork = TRUE),
                        warn = FALSE, encoding = "UTF-8")
  # Escape one template value
  # @param value One nonmissing character string.
  # @return HTML-escaped scalar text.
  escape <- function(value) {
    if (!is.character(value) || length(value) != 1L || is.na(value)) stop("Template values must be scalar text")
    for (pair in list(c("&", "&amp;"), c("<", "&lt;"), c(">", "&gt;"), c('"', "&quot;")))
      value <- gsub(pair[[1]], pair[[2]], value, fixed = TRUE)
    value
  }
  unlist(lapply(template, function(line) {
    matches <- gregexpr("\\{\\{[a-z_]+\\}\\}", line, perl = TRUE)
    tokens <- regmatches(line, matches)[[1L]]
    if (!length(tokens)) return(line)
    keys <- substring(tokens, 3L, nchar(tokens) - 2L)
    if (any(!keys %in% names(values))) stop("Unknown template placeholder")
    if (any(vapply(values[keys], is.null, logical(1)))) return(character())
    regmatches(line, matches) <- list(vapply(values[keys], escape, character(1)))
    line
  }), use.names = FALSE)
}

#' Copy content-addressed report assets
#' @param sources Named vector of existing source file paths.
#' @param directory Destination asset directory, created if needed.
#' @return Named filenames using an MD5 content hash and the original extension.
#' @details Reuses identical files and rejects conflicting contents. Returned names retain the source names for template lookup.
#' @keywords internal
copy_report_assets <- function(sources, directory) {
  if (any(!file.exists(sources))) stop("Report asset does not exist", call. = FALSE)
  if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE) && !dir.exists(directory))
    stop("Cannot create report asset directory", call. = FALSE)
  hashes <- unname(tools::md5sum(sources))
  filenames <- stats::setNames(paste0("report-", hashes, ".", tools::file_ext(sources)), names(sources))
  for (i in seq_along(sources)) {
    destination <- file.path(directory, filenames[[i]])
    if (file.exists(destination)) {
      if (!identical(unname(tools::md5sum(destination)), hashes[[i]]))
        stop("Conflicting report asset: ", destination, call. = FALSE)
    } else {
      # Complete the copy before another report worker can see the asset.
      temporary <- tempfile(".report-asset-", tmpdir = directory)
      if (!file.copy(sources[[i]], temporary)) stop("Cannot write report asset", call. = FALSE)
      moved <- file.rename(temporary, destination)
      if (!moved && (!file.exists(destination) ||
                     !identical(unname(tools::md5sum(destination)), hashes[[i]])))
        stop("Cannot write report asset", call. = FALSE)
      if (!moved) unlink(temporary)
    }
  }
  filenames
}
