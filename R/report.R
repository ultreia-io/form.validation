#' Prepare one input's validation report
#'
#' Prepare plain tables once for HTML output or a future interactive consumer.
#' No workbook is read and no validation rule is executed.
#' @param result A result from [validate_data()].
#' @param model The form used to validate this input, supplying sheet order and layout.
#' @return A `validation_report` list with `tables`, `summary`, `structure_passed`,
#'   `sheets`, `execution`, and `generated_at`. Each sheet has its name, finding
#'   counts, a summary, and details. Values are readable text; the original
#'   result retains its types. Execution contains only failed or incomplete checks.
#' @details Table-sheet summaries group findings by field, column, value,
#'   severity, stage, and message, counting occurrences. Every sheet retains
#'   individual findings in Validation messages, including fixed-cell sheets. Whole-workbook findings have a
#'   separate section.
#'   Incomplete and failed execution remain separate from validation findings.
#'   Pass the report to [write_validation_results()] to reuse its formatted
#'   `tables` for CSV export without preparing them again.
#' @examples
#' model <- model.form::read_form_model("examples/survey-form.yaml", "model.form")
#' result <- validate_data(validation_plan(model), list(), source = "submission.xlsx")
#' report <- validation_report_data(result, model)
#' report$summary
#' @export
validation_report_data <- function(result, model) {
  if (!inherits(result, "validation_result")) stop("result must come from validate_data()", call. = FALSE)
  if (!identical(result$model, model.form::form_metadata(model))) {
    stop("Report form differs from validation result", call. = FALSE)
  }
  tables <- validation_result_tables(result)
  sheets <- model.form::form_sheets(model)
  tables$details$sheet_order <- match(tables$details$sheet, names(sheets))
  tables$details$layout <- vapply(tables$details$sheet, function(name) {
    if (is.na(name) || !name %in% names(sheets)) "table" else sheets[[name]]$layout$kind
  }, character(1))
  tables$details$field_label <- vapply(seq_len(nrow(tables$details)), function(i) {
    row <- tables$details[i, ]
    fields <- if (!is.na(row$sheet) && row$sheet %in% names(sheets)) model.form::form_fields(model, row$sheet) else list()
    if (!is.na(row$field) && row$field %in% names(fields)) fields[[row$field]]$name else row$field
  }, character(1))
  findings <- tables$details[tables$details$kind == "validation", , drop = FALSE]
  findings$stage <- result$execution$stage[match(findings$rule_id, result$execution$rule_id)]
  findings$stage <- report_stage_labels(findings$stage)
  sheets <- model.form::form_sheets(model)
  names_in_result <- unique(findings$sheet[!is.na(findings$sheet)])
  order <- c(names(sheets), setdiff(names_in_result, names(sheets)))
  if (anyNA(findings$sheet)) order <- c(order, NA_character_)
  rows <- split(seq_len(nrow(findings)), match(findings$sheet, order))
  sections <- lapply(seq_along(order), function(i) {
    name <- order[[i]]
    data <- findings[rows[[as.character(i)]], , drop = FALSE]
    if (!nrow(data)) return(NULL)
    fields <- if (!is.na(name) && name %in% names(sheets)) model.form::form_fields(model, name) else list()
    labels <- vapply(fields, `[[`, character(1), "name")
    matched <- match(data$field, names(labels))
    data$column_name <- ifelse(is.na(matched), data$field, unname(labels[matched]))
    details <- data[c("severity", "stage", "column_name", "column", "row", "value", "message")]
    summary <- details[setdiff(names(details), "row")]
    fixed <- !is.na(name) && name %in% names(sheets) && sheets[[name]]$layout$kind == "cells"
    if (fixed) {
      summary <- summary[c("severity", "stage", "column_name", "value", "message")]
      names(summary)[3:4] <- c("meta_name", "meta_value")
    } else {
      codes <- lapply(summary, function(x) match(x, unique(x)))
      keys <- do.call(paste, c(codes, sep = ":"))
      groups <- match(keys, unique(keys))
      summary <- summary[!duplicated(groups), , drop = FALSE]
      summary$count <- tabulate(groups)
    }
    rownames(summary) <- NULL
    if (!is.null(details)) rownames(details) <- NULL
    list(name = if (is.na(name)) "Workbook" else name,
         errors = sum(data$severity == "error"), warnings = sum(data$severity == "warning"),
         summary = summary, details = details)
  })
  execution <- result$execution[result$execution$status %in% c("failed", "incomplete"),
    c("severity", "stage", "sheet", "field", "rule_id", "status", "message"), drop = FALSE]
  execution$stage <- report_stage_labels(execution$stage)
  structure(list(tables = tables, summary = tables$summary, structure_passed = result$structure_passed,
                 sheets = Filter(Negate(is.null), sections), execution = execution,
                 generated_at = tables$summary$generated_at),
            class = "validation_report")
}

#' Write one input's validation results as two CSV files
#'
#' Create `summary.csv` with one row and `findings.csv` with one row per finding
#' or incomplete/failed check. Successful checks produce no detail rows.
#' @param result A result from [validate_data()] or a prepared
#'   [validation_report_data()] report. A report reuses its formatted tables.
#' @param directory Output folder for this one input. Created if necessary.
#' @param overwrite Whether to replace existing CSV outputs. Defaults to `FALSE`.
#' @return A named character vector with `summary` and `findings` paths, invisibly.
#' @details Summary columns are `input`, `model_version` (`form.version`),
#'   `rules_version`, `status`, `structure_passed`, `generated_at` (UTC),
#'   `errors`, `warnings`, `incomplete`, and `failed`.
#'   Errors and warnings count validation findings, not execution problems.
#'   Direct in-memory plans have no rule version and leave that cell empty.
#'
#' Detail columns are `rule_id`, `kind`, `severity`, `sheet`, `field`, `row`,
#'   `column`, `value`, `expected`, `message`, and `stage`. Prepared reports also
#'   populate `sheet_order`, `layout`, and `field_label` for browser presentation;
#'   these columns are blank when exporting a raw validation result.
#'   Kind is `validation`,
#'   `incomplete`, or `failed`. Execution problems retain the rule's severity and
#'   known sheet/field; no cell coordinate is invented. Empty locations are blank.
#'   An input without problems has a header-only findings file.
#'
#' Files use UTF-8, comma separators, decimal points, and standard CSV quoting.
#'   Scalar values become readable text; null is `null`, scalar NA is `NA`, and
#'   compound values use YAML text inside the quoted CSV cell. Temporal values
#'   use ISO text. CSV is for reporting; the in-memory result retains R types.
#'   Read value/expected columns as character to preserve codes such as `001`.
#'   Formula-like strings are preserved as data, without spreadsheet coercion.
#'
#' Both existing paths are checked before writing either file. Filesystem write
#'   errors are raised; the two-file write is not transactional.
#' @examples
#' model <- model.form::read_form_model("examples/survey-form.yaml", "model.form")
#' result <- validate_data(validation_plan(model), list(), source = "submission.xlsx")
#' directory <- tempfile()
#' write_validation_results(result, directory)
#' read.csv(file.path(directory, "summary.csv"))
#' unlink(directory, recursive = TRUE)
#' @export
write_validation_results <- function(result, directory, overwrite = FALSE) {
  if (!inherits(result, "validation_result") && !inherits(result, "validation_report")) {
    stop("result must come from validate_data() or validation_report_data()", call. = FALSE)
  }
  text_value(directory, "directory")
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("overwrite must be TRUE or FALSE", call. = FALSE)
  }
  paths <- c(summary = file.path(directory, "summary.csv"), findings = file.path(directory, "findings.csv"))
  if (any(dir.exists(paths))) stop("A CSV output path is a directory", call. = FALSE)
  if (!overwrite && any(file.exists(paths))) stop("CSV output already exists; use another directory or overwrite = TRUE", call. = FALSE)
  tables <- if (inherits(result, "validation_report")) result$tables else validation_result_tables(result)
  if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE)) {
    stop("Cannot create output directory: ", directory, call. = FALSE)
  }
  utils::write.csv(tables$summary, paths[["summary"]], row.names = FALSE, na = "", fileEncoding = "UTF-8")
  utils::write.csv(tables$details, paths[["findings"]], row.names = FALSE, na = "", fileEncoding = "UTF-8")
  invisible(paths)
}

#' Write a CSV-backed HTML validation report
#'
#' Write a small HTML page that loads adjacent summary.csv and findings.csv.
#' Serve the output over HTTP using Apache or a Shiny static resource directory.
#' @param report Prepared data from [validation_report_data()].
#' @param path Destination HTML filename.
#' @param title Report title.
#' @param author Optional author text, or `NULL`.
#' @param overwrite Replace existing HTML and CSV files? Defaults to `FALSE`.
#' @param assets_directory Asset directory relative to the HTML directory.
#'   Use a shared relative path for a collection of reports.
#' @param stylesheet Optional path to an additional CSS file overriding the default style.
#' @param logo Optional path to a report logo image.
#' @return The normalized HTML path, invisibly.
#' @details Creates CSV files if absent. Existing CSV files must match the prepared
#'   report, unless overwrite is TRUE. JavaScript and CSS are local external files;
#'   no rendering packages, Pandoc, CDN, or embedded validation data are required.
#'   Publish the HTML, both CSV files and the asset directory together. Opening
#'   the HTML directly through file:// is not supported.
#' @export
write_validation_report <- function(report, path, title = "Validation report", author = NULL,
                                    overwrite = FALSE, assets_directory = "report-assets",
                                    stylesheet = NULL, logo = NULL) {
  if (!inherits(report, "validation_report")) stop("report must come from validation_report_data()", call. = FALSE)
  text_value(path, "path")
  text_value(title, "title")
  text_value(assets_directory, "assets_directory")
  if (!is.null(author)) text_value(author, "author")
  if (grepl("^[/~]|^[A-Za-z]+:", assets_directory)) stop("assets_directory must be relative", call. = FALSE)
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("overwrite must be TRUE or FALSE", call. = FALSE)
  }
  if (dir.exists(path)) stop("Report path is a directory", call. = FALSE)
  if (file.exists(path) && !overwrite) stop("Report already exists; use overwrite = TRUE", call. = FALSE)
  directory <- dirname(path)
  if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE)) {
    stop("Cannot create report directory: ", directory, call. = FALSE)
  }
  csv_paths <- file.path(directory, c("summary.csv", "findings.csv"))
  if (overwrite || !any(file.exists(csv_paths))) {
    write_validation_results(report, directory, overwrite = overwrite)
  } else {
    expected <- tempfile("report-csv-")
    on.exit(unlink(expected, recursive = TRUE), add = TRUE)
    write_validation_results(report, expected)
    if (!all(file.exists(csv_paths)) ||
        !identical(unname(tools::md5sum(csv_paths)), unname(tools::md5sum(file.path(expected, basename(csv_paths)))))) {
      stop("Existing CSV files differ from report; use overwrite = TRUE", call. = FALSE)
    }
  }
  assets <- file.path(directory, assets_directory)
  if (!dir.exists(assets) && !dir.create(assets, recursive = TRUE)) stop("Cannot create asset directory", call. = FALSE)
  sources <- stats::setNames(system.file("report", c("report.js", "report.css"),
    package = "model.validation", mustWork = TRUE), c("script", "style"))
  if (!is.null(stylesheet)) sources <- c(sources, stylesheet = stylesheet)
  if (!is.null(logo)) sources <- c(sources, logo = logo)
  if (any(!file.exists(sources))) stop("Report asset does not exist", call. = FALSE)
  filenames <- stats::setNames(paste0("report-", unname(tools::md5sum(sources)), ".",
    tools::file_ext(sources)), names(sources))
  for (i in seq_along(sources)) {
    destination <- file.path(assets, filenames[[i]])
    if (!file.exists(destination) && !file.copy(sources[[i]], destination)) stop("Cannot write report asset", call. = FALSE)
  }
  escape <- function(x) {
    for (pair in list(c("&", "&amp;"), c("<", "&lt;"), c(">", "&gt;"), c('"', "&quot;"))) {
      x <- gsub(pair[[1]], pair[[2]], x, fixed = TRUE)
    }
    x
  }
  urls <- stats::setNames(vapply(file.path(assets_directory, filenames), function(path) {
    paste(vapply(strsplit(path, "/", fixed = TRUE)[[1]], utils::URLencode, character(1), reserved = TRUE), collapse = "/")
  }, character(1)), names(sources))
  html <- c('<!doctype html>', '<html lang="en"><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    paste0('<title>', escape(title), '</title>'),
    paste0('<link rel="stylesheet" href="', escape(urls[[2]]), '">'),
    if (!is.null(stylesheet)) paste0('<link rel="stylesheet" href="', escape(urls[["stylesheet"]]), '">'),
    paste0('<script defer src="', escape(urls[[1]]), '"></script></head><body><main>'),
    if (!is.null(logo)) paste0('<img class="report-logo" src="', escape(urls[["logo"]]), '" alt="Report logo">'),
    paste0('<h1>', escape(title), '</h1>'),
    if (!is.null(author)) paste0('<p class="author">', escape(author), '</p>'),
    '<p id="loading" role="status">Loading report data\u2026</p>',
    '<noscript>JavaScript is required to display this report.</noscript>',
    '<p><a href="summary.csv">Summary CSV</a> \u00b7 <a href="findings.csv">Findings CSV</a></p>',
    '<div id="report"></div></main></body></html>')
  writeLines(html, path, useBytes = TRUE)
  invisible(normalizePath(path, mustWork = TRUE))
}

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
    errors = sum(findings$severity == "error"), warnings = sum(findings$severity == "warning"),
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
