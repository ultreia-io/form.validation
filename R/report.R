#' Prepare one input's validation report
#'
#' Prepare plain tables once for HTML output or a future interactive consumer.
#' No workbook is read and no validation rule is executed.
#' @param result A result from [`validate_data()`][form.validation::validate_data].
#' @param model The form used to validate this input, supplying sheet order and layout.
#' @param reference_provenance Snapshot records keyed by reference identifier,
#'   with `status` and `sha256`. Defaults to provenance retained by validation.
#' @param context Named public metadata, such as a site or gear, retained in evidence.
#' @return A `validation_report` list with `tables`, prepared `evidence`, `summary`, `structure_passed`,
#'   `sheets`, `execution`, `generated_at`, and caller `metadata`. Each sheet has its name, finding
#'   counts, a summary, and details. Values are readable text; the original
#'   result retains its types. Execution contains only failed or incomplete checks.
#' @details Table-sheet summaries group findings by field, column, value,
#'   severity, stage, and message, counting occurrences. Every sheet retains
#'   individual findings in Validation messages, including fixed-cell sheets. Whole-workbook findings have a
#'   separate section.
#'   Incomplete and failed execution remain separate from validation findings.
#'   Pass the report to [`write_validation_results()`][form.validation::write_validation_results] to reuse its formatted
#'   `tables` for CSV export without preparing them again.
#'   Optional `metadata$report_notes` is a character vector displayed as escaped
#'   text in the Warning findings card. Other metadata is retained without interpretation.
#' @examples
#' model <- form.model::read_form_model("examples/survey-form.yaml", "form.model")
#' result <- validate_data(validation_plan(model), list(), source = "submission.xlsx")
#' report <- validation_report_data(result, model)
#' report$summary
#' @export
validation_report_data <- function(result, model, reference_provenance = result$reference_provenance,
                                   context = list()) {
  if (!inherits(result, "validation_result")) stop("result must come from validate_data()", call. = FALSE)
  if (!identical(result$model, form.model::form_metadata(model))) {
    stop("Report form differs from validation result", call. = FALSE)
  }
  tables <- validation_result_tables(result)
  sheets <- form.model::form_sheets(model)
  # Resolve static sheet and field metadata once, then match all finding rows.
  field_labels <- stats::setNames(lapply(names(sheets), function(name) {
    vapply(form.model::form_fields(model, name), `[[`, character(1), "name")
  }), names(sheets))
  tables$details$sheet_order <- match(tables$details$sheet, names(sheets))
  layouts <- vapply(sheets, function(sheet) sheet$layout$kind, character(1))
  tables$details$layout <- unname(layouts[tables$details$sheet_order])
  tables$details$layout[is.na(tables$details$sheet_order)] <- "table"
  tables$details$field_label <- tables$details$field
  for (name in names(sheets)) {
    selected <- which(tables$details$sheet == name)
    labels <- field_labels[[name]]
    matched <- match(tables$details$field[selected], names(labels))
    known <- !is.na(matched)
    tables$details$field_label[selected[known]] <- unname(labels[matched[known]])
  }
  findings <- tables$details[tables$details$kind == "validation", , drop = FALSE]
  findings$stage <- result$execution$stage[match(findings$rule_id, result$execution$rule_id)]
  findings$stage <- report_stage_labels(findings$stage)
  names_in_result <- unique(findings$sheet[!is.na(findings$sheet)])
  order <- c(names(sheets), setdiff(names_in_result, names(sheets)))
  if (anyNA(findings$sheet)) order <- c(order, NA_character_)
  rows <- split(seq_len(nrow(findings)), match(findings$sheet, order))
  sections <- lapply(seq_along(order), function(i) {
    name <- order[[i]]
    data <- findings[rows[[as.character(i)]], , drop = FALSE]
    if (!nrow(data)) return(NULL)
    labels <- if (!is.na(name) && name %in% names(sheets)) field_labels[[name]] else character()
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
    list(name = if (is.na(name)) "Form" else name,
         errors = sum(data$severity == "error"), warnings = sum(data$severity == "warning"),
         summary = summary, details = details)
  })
  execution <- result$execution[result$execution$status %in% c("failed", "incomplete"),
    c("severity", "stage", "sheet", "field", "rule_id", "status", "message"), drop = FALSE]
  execution$stage <- report_stage_labels(execution$stage)
  evidence <- prepare_report_evidence(tables, reference_provenance, context)
  structure(list(tables = tables, evidence = evidence, summary = tables$summary, structure_passed = result$structure_passed,
                 sheets = Filter(Negate(is.null), sections), execution = execution,
                 generated_at = tables$summary$generated_at, metadata = result$metadata),
            class = "validation_report")
}

#' Write one input's validation results as two CSV files
#'
#' Create `summary.csv` with one row and `findings.csv` with one row per finding
#' or incomplete/failed check. Successful checks produce no detail rows.
#' @param result A result from [`validate_data()`][form.validation::validate_data] or a prepared
#'   [`validation_report_data()`][form.validation::validation_report_data] report. A report reuses its formatted tables.
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
#' Files use UTF-8 with a BOM, comma separators, decimal points, and standard CSV quoting.
#'   Scalar values become readable text; null is `null`, scalar NA is `NA`, and
#'   compound values use YAML text inside the quoted CSV cell. Temporal values
#'   use ISO text. CSV is for reporting; the in-memory result retains R types.
#'   Read value/expected columns as character to preserve codes such as `001`.
#'   Formula-like strings are preserved as data, without spreadsheet coercion.
#'
#' Both existing paths are checked before writing either file. Filesystem write
#'   errors are raised; the two-file write is not transactional.
#' @examples
#' model <- form.model::read_form_model("examples/survey-form.yaml", "form.model")
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
  write_report_csv(tables$summary, paths[["summary"]])
  write_report_csv(tables$details, paths[["findings"]])
  invisible(paths)
}

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

#' Write a CSV-backed HTML validation report
#'
#' Write a small HTML page that loads adjacent summary.csv and findings.csv.
#' Serve the output over HTTP using Apache or a Shiny static resource directory.
#' @param report Prepared data from [`validation_report_data()`][form.validation::validation_report_data].
#' @param path Destination HTML filename.
#' @param title Report title.
#' @param author Optional author text, or `NULL`.
#' @param overwrite Replace existing HTML and CSV files? Defaults to `FALSE`.
#' @param assets_directory Asset directory relative to the HTML directory.
#'   Use a shared relative path for a collection of reports.
#' @param stylesheet Optional path to an additional CSS file overriding the default style.
#' @param logo Optional path to a report logo image.
#' @param parent_report Optional URL of the parent aggregate report; NULL omits navigation.
#' @return The normalized HTML path, invisibly.
#' @details Creates CSV files if absent. Existing CSV files must match the prepared
#'   report, unless overwrite is TRUE. JavaScript and CSS are local external files;
#'   no rendering packages, Pandoc, CDN, or embedded validation data are required.
#'   Also writes reusable `evidence.json` and nonempty evidence report directories.
#'   Zero-count evidence cards remain visible without links.
#'   Publish the complete output directory and its assets together. Opening
#'   the HTML directly through file:// is not supported.
#' @export
write_validation_report <- function(report, path, title = "Validation report", author = NULL,
                                    overwrite = FALSE, assets_directory = "report-assets",
                                    stylesheet = NULL, logo = NULL, parent_report = NULL) {
  if (!inherits(report, "validation_report")) stop("report must come from validation_report_data()", call. = FALSE)
  notes <- report$metadata$report_notes
  if (!is.null(notes) && (!is.character(notes) || anyNA(notes)))
    stop("metadata$report_notes must be character text without missing values", call. = FALSE)
  notes <- if (length(notes)) paste(notes, collapse = "\n") else NULL
  text_value(path, "path")
  text_value(title, "title")
  text_value(assets_directory, "assets_directory")
  if (!is.null(author)) text_value(author, "author")
  if (!is.null(parent_report)) text_value(parent_report, "parent_report")
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
  sources <- stats::setNames(system.file("report", c("report.js", "report.css", "overview.css", "report-ui.js", "csv-worker.js"),
    package = "form.validation", mustWork = TRUE), c("script", "style", "overview_style", "loading_script", "csv_worker"))
  if (!is.null(stylesheet)) sources <- c(sources, stylesheet = stylesheet)
  if (!is.null(logo)) sources <- c(sources, logo = logo)
  filenames <- copy_report_assets(sources, assets)
  urls <- stats::setNames(vapply(file.path(assets_directory, filenames), function(path) {
    paste(vapply(strsplit(path, "/", fixed = TRUE)[[1]], utils::URLencode, character(1), reserved = TRUE), collapse = "/")
  }, character(1)), names(sources))
  evidence <- write_individual_evidence(report, directory, basename(path), title,
    stylesheet, logo, assets_directory, overwrite)
  html <- render_report_template("report.html", list(
    title = title, style = urls[["style"]], script = urls[["script"]],
    overview_style = urls[["overview_style"]],
    loading_script = urls[["loading_script"]], csv_worker = urls[["csv_worker"]],
    stylesheet = if (!is.null(stylesheet)) urls[["stylesheet"]] else NULL,
    logo = if (!is.null(logo)) urls[["logo"]] else NULL, author = author, parent_report = parent_report,
    report_notes = notes))
  html <- sub("<!-- evidence-cards -->", paste(render_report_template("evidence-cards.html", evidence), collapse = "\n"), html, fixed = TRUE)
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
