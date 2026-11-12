# Public operations ---------------------------------------------------------

# Report preparation ---------------------------------------------------------

#' Prepare a validation report from one input's result
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
#'   Pass the report to [`write_validation_results_csv()`][form.validation::write_validation_results_csv] to reuse its formatted
#'   `tables` for CSV export without preparing them again.
#'   Optional `metadata$report_notes` is a character vector displayed as escaped
#'   text in the Warning findings card. Other metadata is retained without interpretation.
#' @examples
#' # Load the requested form model.
#' form_model_definition <- form.model::read_form_model("examples/survey/form.yaml", "form.model")
#' # Read the named reference lookup settings.
#' reference_configuration <- form.references::read_reference_configuration(
#'   system.file("examples", "survey", "references.yaml", package = "form.references"))
#' # Resolve reference lookups without changing the original form model.
#' reference_resolved_form_model <- form.references::resolve_reference_lookups(
#'   form_model_definition, reference_configuration)
#' # Prepare the validation rules and their execution order.
#' validation_result <- validate_data(
#'   create_validation_plan(reference_resolved_form_model), list(), source = "submission.xlsx")
#' # Prepare report.
#' validation_report <- prepare_validation_report(validation_result, reference_resolved_form_model)
#' # Display the returned values.
#' validation_report$summary
#' @export
prepare_validation_report <- function(result, model, reference_provenance = result$reference_provenance,
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
         errors = sum(data$severity == "error"), warnings = sum(data$severity == "warning"), infos = sum(data$severity == "info"),
         summary = summary, details = details)
  })
  execution <- result$execution[result$execution$status %in% c("failed", "incomplete"),
    c("severity", "stage", "sheet", "field", "rule_id", "status", "message"), drop = FALSE]
  execution$stage <- report_stage_labels(execution$stage)
  evidence <- prepare_report_evidence(tables, reference_provenance, context, result$findings,
    if (is.null(result$reference_lookups)) reference_evidence_definitions(model) else result$reference_lookups)
  structure(list(tables = tables, evidence = evidence, summary = tables$summary, structure_passed = result$structure_passed,
                 sheets = Filter(Negate(is.null), sections), execution = execution,
                 generated_at = tables$summary$generated_at, metadata = result$metadata),
            class = "validation_report")
}

# CSV output ---------------------------------------------------------

#' Write one input's validation results as two CSV files
#'
#' Create `summary.csv` with one row and `findings.csv` with one row per finding
#' or incomplete/failed check. Successful checks produce no detail rows.
#' @param result A result from [`validate_data()`][form.validation::validate_data] or a prepared
#'   [`prepare_validation_report()`][form.validation::prepare_validation_report] report. A report reuses its formatted tables.
#' @param directory Output folder for this one input. Created if necessary.
#' @param overwrite Whether to replace existing CSV outputs. Defaults to `FALSE`.
#' @return A named character vector with `summary` and `findings` paths, invisibly.
#' @details Summary columns are `input`, `model_version` (`form.version`),
#'   `rules_version`, `status`, `structure_passed`, `generated_at` (UTC),
#'   `errors`, `warnings`, `infos`, `incomplete`, and `failed`.
#'   Errors, warnings and information counts describe validation findings, not execution problems.
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
#' # Load the requested form model.
#' form_model_definition <- form.model::read_form_model("examples/survey/form.yaml", "form.model")
#' # Read the named reference lookup settings.
#' reference_configuration <- form.references::read_reference_configuration(
#'   system.file("examples", "survey", "references.yaml", package = "form.references"))
#' # Resolve reference lookups without changing the original form model.
#' reference_resolved_form_model <- form.references::resolve_reference_lookups(
#'   form_model_definition, reference_configuration)
#' # Prepare the validation rules and their execution order.
#' validation_result <- validate_data(
#'   create_validation_plan(reference_resolved_form_model), list(), source = "submission.xlsx")
#' # Choose a temporary location for this example.
#' validation_directory <- tempfile()
#' # Write the prepared results to the output directory.
#' write_validation_results_csv(validation_result, validation_directory)
#' # Display the returned values.
#' read.csv(file.path(validation_directory, "summary.csv"))
#' # Remove the temporary files created by this example.
#' unlink(validation_directory, recursive = TRUE)
#' @export
write_validation_results_csv <- function(result, directory, overwrite = FALSE) {
  if (!inherits(result, "validation_result") && !inherits(result, "validation_report")) {
    stop("result must come from validate_data() or prepare_validation_report()", call. = FALSE)
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

# Individual reports ---------------------------------------------------------

#' Write a CSV-backed HTML validation report
#'
#' Write an HTML overview with counts and load adjacent findings.csv for tables.
#' Serve the output over HTTP using Apache or a Shiny static resource directory.
#' @param report Prepared data from [`prepare_validation_report()`][form.validation::prepare_validation_report].
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
  if (!inherits(report, "validation_report")) stop("report must come from prepare_validation_report()", call. = FALSE)
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
    write_validation_results_csv(report, directory, overwrite = overwrite)
  } else {
    expected <- tempfile("report-csv-")
    on.exit(unlink(expected, recursive = TRUE), add = TRUE)
    write_validation_results_csv(report, expected)
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
  summary <- report$summary
  counts <- report_count_values(summary[c("errors", "warnings", "infos", "incomplete", "failed")])
  explanation <- switch(summary$status,
    valid = "This form passed validation.", invalid = "This form has not passed validation.",
    incomplete = paste(format(summary$incomplete + summary$failed, big.mark = ",", trim = TRUE),
      "checks could not finish. This form has not passed validation."),
    failed = "Validation failed to complete. This form has not passed validation.")
  if (!isTRUE(summary$structure_passed))
    explanation <- "Structure validation did not pass. Field and business checks were not run."
  html <- render_report_template("report.html", c(counts, list(
    generated_at = sub("Z$", " UTC", sub("T", " ", summary$generated_at, fixed = TRUE)),
    status = summary$status,
    status_label = if (summary$status == "valid") "Validation passed" else paste("Validation", summary$status),
    status_icon = c(valid = "\u2705", invalid = "\U0001f534", incomplete = "\u23f3", failed = "\u274c")[[summary$status]],
    explanation = explanation, structure = if (isTRUE(summary$structure_passed)) "Passed" else "Not passed",
    model_version = as.character(summary$model_version),
    rules_version = if (is.na(summary$rules_version)) "" else as.character(summary$rules_version),
    title = title, style = urls[["style"]], script = urls[["script"]],
    overview_style = urls[["overview_style"]],
    loading_script = urls[["loading_script"]], csv_worker = urls[["csv_worker"]],
    stylesheet = if (!is.null(stylesheet)) urls[["stylesheet"]] else NULL,
    logo = if (!is.null(logo)) urls[["logo"]] else NULL, author = author, parent_report = parent_report,
    report_notes = notes)))
  html <- sub("<!-- evidence-cards -->", paste(render_evidence_cards(evidence), collapse = "\n"), html, fixed = TRUE)
  writeLines(html, path, useBytes = TRUE)
  invisible(normalizePath(path, mustWork = TRUE))
}

# Aggregate reports ---------------------------------------------------------

#' Write grouped summaries of completed validation reports
#'
#' @param descriptors Data frame with `directory` (existing report directory),
#'   `report` (HTML filename), and public grouping metadata columns.
#' @param directory Destination containing the copied individual reports.
#' @param group_columns Ordered metadata columns defining directory levels.
#' @param identity_columns Metadata columns distinguishing finding identities.
#' @param report_filename Aggregate HTML filename, default `index.html`.
#' @param title Aggregate report title.
#' @inheritParams write_validation_report
#' @param notes Optional public information displayed on the root page.
#' @param report_notes Public validation-policy notes shown in the Warning
#'   findings card on every aggregate page. Text is HTML-escaped.
#' @param model_versions Optional model versions to show, including versions with no files.
#' @param reference_context_columns Descriptor columns to show in code summaries and source tables.
#' @param workers Number of top-level aggregate directories to render concurrently.
#' @return Relative aggregate HTML paths, invisibly, with a `reference_report`
#'   attribute for catalogue evidence, `catalogues` for frozen catalogue metadata,
#'   and `conversion_report` for number, integer, boolean and temporal type failures.
#'   Attributes contain all contextual evidence report paths. Each aggregate's
#'   evidence includes only its forms, with counts and links scoped accordingly.
#'   Conversion evidence uses saved errors and warnings without altering values.
#'   The `primary_key_report` attribute lists missing-component and duplicate-key
#'   evidence reports, retaining the saved key values and physical Excel rows.
#' @details Reads all CSV columns as text. Each directory level receives files.csv,
#'   issues.csv and execution.csv and an HTML overview. Validation findings and
#'   execution problems remain separate. Grouped findings show occurrences and
#'   affected files; their identity includes model/rules versions and caller metadata.
#'   Individual reports and prepared `evidence.json` must already exist below
#'   directory, at descriptor paths. Evidence is reused, never extracted from CSV.
#'   No input workbook is read, copied or revalidated. Existing outputs are refused.
#' @export
write_validation_report_summary <- function(descriptors, directory, group_columns,
                                       identity_columns = group_columns,
                                       report_filename = "index.html", title = "Validation reports", notes = character(),
                                       author = NULL, stylesheet = NULL, logo = NULL, model_versions = NULL,
                                       reference_context_columns = character(),
                                       report_notes = character(), workers = 1L) {
  if (!is.character(report_notes) || anyNA(report_notes))
    stop("report_notes must be character text without missing values", call. = FALSE)
  if (length(workers) != 1L || !is.numeric(workers) || is.na(workers) ||
      !is.finite(workers) || workers < 1 || workers > .Machine$integer.max ||
      workers != as.integer(workers))
    stop("workers must be a positive whole number", call. = FALSE)
  workers <- as.integer(workers)
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*\\.html$", report_filename)) stop("Invalid report filename")
  required <- unique(c("directory", "report", group_columns, identity_columns))
  required <- unique(c(required, reference_context_columns))
  if (!is.data.frame(descriptors) || !all(required %in% names(descriptors))) stop("Missing descriptor columns")
  for (key in group_columns) {
    values <- descriptors[[key]]
    if (anyNA(values) || any(!nzchar(values) | values %in% c(".", "..") | grepl("[/\\\\]", values)))
      stop("Invalid grouping directory component: ", key)
  }
  if (nrow(descriptors)) for (i in seq_len(nrow(descriptors))) {
    prefix <- paste(unlist(descriptors[i, group_columns, drop = FALSE], use.names = FALSE), collapse = "/")
    if (nzchar(prefix) && !startsWith(descriptors$directory[i], paste0(prefix, "/"))) stop("Descriptor directory differs from grouping metadata")
    if (!grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*\\.html$", descriptors$report[i])) stop("Invalid individual report filename")
  }
  # Read report CSV without type conversion
  # @param path CSV file with a UTF-8 BOM.
  # @return Data frame retaining all values as text, including literal NA strings.
  read <- function(path) utils::read.csv(path, colClasses = "character", check.names = FALSE,
    na.strings = NULL, fileEncoding = "UTF-8-BOM")
  # Encode a report link
  # @param x One slash-separated relative path.
  # @return URL text with encoded components and unchanged separators.
  url <- function(x) paste(vapply(strsplit(x, "/", fixed = TRUE)[[1]], utils::URLencode,
                                 character(1), reserved = TRUE), collapse = "/")
  metadata <- setdiff(names(descriptors), c("directory", "report"))
  # Summarize findings in the current aggregate
  # @param rows Finding rows with input identities and optional occurrence weights.
  # @param by_input Whether to retain each form and sheet order as grouping keys.
  # @return Grouped rows with occurrence and distinct-form counts.
  grouped <- function(rows, by_input = FALSE) {
    keys <- unique(c(identity_columns, "model_version", "rules_version", "rule_id", "kind", "severity", "stage", "sheet", "field", "column", "value", "expected", "message"))
    if (by_input) keys <- intersect(unique(c(keys, "input", "sheet_order")), names(rows))
    if (!nrow(rows)) {
      result <- as.data.frame(stats::setNames(rep(list(character()), length(keys)), keys), stringsAsFactors = FALSE)
      result$occurrences <- integer(); result$affected_files <- integer(); return(result)
    }
    keys <- intersect(keys, names(rows))
    codes <- lapply(rows[keys], function(x) match(x, unique(x)))
    ids <- do.call(paste, c(codes, sep = ":"))
    order <- unique(ids); groups <- match(ids, order)
    result <- rows[!duplicated(ids), keys, drop = FALSE]
    weights <- if ("occurrences" %in% names(rows)) as.numeric(rows$occurrences) else rep(1, nrow(rows))
    result$occurrences <- as.numeric(rowsum(weights, groups, reorder = FALSE))
    pairs <- unique(data.frame(group = groups, input = rows$input))
    result$affected_files <- tabulate(pairs$group, nbins = length(order))
    result
  }
  files <- list(); issues <- list(); execution <- list(); reference_evidence <- list(); catalogues <- list(); form_catalogues <- list(); conversion_evidence <- list(); primary_key_evidence <- list()
  for (i in seq_len(nrow(descriptors))) {
    d <- descriptors[i, , drop = FALSE]
    if (grepl("(^/|(^|/)\\.\\.(/|$))", d$directory)) stop("Unsafe descriptor path")
    summary <- read(file.path(directory, d$directory, "summary.csv"))
    detail <- read(file.path(directory, d$directory, "findings.csv"))
    if (nrow(summary) != 1L) stop("Expected one summary row: ", d$directory)
    if (!all(c("input", "model_version", "rules_version", "status", "structure_passed", "errors", "warnings", "incomplete", "failed") %in% names(summary))) stop("Incomplete summary schema")
    if (!all(c("kind", "rule_id", "severity", "sheet", "field", "value", "expected", "message", "stage") %in% names(detail))) stop("Incomplete findings schema")
    if (!"infos" %in% names(summary)) summary$infos <- "0"
    summary$input <- d$directory
    summary$report <- file.path(d$directory, d$report)
    for (key in metadata) summary[[key]] <- d[[key]]
    files[[i]] <- summary
    detail$input <- rep(d$directory, nrow(detail))
    for (key in unique(c(metadata, "model_version", "rules_version"))) detail[[key]] <- rep(summary[[key]], nrow(detail))
    if (any(!detail$kind %in% c("validation", "failed", "incomplete"))) stop("Unknown finding kind")
    prepared <- read_report_evidence(file.path(directory, d$directory, "evidence.json"))
    # Read one prepared evidence kind for this form
    # @param kind Name of the evidence part.
    # @return Prepared rows with descriptor metadata and rebased report links, or NULL.
    part <- function(kind) {
      rows <- prepared$parts[[kind]]
      if (is.null(rows) || !nrow(rows)) return(NULL)
      rows$input <- rep(d$directory, nrow(rows))
      rows$report <- rep(paste0("../", d$directory, "/", d$report), nrow(rows))
      for (key in reference_context_columns) rows[[key]] <- rep(d[[key]], nrow(rows))
      rows
    }
    reference_evidence[i] <- list(part("reference"))
    form_catalogues[i] <- list(names(prepared$catalogues))
    for (id in names(prepared$catalogues)) {
      spec <- prepared$catalogues[[id]]
      if (!is.null(catalogues[[id]]) && !identical(catalogues[[id]], spec)) stop("Conflicting catalogue metadata: ", id)
      catalogues[[id]] <- spec
    }
    conversion_evidence[i] <- list(part("conversion"))
    primary_key_evidence[i] <- list(part("primary_key"))
    # Collapse each form before retaining it; the full corpus never occupies memory.
    issues[[i]] <- grouped(detail[detail$kind == "validation", , drop = FALSE], by_input = TRUE)
    execution[[i]] <- grouped(detail[detail$kind != "validation", , drop = FALSE], by_input = TRUE)
  }
  if (!length(files)) {
    files <- data.frame(input = character(), report = character(), status = character(),
      structure_passed = character(), errors = character(), warnings = character(), infos = character(), incomplete = character(), failed = character())
    for (key in metadata) files[[key]] <- character()
    issues <- execution <- data.frame(input = character())
  } else {
    files <- do.call(rbind, files); issues <- do.call(rbind, issues); execution <- do.call(rbind, execution)
  }
  model_versions <- unique(c(as.character(model_versions), files$model_version))
  groups <- ""
  for (depth in seq_along(group_columns)) if (nrow(files))
    groups <- c(groups, unique(apply(files[group_columns[seq_len(depth)]], 1L, paste, collapse = "/")))
  assets <- file.path(directory, "report-assets")
  sources <- stats::setNames(system.file("report", c("report.css", "aggregate.js", "overview.css", "report-ui.js", "csv-worker.js", "report-table.js"),
    package = "form.validation", mustWork = TRUE), c("style", "script", "overview_style", "loading_script", "csv_worker", "table_script"))
  if (!is.null(stylesheet)) sources <- c(sources, stylesheet = stylesheet)
  if (!is.null(logo)) sources <- c(sources, logo = logo)
  asset_names <- copy_report_assets(sources, assets)
  # Format the total evidence occurrence count
  # @param parts List of prepared per-form evidence tables or NULL entries.
  # @return A formatted count using comma thousands separators.
  evidence_count <- function(parts) format(sum(vapply(parts, function(part)
    sum(as.numeric(part$occurrences)), numeric(1))), big.mark = ",", scientific = FALSE, trim = TRUE)
  render_group <- function(group) {
    depth <- if (nzchar(group)) length(strsplit(group, "/", fixed = TRUE)[[1]]) else 0L
    selected <- if (nzchar(group)) startsWith(files$input, paste0(group, "/")) else rep(TRUE, nrow(files))
    f <- files[selected, , drop = FALSE]
    v <- issues[issues$input %in% f$input, , drop = FALSE]
    e <- execution[execution$input %in% f$input, , drop = FALSE]
    folder <- file.path(directory, group); dir.create(folder, showWarnings = FALSE, recursive = TRUE)
    outputs <- file.path(folder, c(report_filename, "files.csv", "issues.csv", "execution.csv"))
    if (any(file.exists(outputs))) stop("Aggregate output already exists: ", folder)
    prefix <- strrep("../", depth)
    page_title <- if (nzchar(group)) paste(title, "\u2014", group) else title
    # Reuse the evidence collected once per form, scoped to this aggregate.
    # Source links still target the original individual reports at the root.
    # Select evidence for the current aggregate
    # @param parts Evidence parts in descriptor order.
    # @return Selected parts with report links rebased from the current group.
    scoped_evidence <- function(parts) lapply(parts[which(selected)], function(part) {
      if (!is.null(part)) part$report <- paste0(prefix, part$report)
      part
    })
    reference_parts <- scoped_evidence(reference_evidence)
    conversion_parts <- scoped_evidence(conversion_evidence)
    primary_key_parts <- scoped_evidence(primary_key_evidence)
    conversion_count <- evidence_count(conversion_parts)
    primary_key_count <- evidence_count(primary_key_parts)
    evidence_assets <- paste0(prefix, "../report-assets")
    scoped_catalogues <- catalogues[unique(unlist(form_catalogues[which(selected)], use.names = FALSE))]
    reference_reports <- list()
    for (id in names(scoped_catalogues)) {
      selected_parts <- catalogue_evidence_parts(reference_parts, id)
      if (evidence_count(selected_parts) == "0") next
      reference_reports[[id]] <- write_reference_evidence(selected_parts, folder, report_filename,
        reference_context_columns, page_title, stylesheet, logo, catalogue = catalogues[[id]],
        evidence_assets = evidence_assets, scope = group)
    }
    conversion_report <- if (conversion_count == "0") NULL else write_conversion_evidence(conversion_parts, folder, report_filename,
      reference_context_columns, page_title, stylesheet, logo, evidence_assets = evidence_assets, scope = group)
    primary_key_report <- if (primary_key_count == "0") NULL else write_primary_key_evidence(primary_key_parts, folder, report_filename,
      reference_context_columns, page_title, stylesheet, logo, evidence_assets = evidence_assets, scope = group)
    # Locate a contextual evidence page from the root
    # @param path Evidence report path, or NULL for an empty report.
    # @return Root-relative path, or NULL when no report exists.
    relative_report <- function(path) if (length(path)) if (nzchar(group)) file.path(group, path) else path
    f$report <- paste0(prefix, f$report)
    write_report_csv(f, outputs[2]); write_report_csv(grouped(v), outputs[3]); write_report_csv(grouped(e), outputs[4])
    breadcrumbs <- list(list(label = "All reports", href = paste0(prefix, report_filename)))
    if (depth) for (i in seq_len(depth)) {
      part <- strsplit(group, "/", fixed = TRUE)[[1]][i]
      breadcrumbs[[length(breadcrumbs) + 1L]] <- list(label = part,
        href = paste0(strrep("../", depth - i), report_filename))
    }
    child_paths <- groups[vapply(groups, function(x) nzchar(x) && identical(dirname(x),
      if (nzchar(group)) group else "."), logical(1))]
    children <- lapply(child_paths, function(path) list(label = basename(path),
      href = url(file.path(basename(path), report_filename))))
    totals <- data.frame(files = nrow(f), structure_passed = sum(toupper(f$structure_passed) == "TRUE"),
      structure_blocked = sum(toupper(f$structure_passed) == "FALSE"), errors = sum(as.numeric(f$errors)),
      warnings = sum(as.numeric(f$warnings)), infos = sum(as.numeric(f$infos)), incomplete_checks = sum(as.numeric(f$incomplete)), failed_checks = sum(as.numeric(f$failed)))
    version_counts <- data.frame(version = model_versions,
      files = vapply(model_versions, function(version) sum(f$model_version == version), integer(1)))
    sheet_rows <- rbind(v, e)
    sheet_order <- if ("sheet_order" %in% names(sheet_rows))
      unique(sheet_rows$sheet[order(suppressWarnings(as.numeric(sheet_rows$sheet_order)), na.last = TRUE)]) else unique(sheet_rows$sheet)
    sheet_order <- sheet_order[!is.na(sheet_order) & nzchar(sheet_order)]
    # Navigation and table order belong to the HTML, not a separate browser payload.
    unlink(file.path(folder, "aggregate.json"))
    status_counts <- stats::setNames(lapply(c("valid", "invalid", "incomplete", "failed"),
      function(status) sum(f$status == status)), paste0("status_", c("valid", "invalid", "incomplete", "failed")))
    html <- render_report_template("aggregate.html", c(report_count_values(c(as.list(totals), status_counts)), list(title = page_title,
      current_label = breadcrumbs[[length(breadcrumbs)]]$label,
      children_hidden = if (length(children)) "" else "hidden",
      report_notes = if (length(report_notes)) paste(report_notes, collapse = "\n") else NULL,
      generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
      style = paste0(prefix, "report-assets/", asset_names[["style"]]),
      script = paste0(prefix, "report-assets/", asset_names[["script"]]),
      loading_script = paste0(prefix, "report-assets/", asset_names[["loading_script"]]),
      csv_worker = paste0(prefix, "report-assets/", asset_names[["csv_worker"]]),
      table_script = paste0(prefix, "report-assets/", asset_names[["table_script"]]),
      overview_style = paste0(prefix, "report-assets/", asset_names[["overview_style"]]),
      stylesheet = if (!is.null(stylesheet)) paste0(prefix, "report-assets/", asset_names[["stylesheet"]]) else NULL,
      logo = if (!is.null(logo)) paste0(prefix, "report-assets/", asset_names[["logo"]]) else NULL,
      author = author)))
    breadcrumb_html <- vapply(utils::head(breadcrumbs, -1L), function(item)
      paste(render_report_template("breadcrumb-link.html", item), collapse = "\n"), character(1))
    html <- sub("<!-- aggregate-breadcrumbs -->", paste(breadcrumb_html, collapse = "\n"), html, fixed = TRUE)
    child_html <- vapply(children, function(item)
      paste(render_report_template("aggregate-child.html", item), collapse = "\n"), character(1))
    html <- sub("<!-- aggregate-children -->", paste(child_html, collapse = "\n"), html, fixed = TRUE)
    sheet_html <- vapply(sheet_order, function(sheet)
      paste(render_report_template("aggregate-sheet.html", list(sheet = sheet)), collapse = "\n"), character(1))
    html <- sub("<!-- aggregate-sheets -->", paste(sheet_html, collapse = "\n"), html, fixed = TRUE)
    note_html <- vapply(if (!depth) notes else character(), function(note)
      paste(render_report_template("aggregate-note.html", list(note = note)), collapse = "\n"), character(1))
    html <- sub("<!-- aggregate-notes -->", paste(note_html, collapse = "\n"), html, fixed = TRUE)
    versions_html <- vapply(seq_len(nrow(version_counts)), function(i) paste(render_report_template(
      "overview-version-card.html", c(list(version = version_counts$version[i]),
        report_count_values(list(files = version_counts$files[i])))), collapse = "\n"), character(1))
    html <- sub("<!-- version-cards -->", paste(versions_html, collapse = "\n"), html, fixed = TRUE)
    cards <- evidence_card_values(list(reference = reference_parts,
      conversion = conversion_parts, primary_key = primary_key_parts), reference_reports,
      conversion_report, primary_key_report, scoped_catalogues)
    html <- sub("<!-- evidence-cards -->", paste(render_evidence_cards(cards), collapse = "\n"), html, fixed = TRUE)
    writeLines(html, outputs[1], useBytes = TRUE)
    list(page = if (nzchar(group)) file.path(group, report_filename) else report_filename,
      reference_report = lapply(reference_reports, relative_report),
      conversion_report = relative_report(conversion_report), primary_key_report = relative_report(primary_key_report))
  }
  if (workers == 1L || length(groups) == 1L) {
    rendered <- lapply(groups, render_group)
  } else {
    # Each worker owns one top-level directory and every aggregate below it.
    top <- unique(sub("/.*$", "", groups[nzchar(groups)]))
    tasks <- lapply(top, function(path) groups[groups == path | startsWith(groups, paste0(path, "/"))])
    render_directory <- function(paths) stats::setNames(lapply(paths, render_group), paths)
    if (.Platform$OS.type == "windows") {
      cluster <- parallel::makePSOCKcluster(min(workers, length(tasks)))
      on.exit(parallel::stopCluster(cluster), add = TRUE)
      finished <- parallel::parLapplyLB(cluster, tasks, render_directory)
    } else {
      finished <- parallel::mclapply(tasks, render_directory,
        mc.cores = min(workers, length(tasks)), mc.preschedule = FALSE)
    }
    if (any(vapply(finished, inherits, logical(1), "try-error")))
      stop("Aggregate directory worker failed", call. = FALSE)
    rendered <- c(do.call(c, finished), stats::setNames(list(render_group("")), ""))
    rendered <- rendered[match(groups, names(rendered))]
  }
  pages <- vapply(rendered, `[[`, character(1), "page")
  paths <- function(key) as.character(unlist(lapply(rendered, `[[`, key), use.names = FALSE))
  invisible(structure(unname(pages),
    reference_report = paths("reference_report"), catalogues = catalogues,
    conversion_report = paths("conversion_report"), primary_key_report = paths("primary_key_report")))
}
