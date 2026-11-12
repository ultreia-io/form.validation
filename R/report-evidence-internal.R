# Internal helpers ---------------------------------------------------------

# Evidence preparation and reading ---------------------------------------------------------

#' Prepare reusable evidence for one form
#'
#' Collect recorded reference, conversion and primary-key findings into
#' compact tables, retaining their values, occurrence counts and source locations.
#' @param tables Formatted summary and details from `validation_result_tables()`.
#' @param provenance Reference snapshot records keyed by identifier, or `NULL` to
#'   omit reference evidence. Available records include status and hash.
#' @param context Named list of scalar public text values carried into evidence.
#'   Names must not conflict with evidence or report columns.
#' @param findings Original `result$findings` corresponding to `tables`, supplying
#'   structured primary-key expectations without parsing the formatted CSV text.
#' @param lookups Frozen reference lookup definitions, keyed by reference identifier.
#' @return A serializable list with `schema_version`, `context_columns` and `parts`.
#'   Each part (`reference`, `conversion`, `primary_key`) is a data frame
#'   or `NULL` when empty. Report links are added by the writer, not stored here.
#' @details Snapshot hashes distinguish reference cases; snapshot contents are not
#'   included. Aggregates reuse these tables rather than collecting evidence again.
#' @keywords internal
prepare_report_evidence <- function(tables, provenance, context, findings, lookups = list()) {
  if (!is.list(context) || (length(context) && (is.null(names(context)) ||
      any(!nzchar(names(context))) || anyDuplicated(names(context))))) stop("context must be a named list")
  reserved <- c(names(tables$details), names(tables$summary), "report", "cells", "occurrences", "snapshot_sha256", "rows", "key_fields", "parent_sheet", "problem", "catalogue", "table", "lookup", "reference", "reason")
  if (length(intersect(names(context), reserved))) stop("Context names conflict with report columns")
  if (any(!vapply(context, function(x) is.character(x) && length(x) == 1L && !is.na(x), logical(1))))
    stop("Context values must be scalar text")
  detail <- tables$details
  # Match the public CSV representation without writing and reading it again.
  detail[] <- lapply(detail, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
  for (key in c("input", "model_version", "rules_version")) detail[[key]] <- rep(tables$summary[[key]], nrow(detail))
  for (key in names(context)) detail[[key]] <- rep(context[[key]], nrow(detail))
  parts <- list(
    reference = collect_reference_evidence(detail, findings, provenance, lookups, "", names(context)),
    conversion = collect_conversion_evidence(detail, "", names(context)),
    primary_key = collect_primary_key_evidence(detail, findings, "", names(context)))
  parts <- lapply(parts, function(rows) { if (!is.null(rows)) rows$report <- NULL; rows })
  list(schema_version = 3L, catalogues = reference_catalogues(lookups), context_columns = as.list(names(context)), parts = parts)
}

#' Read a form's prepared evidence
#' @param path Path to `evidence.json` written by `write_individual_evidence()`.
#' @return The prepared evidence list, with JSON table arrays restored as data frames.
#' @details Rejects missing files and unsupported schemas. Regenerate individual
#'   reports from saved validation results before aggregating older evidence files.
#'   This reader does not reconstruct evidence from findings or load R objects.
#' @keywords internal
read_report_evidence <- function(path) {
  if (!file.exists(path)) stop("Missing prepared evidence; regenerate individual reports: ", path)
  value <- jsonlite::read_json(path, simplifyVector = TRUE)
  if (!identical(value$schema_version, 3L) || !"catalogues" %in% names(value) ||
      !all(c("reference", "conversion", "primary_key") %in% names(value$parts)))
    stop("Unsupported prepared evidence: ", path)
  value
}

#' Write individual evidence reports and their reusable data
#' @param report Prepared report from [prepare_validation_report()].
#' @param directory Destination directory of the individual form report.
#' @param report_filename Filename of that form's HTML report, used for back links.
#' @param title Parent report title, extended by each evidence report's label.
#' @param stylesheet,logo Optional branding file paths, or `NULL`.
#' @param assets_directory Shared asset directory relative to `directory`.
#' @param overwrite Whether existing evidence folders may be replaced.
#' @return Template values from `evidence_card_values()` for the individual report.
#' @details Writes HTML, reviewer CSV files and summary counts only for
#'   nonempty evidence kinds. On replacement, stale folders are removed even when
#'   the new count is zero. Always saves `evidence.json` for later aggregation.
#'   Breadcrumbs point back to the form report; shared assets use content hashes.
#' @keywords internal
write_individual_evidence <- function(report, directory, report_filename, title,
                                      stylesheet, logo, assets_directory, overwrite) {
  evidence <- report$evidence
  if (is.null(evidence)) stop("Report has no prepared evidence; call prepare_validation_report() again")
  kinds <- c(conversion = "conversion", primary_key = "primary-key")
  reference_reports <- list()
  old_path <- file.path(directory, "evidence.json")
  if (overwrite && file.exists(old_path)) {
    old <- jsonlite::read_json(old_path, simplifyVector = FALSE)
    folders <- vapply(old$catalogues, `[[`, character(1), "folder")
    if (any(!grepl("^reference-[0-9a-f]+-evidence$", folders))) stop("Unsafe saved evidence folder")
    unlink(file.path(directory, folders), recursive = TRUE)
  }
  paths <- stats::setNames(vector("list", length(kinds)), names(kinds))
  parts <- lapply(evidence$parts, function(rows) {
    if (!is.null(rows)) rows$report <- rep(paste0("../", report_filename), nrow(rows))
    list(rows)
  })
  for (kind in names(kinds)) {
    folder <- file.path(directory, paste0(kinds[[kind]], "-evidence"))
    if (dir.exists(folder)) {
      if (!overwrite) stop("Evidence report already exists; use overwrite = TRUE: ", folder)
      unlink(folder, recursive = TRUE)
    }
    if (!sum(vapply(parts[[kind]], function(rows) sum(as.numeric(rows$occurrences)), numeric(1)))) next
    arguments <- list(parts = parts[[kind]], directory = directory, report_filename = report_filename,
      context_columns = unlist(evidence$context_columns, use.names = FALSE), title = title,
      stylesheet = stylesheet, logo = logo, evidence_assets = file.path("..", assets_directory),
      parent_label = basename(report$summary$input))
    paths[[kind]] <- if (kind == "primary_key") do.call(write_primary_key_evidence, arguments) else if (kind == "conversion") do.call(write_conversion_evidence, arguments) else stop("Unknown evidence kind")
  }
  for (id in names(evidence$catalogues)) {
    selected <- catalogue_evidence_parts(parts$reference, id)
    if (!sum(vapply(selected, function(x) sum(as.numeric(x$occurrences)), numeric(1)))) next
    folder <- file.path(directory, evidence$catalogues[[id]]$folder)
    if (dir.exists(folder) && !overwrite) stop("Evidence report already exists: ", folder)
    reference_reports[[id]] <- write_reference_evidence(selected, directory, report_filename,
      unlist(evidence$context_columns, use.names = FALSE), title, stylesheet, logo,
      catalogue = evidence$catalogues[[id]], evidence_assets = file.path("..", assets_directory),
      parent_label = basename(report$summary$input))
  }
  jsonlite::write_json(evidence, file.path(directory, "evidence.json"), auto_unbox = TRUE, null = "null", digits = NA)
  evidence_card_values(parts, reference_reports, paths$conversion, paths$primary_key, evidence$catalogues)
}

# Reference evidence ---------------------------------------------------------

#' Recover frozen lookup definitions for evidence
#' @param model Resolved model, or a saved model with named reference bindings.
#' @return Lookup definitions keyed by reference identifier.
#' @keywords internal
reference_evidence_definitions <- function(model) {
  lookups <- attr(model, "reference_lookups", exact = TRUE)
  if (is.null(lookups)) {
    lookups <- list()
    for (sheet in form.model::form_definition(model)$sheets) for (field in sheet$fields) {
      binding <- field$reference
      if (is.list(binding)) lookups[[paste(binding$catalogue, binding$table, binding$lookup, sep = "/")]] <-
        list(catalogue = binding$catalogue, table = binding$table, name = binding$lookup)
    }
  }
  lookups
}

#' Describe the catalogues used by resolved lookups
#' @param lookups Named resolved lookup definitions.
#' @return Catalogue descriptors with identifier, display label and safe folder.
#' @keywords internal
reference_catalogues <- function(lookups) {
  result <- list()
  for (lookup in lookups) {
    id <- lookup$catalogue
    label <- if (is.null(lookup$catalogue_label)) id else lookup$catalogue_label
    # Hex encoding is deterministic, collision-free and safe as a path component.
    folder <- paste0("reference-", paste(sprintf("%02x", as.integer(charToRaw(enc2utf8(id)))), collapse = ""), "-evidence")
    spec <- list(id = id, label = label, folder = folder)
    if (!is.null(result[[id]]) && !identical(result[[id]], spec)) stop("Conflicting catalogue metadata: ", id)
    result[[id]] <- spec
  }
  result
}

#' Collect reference findings using their structured targets
#' @param detail Formatted finding rows, in original finding order.
#' @param findings Original findings containing structured expectations.
#' @param provenance Snapshot provenance keyed by reference identifier.
#' @param lookups Frozen lookup definitions keyed by reference identifier.
#' @param report Parent report path.
#' @param context_columns Public metadata columns retained in evidence.
#' @return Grouped reference evidence with catalogue, table and lookup identity.
#' @keywords internal
collect_reference_evidence <- function(detail, findings, provenance, lookups, report, context_columns) {
  if (is.null(provenance) || !nrow(findings)) return(NULL)
  targets <- vapply(findings$expected, function(value) {
    if (is.list(value) && is.character(value$reference) && length(value$reference) == 1L) return(value$reference)
    if (is.character(value) && length(value) == 1L && value %in% names(lookups)) return(value)
    ""
  }, character(1))
  keep <- nzchar(targets)
  if (!any(keep)) return(NULL)
  rows <- detail[detail$kind == "validation", , drop = FALSE][keep, , drop = FALSE]
  rows$reference <- targets[keep]
  specs <- lapply(rows$reference, function(id) {
    spec <- lookups[[id]]
    if (is.null(spec)) stop("Missing frozen lookup definition for reference evidence: ", id)
    spec
  })
  rows$catalogue <- vapply(specs, `[[`, character(1), "catalogue")
  rows$table <- vapply(specs, `[[`, character(1), "table")
  rows$lookup <- vapply(specs, `[[`, character(1), "name")
  rows$reason <- rows$message
  summarize_reference_evidence(rows, provenance, report, context_columns,
    c("catalogue", "table", "lookup", "severity", "reason"))
}

#' Select one catalogue from per-form evidence
#' @param parts List of per-form reference evidence tables.
#' @param id Catalogue identifier.
#' @return Per-form tables for that catalogue, retaining NULL entries.
#' @keywords internal
catalogue_evidence_parts <- function(parts, id) lapply(parts, function(rows) {
  if (is.null(rows)) return(NULL)
  rows <- rows[rows$catalogue == id, , drop = FALSE]
  if (nrow(rows)) rows else NULL
})

#' Write reviewer-facing evidence CSV
#' @param rows Data frame containing the columns intended for download.
#' @param path Destination CSV path.
#' @param reference_label Public heading for the reference column.
#' @return `NULL`, invisibly, after writing the CSV file.
#' @details Maps technical column names to readable headings and uses the shared UTF-8 BOM CSV writer.
#' @keywords internal
write_reference_evidence_csv <- function(rows, path, reference_label = "Reference") {
  labels <- c(input = "Reporting form", model_version = "Model version", reference = reference_label,
    value = "Submitted value", sheet = "Sheet", field = "Field", cells = "Cells",
    occurrences = "Occurrences", affected_files = "Affected forms")
  headings <- gsub("_", " ", names(rows), fixed = TRUE)
  headings <- paste0(toupper(substring(headings, 1L, 1L)), substring(headings, 2L))
  known <- names(rows) %in% names(labels)
  headings[known] <- labels[names(rows)[known]]
  names(rows) <- headings
  write_report_csv(rows, path)
}

#' Write one catalogue's reference evidence
#' @param parts List of per-form prepared evidence data frames; empty entries may be `NULL`.
#' @param directory Parent report directory receiving the evidence subdirectory.
#' @param report_filename Filename for the generated HTML page and parent report links.
#' @param context_columns Public metadata columns shown in case and source tables.
#' @param title Parent report title, extended with the evidence label.
#' @param stylesheet,logo Optional branding file paths, or `NULL`.
#' @param evidence_assets Shared asset directory relative to the evidence subdirectory.
#' @param scope Slash-separated aggregate path, or an empty string for the root.
#' @param parent_label Label for the root or single-form parent breadcrumb.
#' @param catalogue Frozen catalogue identifier, label and safe folder name.
#' @return Relative HTML path under `selected evidence`.
#' @details Cases are separated by reference snapshot and value, including severity and reason. Source CSV files supply grouped browser tables and downloads with form names and cell locations. Detailed evidence remains a CSV download; counts are rendered directly in HTML.
#' @keywords internal
write_reference_evidence <- function(parts, directory, report_filename, context_columns,
                                     title, stylesheet, logo, catalogue, evidence_assets = "../report-assets", scope = "", parent_label = "All reports") {
  extra_columns <- c("table", "lookup", "severity", "reason")
  labels <- list(reference = "Reference", cases = "Values to review", filename = "values")
  folder_name <- catalogue$folder
  folder <- file.path(directory, folder_name)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  parts <- Filter(Negate(is.null), parts)
  columns <- unique(c("input", "model_version", "rules_version", context_columns, "reference",
    "snapshot_sha256", "value", extra_columns, "sheet", "field", "column", "rule_id",
    "cells", "occurrences", "report"))
  detail <- if (length(parts)) do.call(rbind, parts) else
    as.data.frame(stats::setNames(rep(list(character()), length(columns)), columns))
  detail <- detail[order(detail$reference, detail$snapshot_sha256, detail$value, detail$input,
                         detail$sheet, detail$field), , drop = FALSE]
  groups <- reference_evidence_groups(detail, c("reference", "snapshot_sha256", "value", extra_columns))
  summary_columns <- c("reference", "model_version", context_columns, "value", extra_columns, "occurrences",
                       "affected_files", "snapshot_sha256")
  cases <- as.data.frame(stats::setNames(rep(list(character()), length(summary_columns)), summary_columns))
  if (length(groups)) {
    cases <- do.call(rbind, lapply(seq_along(groups), function(i) {
      selected <- detail[groups[[i]], , drop = FALSE]
      row <- selected[1L, c("reference", "value", extra_columns, "snapshot_sha256"), drop = FALSE]
      for (key in c("model_version", context_columns)) row[[key]] <- paste(sort(unique(selected[[key]])), collapse = ", ")
      row$occurrences <- sum(as.numeric(selected$occurrences))
      row$affected_files <- length(unique(selected$input))
      row[summary_columns]
    }))
  }
  prefix <- paste0(detail$sheet, ".")
  detail$field <- ifelse(nzchar(detail$sheet) & startsWith(detail$field, prefix),
    substring(detail$field, nchar(prefix) + 1L), detail$field)
  case_columns <- c("reference", "model_version", context_columns, "value", extra_columns, "occurrences", "affected_files")
  evidence_columns <- c("input", "model_version", "reference", "value", extra_columns, "sheet", "field", "cells", "occurrences", "snapshot_sha256")
  summary_file <- paste0(labels$filename, ".csv")
  write_reference_evidence_csv(detail[unique(c("input", case_columns[!case_columns %in% "affected_files"], "sheet", "field", "cells", "snapshot_sha256"))],
    file.path(folder, summary_file), labels$reference)
  write_reference_evidence_csv(detail[evidence_columns], file.path(folder, "evidence.csv"), labels$reference)
  summary <- list(
    tables = list(values = nrow(cases)),
    totals = list(cases = nrow(cases), references = length(unique(detail$reference)),
      forms = length(unique(detail$input)), occurrences = sum(as.numeric(detail$occurrences)))
  )
  write_evidence_page(directory, folder_name, report_filename, title, stylesheet, logo, list(
    report_kind = "reference", evidence_label = paste(catalogue$label, "evidence"),
    cases_label = "Values to review", category_total = "references", references_label = "Lookups",
    description = paste("Reference findings from", catalogue$label),
    instructions = "Review each case before changing a form, a reference table or its matching rules.",
    summary_file = summary_file, summary_label = "Values CSV",
    summary_description = "Reference values and their source locations"), summary = summary,
    evidence_assets = evidence_assets, scope = scope, parent_label = parent_label)
}

# Conversion evidence ---------------------------------------------------------

#' Collect recorded type-conversion failures
#' @param detail Formatted finding rows with kind, rule, value, expectation, location and public metadata columns.
#' @param report Parent form report path relative to the current report context.
#' @param context_columns Public metadata columns to retain.
#' @return A compact evidence data frame, or `NULL` when no supported type findings exist.
#' @details Includes number, integer, boolean and temporal errors or warnings. Excludes coordinate range checks and never converts or trims saved values.
#' @keywords internal
collect_conversion_evidence <- function(detail, report, context_columns) {
  types <- c("number", "integer", "boolean", "date", "datetime", "time")
  keep <- detail$kind == "validation" & detail$severity %in% c("error", "warning") &
    detail$rule_id == paste0("field/", detail$field, "/type") & detail$expected %in% types
  rows <- detail[keep, , drop = FALSE]
  if (!nrow(rows)) return(NULL)
  rows$expected_type <- rows$expected
  keys <- c("model_version", "rules_version", context_columns, "sheet", "field",
    "expected_type", "value", "severity", "column")
  groups <- reference_evidence_groups(rows, keys)
  result <- rows[vapply(groups, `[[`, integer(1), 1L), c("input", keys), drop = FALSE]
  result$cells <- vapply(groups, function(i) reference_evidence_cells(rows$row[i], rows$column[i[1L]]), character(1))
  result$occurrences <- lengths(groups)
  result$report <- paste0("../", report)
  rownames(result) <- NULL
  result
}

#' Write conversion evidence
#' @param parts List of per-form prepared evidence data frames; empty entries may be `NULL`.
#' @param directory Parent report directory receiving the evidence subdirectory.
#' @param report_filename Filename for the generated HTML page and parent report links.
#' @param context_columns Public metadata columns shown in case and source tables.
#' @param title Parent report title, extended with the evidence label.
#' @param stylesheet,logo Optional branding file paths, or `NULL`.
#' @param evidence_assets Shared asset directory relative to the evidence subdirectory.
#' @param scope Slash-separated aggregate path, or an empty string for the root.
#' @param parent_label Label for the root or single-form parent breadcrumb.
#' @return Relative HTML path under `conversion-evidence`.
#' @details Cases retain model, field, expected type, value and severity. Values are not converted. The browser groups source rows from values.csv; downloads retain form names and cell locations. Counts are rendered directly in HTML.
#' @keywords internal
write_conversion_evidence <- function(parts, directory, report_filename, context_columns, title, stylesheet, logo, evidence_assets = "../report-assets", scope = "", parent_label = "All reports") {
  folder_name <- "conversion-evidence"
  folder <- file.path(directory, folder_name)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  keys <- c("model_version", "rules_version", context_columns, "sheet", "field", "expected_type", "value", "severity")
  columns <- c("input", keys, "column", "cells", "occurrences", "report")
  parts <- Filter(Negate(is.null), parts)
  detail <- if (length(parts)) do.call(rbind, parts) else
    as.data.frame(stats::setNames(rep(list(character()), length(columns)), columns))
  # Preserve the full field identifier for grouping; shorten only its display label.
  groups <- reference_evidence_groups(detail, keys)
  case_columns <- c("field", "model_version", context_columns, "sheet", "expected_type", "value", "severity", "occurrences", "affected_files")
  summary_columns <- c(keys, "occurrences", "affected_files")
  cases <- as.data.frame(stats::setNames(rep(list(character()), length(summary_columns)), summary_columns))
  if (length(groups)) cases <- do.call(rbind, lapply(seq_along(groups), function(i) {
    selected <- detail[groups[[i]], , drop = FALSE]
    row <- selected[1L, keys, drop = FALSE]
    row$occurrences <- sum(as.numeric(selected$occurrences))
    row$affected_files <- length(unique(selected$input))
    row[summary_columns]
  }))
  # Shorten displayed field identifiers
  # @param rows Case or evidence rows with sheet and field columns.
  # @return Rows with a matching sheet prefix removed from field labels.
  shorten <- function(rows) {
    prefix <- paste0(rows$sheet, ".")
    rows$field <- ifelse(nzchar(rows$sheet) & startsWith(rows$field, prefix),
      substring(rows$field, nchar(prefix) + 1L), rows$field)
    rows
  }
  cases <- shorten(cases)
  detail <- shorten(detail)
  evidence_columns <- c("input", "model_version", "sheet", "field", "expected_type", "value", "severity", "cells", "occurrences")
  write_reference_evidence_csv(detail[c("input", case_columns[!case_columns %in% "affected_files"], "cells")], file.path(folder, "values.csv"))
  write_reference_evidence_csv(detail[evidence_columns], file.path(folder, "evidence.csv"))
  summary <- list(
    tables = list(values = nrow(cases)),
    filter_cards = list(
      severity = lapply(c("error", "warning"), function(value) list(value = value,
        count = sum(as.numeric(detail$occurrences[detail$severity == value])))),
      expected_type = lapply(unique(c("boolean", "integer", "number", "date", "datetime", detail$expected_type)),
        function(value) list(value = value,
          count = sum(as.numeric(detail$occurrences[detail$expected_type == value]))))),
    totals = list(cases = nrow(cases), types = length(unique(detail$expected_type)),
      forms = length(unique(detail$input)), occurrences = sum(as.numeric(detail$occurrences)))
  )
  write_evidence_page(directory, folder_name, report_filename, title, stylesheet, logo, list(
    report_kind = "conversion", evidence_label = "Conversion errors", cases_label = "Values to review", category_total = "types",
    references_label = "Expected types", summary_file = "values.csv", summary_label = "Values CSV",
    summary_description = "Rejected values by model, sheet, field and expected type",
    description = NULL, instructions = NULL), summary = summary, evidence_assets = evidence_assets, scope = scope, parent_label = parent_label)
}

# Primary-key evidence ---------------------------------------------------------

#' Collect recorded primary-key failures
#' @param detail Formatted finding rows with kind, rule, value, expectation, location and public metadata columns.
#' @param findings Original findings corresponding to detail, with structured key values and expectations.
#' @param report Parent form report path relative to the current report context.
#' @param context_columns Public metadata columns to retain.
#' @return A compact evidence data frame, or `NULL` when no key findings exist.
#' @details Separates missing components, duplicate keys and missing parent keys. Preserves declared component order and physical row ranges; every involved duplicate row contributes an occurrence. No key check is rerun.
#' @keywords internal
collect_primary_key_evidence <- function(detail, findings, report, context_columns) {
  checks <- c("primary_key_present", "primary_key_unique", "parent_key")
  ids <- unlist(lapply(checks, function(check) paste0("sheet/", unique(detail$sheet), "/", check)))
  rows <- detail[detail$kind == "validation" & detail$rule_id %in% ids, , drop = FALSE]
  if (!nrow(rows)) return(NULL)
  raw <- findings[findings$rule_id %in% ids, , drop = FALSE]
  # Keep components in declared order; distinguish text from numbers and missing values.
  rows$value <- vapply(raw$value, function(key) paste(vapply(key, function(value) {
    if (is.null(value) || (length(value) == 1L && is.na(value))) return("(missing)")
    if (is.character(value)) encodeString(value, quote = '\"') else csv_value(value)
  }, character(1)), collapse = " | "), character(1))
  rows$problem <- ifelse(endsWith(rows$rule_id, "/primary_key_present"), "Missing components",
    ifelse(endsWith(rows$rule_id, "/parent_key"), "Missing parent keys", "Duplicate keys"))
  rows$key_fields <- vapply(raw$expected, function(value) paste(value$primary_key, collapse = ", "), character(1))
  rows$parent_sheet <- vapply(raw$expected, function(value) {
    if (is.null(value$parent_sheet)) "" else value$parent_sheet
  }, character(1))
  keys <- c("model_version", "rules_version", context_columns, "sheet", "parent_sheet", "key_fields", "problem", "value", "severity")
  groups <- reference_evidence_groups(rows, keys)
  result <- rows[vapply(groups, `[[`, integer(1), 1L), c("input", keys), drop = FALSE]
  # Row numbers, not invented cell coordinates: a composite key spans several fields.
  result$rows <- vapply(groups, function(i) {
    positions <- sort(unique(as.integer(rows$row[i])))
    first <- c(1L, which(diff(positions) != 1L) + 1L)
    last <- c(first[-1L] - 1L, length(positions))
    paste(ifelse(first == last, positions[first], paste0(positions[first], "-", positions[last])), collapse = ", ")
  }, character(1))
  result$occurrences <- lengths(groups)
  result$report <- paste0("../", report)
  rownames(result) <- NULL
  result
}

#' Write primary-key evidence
#' @param parts List of per-form prepared evidence data frames; empty entries may be `NULL`.
#' @param directory Parent report directory receiving the evidence subdirectory.
#' @param report_filename Filename for the generated HTML page and parent report links.
#' @param context_columns Public metadata columns shown in case and source tables.
#' @param title Parent report title, extended with the evidence label.
#' @param stylesheet,logo Optional branding file paths, or `NULL`.
#' @param evidence_assets Shared asset directory relative to the evidence subdirectory.
#' @param scope Slash-separated aggregate path, or an empty string for the root.
#' @param parent_label Label for the root or single-form parent breadcrumb.
#' @return Relative HTML path under `primary-key-evidence`. Returns `NULL` without writing files when all parts are empty.
#' @details Missing-component, duplicate-key and missing-parent-key tabs read separate source CSV files without a Problem column. Browser tables group their rows; downloads retain form names and row locations. Detailed evidence is a combined CSV download retaining that column. Duplicate findings remain scoped to each source form; shared values across forms do not imply new duplicates. Counts are rendered directly in HTML.
#' @keywords internal
write_primary_key_evidence <- function(parts, directory, report_filename, context_columns, title,
                                       stylesheet, logo, evidence_assets = "../report-assets", scope = "", parent_label = "All reports") {
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) return(NULL)
  parts <- lapply(parts, function(part) {
    if (!"parent_sheet" %in% names(part)) part$parent_sheet <- ""
    if (!"rules_version" %in% names(part)) part$rules_version <- NA_character_
    part
  })
  detail <- do.call(rbind, parts)
  if (!nrow(detail)) return(NULL)
  folder_name <- "primary-key-evidence"
  folder <- file.path(directory, folder_name)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  keys <- c("model_version", "rules_version", context_columns, "sheet", "parent_sheet", "key_fields", "problem", "value", "severity")
  groups <- reference_evidence_groups(detail, keys)
  cases <- do.call(rbind, lapply(seq_along(groups), function(i) {
    selected <- detail[groups[[i]], , drop = FALSE]
    row <- selected[1L, keys, drop = FALSE]
    row$occurrences <- sum(as.numeric(selected$occurrences))
    row$affected_files <- length(unique(selected$input))
    row
  }))
  case_columns <- c("sheet", "model_version", context_columns, "key_fields", "value", "severity", "occurrences", "affected_files")
  evidence_columns <- c("input", "model_version", "sheet", "parent_sheet", "key_fields", "problem", "value", "severity", "rows", "occurrences")
  missing <- cases$problem == "Missing components"
  duplicates <- cases$problem == "Duplicate keys"
  parents <- cases$problem == "Missing parent keys"
  download_columns <- c("input", case_columns[!case_columns %in% "affected_files"], "rows")
  write_reference_evidence_csv(detail[detail$problem == "Missing components", download_columns], file.path(folder, "missing-components.csv"))
  write_reference_evidence_csv(detail[detail$problem == "Duplicate keys", download_columns], file.path(folder, "duplicate-keys.csv"))
  parent_columns <- append(download_columns, "parent_sheet", after = 2L)
  write_reference_evidence_csv(detail[detail$problem == "Missing parent keys", parent_columns], file.path(folder, "missing-parent-keys.csv"))
  write_reference_evidence_csv(detail[evidence_columns], file.path(folder, "evidence.csv"))
  summary <- list(
    tables = list(missing = sum(missing), duplicates = sum(duplicates), parents = sum(parents)),
    totals = list(cases = nrow(cases), sheets = length(unique(detail$sheet)),
      forms = length(unique(detail$input)), occurrences = sum(as.numeric(detail$occurrences)))
  )
  write_evidence_page(directory, folder_name, report_filename, title, stylesheet, logo, list(
    report_kind = "primary-key", evidence_label = "Primary-key evidence", cases_label = "Keys to review", category_total = "sheets",
    references_label = "Affected sheets", summary_file = "missing-components.csv", summary_label = "Missing components CSV",
    summary_description = "Keys with missing components",
    secondary_file = "duplicate-keys.csv", secondary_label = "Duplicate keys CSV",
    secondary_description = "Repeated keys within a form",
    tertiary_file = "missing-parent-keys.csv", tertiary_label = "Missing parent keys CSV",
    tertiary_description = "Child keys absent from their parent sheet",
    description = NULL, instructions = NULL), summary = summary, evidence_assets = evidence_assets, scope = scope, parent_label = parent_label)
}

# Shared grouping, summaries and rendering ---------------------------------------------------------

#' Group evidence rows by exact column values
#' @param rows Evidence data frame.
#' @param keys Column names defining case identity.
#' @return List of integer row-index vectors in first-occurrence order; an empty list for no rows.
#' @details Groups integer encodings of column values so separators in submitted text do not create collisions.
#' @keywords internal
reference_evidence_groups <- function(rows, keys) {
  if (!nrow(rows)) return(list())
  codes <- lapply(rows[keys], function(x) match(x, unique(x)))
  ids <- do.call(paste, c(codes, sep = ":"))
  group <- match(ids, unique(ids))
  split(seq_len(nrow(rows)), factor(group, levels = seq_len(max(group))))
}

#' Compact exact Excel cell locations
#' @param rows Positive physical Excel row numbers. Duplicates are removed.
#' @param column One positive physical Excel column number.
#' @return Comma-separated A1 cells and contiguous ranges.
#' @details Sorts rows without filling gaps and rejects missing or invalid coordinates.
#' @keywords internal
reference_evidence_cells <- function(rows, column) {
  column <- as.integer(column)
  rows <- sort(unique(as.integer(rows)))
  if (!length(rows) || anyNA(rows) || any(rows < 1L) || is.na(column) || column < 1L)
    stop("Reference evidence requires physical cell coordinates")
  letters <- character()
  while (column > 0L) {
    letters <- c(LETTERS[(column - 1L) %% 26L + 1L], letters)
    column <- (column - 1L) %/% 26L
  }
  prefix <- paste0(letters, collapse = "")
  first <- c(1L, which(diff(rows) != 1L) + 1L)
  last <- c(first[-1L] - 1L, length(rows))
  paste(ifelse(first == last, paste0(prefix, rows[first]),
    paste0(prefix, rows[first], ":", prefix, rows[last])), collapse = ", ")
}

#' Prepare reference evidence with snapshot identity
#' @param rows Recorded findings with a reference identifier column.
#' @param provenance Reference records keyed by identifier, including available status and SHA-256.
#' @param report Parent form report path relative to the current report context.
#' @param context_columns Public metadata columns to retain.
#' @param case_columns Additional case identity columns, such as severity and reason.
#' @return One row per case and source location group, with exact cell ranges, occurrence counts and a parent report link.
#' @details Requires available snapshot provenance for every reference. Hashes distinguish snapshots; no reference contents are copied.
#' @keywords internal
summarize_reference_evidence <- function(rows, provenance, report, context_columns, case_columns = character()) {
  references <- unique(rows$reference)
  snapshots <- lapply(references, function(id) {
    snapshot <- provenance[[id]]
    if (is.null(snapshot) || !identical(snapshot$status, "available") ||
        is.null(snapshot$sha256) || !grepl("^[a-f0-9]{64}$", snapshot$sha256))
      stop("Missing reference evidence provenance: ", id)
    snapshot
  })
  snapshot_index <- match(rows$reference, references)
  rows$snapshot_sha256 <- vapply(snapshots, `[[`, character(1), "sha256")[snapshot_index]
  keys <- c("reference", "snapshot_sha256", "value", case_columns, "sheet", "field", "column", "rule_id")
  groups <- reference_evidence_groups(rows, keys)
  result <- rows[vapply(groups, `[[`, integer(1), 1L),
    unique(c("input", "model_version", "rules_version", context_columns, keys)), drop = FALSE]
  result$cells <- vapply(groups, function(i) reference_evidence_cells(rows$row[i], rows$column[i[1L]]), character(1))
  result$occurrences <- lengths(groups)
  result$report <- paste0("../", report)
  rownames(result) <- NULL
  result
}

#' Prepare evidence navigation cards
#' @param parts Named list of evidence kinds, each containing a list of per-form
#'   data frames or `NULL` entries. Counts sum their `occurrences` columns.
#' @param reference_reports Named catalogue report paths.
#' @param catalogues Frozen catalogue identifiers, labels and folders.
#' @param conversion_report,primary_key_report
#'   Relative HTML paths for nonempty evidence reports, or `NULL` when absent.
#' @return A list of card template values, one per catalogue and structural evidence kind.
#' @keywords internal
evidence_card_values <- function(parts, reference_reports, conversion_report, primary_key_report = NULL, catalogues = list()) {
  cards <- list()
  append_card <- function(label, rows, path, description) {
    count <- sum(vapply(rows, function(x) sum(as.numeric(x$occurrences)), numeric(1)))
    cards[[length(cards) + 1L]] <<- list(label = label, description = description,
      report = if (count > 0) path else NULL, empty = if (count == 0) "0" else NULL,
      count = format(count, big.mark = ",", scientific = FALSE, trim = TRUE))
  }
  for (id in names(catalogues)) append_card(paste(catalogues[[id]]$label, "evidence"),
    catalogue_evidence_parts(parts$reference, id), reference_reports[[id]], "Reference values to review in this context")
  append_card("Conversion errors", parts$conversion, conversion_report, "Rejected values by expected type in this context")
  append_card("Primary-key evidence", parts$primary_key, primary_key_report, "Missing components, duplicate keys and missing parent keys")
  cards
}

#' Render the dynamically prepared evidence cards
#' @param cards List of escaped-at-render-time card template values.
#' @return HTML lines containing the evidence navigation.
#' @keywords internal
render_evidence_cards <- function(cards) {
  html <- vapply(cards, function(card) paste(render_report_template("evidence-card.html", card), collapse = "\n"), character(1))
  sub("<!-- cards -->", paste(html, collapse = "\n"), render_report_template("evidence-cards.html", list()), fixed = TRUE)
}

#' Render a shared evidence report page
#' @param directory Parent report directory receiving the evidence subdirectory.
#' @param folder_name Evidence subdirectory already containing its CSV data.
#' @param report_filename Filename for the generated HTML page and parent report links.
#' @param title Parent report title, extended with the evidence label.
#' @param stylesheet,logo Optional branding file paths, or `NULL`.
#' @param labels Template values for headings, counts, descriptions and CSV resource links.
#' @param summary Table sizes, overview totals and optional conversion filter counts.
#' @param evidence_assets Shared asset directory relative to the evidence subdirectory.
#' @param scope Slash-separated aggregate path, or an empty string for the root.
#' @param parent_label Label for the root or single-form parent breadcrumb.
#' @return Relative HTML path under the evidence subdirectory.
#' @details Copies content-addressed assets, builds escaped ancestor breadcrumbs and writes the shared external template. Does not collect findings or write case data.
#' @keywords internal
write_evidence_page <- function(directory, folder_name, report_filename, title, stylesheet, logo, labels, summary, evidence_assets = "../report-assets", scope = "", parent_label = "All reports") {
  folder <- file.path(directory, folder_name)
  # Remove artifacts retired by the CSV-backed report layout when rebuilding in place.
  unlink(file.path(folder, c("evidence", "metadata.json", "summary.json", "keys.csv")), recursive = TRUE)
  if (is.null(labels$secondary_file)) {
    labels[c("secondary_file", "secondary_label", "secondary_description")] <- list(NULL, NULL, NULL)
  }
  if (is.null(labels$tertiary_file)) {
    labels[c("tertiary_file", "tertiary_label", "tertiary_description")] <- list(NULL, NULL, NULL)
  }
  sources <- stats::setNames(system.file("report",
    c("report.css", "overview.css", "report-ui.js", "csv-worker.js", "report-table.js", "reference-evidence.js"),
    package = "form.validation", mustWork = TRUE),
    c("style", "overview_style", "loading_script", "csv_worker", "table_script", "script"))
  if (!is.null(stylesheet)) sources <- c(sources, stylesheet = stylesheet)
  if (!is.null(logo)) sources <- c(sources, logo = logo)
  assets <- paste0(evidence_assets, "/",
    copy_report_assets(sources, file.path(folder, evidence_assets)))
  names(assets) <- names(sources)
  counts <- summary$totals
  counts$categories <- counts[[labels$category_total]]
  table_counts <- c(list(values = 0, missing = 0, duplicates = 0, parents = 0), summary$tables)
  table_counts <- table_counts[!duplicated(names(table_counts), fromLast = TRUE)]
  values <- c(report_count_values(counts), report_count_values(table_counts),
    list(filters_hidden = if (is.null(summary$filter_cards)) "hidden" else ""),
    as.list(assets), labels, list(title = paste(title, "\u2014", labels$evidence_label),
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
    parent_report = paste0("../", report_filename)))
  if (is.null(stylesheet)) values["stylesheet"] <- list(NULL)
  if (is.null(logo)) values["logo"] <- list(NULL)
  # Evidence lives one level below its aggregate; expose every ancestor.
  parts <- if (nzchar(scope)) strsplit(scope, "/", fixed = TRUE)[[1L]] else character()
  ancestors <- c(parent_label, parts)
  breadcrumbs <- vapply(seq_along(ancestors), function(i) paste(
    render_report_template("breadcrumb-link.html", list(label = ancestors[i],
      href = paste0(strrep("../", length(ancestors) - i + 1L), report_filename))), collapse = "\n"), character(1))
  html <- render_report_template("reference-evidence.html", values)
  html <- sub("<!-- evidence-breadcrumbs -->", paste(breadcrumbs, collapse = "\n"), html, fixed = TRUE)
  for (key in c("severity", "expected_type")) {
    cards <- summary$filter_cards[[key]]
    filter_labels <- c(error = "Errors", warning = "Warnings", boolean = "Boolean", integer = "Integer",
      number = "Number", date = "Date", datetime = "Date and time", time = "Time")
    filters <- vapply(cards, function(card) paste(render_report_template("evidence-filter-card.html",
      c(report_count_values(list(filter = card$count)), list(key = key, value = card$value,
        label = if (card$value %in% names(filter_labels)) filter_labels[[card$value]] else card$value,
        tone = if (key == "severity") if (card$value == "error") "error" else "attention" else "neutral"))),
      collapse = "\n"), character(1))
    html <- sub(paste0("<!-- ", key, "-cards -->"), paste(filters, collapse = "\n"), html, fixed = TRUE)
  }
  writeLines(html, file.path(folder, report_filename), useBytes = TRUE)
  file.path(folder_name, report_filename)
}
