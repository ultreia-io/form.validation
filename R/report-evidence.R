# Evidence pipeline: validation_report_data() prepares each form's evidence once.
# write_individual_evidence() renders its pages and saves evidence.json beside them.
# Aggregation reads that JSON and scopes the prepared rows to each parent report.
# These functions neither read submitted forms nor execute validation rules.

#' Prepare reusable evidence for one form
#'
#' Collect recorded code-list, registry, conversion and primary-key findings into
#' compact tables, retaining their values, occurrence counts and source locations.
#' @param tables Formatted summary and details from `validation_result_tables()`.
#' @param provenance Reference snapshot records keyed by identifier, or `NULL` to
#'   omit code-list and registry evidence. Available records include status and hash.
#' @param context Named list of scalar public text values carried into evidence.
#'   Names must not conflict with evidence or report columns.
#' @param findings Original `result$findings` corresponding to `tables`, supplying
#'   structured primary-key expectations without parsing the formatted CSV text.
#' @return A serializable list with `schema_version`, `context_columns` and `parts`.
#'   Each part (`reference`, `registry`, `conversion`, `primary_key`) is a data frame
#'   or `NULL` when empty. Report links are added by the writer, not stored here.
#' @details Snapshot hashes distinguish reference cases; snapshot contents are not
#'   included. Aggregates reuse these tables rather than collecting evidence again.
#' @keywords internal
prepare_report_evidence <- function(tables, provenance, context, findings) {
  if (!is.list(context) || (length(context) && (is.null(names(context)) ||
      any(!nzchar(names(context))) || anyDuplicated(names(context))))) stop("context must be a named list")
  reserved <- c(names(tables$details), names(tables$summary), "report", "cells", "occurrences", "snapshot_sha256", "rows", "key_fields", "problem")
  if (length(intersect(names(context), reserved))) stop("Context names conflict with report columns")
  if (any(!vapply(context, function(x) is.character(x) && length(x) == 1L && !is.na(x), logical(1))))
    stop("Context values must be scalar text")
  detail <- tables$details
  # Match the public CSV representation without writing and reading it again.
  detail[] <- lapply(detail, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
  for (key in c("input", "model_version", "rules_version")) detail[[key]] <- rep(tables$summary[[key]], nrow(detail))
  for (key in names(context)) detail[[key]] <- rep(context[[key]], nrow(detail))
  parts <- list(
    reference = if (!is.null(provenance)) collect_code_list_evidence(detail, provenance, "", names(context)) else NULL,
    registry = if (!is.null(provenance)) collect_registry_evidence(detail, provenance, "", names(context)) else NULL,
    conversion = collect_conversion_evidence(detail, "", names(context)),
    primary_key = collect_primary_key_evidence(detail, findings, "", names(context)))
  parts <- lapply(parts, function(rows) { if (!is.null(rows)) rows$report <- NULL; rows })
  list(schema_version = 2L, context_columns = as.list(names(context)), parts = parts)
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
  if (!identical(value$schema_version, 2L) ||
      !all(c("reference", "registry", "conversion", "primary_key") %in% names(value$parts)))
    stop("Unsupported prepared evidence: ", path)
  value
}

#' Prepare evidence navigation cards
#' @param parts Named list of evidence kinds, each containing a list of per-form
#'   data frames or `NULL` entries. Counts sum their `occurrences` columns.
#' @param reference_report,registry_report,conversion_report,primary_key_report
#'   Relative HTML paths for nonempty evidence reports, or `NULL` when absent.
#' @return Named template values: `<kind>_count` is a formatted occurrence count,
#'   `<kind>_report` supplies the active link, and `<kind>_empty` supplies a disabled
#'   zero card. Only the applicable active or empty card is rendered.
#' @keywords internal
evidence_card_values <- function(parts, reference_report, registry_report, conversion_report, primary_key_report = NULL) {
  paths <- list(reference = reference_report, registry = registry_report, conversion = conversion_report, primary_key = primary_key_report)
  result <- list()
  for (kind in names(paths)) {
    count <- sum(vapply(parts[[kind]], function(rows) sum(as.numeric(rows$occurrences)), numeric(1)))
    result[paste0(kind, "_report")] <- list(if (count > 0) paths[[kind]] else NULL)
    result[paste0(kind, "_empty")] <- list(if (count == 0) "0" else NULL)
    result[[paste0(kind, "_count")]] <- format(count, big.mark = ",", scientific = FALSE, trim = TRUE)
  }
  result
}

#' Write individual evidence reports and their reusable data
#' @param report Prepared report from [validation_report_data()].
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
  if (is.null(evidence)) stop("Report has no prepared evidence; call validation_report_data() again")
  kinds <- c(reference = "code-list", registry = "registry", conversion = "conversion", primary_key = "primary-key")
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
    paths[[kind]] <- if (kind == "primary_key") do.call(write_primary_key_evidence, arguments) else if (kind == "conversion") do.call(write_conversion_evidence, arguments) else
      do.call(write_reference_evidence, c(arguments, list(kind = kinds[[kind]])))
  }
  jsonlite::write_json(evidence, file.path(directory, "evidence.json"), auto_unbox = TRUE, null = "null", digits = NA)
  evidence_card_values(parts, paths$reference, paths$registry, paths$conversion, paths$primary_key)
}
