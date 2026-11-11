# Internal helpers ----------------------------------------------------------

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
