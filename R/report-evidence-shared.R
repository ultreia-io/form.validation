# Group saved findings without changing their raw value identity.
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

# Compress physical coordinates into exact A1 ranges, without truncating evidence.
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

# Preserve snapshot identity and compress repeated cells for each submitted value.
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

# Export reviewer-facing columns with readable headings.
#' Write reviewer-facing evidence CSV
#' @param rows Data frame containing the columns intended for download.
#' @param path Destination CSV path.
#' @param reference_label Public heading for the reference column.
#' @return `NULL`, invisibly, after writing the CSV file.
#' @details Maps technical column names to readable headings and uses the shared UTF-8 BOM CSV writer.
#' @keywords internal
write_reference_evidence_csv <- function(rows, path, reference_label = "Code list") {
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

# Write a focused report alongside aggregate reports. All HTML lives in templates.
#' Write code-list or registry evidence
#' @param parts List of per-form prepared evidence data frames; empty entries may be `NULL`.
#' @param directory Parent report directory receiving the evidence subdirectory.
#' @param report_filename Filename for the generated HTML page and parent report links.
#' @param context_columns Public metadata columns shown in case and source tables.
#' @param title Parent report title, extended with the evidence label.
#' @param stylesheet,logo Optional branding file paths, or `NULL`.
#' @param evidence_assets Shared asset directory relative to the evidence subdirectory.
#' @param scope Slash-separated aggregate path, or an empty string for the root.
#' @param parent_label Label for the root or single-form parent breadcrumb.
#' @param kind Either `code-list` or `registry`.
#' @return Relative HTML path under `selected evidence`.
#' @details Cases are separated by reference snapshot and value, with registry severity and reason where applicable. Writes case and evidence CSV files, metadata JSON, per-case JSON and HTML using shared assets.
#' @keywords internal
write_reference_evidence <- function(parts, directory, report_filename, context_columns,
                                     title, stylesheet, logo, kind = c("code-list", "registry"), evidence_assets = "../report-assets", scope = "", parent_label = "All reports") {
  kind <- match.arg(kind)
  registry <- kind == "registry"
  extra_columns <- if (registry) c("severity", "reason") else character()
  labels <- if (registry) list(reference = "Registry", cases = "Values to review", filename = "values") else
    list(reference = "Code list", cases = "Codes to review", filename = "codes")
  folder_name <- paste0(kind, "-evidence")
  folder <- file.path(directory, folder_name)
  dir.create(file.path(folder, "evidence"), recursive = TRUE, showWarnings = FALSE)
  parts <- Filter(Negate(is.null), parts)
  columns <- unique(c("input", "model_version", "rules_version", context_columns, "reference",
    "snapshot_sha256", "value", extra_columns, "sheet", "field", "column", "rule_id",
    "cells", "occurrences", "report"))
  detail <- if (length(parts)) do.call(rbind, parts) else
    as.data.frame(stats::setNames(rep(list(character()), length(columns)), columns))
  detail <- detail[order(detail$reference, detail$snapshot_sha256, detail$value, detail$input,
                         detail$sheet, detail$field), , drop = FALSE]
  groups <- reference_evidence_groups(detail, c("reference", "snapshot_sha256", "value", extra_columns))
  summary_columns <- c("case_id", "reference", "model_version", context_columns, "value", extra_columns, "occurrences",
                       "affected_files", "snapshot_sha256", "evidence_file")
  cases <- as.data.frame(stats::setNames(rep(list(character()), length(summary_columns)), summary_columns))
  case_prefix <- if (registry) "registry" else "code"
  case_ids <- sprintf(paste0(case_prefix, "-%04d"), seq_along(groups))
  detail$case_id <- rep("", nrow(detail))
  if (length(groups)) {
    detail$case_id[unlist(groups, use.names = FALSE)] <- rep(case_ids, lengths(groups))
    cases <- do.call(rbind, lapply(seq_along(groups), function(i) {
      selected <- detail[groups[[i]], , drop = FALSE]
      row <- selected[1L, c("reference", "value", extra_columns, "snapshot_sha256"), drop = FALSE]
      row$case_id <- case_ids[i]
      for (key in c("model_version", context_columns)) row[[key]] <- paste(sort(unique(selected[[key]])), collapse = ", ")
      row$occurrences <- sum(as.numeric(selected$occurrences))
      row$affected_files <- length(unique(selected$input))
      row$evidence_file <- paste0("evidence/", row$case_id, ".json")
      row[summary_columns]
    }))
  }
  prefix <- paste0(detail$sheet, ".")
  detail$field <- ifelse(nzchar(detail$sheet) & startsWith(detail$field, prefix),
    substring(detail$field, nchar(prefix) + 1L), detail$field)
  case_columns <- c("reference", "model_version", context_columns, "value", extra_columns, "occurrences", "affected_files")
  evidence_columns <- c("input", "model_version", "reference", "value", extra_columns, "sheet", "field", "cells", "occurrences")
  summary_file <- paste0(labels$filename, ".csv")
  write_reference_evidence_csv(cases[case_columns], file.path(folder, summary_file), labels$reference)
  write_reference_evidence_csv(detail[evidence_columns], file.path(folder, "evidence.csv"), labels$reference)
  for (i in seq_along(groups)) {
    rows <- detail[groups[[i]], unique(c(evidence_columns, context_columns, "report")), drop = FALSE]
    jsonlite::write_json(rows, file.path(folder, cases$evidence_file[i]), auto_unbox = TRUE)
  }
  jsonlite::write_json(list(
    context_columns = as.list(context_columns), labels = labels,
    case_columns = as.list(case_columns), evidence_columns = as.list(evidence_columns),
    detail_columns = as.list(c("input", "model_version", context_columns, extra_columns, "sheet", "field", "cells", "occurrences")),
    cases = cases[setdiff(names(cases), "snapshot_sha256")],
    totals = list(cases = nrow(cases), references = length(unique(detail$reference)),
      forms = length(unique(detail$input)), occurrences = sum(as.numeric(detail$occurrences)))
  ), file.path(folder, "metadata.json"), auto_unbox = TRUE, pretty = TRUE)
  evidence_label <- if (registry) "Registry evidence" else "Code-list evidence"
  write_evidence_page(directory, folder_name, report_filename, title, stylesheet, logo, list(
    evidence_label = evidence_label, cases_label = labels$cases, category_total = "references",
    references_label = if (registry) "Registries" else "Code lists",
    description = if (registry)
      "Values with no unique match in the registries used during validation. Select a value to inspect its source evidence." else
      "Values not found in the code lists used during validation. Select a value to inspect its source evidence.",
    instructions = if (registry) "Review each case before changing a form, a registry or its matching rules." else
      "Review each case before changing a form, a code list or its mapping.",
    summary_file = summary_file, summary_label = if (registry) "Values CSV" else "Codes CSV",
    summary_description = if (registry) "Unmatched or ambiguous values by registry" else "Rejected values by code list"), evidence_assets = evidence_assets, scope = scope, parent_label = parent_label)
}

# Render all evidence pages with the same external template and interactive assets.
#' Render a shared evidence report page
#' @param directory Parent report directory receiving the evidence subdirectory.
#' @param folder_name Evidence subdirectory already containing its CSV and JSON data.
#' @param report_filename Filename for the generated HTML page and parent report links.
#' @param title Parent report title, extended with the evidence label.
#' @param stylesheet,logo Optional branding file paths, or `NULL`.
#' @param labels Template values for headings, counts, descriptions and CSV resource links.
#' @param evidence_assets Shared asset directory relative to the evidence subdirectory.
#' @param scope Slash-separated aggregate path, or an empty string for the root.
#' @param parent_label Label for the root or single-form parent breadcrumb.
#' @return Relative HTML path under the evidence subdirectory.
#' @details Copies content-addressed assets, builds escaped ancestor breadcrumbs and writes the shared external template. Does not collect findings or write case data.
#' @keywords internal
write_evidence_page <- function(directory, folder_name, report_filename, title, stylesheet, logo, labels, evidence_assets = "../report-assets", scope = "", parent_label = "All reports") {
  folder <- file.path(directory, folder_name)
  sources <- stats::setNames(system.file("report",
    c("report.css", "overview.css", "report-ui.js", "csv-worker.js", "report-table.js", "reference-evidence.js"),
    package = "form.validation", mustWork = TRUE),
    c("style", "overview_style", "loading_script", "csv_worker", "table_script", "script"))
  if (!is.null(stylesheet)) sources <- c(sources, stylesheet = stylesheet)
  if (!is.null(logo)) sources <- c(sources, logo = logo)
  assets <- paste0(evidence_assets, "/",
    copy_report_assets(sources, file.path(folder, evidence_assets)))
  names(assets) <- names(sources)
  values <- c(as.list(assets), labels, list(title = paste(title, "\u2014", labels$evidence_label),
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
  writeLines(html, file.path(folder, report_filename), useBytes = TRUE)
  file.path(folder_name, report_filename)
}
