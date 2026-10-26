# Retain the saved key findings and their physical rows; never rerun key checks.
#' Collect recorded primary-key failures
#' @param detail Formatted finding rows with kind, rule, value, expectation, location and public metadata columns.
#' @param findings Original findings corresponding to detail, with structured key values and expectations.
#' @param report Parent form report path relative to the current report context.
#' @param context_columns Public metadata columns to retain.
#' @return A compact evidence data frame, or `NULL` when no key findings exist.
#' @details Separates missing components and duplicate keys. Preserves declared component order and physical row ranges; every involved duplicate row contributes an occurrence. No key check is rerun.
#' @keywords internal
collect_primary_key_evidence <- function(detail, findings, report, context_columns) {
  checks <- c("primary_key_present", "primary_key_unique")
  ids <- unlist(lapply(checks, function(check) paste0("sheet/", unique(detail$sheet), "/", check)))
  rows <- detail[detail$kind == "validation" & detail$rule_id %in% ids, , drop = FALSE]
  if (!nrow(rows)) return(NULL)
  raw <- findings[findings$rule_id %in% ids, , drop = FALSE]
  # Keep components in declared order; distinguish text from numbers and missing values.
  rows$value <- vapply(raw$value, function(key) paste(vapply(key, function(value) {
    if (is.null(value) || (length(value) == 1L && is.na(value))) return("(missing)")
    if (is.character(value)) encodeString(value, quote = '\"') else csv_value(value)
  }, character(1)), collapse = " | "), character(1))
  rows$problem <- ifelse(endsWith(rows$rule_id, "/primary_key_present"), "Missing components", "Duplicate keys")
  rows$key_fields <- vapply(raw$expected, function(value) paste(value$primary_key, collapse = ", "), character(1))
  keys <- c("model_version", "rules_version", context_columns, "sheet", "key_fields", "problem", "value", "severity")
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

# Reuse one form's evidence at every aggregate level, preserving form-local failures.
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
#' @details Creates missing-component and duplicate-key tabs. Duplicate findings remain scoped to each source form; shared values across forms do not imply new duplicates. Writes case and evidence CSV files, metadata JSON, per-case JSON and HTML using shared assets.
#' @keywords internal
write_primary_key_evidence <- function(parts, directory, report_filename, context_columns, title,
                                       stylesheet, logo, evidence_assets = "../report-assets", scope = "", parent_label = "All reports") {
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) return(NULL)
  detail <- do.call(rbind, parts)
  if (!nrow(detail)) return(NULL)
  folder_name <- "primary-key-evidence"
  folder <- file.path(directory, folder_name)
  dir.create(file.path(folder, "evidence"), recursive = TRUE, showWarnings = FALSE)
  keys <- c("model_version", "rules_version", context_columns, "sheet", "key_fields", "problem", "value", "severity")
  groups <- reference_evidence_groups(detail, keys)
  cases <- do.call(rbind, lapply(seq_along(groups), function(i) {
    selected <- detail[groups[[i]], , drop = FALSE]
    row <- selected[1L, keys, drop = FALSE]
    row$case_id <- sprintf("key-%04d", i)
    row$occurrences <- sum(as.numeric(selected$occurrences))
    row$affected_files <- length(unique(selected$input))
    row$evidence_file <- paste0("evidence/", row$case_id, ".json")
    row
  }))
  case_columns <- c("sheet", "model_version", context_columns, "key_fields", "value", "severity", "occurrences", "affected_files")
  evidence_columns <- c("input", "model_version", "sheet", "key_fields", "problem", "value", "severity", "rows", "occurrences")
  write_reference_evidence_csv(cases[c("problem", case_columns)], file.path(folder, "keys.csv"))
  write_reference_evidence_csv(detail[evidence_columns], file.path(folder, "evidence.csv"))
  for (i in seq_along(groups)) {
    rows <- detail[groups[[i]], unique(c(evidence_columns, context_columns, "report")), drop = FALSE]
    jsonlite::write_json(rows, file.path(folder, cases$evidence_file[i]), auto_unbox = TRUE)
  }
  jsonlite::write_json(list(
    labels = list(cases = "Keys to review", filename = "keys", value = "Key values", rows = "Excel rows"),
    case_tabs = list(list(id = "missing", label = "Missing components", key = "problem", value = "Missing components"),
      list(id = "duplicates", label = "Duplicate keys", key = "problem", value = "Duplicate keys")),
    case_heading_columns = as.list(c("sheet", "problem")),
    context_columns = as.list(context_columns), case_columns = as.list(case_columns),
    evidence_columns = as.list(evidence_columns), detail_columns = as.list(evidence_columns),
    cases = cases[c("case_id", "problem", case_columns, "evidence_file")],
    totals = list(cases = nrow(cases), sheets = length(unique(detail$sheet)),
      forms = length(unique(detail$input)), occurrences = sum(as.numeric(detail$occurrences)))
  ), file.path(folder, "metadata.json"), auto_unbox = TRUE, pretty = TRUE)
  write_evidence_page(directory, folder_name, report_filename, title, stylesheet, logo, list(
    evidence_label = "Primary-key evidence", cases_label = "Keys to review", category_total = "sheets",
    references_label = "Affected sheets", summary_file = "keys.csv", summary_label = "Keys CSV",
    summary_description = "Missing components and duplicate keys",
    description = NULL, instructions = NULL), evidence_assets = evidence_assets, scope = scope, parent_label = parent_label)
}
