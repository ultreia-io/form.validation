# Retain the saved key findings and their physical rows; never rerun key checks.
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
#' @details Missing-component, duplicate-key and missing-parent-key tabs read separate summary CSV files without a Problem column. Detailed evidence is a combined CSV download retaining that column. Duplicate findings remain scoped to each source form; shared values across forms do not imply new duplicates. Counts are rendered directly in HTML.
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
  write_reference_evidence_csv(cases[missing, case_columns], file.path(folder, "missing-components.csv"))
  write_reference_evidence_csv(cases[duplicates, case_columns], file.path(folder, "duplicate-keys.csv"))
  parent_columns <- append(case_columns, "parent_sheet", after = 1L)
  write_reference_evidence_csv(cases[parents, parent_columns], file.path(folder, "missing-parent-keys.csv"))
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
