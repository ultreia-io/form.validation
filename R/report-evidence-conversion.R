# Collect recorded type failures only; never parse, trim or coerce submitted values.
# Coordinate types combine numeric and range validation and are deliberately excluded.
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

# Group conversion cases by model, field and unchanged submitted value.
write_conversion_evidence <- function(parts, directory, report_filename, context_columns, title, stylesheet, logo, evidence_assets = "../report-assets", scope = "", parent_label = "All reports") {
  folder_name <- "conversion-evidence"
  folder <- file.path(directory, folder_name)
  dir.create(file.path(folder, "evidence"), recursive = TRUE, showWarnings = FALSE)
  keys <- c("model_version", "rules_version", context_columns, "sheet", "field", "expected_type", "value", "severity")
  columns <- c("input", keys, "column", "cells", "occurrences", "report")
  parts <- Filter(Negate(is.null), parts)
  detail <- if (length(parts)) do.call(rbind, parts) else
    as.data.frame(stats::setNames(rep(list(character()), length(columns)), columns))
  # Preserve the full field identifier for grouping; shorten only its display label.
  groups <- reference_evidence_groups(detail, keys)
  case_ids <- sprintf("conversion-%04d", seq_along(groups))
  case_columns <- c("field", "model_version", context_columns, "sheet", "expected_type", "value", "severity", "occurrences", "affected_files")
  summary_columns <- c("case_id", keys, "occurrences", "affected_files", "evidence_file")
  cases <- as.data.frame(stats::setNames(rep(list(character()), length(summary_columns)), summary_columns))
  if (length(groups)) cases <- do.call(rbind, lapply(seq_along(groups), function(i) {
    selected <- detail[groups[[i]], , drop = FALSE]
    row <- selected[1L, keys, drop = FALSE]
    row$case_id <- case_ids[i]
    row$occurrences <- sum(as.numeric(selected$occurrences))
    row$affected_files <- length(unique(selected$input))
    row$evidence_file <- paste0("evidence/", row$case_id, ".json")
    row[summary_columns]
  }))
  shorten <- function(rows) {
    prefix <- paste0(rows$sheet, ".")
    rows$field <- ifelse(nzchar(rows$sheet) & startsWith(rows$field, prefix),
      substring(rows$field, nchar(prefix) + 1L), rows$field)
    rows
  }
  cases <- shorten(cases)
  detail <- shorten(detail)
  evidence_columns <- c("input", "model_version", "sheet", "field", "expected_type", "value", "severity", "cells", "occurrences")
  write_reference_evidence_csv(cases[case_columns], file.path(folder, "values.csv"))
  write_reference_evidence_csv(detail[evidence_columns], file.path(folder, "evidence.csv"))
  for (i in seq_along(groups)) {
    rows <- detail[groups[[i]], unique(c(evidence_columns, context_columns, "report")), drop = FALSE]
    jsonlite::write_json(rows, file.path(folder, cases$evidence_file[i]), auto_unbox = TRUE)
  }
  jsonlite::write_json(list(
    labels = list(cases = "Values to review", filename = "values"),
    filter_cards = list(
      severity = lapply(c("error", "warning"), function(value) list(value = value,
        count = sum(as.numeric(detail$occurrences[detail$severity == value])))),
      expected_type = lapply(unique(c("boolean", "integer", "number", "date", "datetime", detail$expected_type)),
        function(value) list(value = value,
          count = sum(as.numeric(detail$occurrences[detail$expected_type == value]))))),
    case_heading_columns = as.list(c("sheet", "field", "expected_type")),
    context_columns = as.list(context_columns), case_columns = as.list(case_columns),
    evidence_columns = as.list(evidence_columns),
    detail_columns = as.list(c("input", "model_version", context_columns, "sheet", "field", "severity", "cells", "occurrences")),
    cases = cases[c("case_id", case_columns, "evidence_file")],
    totals = list(cases = nrow(cases), types = length(unique(detail$expected_type)),
      forms = length(unique(detail$input)), occurrences = sum(as.numeric(detail$occurrences)))
  ), file.path(folder, "metadata.json"), auto_unbox = TRUE, pretty = TRUE)
  write_evidence_page(directory, folder_name, report_filename, title, stylesheet, logo, list(
    evidence_label = "Conversion errors", cases_label = "Values to review", category_total = "types",
    references_label = "Expected types", summary_file = "values.csv", summary_label = "Values CSV",
    summary_description = "Rejected values by model, sheet, field and expected type",
    description = NULL, instructions = NULL), evidence_assets = evidence_assets, scope = scope, parent_label = parent_label)
}
