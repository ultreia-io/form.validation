# Collect only completed built-in code-list findings from one input.
# Caller provenance identifies the exact available snapshot used by that input.
#' Collect completed code-list findings
#' @param detail Formatted finding rows with kind, rule, value, expectation, location and public metadata columns.
#' @param provenance Reference records keyed by identifier, including available status and SHA-256.
#' @param report Parent form report path relative to the current report context.
#' @param context_columns Public metadata columns to retain.
#' @return A compact evidence data frame, or `NULL` when no code-list findings exist.
#' @details Selects built-in reference checks; incomplete execution records do not contribute cases.
#' @keywords internal
collect_code_list_evidence <- function(detail, provenance, report, context_columns) {
  keep <- detail$kind == "validation" &
    (detail$rule_id == paste0("field/", detail$field, "/reference") |
     detail$rule_id == paste0("field/", detail$field, "/code_list"))
  rows <- detail[keep, , drop = FALSE]
  if (!nrow(rows)) return(NULL)
  rows$reference <- rows$expected
  summarize_reference_evidence(rows, provenance, report, context_columns)
}
