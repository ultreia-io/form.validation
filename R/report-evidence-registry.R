# Registry callbacks record their target and required match count in expected.
# Only completed findings are evidence; unavailable registries stay unfinished checks.
#' Collect completed registry findings
#' @param detail Formatted finding rows with kind, rule, value, expectation, location and public metadata columns.
#' @param provenance Reference records keyed by identifier, including available status and SHA-256.
#' @param report Parent form report path relative to the current report context.
#' @param context_columns Public metadata columns to retain.
#' @return A compact evidence data frame, or `NULL` when no registry findings exist.
#' @details Includes errors and warnings from registry-match callbacks. Expectations must identify a registry and exactly one required match; severity and reason distinguish cases.
#' @keywords internal
collect_registry_evidence <- function(detail, provenance, report, context_columns) {
  keep <- detail$kind == "validation" &
    (endsWith(detail$rule_id, "/registry_match") | endsWith(detail$rule_id, "/registry")) &
    detail$severity %in% c("error", "warning")
  rows <- detail[keep, , drop = FALSE]
  if (!nrow(rows)) return(NULL)
  expected <- unique(rows$expected)
  references <- vapply(expected, function(value) {
    expectation <- yaml::yaml.load(value, eval.expr = FALSE)
    if (!is.list(expectation) || !is.character(expectation$reference) ||
        length(expectation$reference) != 1L || is.na(expectation$reference) ||
        !nzchar(expectation$reference) || !identical(as.integer(expectation$matches), 1L))
      stop("Registry evidence requires a reference and exactly one expected match")
    expectation$reference
  }, character(1))
  rows$reference <- unname(references[match(rows$expected, expected)])
  rows$reason <- rows$message
  summarize_reference_evidence(rows, provenance, report, context_columns, c("severity", "reason"))
}
