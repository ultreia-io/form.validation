# Internal helpers ----------------------------------------------------------

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
