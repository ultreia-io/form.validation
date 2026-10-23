# Prepare evidence once alongside the individual report's formatted findings.
# This portable data retains grouping keys and cells; it contains no snapshot contents.
prepare_report_evidence <- function(tables, provenance, context) {
  if (!is.list(context) || (length(context) && (is.null(names(context)) ||
      any(!nzchar(names(context))) || anyDuplicated(names(context))))) stop("context must be a named list")
  reserved <- c(names(tables$details), names(tables$summary), "report", "cells", "occurrences", "snapshot_sha256")
  if (length(intersect(names(context), reserved))) stop("Context names conflict with report columns")
  if (any(!vapply(context, function(x) is.character(x) && length(x) == 1L && !is.na(x), logical(1))))
    stop("Context values must be scalar text")
  detail <- tables$details
  # Match the public CSV representation without writing and reading it again.
  detail[] <- lapply(detail, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
  for (key in c("input", "model_version", "rules_version")) detail[[key]] <- rep(tables$summary[[key]], nrow(detail))
  for (key in names(context)) detail[[key]] <- rep(context[[key]], nrow(detail))
  parts <- list(
    reference = if (!is.null(provenance)) collect_reference_evidence(detail, provenance, "", names(context)) else NULL,
    registry = if (!is.null(provenance)) collect_registry_evidence(detail, provenance, "", names(context)) else NULL,
    conversion = collect_conversion_evidence(detail, "", names(context)))
  parts <- lapply(parts, function(rows) { if (!is.null(rows)) rows$report <- NULL; rows })
  list(schema_version = 1L, context_columns = as.list(names(context)), parts = parts)
}

# Read only prepared data, never executable R objects or findings to reconstruct evidence.
read_report_evidence <- function(path) {
  if (!file.exists(path)) stop("Missing prepared evidence; regenerate individual reports: ", path)
  value <- jsonlite::read_json(path, simplifyVector = TRUE)
  if (!identical(value$schema_version, 1L) ||
      !all(c("reference", "registry", "conversion") %in% names(value$parts)))
    stop("Unsupported prepared evidence: ", path)
  value
}

# Supply the shared card template; zero counts remain visible without navigation.
evidence_card_values <- function(parts, reference_report, registry_report, conversion_report) {
  paths <- list(reference = reference_report, registry = registry_report, conversion = conversion_report)
  result <- list()
  for (kind in names(paths)) {
    count <- sum(vapply(parts[[kind]], function(rows) sum(as.numeric(rows$occurrences)), numeric(1)))
    result[paste0(kind, "_report")] <- list(if (count > 0) paths[[kind]] else NULL)
    result[paste0(kind, "_empty")] <- list(if (count == 0) "0" else NULL)
    result[[paste0(kind, "_count")]] <- format(count, big.mark = ",", scientific = FALSE, trim = TRUE)
  }
  result
}

# Write one form's evidence pages and the reusable data consumed by aggregation.
write_individual_evidence <- function(report, directory, report_filename, title,
                                      stylesheet, logo, assets_directory, overwrite) {
  evidence <- report$evidence
  if (is.null(evidence)) stop("Report has no prepared evidence; call validation_report_data() again")
  kinds <- c(reference = "code-list", registry = "registry", conversion = "conversion")
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
    paths[[kind]] <- if (kind == "conversion") do.call(write_conversion_evidence, arguments) else
      do.call(write_reference_evidence, c(arguments, list(kind = kinds[[kind]])))
  }
  jsonlite::write_json(evidence, file.path(directory, "evidence.json"), auto_unbox = TRUE, null = "null", digits = NA)
  evidence_card_values(parts, paths$reference, paths$registry, paths$conversion)
}
