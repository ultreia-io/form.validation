#' Build a publication tree from a completed validation batch
#' @inheritParams read_validation_batch_plan
#' @param output New directory, defaulting to `aggregated_<batch>` beside the batch.
#' @inheritParams write_validation_aggregate
#' @param comment Optional plain-text publication note for the batch catalog.
#' @param workers Number of top-level aggregate directories to build concurrently.
#' @return The prepared directory, invisibly. No upload is performed.
#' @details Uses the saved inventory and report manifest. Copies verified reports,
#'   adds ancestor navigation, aggregates, evidence and a portable `plan.json`.
#'   Private results and resources are excluded. Local links and artifact counts
#'   are checked before the staged directory is finalized.
#' @export
build_validation_publication <- function(plan, output = NULL, report_filename = "index.html",
                                         title = "Validation reports", author = NULL,
                                         stylesheet = NULL, logo = NULL,
                                         reference_context_columns = character(), report_notes = character(),
                                         comment = NULL, workers = 1L) {
  if (!is.null(comment) && (!is.character(comment) || length(comment) != 1L || is.na(comment)))
    stop("comment must be a single text value or NULL", call. = FALSE)
  if (length(workers) != 1L || !is.numeric(workers) || is.na(workers) ||
      !is.finite(workers) || workers < 1 || workers > .Machine$integer.max ||
      workers != as.integer(workers))
    stop("workers must be a positive whole number", call. = FALSE)
  plan <- read_validation_batch_plan(plan)
  check_batch_report_filename(report_filename)
  lock <- lock_validation_batch(plan)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  read_batch_state(plan, "validation")
  reports <- read_batch_state(plan, "reports")
  if (!identical(reports$validation_sha256, batch_file_hash(file.path(plan$directory, "validation.rds"))))
    stop("Validation results changed; generate reports again", call. = FALSE)
  if (is.null(output)) output <- file.path(dirname(plan$directory), paste0("aggregated_", basename(plan$directory)))
  if (file.exists(output)) stop("Publication output already exists: ", output, call. = FALSE)
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  output <- file.path(normalizePath(dirname(output), mustWork = TRUE), basename(output))
  if (startsWith(paste0(output, "/"), paste0(plan$directory, "/"))) stop("Publication must be outside raw batch")
  stage <- tempfile(".publication-", tmpdir = dirname(output))
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  # Copy one artifact into the publication stage
  # @param source Existing artifact file.
  # @param relative Destination path relative to the staging directory.
  # @return NULL after copying, or an error if copying fails.
  copy <- function(source, relative) {
    target <- file.path(stage, relative)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(source, target, copy.date = TRUE)) stop("Cannot copy ", source, call. = FALSE)
  }
  report_files <- names(reports$artifacts)
  phase_started <- proc.time()[["elapsed"]]
  for (relative in report_files) {
    source <- file.path(plan$directory, "reports", relative)
    if (!file.exists(source) || !identical(batch_file_hash(source), reports$artifacts[[relative]]))
      stop("Report artifact is missing or changed: ", relative, call. = FALSE)
    copy(source, relative)
  }
  files <- plan$files
  columns <- unique(c(plan$group_columns, plan$identity_columns, reference_context_columns))
  if (!all(columns %in% names(files))) stop("Missing publication metadata columns", call. = FALSE)
  descriptors <- data.frame(directory = files$relative, report = rep(reports$report_filename, nrow(files)))
  descriptors[columns] <- files[columns]
  provenance <- list()
  for (i in seq_len(nrow(files))) {
    file <- files[i, ]
    add_batch_breadcrumbs(file.path(stage, file$relative, reports$report_filename), file$relative, report_filename)
    evidence_labels <- c(`code-list-evidence` = "Code-list evidence", `registry-evidence` = "Registry evidence",
      `conversion-evidence` = "Conversion errors", `primary-key-evidence` = "Primary-key evidence")
    for (kind in names(evidence_labels)) {
      relative <- file.path(file$relative, kind)
      path <- file.path(stage, relative, reports$report_filename)
      if (file.exists(path)) add_batch_breadcrumbs(path, relative, report_filename,
        current_label = evidence_labels[[kind]], parent_filename = reports$report_filename)
    }
    job <- plan$jobs[match(file$job, plan$jobs$id), ]
    refs <- plan$resources[[job$resource]]$references
    provenance[[file$relative]] <- Filter(function(reference) identical(reference$status, "available"), refs)
  }
  write_batch_plan_json(plan, file.path(stage, "plan.json"), reports)
  for (name in c("validation.log", "report.log")) copy(file.path(plan$directory, name), file.path("logs", name))
  message(sprintf("Publication copy and navigation: %.1fs", proc.time()[["elapsed"]] - phase_started))
  phase_started <- proc.time()[["elapsed"]]
  pages <- write_validation_aggregate(descriptors, stage,
    group_columns = plan$group_columns, identity_columns = plan$identity_columns,
    report_filename = report_filename, title = title, author = author, stylesheet = stylesheet, logo = logo,
    model_versions = plan$model_versions, reference_provenance = if (length(Filter(length, provenance))) provenance else NULL,
    reference_context_columns = reference_context_columns, report_notes = report_notes,
    workers = as.integer(workers))
  message(sprintf("Aggregate reports: %.1fs", proc.time()[["elapsed"]] - phase_started))
  phase_started <- proc.time()[["elapsed"]]
  individual_evidence <- basename(dirname(report_files[basename(report_files) == reports$report_filename]))
  reference_reports <- sum(individual_evidence == "code-list-evidence") + length(attr(pages, "reference_report"))
  registry_reports <- sum(individual_evidence == "registry-evidence") + length(attr(pages, "registry_report"))
  conversion_reports <- sum(individual_evidence == "conversion-evidence") + length(attr(pages, "conversion_report"))
  primary_key_reports <- sum(individual_evidence == "primary-key-evidence") + length(attr(pages, "primary_key_report"))
  html <- list.files(stage, pattern = "\\.html$", recursive = TRUE, full.names = TRUE)
  if (length(html) != nrow(descriptors) + length(pages) + reference_reports + registry_reports + conversion_reports + primary_key_reports) stop("Unexpected HTML report count")
  for (path in html) {
    text <- paste(readLines(path, warn = FALSE), collapse = "\n")
    links <- regmatches(text, gregexpr('(?:href|src)="[^"]+"', text, perl = TRUE))[[1]]
    links <- sub('^[^=]+="', "", sub('"$', "", links))
    for (link in links) {
      link <- gsub("&amp;", "&", link, fixed = TRUE)
      if (grepl("^[A-Za-z]+:|^//|^#", link)) stop("Unexpected external link: ", link)
      target <- normalizePath(file.path(dirname(path), utils::URLdecode(link)), mustWork = TRUE)
      if (!startsWith(target, paste0(normalizePath(stage), "/")) || dir.exists(target)) stop("Invalid publication link: ", link)
    }
  }
  for (page in pages) {
    folder <- dirname(file.path(stage, page))
    metadata <- jsonlite::read_json(file.path(folder, "aggregate.json"))
    files_table <- utils::read.csv(file.path(folder, "files.csv"), colClasses = "character",
      na.strings = NULL, fileEncoding = "UTF-8-BOM")
    links <- c(vapply(metadata$breadcrumbs, `[[`, character(1), "href"),
      vapply(metadata$children, `[[`, character(1), "href"))
    for (link in links) {
      target <- normalizePath(file.path(folder, utils::URLdecode(link)), mustWork = TRUE)
      if (!startsWith(target, paste0(normalizePath(stage), "/")) || dir.exists(target)) stop("Invalid aggregate link: ", link)
    }
    for (link in files_table$report) {
      target <- normalizePath(file.path(folder, link), mustWork = TRUE)
      if (!startsWith(target, paste0(normalizePath(stage), "/")) || dir.exists(target)) stop("Invalid individual link: ", link)
    }
  }
  files <- sort(list.files(stage, recursive = TRUE, full.names = FALSE, all.files = TRUE, no.. = TRUE))
  manifest <- list(batch = basename(plan$directory), report_filename = report_filename,
    individual_reports = nrow(descriptors), aggregate_reports = length(pages),
    reference_reports = reference_reports, registry_reports = registry_reports, conversion_reports = conversion_reports, primary_key_reports = primary_key_reports, no_inputs = as.list(setdiff(plan$model_versions, plan$files$model_version)),
    files = lapply(files, function(path) list(path = path, sha256 = digest::digest(file = file.path(stage, path), algo = "sha256"))))
  if (!is.null(comment) && nzchar(comment)) manifest$comment <- comment
  jsonlite::write_json(manifest, file.path(stage, "publication.json"), auto_unbox = TRUE, pretty = TRUE)
  message(sprintf("Publication checks and inventory: %.1fs", proc.time()[["elapsed"]] - phase_started))
  if (!file.rename(stage, output)) stop("Cannot finalize publication directory: ", output)
  invisible(normalizePath(output, mustWork = TRUE))
}

# Add ancestor navigation only to the staged publication copy.
#' Add ancestor navigation to a publication copy
#' @param path HTML file to update in place.
#' @param relative Report directory relative to the publication root.
#' @param report_filename HTML filename used by ancestor aggregates.
#' @param current_label Current-page label, or `NULL` to use the final path component.
#' @param parent_filename HTML filename used by the immediate parent.
#' @return `NULL`, invisibly, after writing the updated HTML.
#' @details Replaces existing parent or breadcrumb navigation. Links are encoded, labels escaped and the current page remains unlinked.
#' @keywords internal
add_batch_breadcrumbs <- function(path, relative, report_filename, current_label = NULL, parent_filename = report_filename) {
  parts <- strsplit(relative, "/", fixed = TRUE)[[1L]]
  depth <- length(parts)
  labels <- c("All reports", utils::head(parts, -1L))
  links <- vapply(seq_along(labels), function(i) paste(render_report_template("breadcrumb-link.html", list(
    href = batch_url(paste0(strrep("../", depth - i + 1L), if (i == length(labels)) parent_filename else report_filename)), label = labels[i])), collapse = "\n"), character(1))
  navigation <- paste(render_report_template("breadcrumbs.html", list(filename = if (is.null(current_label)) parts[depth] else current_label)), collapse = "\n")
  navigation <- sub("<!-- breadcrumb-items -->", paste(links, collapse = "\n"), navigation, fixed = TRUE)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  html <- gsub('<nav (?:class="report-parent"|id="breadcrumbs"|aria-label="Breadcrumb")[^>]*>.*?</nav>', "", html, perl = TRUE)
  anchor <- '<div class="report-navigation">'
  if (!grepl(anchor, html, fixed = TRUE)) stop("Missing report navigation element: ", path)
  html <- sub(anchor, paste0(anchor, "\n", navigation), html, fixed = TRUE)
  writeLines(html, path, useBytes = TRUE)
}

#' Validate an HTML report filename
#' @param filename Single filename without a directory.
#' @return `NULL` on success; otherwise an error.
#' @details Requires an alphanumeric first character, a `.html` suffix and only letters, digits, underscores, dots or hyphens.
#' @keywords internal
check_batch_report_filename <- function(filename) {
  if (!is.character(filename) || length(filename) != 1L || is.na(filename) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*\\.html$", filename))
    stop("Expected a plain .html report filename", call. = FALSE)
}
