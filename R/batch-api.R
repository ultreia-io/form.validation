# Public operations ---------------------------------------------------------

# Planning ---------------------------------------------------------

#' Write a batch plan with frozen inputs and resources
#'
#' A batch plan describes work to execute. A [`create_validation_plan()`][form.validation::create_validation_plan] describes the
#' rules for one form; several such rule plans may belong to a batch.
#' @param inputs Data frame with `path`, unique relative `relative` paths and
#'   `job` identifiers. Additional grouping columns and `title` are optional.
#' @param jobs Data frame with unique `id`, `resource` bundle key and `plan` rule
#'   plan key. Additional columns are public metadata. Jobs without inputs remain.
#' @param resources Named list of bundles. Each has named `plans` containing
#'   `validation_plan` objects and `references` already prepared by the caller.
#'   A references `provenance` attribute may identify snapshots for evidence.
#' @param directory New local batch directory, outside the input directories.
#' @param group_columns Ordered input columns defining publication directories.
#'   Each relative path must consist of these components followed by a filename.
#' @param identity_columns Input columns distinguishing aggregate findings.
#' @param model_versions Model versions to display, including those without inputs.
#' @param metadata Private caller metadata, retained only in `plan.rds`.
#' @param packages Optional consumer package names to record with engine versions.
#' @inheritParams validate_workbook
#' @return A `validation_batch_plan`, also saved in `plan.rds`.
#' @details Does no directory discovery. Freezes resources, reader, preparation
#'   policy and input hashes. Later stages use only this inventory.
#'   Public `plan.json` contains relative paths and explicitly selected grouping
#'   metadata; local paths, resources, callbacks and private metadata are excluded.
#'   Extra job columns must contain only public metadata.
#' @export
write_validation_batch_plan <- function(inputs, jobs, resources, directory,
                                  group_columns = character(), identity_columns = group_columns,
                                  model_versions = character(), metadata = list(), packages = character(),
                                  config = list(), reader = read_workbook_data,
                                  prepare_data = prepare_validation_data) {
  required <- unique(c("path", "relative", "job", group_columns, identity_columns))
  if (!is.data.frame(inputs) || !all(required %in% names(inputs)))
    stop("Missing batch input columns", call. = FALSE)
  if (!is.data.frame(jobs) || !all(c("id", "resource", "plan") %in% names(jobs)))
    stop("Missing batch job columns", call. = FALSE)
  for (column in c("id", "resource", "plan")) {
    if (!is.character(jobs[[column]]) || anyNA(jobs[[column]]) || any(!nzchar(jobs[[column]])))
      stop("Invalid job column: ", column, call. = FALSE)
  }
  if (anyDuplicated(jobs$id) || anyNA(inputs$job) || !all(inputs$job %in% jobs$id))
    stop("Each input must identify one unique job", call. = FALSE)
  check_batch_paths(inputs$relative)
  if (anyDuplicated(inputs$relative)) stop("Duplicate input path", call. = FALSE)
  for (column in unique(c(group_columns, identity_columns))) {
    if (!is.character(inputs[[column]]) || anyNA(inputs[[column]]))
      stop("Grouping and identity columns must contain text: ", column, call. = FALSE)
  }
  for (i in seq_len(nrow(inputs))) {
    parts <- strsplit(inputs$relative[i], "/", fixed = TRUE)[[1L]]
    groups <- unlist(inputs[i, group_columns, drop = FALSE], use.names = FALSE)
    if (length(parts) != length(group_columns) + 1L ||
        !identical(unname(utils::head(parts, -1L)), unname(as.character(groups))))
      stop("Input path differs from grouping metadata: ", inputs$relative[i], call. = FALSE)
    if (parts[1L] %in% c("report-assets", "conversion-evidence", "primary-key-evidence", "logs") ||
        grepl("^reference-[0-9a-f]+-evidence$", parts[1L]))
      stop("Input path uses a reserved publication directory", call. = FALSE)
  }
  named_list(resources, "resources")
  if (!all(jobs$resource %in% names(resources))) stop("Missing resource bundle", call. = FALSE)
  for (i in seq_len(nrow(jobs))) {
    bundle <- resources[[jobs$resource[i]]]
    if (!inherits(bundle$plans[[jobs$plan[i]]], "validation_plan"))
      stop("Missing rule plan for job: ", jobs$id[i], call. = FALSE)
  }
  jobs$model_version <- vapply(seq_len(nrow(jobs)), function(i) {
    rules <- resources[[jobs$resource[i]]]$plans[[jobs$plan[i]]]
    form.model::form_metadata(rules$model)$form$version
  }, character(1))
  inputs$model_version <- jobs$model_version[match(inputs$job, jobs$id)]
  model_versions <- unique(c(model_versions, jobs$model_version))
  if (!is.function(reader) || (!is.null(prepare_data) && !is.function(prepare_data)))
    stop("Supply a reader and an optional preparation function", call. = FALSE)
  config <- execution_config(config)
  text_value(directory, "directory")
  if (file.exists(directory)) stop("Batch directory already exists: ", directory, call. = FALSE)
  inputs$path <- vapply(inputs$path, normalizePath, character(1), mustWork = TRUE)
  if (any(dir.exists(inputs$path))) stop("Batch inputs must be files", call. = FALSE)
  dir.create(dirname(directory), recursive = TRUE, showWarnings = FALSE)
  directory <- file.path(normalizePath(dirname(directory), mustWork = TRUE), basename(directory))
  if (any(vapply(dirname(inputs$path), function(path)
      startsWith(paste0(directory, "/"), paste0(path, "/")), logical(1))))
    stop("Batch output must be outside input directories", call. = FALSE)
  stage <- tempfile(".plan-", tmpdir = dirname(directory))
  dir.create(file.path(stage, "resources"), recursive = TRUE)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  inputs$sha256 <- vapply(inputs$path, batch_file_hash, character(1))
  inputs$result <- file.path("validation", "results", inputs$relative, "result.rds")
  records <- list()
  for (i in seq_along(resources)) {
    key <- names(resources)[i]
    relative <- file.path("resources", paste0(i, ".rds"))
    saveRDS(resources[[i]], file.path(stage, relative))
    records[[key]] <- list(file = relative, sha256 = batch_file_hash(file.path(stage, relative)),
      references = attr(resources[[i]]$references, "provenance"))
  }
  jobs$forms <- vapply(jobs$id, function(id) sum(inputs$job == id), integer(1))
  packages <- unique(c("form.validation", "form.model", packages))
  versions <- stats::setNames(vapply(packages, function(package)
    as.character(utils::packageVersion(package)), character(1)), packages)
  plan <- structure(list(schema_version = 1L, created_at = batch_time(), directory = directory,
    files = inputs, jobs = jobs, resources = records, group_columns = group_columns,
    identity_columns = identity_columns, model_versions = model_versions, metadata = metadata,
    packages = versions, config = config, reader = reader, prepare_data = prepare_data),
    class = "validation_batch_plan")
  plan$id <- batch_plan_id(plan)
  saveRDS(plan, file.path(stage, "plan.rds"))
  write_batch_plan_json(plan, file.path(stage, "plan.json"))
  if (!file.rename(stage, directory)) stop("Cannot finalize batch plan: ", directory, call. = FALSE)
  plan
}

#' Read a saved validation batch plan
#' @param plan A `validation_batch_plan`, its directory or its local `plan.rds` file.
#' @return The verified plan, with its current local directory.
#' @details RDS files are local application artifacts; read only plans you trust.
#'   Loading does not scan inputs or reopen reference sources.
#' @export
read_validation_batch_plan <- function(plan) {
  if (is.character(plan) && length(plan) == 1L && !is.na(plan)) {
    path <- if (dir.exists(plan)) file.path(plan, "plan.rds") else plan
    path <- normalizePath(path, mustWork = TRUE)
    plan <- readRDS(path)
    plan$directory <- dirname(path)
  }
  if (!inherits(plan, "validation_batch_plan") || !identical(plan$schema_version, 1L) ||
      !identical(plan$id, batch_plan_id(plan)))
    stop("Invalid or modified batch plan", call. = FALSE)
  if (!dir.exists(plan$directory)) stop("Batch directory is missing", call. = FALSE)
  plan
}

# Validation ---------------------------------------------------------

#' Validate every input in a saved batch plan
#' @inheritParams read_validation_batch_plan
#' @param resume Reuse completed, checksum-verified results after an interruption.
#' @param workers Number of forms to validate concurrently. The default is one.
#'   Parallel validation uses forked processes on Unix and socket workers on Windows.
#' @return Validation stage state, invisibly.
#' @details Reads only planned inputs and frozen resources. Each form saves its
#'   typed result and definition independently. The parent checkpoints outcomes
#'   as workers finish on Unix, or after each small worker group on Windows.
#'   No report tables, CSV files or HTML pages are generated. Reader failures are
#'   ordinary failed validation results; infrastructure failures keep the stage
#'   incomplete and can be retried. Result RDS files are uncompressed for fast reuse.
#' @export
validate_batch <- function(plan, resume = TRUE, workers = 1L) {
  if (length(workers) != 1L || !is.numeric(workers) || is.na(workers) ||
      !is.finite(workers) || workers < 1 || workers > .Machine$integer.max ||
      workers != as.integer(workers))
    stop("workers must be a positive whole number", call. = FALSE)
  workers <- as.integer(workers)
  plan <- read_validation_batch_plan(plan)
  lock <- lock_validation_batch(plan)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  state_path <- file.path(plan$directory, "validation.rds")
  state <- if (file.exists(state_path)) readRDS(state_path) else
    list(schema_version = 1L, plan_id = plan$id, started_at = batch_time(),
      complete = FALSE, outcomes = list())
  if (!identical(state$plan_id, plan$id)) stop("Validation state belongs to another plan", call. = FALSE)
  if (!isTRUE(resume) && length(state$outcomes)) stop("Validation has already started; use resume = TRUE", call. = FALSE)
  done <- vapply(plan$files$relative, function(input) {
    outcome <- state$outcomes[[input]]
    if (is.null(outcome$result_sha256)) return(FALSE)
    path <- file.path(plan$directory, outcome$result)
    if (!file.exists(path) || !identical(batch_file_hash(path), outcome$result_sha256))
      stop("Saved validation result is missing or changed: ", input, call. = FALSE)
    TRUE
  }, logical(1))
  if (isTRUE(state$complete) && all(done)) return(invisible(state))
  pending <- plan$files[!done, , drop = FALSE]
  for (i in seq_len(nrow(pending))) {
    file <- pending[i, ]
    if (!file.exists(file$path) || !identical(batch_file_hash(file$path), file$sha256))
      stop("Planned input is missing or changed; create a new plan: ", file$relative, call. = FALSE)
  }
  log <- batch_log(plan$directory, "validation.log")
  started <- proc.time()[["elapsed"]]
  state$complete <- FALSE
  write_batch_state(state, state_path)
  log(sprintf("VALIDATE %d planned inputs; %d already completed; workers=%d",
    nrow(plan$files), sum(done), workers))
  cluster <- if (workers > 1L && .Platform$OS.type == "windows")
    parallel::makePSOCKcluster(workers) else NULL
  if (!is.null(cluster)) on.exit(parallel::stopCluster(cluster), add = TRUE)
  for (resource in names(plan$resources)) {
    ids <- plan$jobs$id[plan$jobs$resource == resource]
    selected <- pending[pending$job %in% ids, , drop = FALSE]
    if (!nrow(selected)) next
    bundle <- read_batch_resources(plan, resource)
    state <- validate_batch_inputs(selected, bundle, plan, state, state_path, log, workers, cluster)
    rm(bundle)
  }
  state$complete <- all(vapply(plan$files$relative,
    function(input) !is.null(state$outcomes[[input]]$result_sha256), logical(1)))
  state$finished_at <- batch_time()
  state$jobs <- plan$jobs
  state$jobs$elapsed <- vapply(seq_len(nrow(plan$jobs)), function(i) {
    selected <- plan$files$relative[plan$files$job == plan$jobs$id[i]]
    sum(vapply(selected, function(input) state$outcomes[[input]]$elapsed, numeric(1)))
  }, numeric(1))
  write_batch_state(state, state_path)
  log(sprintf("FINISHED validation; complete=%s; invocation=%.3fs", state$complete, proc.time()[["elapsed"]] - started))
  if (!state$complete) stop("Validation could not finish; rerun validate with the same plan: ", plan$directory, call. = FALSE)
  invisible(state)
}

# Reports ---------------------------------------------------------

#' Generate individual reports from completed validation results
#' @inheritParams read_validation_batch_plan
#' @param report_filename HTML basename, ending in `.html`.
#' @inheritParams write_validation_report
#' @param overwrite Rebuild the complete reports directory from saved results.
#' @param workers Number of forms to render concurrently. The default is one.
#' @param reference_context_columns Public input columns shown in evidence tables.
#'   Defaults to the batch grouping and identity columns.
#' @return The reports directory, invisibly.
#' @details Requires every planned input to have a saved result. Reads no input
#'   workbook or reference CSV. Uses the form saved with each result, so rendering
#'   does not depend on changes to installed form definitions.
#'   Reports are staged together and replace the previous report tree only after
#'   every form has rendered successfully. The batch plan and results stay intact.
#' @export
write_validation_batch_reports <- function(plan, report_filename = "index.html", overwrite = FALSE,
                                            title = "Validation report", author = NULL,
                                            stylesheet = NULL, logo = NULL, workers = 1L,
                                            reference_context_columns = NULL) {
  if (length(workers) != 1L || !is.numeric(workers) || is.na(workers) ||
      !is.finite(workers) || workers < 1 || workers > .Machine$integer.max ||
      workers != as.integer(workers))
    stop("workers must be a positive whole number", call. = FALSE)
  workers <- as.integer(workers)
  plan <- read_validation_batch_plan(plan)
  check_batch_report_filename(report_filename)
  if (is.null(reference_context_columns))
    reference_context_columns <- unique(c(plan$group_columns, plan$identity_columns))
  if (!is.character(reference_context_columns) || anyNA(reference_context_columns) ||
      anyDuplicated(reference_context_columns) ||
      !all(reference_context_columns %in% names(plan$files)))
    stop("reference_context_columns must name public input columns", call. = FALSE)
  lock <- lock_validation_batch(plan)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  validation <- read_batch_state(plan, "validation")
  output <- file.path(plan$directory, "reports")
  if (file.exists(output) && !isTRUE(overwrite)) stop("Reports already exist; use overwrite = TRUE", call. = FALSE)
  stage <- tempfile(".reports-", tmpdir = plan$directory)
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  log <- batch_log(plan$directory, "report.log")
  started <- proc.time()[["elapsed"]]
  outcomes <- list()
  files <- lapply(seq_len(nrow(plan$files)), function(i) plan$files[i, ])
  render <- function(file) render_batch_input_report(file, plan, validation, stage,
    report_filename, title, author, stylesheet, logo, reference_context_columns)
  if (workers == 1L) {
    rendered <- lapply(files, render)
  } else if (.Platform$OS.type == "windows") {
    cluster <- parallel::makePSOCKcluster(workers)
    on.exit(parallel::stopCluster(cluster), add = TRUE)
    rendered <- parallel::parLapplyLB(cluster, files, render)
  } else {
    rendered <- parallel::mclapply(files, render, mc.cores = workers, mc.preschedule = FALSE)
  }
  for (i in seq_along(files)) {
    file <- files[[i]]
    result <- rendered[[i]]
    if (inherits(result, "try-error") || !is.null(result$error))
      stop("Cannot render report for ", file$relative, ": ",
        if (inherits(result, "try-error")) as.character(result) else result$error, call. = FALSE)
    outcomes[[file$relative]] <- result
    log(sprintf("REPORTED %s elapsed=%.3fs", file$relative, result$elapsed))
  }
  paths <- sort(list.files(stage, recursive = TRUE, all.files = TRUE, no.. = TRUE))
  artifacts <- stats::setNames(vapply(file.path(stage, paths), batch_file_hash, character(1)), paths)
  state <- list(schema_version = 1L, plan_id = plan$id, complete = TRUE,
    validation_sha256 = batch_file_hash(file.path(plan$directory, "validation.rds")),
    report_filename = report_filename, finished_at = batch_time(),
    elapsed = proc.time()[["elapsed"]] - started, outcomes = outcomes, artifacts = artifacts)
  backup <- tempfile(".reports-previous-", tmpdir = plan$directory)
  if (dir.exists(output) && !file.rename(output, backup)) stop("Cannot replace reports directory", call. = FALSE)
  if (!file.rename(stage, output)) {
    if (dir.exists(backup)) file.rename(backup, output)
    stop("Cannot finalize reports", call. = FALSE)
  }
  write_batch_state(state, file.path(plan$directory, "reports.rds"))
  unlink(backup, recursive = TRUE)
  log(sprintf("FINISHED reports; %d forms; elapsed=%.3fs", nrow(plan$files), state$elapsed))
  invisible(output)
}

# Publication ---------------------------------------------------------

#' Write a publication directory from a completed validation batch
#' @inheritParams read_validation_batch_plan
#' @param output New directory, defaulting to `publication` inside the batch.
#' @inheritParams write_validation_report_summary
#' @param comment Optional plain-text publication note for the batch catalog.
#' @param workers Number of top-level aggregate directories to build concurrently.
#' @return The prepared directory, invisibly. No upload is performed.
#' @details Uses the saved inventory and report manifest. Copies verified reports,
#'   adds ancestor navigation, aggregates and evidence reports. The manifest
#'   `<output>.json` and portable `<output>-plan.json` stay beside the served directory.
#'   Private results, resources and prepared evidence JSON are excluded. Local links and artifact counts
#'   are checked before the staged directory is finalized.
#' @export
write_validation_batch_publication <- function(plan, output = NULL, report_filename = "index.html",
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
  if (is.null(output)) output <- file.path(plan$directory, "publication")
  if (file.exists(output)) stop("Publication output already exists: ", output, call. = FALSE)
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  output <- file.path(normalizePath(dirname(output), mustWork = TRUE), basename(output))
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
  for (i in seq_len(nrow(files))) {
    file <- files[i, ]
    add_batch_breadcrumbs(file.path(stage, file$relative, reports$report_filename), file$relative, report_filename)
    prepared <- read_report_evidence(file.path(stage, file$relative, "evidence.json"))
    evidence_labels <- c(stats::setNames(vapply(prepared$catalogues, function(x) paste(x$label, "evidence"), character(1)),
      vapply(prepared$catalogues, `[[`, character(1), "folder")),
      `conversion-evidence` = "Conversion errors", `primary-key-evidence` = "Primary-key evidence")
    for (kind in names(evidence_labels)) {
      relative <- file.path(file$relative, kind)
      path <- file.path(stage, relative, reports$report_filename)
      if (file.exists(path)) add_batch_breadcrumbs(path, relative, report_filename,
        current_label = evidence_labels[[kind]], parent_filename = reports$report_filename)
    }
  }
  for (name in c("validation.log", "report.log")) copy(file.path(plan$directory, name), file.path("logs", name))
  message(sprintf("Publication copy and navigation: %.1fs", proc.time()[["elapsed"]] - phase_started))
  phase_started <- proc.time()[["elapsed"]]
  pages <- write_validation_report_summary(descriptors, stage,
    group_columns = plan$group_columns, identity_columns = plan$identity_columns,
    report_filename = report_filename, title = title, author = author, stylesheet = stylesheet, logo = logo,
    model_versions = plan$model_versions,
    reference_context_columns = reference_context_columns, report_notes = report_notes,
    workers = as.integer(workers))
  message(sprintf("Aggregate reports: %.1fs", proc.time()[["elapsed"]] - phase_started))
  phase_started <- proc.time()[["elapsed"]]
  individual_evidence <- basename(dirname(report_files[basename(report_files) == reports$report_filename]))
  catalogues <- attr(pages, "catalogues")
  catalogue_reports <- lapply(catalogues, function(spec) {
    spec$reports <- sum(individual_evidence == spec$folder) +
      sum(basename(dirname(attr(pages, "reference_report"))) == spec$folder)
    spec
  })
  reference_reports <- sum(vapply(catalogue_reports, `[[`, numeric(1), "reports"))
  conversion_reports <- sum(individual_evidence == "conversion-evidence") + length(attr(pages, "conversion_report"))
  primary_key_reports <- sum(individual_evidence == "primary-key-evidence") + length(attr(pages, "primary_key_report"))
  html <- list.files(stage, pattern = "\\.html$", recursive = TRUE, full.names = TRUE)
  if (length(html) != nrow(descriptors) + length(pages) + reference_reports + conversion_reports + primary_key_reports) stop("Unexpected HTML report count")
  stage_prefix <- paste0(normalizePath(stage, winslash = "/", mustWork = TRUE), "/")
  for (path in html) {
    text <- paste(readLines(path, warn = FALSE), collapse = "\n")
    links <- regmatches(text, gregexpr('(?:href|src)="[^"]+"', text, perl = TRUE))[[1]]
    links <- sub('^[^=]+="', "", sub('"$', "", links))
    for (link in links) {
      link <- gsub("&amp;", "&", link, fixed = TRUE)
      if (grepl("^[A-Za-z]+:|^//|^#", link)) stop("Unexpected external link: ", link)
      target <- normalizePath(file.path(dirname(path), utils::URLdecode(link)), winslash = "/", mustWork = TRUE)
      if (!startsWith(target, stage_prefix) || dir.exists(target)) stop("Invalid publication link: ", link)
    }
  }
  for (page in pages) {
    folder <- dirname(file.path(stage, page))
    files_table <- utils::read.csv(file.path(folder, "files.csv"), colClasses = "character",
      na.strings = NULL, fileEncoding = "UTF-8-BOM")
    for (link in files_table$report) {
      target <- normalizePath(file.path(folder, link), winslash = "/", mustWork = TRUE)
      if (!startsWith(target, stage_prefix) || dir.exists(target)) stop("Invalid individual link: ", link)
    }
  }
  # Prepared evidence is needed while aggregating, but never by report readers.
  unlink(file.path(stage, report_files[basename(report_files) == "evidence.json"]))
  files <- sort(list.files(stage, recursive = TRUE, full.names = FALSE, all.files = TRUE, no.. = TRUE))
  manifest <- list(batch = basename(plan$directory), report_filename = report_filename,
    individual_reports = nrow(descriptors), aggregate_reports = length(pages), aggregate_pages = as.list(pages),
    reference_reports = reference_reports, catalogue_reports = catalogue_reports, conversion_reports = conversion_reports, primary_key_reports = primary_key_reports, no_inputs = as.list(setdiff(plan$model_versions, plan$files$model_version)),
    files = lapply(files, function(path) list(path = path, sha256 = digest::digest(file = file.path(stage, path), algo = "sha256"))))
  if (!is.null(comment) && nzchar(comment)) manifest$comment <- comment
  message(sprintf("Publication checks and inventory: %.1fs", proc.time()[["elapsed"]] - phase_started))
  if (!file.rename(stage, output)) stop("Cannot finalize publication directory: ", output)
  write_batch_plan_json(plan, paste0(output, "-plan.json"), reports)
  jsonlite::write_json(manifest, paste0(output, ".json"), auto_unbox = TRUE, pretty = TRUE)
  invisible(normalizePath(output, mustWork = TRUE))
}
