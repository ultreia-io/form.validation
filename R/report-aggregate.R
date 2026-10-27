#' Aggregate completed validation reports without running validation
#'
#' @param descriptors Data frame with `directory` (existing report directory),
#'   `report` (HTML filename), and public grouping metadata columns.
#' @param directory Destination containing the copied individual reports.
#' @param group_columns Ordered metadata columns defining directory levels.
#' @param identity_columns Metadata columns distinguishing finding identities.
#' @param report_filename Aggregate HTML filename, default `index.html`.
#' @param title Aggregate report title.
#' @inheritParams write_validation_report
#' @param notes Optional public information displayed on the root page.
#' @param report_notes Public validation-policy notes shown in the Warning
#'   findings card on every aggregate page. Text is HTML-escaped.
#' @param model_versions Optional model versions to show, including versions with no files.
#' @param reference_provenance Retained for compatibility. Snapshot identity is
#'   already stored in each individual report's prepared evidence.
#' @param reference_context_columns Descriptor columns to show in code summaries and source tables.
#' @param workers Number of top-level aggregate directories to render concurrently.
#' @return Relative aggregate HTML paths, invisibly, with a `reference_report`
#'   attribute for code-list evidence, `registry_report` for registry evidence,
#'   and `conversion_report` for number, integer, boolean and temporal type failures.
#'   Attributes contain all contextual evidence report paths. Each aggregate's
#'   evidence includes only its forms, with counts and links scoped accordingly.
#'   Conversion evidence uses saved errors and warnings without altering values.
#'   The `primary_key_report` attribute lists missing-component and duplicate-key
#'   evidence reports, retaining the saved key values and physical Excel rows.
#' @details Reads all CSV columns as text. Each directory level receives files.csv,
#'   issues.csv and execution.csv and an HTML overview. Validation findings and
#'   execution problems remain separate. Grouped findings show occurrences and
#'   affected files; their identity includes model/rules versions and caller metadata.
#'   Individual reports and prepared `evidence.json` must already exist below
#'   directory, at descriptor paths. Evidence is reused, never extracted from CSV.
#'   No input workbook is read, copied or revalidated. Existing outputs are refused.
#' @export
write_validation_aggregate <- function(descriptors, directory, group_columns,
                                       identity_columns = group_columns,
                                       report_filename = "index.html", title = "Validation reports", notes = character(),
                                       author = NULL, stylesheet = NULL, logo = NULL, model_versions = NULL,
                                       reference_provenance = NULL, reference_context_columns = character(),
                                       report_notes = character(), workers = 1L) {
  if (!is.character(report_notes) || anyNA(report_notes))
    stop("report_notes must be character text without missing values", call. = FALSE)
  if (length(workers) != 1L || !is.numeric(workers) || is.na(workers) ||
      !is.finite(workers) || workers < 1 || workers > .Machine$integer.max ||
      workers != as.integer(workers))
    stop("workers must be a positive whole number", call. = FALSE)
  workers <- as.integer(workers)
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*\\.html$", report_filename)) stop("Invalid report filename")
  required <- unique(c("directory", "report", group_columns, identity_columns))
  required <- unique(c(required, reference_context_columns))
  if (!is.data.frame(descriptors) || !all(required %in% names(descriptors))) stop("Missing descriptor columns")
  for (key in group_columns) {
    values <- descriptors[[key]]
    if (anyNA(values) || any(!nzchar(values) | values %in% c(".", "..") | grepl("[/\\\\]", values)))
      stop("Invalid grouping directory component: ", key)
  }
  if (nrow(descriptors)) for (i in seq_len(nrow(descriptors))) {
    prefix <- paste(unlist(descriptors[i, group_columns, drop = FALSE], use.names = FALSE), collapse = "/")
    if (nzchar(prefix) && !startsWith(descriptors$directory[i], paste0(prefix, "/"))) stop("Descriptor directory differs from grouping metadata")
    if (!grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*\\.html$", descriptors$report[i])) stop("Invalid individual report filename")
  }
  # Read report CSV without type conversion
  # @param path CSV file with a UTF-8 BOM.
  # @return Data frame retaining all values as text, including literal NA strings.
  read <- function(path) utils::read.csv(path, colClasses = "character", check.names = FALSE,
    na.strings = NULL, fileEncoding = "UTF-8-BOM")
  # Encode a report link
  # @param x One slash-separated relative path.
  # @return URL text with encoded components and unchanged separators.
  url <- function(x) paste(vapply(strsplit(x, "/", fixed = TRUE)[[1]], utils::URLencode,
                                 character(1), reserved = TRUE), collapse = "/")
  metadata <- setdiff(names(descriptors), c("directory", "report"))
  # Summarize findings in the current aggregate
  # @param rows Finding rows with input identities and optional occurrence weights.
  # @param by_input Whether to retain each form and sheet order as grouping keys.
  # @return Grouped rows with occurrence and distinct-form counts.
  grouped <- function(rows, by_input = FALSE) {
    keys <- unique(c(identity_columns, "model_version", "rules_version", "rule_id", "kind", "severity", "stage", "sheet", "field", "column", "value", "expected", "message"))
    if (by_input) keys <- intersect(unique(c(keys, "input", "sheet_order")), names(rows))
    if (!nrow(rows)) {
      result <- as.data.frame(stats::setNames(rep(list(character()), length(keys)), keys), stringsAsFactors = FALSE)
      result$occurrences <- integer(); result$affected_files <- integer(); return(result)
    }
    keys <- intersect(keys, names(rows))
    codes <- lapply(rows[keys], function(x) match(x, unique(x)))
    ids <- do.call(paste, c(codes, sep = ":"))
    order <- unique(ids); groups <- match(ids, order)
    result <- rows[!duplicated(ids), keys, drop = FALSE]
    weights <- if ("occurrences" %in% names(rows)) as.numeric(rows$occurrences) else rep(1, nrow(rows))
    result$occurrences <- as.numeric(rowsum(weights, groups, reorder = FALSE))
    pairs <- unique(data.frame(group = groups, input = rows$input))
    result$affected_files <- tabulate(pairs$group, nbins = length(order))
    result
  }
  files <- list(); issues <- list(); execution <- list(); reference_evidence <- list(); registry_evidence <- list(); conversion_evidence <- list(); primary_key_evidence <- list()
  for (i in seq_len(nrow(descriptors))) {
    d <- descriptors[i, , drop = FALSE]
    if (grepl("(^/|(^|/)\\.\\.(/|$))", d$directory)) stop("Unsafe descriptor path")
    summary <- read(file.path(directory, d$directory, "summary.csv"))
    detail <- read(file.path(directory, d$directory, "findings.csv"))
    if (nrow(summary) != 1L) stop("Expected one summary row: ", d$directory)
    if (!all(c("input", "model_version", "rules_version", "status", "structure_passed", "errors", "warnings", "incomplete", "failed") %in% names(summary))) stop("Incomplete summary schema")
    if (!all(c("kind", "rule_id", "severity", "sheet", "field", "value", "expected", "message", "stage") %in% names(detail))) stop("Incomplete findings schema")
    if (!"infos" %in% names(summary)) summary$infos <- "0"
    summary$input <- d$directory
    summary$report <- file.path(d$directory, d$report)
    for (key in metadata) summary[[key]] <- d[[key]]
    files[[i]] <- summary
    detail$input <- rep(d$directory, nrow(detail))
    for (key in unique(c(metadata, "model_version", "rules_version"))) detail[[key]] <- rep(summary[[key]], nrow(detail))
    if (any(!detail$kind %in% c("validation", "failed", "incomplete"))) stop("Unknown finding kind")
    prepared <- read_report_evidence(file.path(directory, d$directory, "evidence.json"))
    # Read one prepared evidence kind for this form
    # @param kind Name of the evidence part.
    # @return Prepared rows with descriptor metadata and rebased report links, or NULL.
    part <- function(kind) {
      rows <- prepared$parts[[kind]]
      if (is.null(rows) || !nrow(rows)) return(NULL)
      rows$input <- rep(d$directory, nrow(rows))
      rows$report <- rep(paste0("../", d$directory, "/", d$report), nrow(rows))
      for (key in reference_context_columns) rows[[key]] <- rep(d[[key]], nrow(rows))
      rows
    }
    reference_evidence[i] <- list(part("reference"))
    registry_evidence[i] <- list(part("registry"))
    conversion_evidence[i] <- list(part("conversion"))
    primary_key_evidence[i] <- list(part("primary_key"))
    # Collapse each form before retaining it; the full corpus never occupies memory.
    issues[[i]] <- grouped(detail[detail$kind == "validation", , drop = FALSE], by_input = TRUE)
    execution[[i]] <- grouped(detail[detail$kind != "validation", , drop = FALSE], by_input = TRUE)
  }
  if (!length(files)) {
    files <- data.frame(input = character(), report = character(), status = character(),
      structure_passed = character(), errors = character(), warnings = character(), infos = character(), incomplete = character(), failed = character())
    for (key in metadata) files[[key]] <- character()
    issues <- execution <- data.frame(input = character())
  } else {
    files <- do.call(rbind, files); issues <- do.call(rbind, issues); execution <- do.call(rbind, execution)
  }
  model_versions <- unique(c(as.character(model_versions), files$model_version))
  groups <- ""
  for (depth in seq_along(group_columns)) if (nrow(files))
    groups <- c(groups, unique(apply(files[group_columns[seq_len(depth)]], 1L, paste, collapse = "/")))
  assets <- file.path(directory, "report-assets")
  sources <- stats::setNames(system.file("report", c("report.css", "aggregate.js", "overview.css", "report-ui.js", "csv-worker.js", "report-table.js"),
    package = "form.validation", mustWork = TRUE), c("style", "script", "overview_style", "loading_script", "csv_worker", "table_script"))
  if (!is.null(stylesheet)) sources <- c(sources, stylesheet = stylesheet)
  if (!is.null(logo)) sources <- c(sources, logo = logo)
  asset_names <- copy_report_assets(sources, assets)
  # Format the total evidence occurrence count
  # @param parts List of prepared per-form evidence tables or NULL entries.
  # @return A formatted count using comma thousands separators.
  evidence_count <- function(parts) format(sum(vapply(parts, function(part)
    sum(as.numeric(part$occurrences)), numeric(1))), big.mark = ",", scientific = FALSE, trim = TRUE)
  render_group <- function(group) {
    depth <- if (nzchar(group)) length(strsplit(group, "/", fixed = TRUE)[[1]]) else 0L
    selected <- if (nzchar(group)) startsWith(files$input, paste0(group, "/")) else rep(TRUE, nrow(files))
    f <- files[selected, , drop = FALSE]
    v <- issues[issues$input %in% f$input, , drop = FALSE]
    e <- execution[execution$input %in% f$input, , drop = FALSE]
    folder <- file.path(directory, group); dir.create(folder, showWarnings = FALSE, recursive = TRUE)
    outputs <- file.path(folder, c(report_filename, "files.csv", "issues.csv", "execution.csv"))
    if (any(file.exists(outputs))) stop("Aggregate output already exists: ", folder)
    prefix <- strrep("../", depth)
    page_title <- if (nzchar(group)) paste(title, "\u2014", group) else title
    # Reuse the evidence collected once per form, scoped to this aggregate.
    # Source links still target the original individual reports at the root.
    # Select evidence for the current aggregate
    # @param parts Evidence parts in descriptor order.
    # @return Selected parts with report links rebased from the current group.
    scoped_evidence <- function(parts) lapply(parts[which(selected)], function(part) {
      if (!is.null(part)) part$report <- paste0(prefix, part$report)
      part
    })
    reference_parts <- scoped_evidence(reference_evidence)
    registry_parts <- scoped_evidence(registry_evidence)
    conversion_parts <- scoped_evidence(conversion_evidence)
    primary_key_parts <- scoped_evidence(primary_key_evidence)
    reference_count <- evidence_count(reference_parts)
    registry_count <- evidence_count(registry_parts)
    conversion_count <- evidence_count(conversion_parts)
    primary_key_count <- evidence_count(primary_key_parts)
    evidence_assets <- paste0(prefix, "../report-assets")
    reference_report <- if (reference_count == "0") NULL else write_reference_evidence(
      reference_parts, folder, report_filename, reference_context_columns, page_title, stylesheet, logo,
      evidence_assets = evidence_assets, scope = group)
    registry_report <- if (registry_count == "0") NULL else write_reference_evidence(
      registry_parts, folder, report_filename, reference_context_columns, page_title, stylesheet, logo,
      "registry", evidence_assets = evidence_assets, scope = group)
    conversion_report <- if (conversion_count == "0") NULL else write_conversion_evidence(conversion_parts, folder, report_filename,
      reference_context_columns, page_title, stylesheet, logo, evidence_assets = evidence_assets, scope = group)
    primary_key_report <- if (primary_key_count == "0") NULL else write_primary_key_evidence(primary_key_parts, folder, report_filename,
      reference_context_columns, page_title, stylesheet, logo, evidence_assets = evidence_assets, scope = group)
    # Locate a contextual evidence page from the root
    # @param path Evidence report path, or NULL for an empty report.
    # @return Root-relative path, or NULL when no report exists.
    relative_report <- function(path) if (length(path)) if (nzchar(group)) file.path(group, path) else path
    f$report <- paste0(prefix, f$report)
    write_report_csv(f, outputs[2]); write_report_csv(grouped(v), outputs[3]); write_report_csv(grouped(e), outputs[4])
    breadcrumbs <- list(list(label = "All reports", href = paste0(prefix, report_filename)))
    if (depth) for (i in seq_len(depth)) {
      part <- strsplit(group, "/", fixed = TRUE)[[1]][i]
      breadcrumbs[[length(breadcrumbs) + 1L]] <- list(label = part,
        href = paste0(strrep("../", depth - i), report_filename))
    }
    child_paths <- groups[vapply(groups, function(x) nzchar(x) && identical(dirname(x),
      if (nzchar(group)) group else "."), logical(1))]
    children <- lapply(child_paths, function(path) list(label = basename(path),
      href = url(file.path(basename(path), report_filename))))
    totals <- data.frame(files = nrow(f), structure_passed = sum(toupper(f$structure_passed) == "TRUE"),
      structure_blocked = sum(toupper(f$structure_passed) == "FALSE"), errors = sum(as.numeric(f$errors)),
      warnings = sum(as.numeric(f$warnings)), infos = sum(as.numeric(f$infos)), incomplete_checks = sum(as.numeric(f$incomplete)), failed_checks = sum(as.numeric(f$failed)))
    version_counts <- data.frame(version = model_versions,
      files = vapply(model_versions, function(version) sum(f$model_version == version), integer(1)))
    sheet_rows <- rbind(v, e)
    sheet_order <- if ("sheet_order" %in% names(sheet_rows))
      unique(sheet_rows$sheet[order(suppressWarnings(as.numeric(sheet_rows$sheet_order)), na.last = TRUE)]) else unique(sheet_rows$sheet)
    sheet_order <- sheet_order[!is.na(sheet_order) & nzchar(sheet_order)]
    # Navigation and table order belong to the HTML, not a separate browser payload.
    unlink(file.path(folder, "aggregate.json"))
    status_counts <- stats::setNames(lapply(c("valid", "invalid", "incomplete", "failed"),
      function(status) sum(f$status == status)), paste0("status_", c("valid", "invalid", "incomplete", "failed")))
    html <- render_report_template("aggregate.html", c(report_count_values(c(as.list(totals), status_counts)), list(title = page_title,
      current_label = breadcrumbs[[length(breadcrumbs)]]$label,
      children_hidden = if (length(children)) "" else "hidden",
      report_notes = if (length(report_notes)) paste(report_notes, collapse = "\n") else NULL,
      generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
      style = paste0(prefix, "report-assets/", asset_names[["style"]]),
      script = paste0(prefix, "report-assets/", asset_names[["script"]]),
      loading_script = paste0(prefix, "report-assets/", asset_names[["loading_script"]]),
      csv_worker = paste0(prefix, "report-assets/", asset_names[["csv_worker"]]),
      table_script = paste0(prefix, "report-assets/", asset_names[["table_script"]]),
      reference_report = if (reference_count != "0") reference_report else NULL,
      registry_report = if (registry_count != "0") registry_report else NULL,
      conversion_report = if (conversion_count != "0") conversion_report else NULL,
      reference_empty = if (reference_count == "0") "0" else NULL,
      registry_empty = if (registry_count == "0") "0" else NULL,
      conversion_empty = if (conversion_count == "0") "0" else NULL,
      reference_count = reference_count, registry_count = registry_count, conversion_count = conversion_count,
      overview_style = paste0(prefix, "report-assets/", asset_names[["overview_style"]]),
      stylesheet = if (!is.null(stylesheet)) paste0(prefix, "report-assets/", asset_names[["stylesheet"]]) else NULL,
      logo = if (!is.null(logo)) paste0(prefix, "report-assets/", asset_names[["logo"]]) else NULL,
      author = author)))
    breadcrumb_html <- vapply(utils::head(breadcrumbs, -1L), function(item)
      paste(render_report_template("breadcrumb-link.html", item), collapse = "\n"), character(1))
    html <- sub("<!-- aggregate-breadcrumbs -->", paste(breadcrumb_html, collapse = "\n"), html, fixed = TRUE)
    child_html <- vapply(children, function(item)
      paste(render_report_template("aggregate-child.html", item), collapse = "\n"), character(1))
    html <- sub("<!-- aggregate-children -->", paste(child_html, collapse = "\n"), html, fixed = TRUE)
    sheet_html <- vapply(sheet_order, function(sheet)
      paste(render_report_template("aggregate-sheet.html", list(sheet = sheet)), collapse = "\n"), character(1))
    html <- sub("<!-- aggregate-sheets -->", paste(sheet_html, collapse = "\n"), html, fixed = TRUE)
    note_html <- vapply(if (!depth) notes else character(), function(note)
      paste(render_report_template("aggregate-note.html", list(note = note)), collapse = "\n"), character(1))
    html <- sub("<!-- aggregate-notes -->", paste(note_html, collapse = "\n"), html, fixed = TRUE)
    versions_html <- vapply(seq_len(nrow(version_counts)), function(i) paste(render_report_template(
      "overview-version-card.html", c(list(version = version_counts$version[i]),
        report_count_values(list(files = version_counts$files[i])))), collapse = "\n"), character(1))
    html <- sub("<!-- version-cards -->", paste(versions_html, collapse = "\n"), html, fixed = TRUE)
    cards <- evidence_card_values(list(reference = reference_parts, registry = registry_parts,
      conversion = conversion_parts, primary_key = primary_key_parts), reference_report, registry_report, conversion_report, primary_key_report)
    html <- sub("<!-- evidence-cards -->", paste(render_report_template("evidence-cards.html", cards), collapse = "\n"), html, fixed = TRUE)
    writeLines(html, outputs[1], useBytes = TRUE)
    list(page = if (nzchar(group)) file.path(group, report_filename) else report_filename,
      reference_report = relative_report(reference_report), registry_report = relative_report(registry_report),
      conversion_report = relative_report(conversion_report), primary_key_report = relative_report(primary_key_report))
  }
  if (workers == 1L || length(groups) == 1L) {
    rendered <- lapply(groups, render_group)
  } else {
    # Each worker owns one top-level directory and every aggregate below it.
    top <- unique(sub("/.*$", "", groups[nzchar(groups)]))
    tasks <- lapply(top, function(path) groups[groups == path | startsWith(groups, paste0(path, "/"))])
    render_directory <- function(paths) stats::setNames(lapply(paths, render_group), paths)
    if (.Platform$OS.type == "windows") {
      cluster <- parallel::makePSOCKcluster(min(workers, length(tasks)))
      on.exit(parallel::stopCluster(cluster), add = TRUE)
      finished <- parallel::parLapplyLB(cluster, tasks, render_directory)
    } else {
      finished <- parallel::mclapply(tasks, render_directory,
        mc.cores = min(workers, length(tasks)), mc.preschedule = FALSE)
    }
    if (any(vapply(finished, inherits, logical(1), "try-error")))
      stop("Aggregate directory worker failed", call. = FALSE)
    rendered <- c(do.call(c, finished), stats::setNames(list(render_group("")), ""))
    rendered <- rendered[match(groups, names(rendered))]
  }
  pages <- vapply(rendered, `[[`, character(1), "page")
  paths <- function(key) as.character(unlist(lapply(rendered, `[[`, key), use.names = FALSE))
  invisible(structure(unname(pages),
    reference_report = paths("reference_report"), registry_report = paths("registry_report"),
    conversion_report = paths("conversion_report"), primary_key_report = paths("primary_key_report")))
}
