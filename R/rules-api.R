# Public operations ---------------------------------------------------------

# Plan construction ---------------------------------------------------------

#' Define a custom validation rule
#'
#' Add a check that model declarations cannot express. The callback runs once
#' per validation. Business callbacks run after structure and generated checks.
#' Structural callbacks run with all other structural checks and gate content.
#' Field findings or execution failures never suppress business checks.
#' @param id One unique nonempty rule ID. IDs must not collide with generated IDs.
#' @param check Function accepting `model`, `data`, `references`, and `config`.
#'   Return `NULL` on success or rows from [`validation_finding()`][form.validation::validation_finding] on failure.
#'   Signal [`validation_incomplete()`][form.validation::validation_incomplete] when prerequisites are missing.
#'   `config$field_values("Sheet.field")` reuses the run's prepared field data.
#'   It returns `sheet`, `field`, `type`, physical `rows` and `column`, and
#'   `original` values as a list. Logical `present`, `null`, `filled`, and `typed`
#'   masks align with `rows`; `eligible` indexes correctly typed, filled cells.
#'   `values` contains normalized comparison values for `eligible` cells only.
#'   Blank-string policy affects the masks, never `original`. Treat returned
#'   data as read-only; caches are private to one validation call.
#' @param stage `"business"` (default) or `"structure"`. Structural checks finish
#'   before any field or business check, even when another structural check fails.
#' @param severity Finding severity: `"error"`, `"warning"`, or `"info"`.
#' @param description Short explanation of what the rule checks.
#' @param parameters Named list of rule-specific values, exposed as `config$rule`.
#'   This is separate from caller-supplied `config$custom`.
#' @return A `validation_rule` list holding the ID, callback, severity, and description.
#' @examples
#' # Prepare rule.
#' validation_custom_rule <- validation_rule(
#'   "review/source", function(model, data, references, config) {
#'   if (!"Observations" %in% names(data)) {
#'     validation_finding("Observations are missing", sheet = "Observations")
#'   }
#' }, severity = "warning", description = "Review missing observations")
#' # Display the returned values.
#' validation_custom_rule$id
#' @export
validation_rule <- function(id, check, severity = "error", description = id, parameters = list(), stage = "business") {
  text_value(id, "id")
  text_value(description, "description")
  named_list(parameters, "parameters")
  if (!is.function(check)) stop("check must be a function", call. = FALSE)
  if (!is.character(severity) || length(severity) != 1L || is.na(severity) ||
      !severity %in% c("error", "warning", "info")) {
    stop("severity must be error, warning, or info", call. = FALSE)
  }
  if (!is.character(stage) || length(stage) != 1L || is.na(stage) ||
      !stage %in% c("structure", "business")) stop("stage must be structure or business", call. = FALSE)
  structure(list(id = id, kind = "custom", stage = stage, severity = severity,
                 description = description, check = check, parameters = parameters), class = "validation_rule")
}

#' Generate a validation plan from a form model
#'
#' Recheck the definition through form.model's public API, then generate ordered
#' sheet and field checks. Append explicit custom rules without modifying the model.
#' @param model A `form_model` from form.model.
#' @param rules List of custom rules created by [`validation_rule()`][form.validation::validation_rule].
#' @return A `validation_plan` with checked `model`, ordered `rules`, and
#'   `rulesets` grouping rule IDs into structure and merged content.
#'   Each rule has a stable `id`, `kind`, and `severity`; generated field rules
#'   also identify their model sheet and field. Treat plans as read-only and
#'   regenerate after editing a model. Direct plans have no `rules_version`;
#'   use [`write_validation_rules()`][form.validation::write_validation_rules] and [`read_validation_rules()`][form.validation::read_validation_rules] for versioned files.
#' @details Supported declarations include declared sheets, sheet order,
#'   row/column counts, table start rows, mandatory fields,
#'   all nine field types, enums, bounds, patterns, uniqueness, reference matching,
#'   and same-sheet conditional requiredness. Generated content rules also include
#'   table row counts (info) and internal empty-row checks (warning).
#'   Sequence declarations generate consecutive numbering checks in worksheet order, within each group.
#'   Declared primary keys generate presence and composite uniqueness rules (error).
#'   Unique keys generate uniqueness checks for complete, correctly typed tuples only.
#'   `at_least_one` checks populated rows for a supplied selected value, using its
#'   declared severity or error by default. Zero and false are supplied values.
#'
#' Uniqueness applies only to table fields. Unsupported schema versions, types, layouts, or constraints
#' are explicit generation errors; they are never silently discarded.
#' @section Errors:
#' Invalid model definitions raise form.model errors. Invalid custom rules,
#' duplicate rule IDs, and unsupported declarations raise generation errors.
#' @examples
#' # Load the requested form model.
#' form_model_definition <- form.model::read_form_model("examples/survey/form.yaml", "form.model")
#' # Read the named reference lookup settings.
#' reference_configuration <- form.references::read_reference_configuration(
#'   system.file("examples", "survey", "references.yaml", package = "form.references"))
#' # Resolve reference lookups without changing the original form model.
#' reference_resolved_form_model <- form.references::resolve_reference_lookups(
#'   form_model_definition, reference_configuration)
#' # Prepare the validation rules and their execution order.
#' validation_plan <- validation_plan(reference_resolved_form_model)
#' # Display the returned values.
#' vapply(validation_plan$rules, function(rule) rule$id, character(1))
#' @export
validation_plan <- function(model, rules = list()) {
  lookups <- attr(model, "reference_lookups", exact = TRUE)
  meta <- form.model::form_metadata(model)
  model <- form.model::as_form_model(form.model::form_definition(model))
  attr(model, "reference_lookups") <- lookups
  if (!identical(meta$schema_version, "1.0.0")) {
    stop("Unsupported model schema version: ", meta$schema_version, call. = FALSE)
  }
  if (!is.list(rules)) stop("rules must be a list", call. = FALSE)
  for (rule in rules) {
    if (!inherits(rule, "validation_rule") || !identical(rule$kind, "custom")) {
      stop("rules must contain validation_rule() objects", call. = FALSE)
    }
    validation_rule(rule$id, rule$check, rule$severity, rule$description, rule$parameters, rule$stage)
  }
  generated <- list(rule_record("workbook/sheets", "sheets", stage = "structure"))
  sheets <- form.model::form_sheets(model)
  for (sheet_name in names(sheets)) {
    sheet <- sheets[[sheet_name]]
    fields <- form.model::form_fields(model, sheet_name)
    layout <- sheet$layout
    if (!layout$kind %in% c("cells", "table")) stop("Unsupported layout: ", layout$kind, call. = FALSE)
    for (kind in c("presence", "layout")) {
      generated[[length(generated) + 1L]] <- rule_record(paste("sheet", sheet_name, kind, sep = "/"),
                                                       kind, sheet = sheet_name, stage = "structure")
    }
    for (field in fields) {
      if (!field$type %in% c("string", "email", "integer", "number", "latitude", "longitude", "boolean", "date", "datetime")) {
        stop("Unsupported field type: ", field$type, call. = FALSE)
      }
      unsupported <- setdiff(names(field),
                             c("id", "name", "sheet", "location", "row", "column", "type",
                               "mandatory", "enum", "minimum", "maximum", "maximum_field", "pattern", "sequence", "unique",
                               "reference", "required_field"))
      if (length(unsupported)) stop("Unsupported constraints: ", paste(unsupported, collapse = ", "), call. = FALSE)
      if (isTRUE(field$unique) && layout$kind != "table") {
        stop("unique is only supported for table fields: ", field$id, call. = FALSE)
      }
      kinds <- c(if (field$mandatory) "mandatory",
                 if (!is.null(field$required_field)) "required_field", "type",
                 intersect(c("reference", "enum", "minimum", "maximum", "maximum_field", "pattern", "sequence", "unique"),
                           names(field)))
      for (kind in kinds) {
        if (kind == "unique" && !isTRUE(field$unique)) next
        severity <- if (kind %in% c("reference")) field[[kind]]$severity else "error"
        rule <- rule_record(paste("field", field$id, kind, sep = "/"), kind,
                            severity, sheet_name, field$id, stage = "fields")
        if (kind == "reference") {
          binding <- field$reference
          key <- paste(binding$catalogue, binding$table, binding$lookup, sep = "/")
          lookup <- lookups[[key]]
          if (is.null(lookup)) stop("Resolve named lookups before generating rules: ", field$id, call. = FALSE)
          rule$ignore_case <- lookup$ignore_case
          rule$value <- list(id = key, matching = lookup$matching)
        } else rule$value <- field[[kind]]
        generated[[length(generated) + 1L]] <- rule
      }
    }
    if (!is.null(sheet$primary_key)) {
      key_ids <- paste(sheet_name, unlist(sheet$primary_key, use.names = FALSE), sep = ".")
      for (kind in c("primary_key_present", "primary_key_unique")) {
        rule <- rule_record(paste("sheet", sheet_name, kind, sep = "/"), kind,
                            sheet = sheet_name, stage = "fields")
        rule$value <- sheet$primary_key
        rule$key_fields <- fields[key_ids]
        generated[[length(generated) + 1L]] <- rule
      }
    }
    for (kind in c("unique_key", "at_least_one")) {
      declaration <- sheet[[kind]]
      if (is.null(declaration)) next
      selected <- if (kind == "at_least_one") declaration$fields else declaration
      severity <- if (kind == "at_least_one" && !is.null(declaration$severity)) declaration$severity else "error"
      rule <- rule_record(paste("sheet", sheet_name, kind, sep = "/"), kind,
                          severity = severity, sheet = sheet_name, stage = "fields")
      rule$value <- selected
      rule$key_fields <- fields[paste(sheet_name, unlist(selected, use.names = FALSE), sep = ".")]
      generated[[length(generated) + 1L]] <- rule
    }
    if (!is.null(sheet$parent_key)) {
      rule <- rule_record(paste("sheet", sheet_name, "parent_key", sep = "/"), "parent_key",
                          sheet = sheet_name, stage = "fields")
      rule$value <- sheet$parent_key
      key_ids <- paste(sheet_name, unlist(sheet$parent_key$fields, use.names = FALSE), sep = ".")
      rule$key_fields <- fields[key_ids]
      generated[[length(generated) + 1L]] <- rule
    }
  }
  observations <- list()
  for (sheet_name in names(sheets)) {
    if (sheets[[sheet_name]]$layout$kind != "table") next
    for (kind in c("row_count", "empty_rows")) {
      rule <- rule_record(paste("sheet", sheet_name, kind, sep = "/"), kind,
        severity = if (kind == "row_count") "info" else "warning",
        sheet = sheet_name, stage = "fields")
      rule$value <- TRUE
      observations[[length(observations) + 1L]] <- rule
    }
  }
  structural <- vapply(generated, function(rule) rule$stage == "structure", logical(1))
  generated <- c(generated[structural], observations, generated[!structural])
  all_rules <- c(generated, unname(rules))
  structural <- vapply(all_rules, function(rule) rule$stage == "structure", logical(1))
  all_rules <- c(all_rules[structural], all_rules[!structural])
  ids <- vapply(all_rules, function(rule) rule$id, character(1))
  if (anyDuplicated(ids)) stop("Duplicate rule IDs: ", paste(unique(ids[duplicated(ids)]), collapse = ", "), call. = FALSE)
  structure(list(model = model, rules = all_rules, rulesets = execution_rulesets(all_rules),
                 rules_version = NA_character_), class = "validation_plan")
}

# Reading ---------------------------------------------------------

#' Inspect the validation YAML contract
#'
#' Return the installed contract for structure, generated field, and business
#' validation files.
#' @param version Exact schema version, currently `"1.0.0"`.
#' @return A nested list read from `schema/validation-1.0.0.yaml`.
#' @examples
#' # Display the returned values.
#' validation_schema()$stages
#' @export
validation_schema <- function(version = "1.0.0") {
  text_value(version, "version")
  if (version != "1.0.0") stop("Unsupported validation schema version: ", version, call. = FALSE)
  read_rules_yaml(system.file("schema/validation-1.0.0.yaml", package = "form.validation", mustWork = TRUE))
}

#' Load a named model and validation process
#' @param name Prefix of `<name>-form.yaml` and `<name>-validation.yaml`.
#' @param directory Directory containing both files.
#' @param checks Named callback functions; see [`read_validation_rules()`][form.validation::read_validation_rules].
#' @param lookup_configuration Optional reference lookup definitions, resolved before rule loading.
#' @return A `validation_plan` containing the model and its checks.
#' @seealso [`read_validation_process()`][form.validation::read_validation_process] for a model and process stored separately.
#' @export
load_validation_plan <- function(name, directory, checks = list(), lookup_configuration = NULL) {
  model <- form.model::load_form_model(name, directory)
  if (!is.null(lookup_configuration))
    model <- form.references::resolve_reference_lookups(model, lookup_configuration)
  read_validation_process(file.path(directory, paste0(name, "-validation.yaml")), model, checks)
}

#' Load a complete validation process
#'
#' Read the ordered structure and content rulesets. Content merges generated
#' field checks followed by explicitly listed business files.
#' Ruleset paths are plain filenames resolved beside the process file.
#' @param path Explicit `<name>-validation.yaml` process path.
#' @param model Model used by every listed ruleset.
#' @param checks Named callbacks required by structural and business rules.
#' @param business Additional explicit business YAML paths, appended to content.
#' @return A `validation_plan` ready for [`validate_data()`][form.validation::validate_data].
#' @examples
#' # Locate the installed example resources.
#' validation_directory <- system.file("examples", "survey", package = "form.validation")
#' # Load the requested form model.
#' form_model_definition <- form.model::read_form_model("examples/survey/form.yaml", "form.model")
#' # Read the named reference lookup settings.
#' reference_configuration <- form.references::read_reference_configuration(
#'   system.file("examples", "survey", "references.yaml", package = "form.references"))
#' # Resolve reference lookups without changing the original form model.
#' reference_resolved_form_model <- form.references::resolve_reference_lookups(
#'   form_model_definition, reference_configuration)
#' # Prepare validation callbacks.
#' validation_callbacks <- new.env(parent = baseenv())
#' # Load the example business callbacks.
#' sys.source(file.path(validation_directory, "business.R"), validation_callbacks)
#' # Prepare the validation rules and their execution order.
#' validation_plan <- read_validation_process(
#'   file.path(validation_directory, "validation.yaml"), reference_resolved_form_model,
#'   checks = list(total_count = validation_callbacks$total_count))
#' # Display the returned values.
#' validation_plan$rules_version
#' @export
read_validation_process <- function(path, model, checks = list(), business = character()) {
  process <- read_rules_yaml(path)
  check_rule_keys(process, c("schema_version", "rules_version", "model", "rulesets"), path)
  check_validation_header(process, model, path)
  if (!identical(names(process$rulesets), c("structure", "content"))) {
    stop(path, ": rulesets must be ordered: structure, content", call. = FALSE)
  }
  text_value(process$rulesets$structure, paste(path, "structure ruleset"))
  content <- process$rulesets$content
  if (!is.list(content) || !is.null(names(content)) || !length(content)) {
    stop(path, ": content must be a nonempty sequence of filenames", call. = FALSE)
  }
  filenames <- vapply(c(list(process$rulesets$structure), content), function(filename) {
    text_value(filename, paste(path, "ruleset"))
    if (basename(filename) != filename || filename %in% c(".", "..")) {
      stop(path, ": rulesets must be filenames in the process directory", call. = FALSE)
    }
    filename
  }, character(1))
  if (anyDuplicated(filenames)) stop(path, ": duplicate ruleset filename", call. = FALSE)
  if (!is.character(business) || anyNA(business) || any(!nzchar(business))) {
    stop("business must contain explicit YAML filenames", call. = FALSE)
  }
  plan <- read_validation_rules(c(file.path(dirname(path), filenames), business), model, checks)
  if (!identical(plan$rules_version, process$rules_version)) {
    stop(path, ": rules_version differs from its rulesets", call. = FALSE)
  }
  plan
}

#' Load validation rules
#' @param paths YAML paths ordered as structure, generated fields, then business files.
#' @param model Checked model supplying field definitions and layouts.
#' @param checks Named callback functions, keyed by the last component of each custom rule ID.
#' @return A `validation_plan` for [`validate_data()`][form.validation::validate_data].
#' @details Requires one structure and one field file, matching the model and rule version.
#'   Generated rule order, values and severity must match; stale files need regeneration.
#'   Enum and reference rules may set `ignore_case: false` for strict matching. Omitted or true
#'   ignores case for character values, without rewriting submitted values.
#'   Structural and business files may declare callbacks as `namespace/[field/]callback`.
#'   Parameters reach callbacks through `config$rule`, including the optional field ID.
#'   Severity defaults to `error`, and description to the rule ID.
#'   YAML is parsed as data; callbacks must be supplied explicitly.
#' @seealso [`read_validation_process()`][form.validation::read_validation_process], [`validation_schema()`][form.validation::validation_schema]
#' @export
read_validation_rules <- function(paths, model, checks = list()) {
  if (!is.character(paths) || !length(paths) || anyNA(paths) || any(!nzchar(paths))) {
    stop("paths must contain explicit YAML filenames", call. = FALSE)
  }
  named_list(checks, "checks")
  if (any(!vapply(checks, is.function, logical(1)))) stop("checks must contain functions", call. = FALSE)
  plan <- validation_plan(model)
  documents <- lapply(paths, read_validation_document, model = plan$model)
  version <- check_ruleset_sequence(documents)
  plan <- bind_generated_rules(plan, documents)
  business <- bind_custom_rules(documents, plan$model, checks)
  ids <- c(vapply(plan$rules, `[[`, character(1), "id"),
           vapply(business, `[[`, character(1), "id"))
  if (anyDuplicated(ids)) stop("Duplicate rule IDs across validation files", call. = FALSE)
  plan$rules <- c(plan$rules, business)
  structural <- vapply(plan$rules, function(rule) rule$stage == "structure", logical(1))
  plan$rules <- c(plan$rules[structural], plan$rules[!structural])
  plan$rulesets <- execution_rulesets(plan$rules)
  plan$rules_version <- version
  plan
}

# Writing ---------------------------------------------------------

#' Generate rules for a named model
#' @param name Prefix of `<name>-form.yaml`.
#' @param directory Directory containing the model.
#' @param version Version shared by all validation files.
#' @param overwrite Replace existing generated files; defaults to `FALSE`.
#' @return Structure and generated field-rule paths, invisibly. Business files are untouched.
#' @seealso [`write_validation_rules()`][form.validation::write_validation_rules], [`load_validation_plan()`][form.validation::load_validation_plan]
#' @param lookup_configuration Optional reference lookup definitions, resolved before generation.
#' @export
generate_validation_rules <- function(name, directory, version = "1.0.0", overwrite = FALSE, lookup_configuration = NULL) {
  model <- form.model::load_form_model(name, directory)
  if (!is.null(lookup_configuration))
    model <- form.references::resolve_reference_lookups(model, lookup_configuration)
  paths <- file.path(directory, paste0(name, c("-structure-validation.yaml", "-generated-validation.yaml")))
  write_validation_rules(model, paths[[1L]], paths[[2L]], version, overwrite)
}

#' Write model-derived validation files
#' @param model Checked form.model model.
#' @param structure_path Output structure YAML path.
#' @param generated_path Output field-rule YAML path.
#' @param version Version shared by all validation files.
#' @param overwrite Replace existing matching generated files; defaults to `FALSE`.
#' @return Both paths, invisibly.
#' @details Preserves custom structural rules. Field rules use compact YAML;
#'   edit the form and regenerate when its definition changes.
#'   Explicit enum and reference `ignore_case` options are preserved when regenerating field rules.
#' @seealso [`generate_validation_rules()`][form.validation::generate_validation_rules], [`read_validation_rules()`][form.validation::read_validation_rules]
#' @export
write_validation_rules <- function(model, structure_path, generated_path,
                                   version = "1.0.0", overwrite = FALSE) {
  text_value(structure_path, "structure_path")
  text_value(generated_path, "generated_path")
  text_value(version, "version")
  if (identical(structure_path, generated_path)) stop("Validation paths must differ", call. = FALSE)
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("overwrite must be TRUE or FALSE", call. = FALSE)
  }
  plan <- validation_plan(model)
  paths <- c(structure = structure_path, generated = generated_path)
  stages <- c(structure = "structure", generated = "fields")
  structure_rules <- NULL
  case_options <- list()
  for (name in names(paths)) {
    path <- paths[[name]]
    if (!dir.exists(dirname(path))) stop("Missing output directory: ", dirname(path), call. = FALSE)
    if (file.exists(path)) {
      if (!overwrite) stop("File exists: ", path, call. = FALSE)
      previous <- read_rules_yaml(path)
      if (name == "structure") structure_rules <- previous[["rules"]]
      if (name == "generated") {
        entries <- lapply(previous[["rules"]], function(rule) {
          if (is.null(names(rule))) list(value = rule) else rule
        })
        check_generated_rules(entries, path)
        case_options <- lapply(entries, function(rule) rule$ignore_case)
      }
      if (!identical(previous$stage, stages[[name]])) {
        stop("Only the matching generated validation file can be replaced: ", path, call. = FALSE)
      }
    }
  }
  common <- list(schema_version = "1.0.0", rules_version = version,
                 model = rules_model_identity(plan$model))
  structure <- c(common, list(stage = "structure", sheets = structure_sheets(plan$model)))
  if (!is.null(structure_rules)) structure$rules <- structure_rules
  rules <- generated_rule_entries(plan)
  header <- yaml::as.yaml(c(common, list(stage = "fields")))
  lines <- vapply(names(rules), function(id) {
    rule <- rules[[id]]
    value <- flow_rule_value(rule$value)
    ignore_case <- if (grepl("/(enum|reference)$", id)) case_options[[id]] else NULL
    if (!is.null(rule$severity) || !is.null(names(rule$value)) || !is.null(ignore_case)) {
      value <- paste0("{value: ", value,
        if (!is.null(rule$severity)) paste0(", severity: ", rule$severity),
        if (!is.null(ignore_case)) paste0(", ignore_case: ", flow_rule_value(ignore_case)), "}")
    }
    paste0("  ", id, ": ", value)
  }, character(1))
  yaml::write_yaml(structure, structure_path, fileEncoding = "UTF-8")
  writeLines(c(sub("\n$", "", header), if (length(rules)) "rules:" else "rules: {}", lines),
             generated_path, useBytes = TRUE)
  invisible(paths)
}
