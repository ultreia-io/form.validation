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
#' rule <- validation_rule("review/source", function(model, data, references, config) {
#'   if (!"Observations" %in% names(data)) {
#'     validation_finding("Observations are missing", sheet = "Observations")
#'   }
#' }, severity = "warning", description = "Review missing observations")
#' rule$id
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
#'   all nine field types, enums, bounds, patterns, uniqueness, code-list membership,
#'   registry matching,
#'   and same-sheet conditional requiredness. Generated content rules also include
#'   table row counts (info) and internal empty-row checks (warning).
#'   Declared primary keys generate presence and composite uniqueness rules (error).
#'
#' Uniqueness applies only to table fields. Unsupported schema versions, types, layouts, or constraints
#' are explicit generation errors; they are never silently discarded.
#' @section Errors:
#' Invalid model definitions raise form.model errors. Invalid custom rules,
#' duplicate rule IDs, and unsupported declarations raise generation errors.
#' @examples
#' model <- form.model::read_form_model("examples/survey-form.yaml", "form.model")
#' plan <- validation_plan(model)
#' vapply(plan$rules, function(rule) rule$id, character(1))
#' @export
validation_plan <- function(model, rules = list()) {
  meta <- form.model::form_metadata(model)
  model <- form.model::as_form_model(form.model::form_definition(model))
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
                               "mandatory", "enum", "minimum", "maximum", "maximum_field", "pattern", "unique",
                               "reference", "code_list", "registry", "required_field"))
      if (length(unsupported)) stop("Unsupported constraints: ", paste(unsupported, collapse = ", "), call. = FALSE)
      if (isTRUE(field$unique) && layout$kind != "table") {
        stop("unique is only supported for table fields: ", field$id, call. = FALSE)
      }
      kinds <- c(if (field$mandatory) "mandatory",
                 if (!is.null(field$required_field)) "required_field", "type",
                 intersect(c("reference", "code_list", "registry", "enum", "minimum", "maximum", "maximum_field", "pattern", "unique"),
                           names(field)))
      for (kind in kinds) {
        if (kind == "unique" && !isTRUE(field$unique)) next
        severity <- if (kind %in% c("reference", "code_list", "registry")) field[[kind]]$severity else "error"
        rule <- rule_record(paste("field", field$id, kind, sep = "/"), kind,
                            severity, sheet_name, field$id, stage = "fields")
        rule$value <- if (kind %in% c("reference", "code_list")) field[[kind]]$id else
          if (kind == "registry") Filter(Negate(is.null), field$registry[c("id", "column", "matching")]) else field[[kind]]
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

#' Describe one generated check
#' @param id,kind,severity Rule identity, operation, and finding severity.
#' @param sheet,field Model identifiers, when the check targets a sheet or field.
#' @param stage Execution stage: `"structure"`, `"fields"`, or `"business"`.
#' @return An ordinary rule descriptor consumed by the executor.
#' @keywords internal
rule_record <- function(id, kind, severity = "error", sheet = NULL, field = NULL,
                        stage = "fields") {
  list(id = id, kind = kind, stage = stage, severity = severity, sheet = sheet, field = field)
}

#' Group rules into structure and content stages
#' @param rules Ordered rule descriptors.
#' @return Structure and merged content rule IDs.
#' @keywords internal
execution_rulesets <- function(rules) {
  ids <- vapply(rules, `[[`, character(1), "id")
  structural <- vapply(rules, function(rule) rule$stage == "structure", logical(1))
  list(structure = ids[structural], content = ids[!structural])
}

#' Extract portable model identity through its public API
#' @param model Checked form model.
#' @return Form ID and version.
#' @keywords internal
rules_model_identity <- function(model) {
  meta <- form.model::form_metadata(model)
  list(id = meta$form$id, version = meta$form$version)
}

#' Summarize model sheet dimensions
#' @param model Checked form model.
#' @return Named mappings in model sheet order.
#' @keywords internal
structure_sheets <- function(model) {
  sheets <- form.model::form_sheets(model)
  stats::setNames(lapply(names(sheets), function(name) {
    sheet <- sheets[[name]]
    result <- list()
    if (!is.null(sheet$layout$row_count)) result$rows <- sheet$layout$row_count
    else if (sheet$layout$kind == "cells") result$min_rows <- minimum_cell_rows(model, name)
    if (!is.null(sheet$layout$column_count)) result$columns <- sheet$layout$column_count
    if (!length(result)) stop("Sheet layout has no dimensions", call. = FALSE)
    result
  }), names(sheets))
}

#' Describe generated field rules for serialization
#' @param plan Generated validation plan.
#' @return Ordered mapping from rule IDs to values and non-default severities.
#' @keywords internal
generated_rule_entries <- function(plan) {
  rules <- Filter(function(rule) rule$stage == "fields", plan$rules)
  entries <- lapply(rules, function(rule) {
    entry <- list(value = rule$value)
    if (rule$severity != "error") entry$severity <- rule$severity
    entry
  })
  stats::setNames(entries, vapply(rules, `[[`, character(1), "id"))
}
