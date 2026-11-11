# Internal helpers ---------------------------------------------------------

# Plan construction ---------------------------------------------------------

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

# Reading ---------------------------------------------------------

#' Validate one parsed validation document
#' @param path YAML path.
#' @param model Checked form model.
#' @return The parsed document.
#' @keywords internal
read_validation_document <- function(path, model) {
  document <- read_rules_yaml(path)
  if (!is.list(document) || is.null(names(document))) stop(path, ": expected a mapping", call. = FALSE)
  required <- c("schema_version", "rules_version", "model", "stage")
  if (!all(required %in% names(document))) stop(path, ": missing required document keys", call. = FALSE)
  text_value(document$stage, paste(path, "stage"))
  if (!document$stage %in% c("structure", "fields", "business")) {
    stop(path, ": unsupported stage: ", document$stage, call. = FALSE)
  }
  keys <- switch(document$stage, structure = c(required, "sheets"),
                 fields = c(required, "rules"), business = c(required, "rules"))
  if (document$stage == "structure" && "rules" %in% names(document)) keys <- c(keys, "rules")
  check_rule_keys(document, keys, path)
  check_validation_header(document, model, path)
  if (document$stage != "structure" || "rules" %in% names(document)) {
    named_list(document[["rules"]], paste(path, "rules"))
    document[["rules"]] <- lapply(document[["rules"]], function(rule) {
      if (is.null(names(rule))) list(value = rule) else rule
    })
  }
  if (document$stage == "structure") check_structure_sheets(document$sheets, path)
  if (document$stage == "fields") check_generated_rules(document[["rules"]], path)
  else check_custom_rules(document[["rules"]], path)
  document
}

#' Check ruleset order and shared version
#' @param documents Parsed validation documents.
#' @return The shared rules version, or an error.
#' @keywords internal
check_ruleset_sequence <- function(documents) {
  stages <- unname(vapply(documents, `[[`, character(1), "stage"))
  if (sum(stages == "structure") != 1L) stop("Supply exactly one structure validation file", call. = FALSE)
  if (sum(stages == "fields") != 1L) stop("Supply exactly one generated field validation file", call. = FALSE)
  expected_stages <- c("structure", "fields", rep("business", sum(stages == "business")))
  if (!identical(stages, expected_stages)) {
    stop("Validation stages must be ordered: structure, fields, business", call. = FALSE)
  }
  versions <- vapply(documents, `[[`, character(1), "rules_version")
  if (length(unique(versions)) != 1L) stop("Validation files have different rules_version values", call. = FALSE)
  versions[[1L]]
}

#' Check and bind model-derived rules
#' @param plan Model-derived validation plan.
#' @param documents Checked rulesets in execution order.
#' @return The plan with field values from the matching generated file.
#' @keywords internal
bind_generated_rules <- function(plan, documents) {
  structure <- documents[[1L]]
  expected_sheets <- structure_sheets(plan$model)
  if (!identical(names(structure$sheets), names(expected_sheets)) ||
      !rules_equal(structure$sheets, expected_sheets)) {
    stop("Structure validation differs from model; regenerate it", call. = FALSE)
  }
  fields <- documents[[2L]]
  fields$rules <- lapply(fields$rules, function(rule) {
    if (identical(rule$severity, "error")) rule$severity <- NULL
    rule
  })
  expected <- generated_rule_entries(plan)
  comparable <- lapply(fields$rules, function(rule) {
    rule$ignore_case <- NULL
    rule
  })
  if (!identical(names(comparable), names(expected)) || !rules_equal(comparable, expected)) {
    stop("Generated field validation differs from model; regenerate it", call. = FALSE)
  }
  for (i in seq_along(plan$rules)) {
    rule <- plan$rules[[i]]
    if (rule$stage == "fields") {
      plan$rules[[i]]$value <- fields$rules[[rule$id]]$value
      if (rule$kind %in% c("enum", "reference") &&
          !is.null(fields$rules[[rule$id]]$ignore_case))
        plan$rules[[i]]$ignore_case <- fields$rules[[rule$id]]$ignore_case
    }
  }
  plan
}

#' Bind custom rules to callbacks
#' @param documents Parsed rulesets.
#' @param model Checked form model.
#' @param checks Named callback functions.
#' @return Custom rules in document order.
#' @keywords internal
bind_custom_rules <- function(documents, model, checks) {
  rules <- list()
  field_ids <- names(form.model::form_fields(model))
  documents <- Filter(function(document) document$stage != "fields", documents)
  for (document in documents) for (id in names(document[["rules"]])) {
    rule <- document[["rules"]][[id]]
    target <- business_rule_target(id)
    check <- checks[[target$check]]
    if (is.null(check)) stop("Missing custom check: ", target$check, " (", id, ")", call. = FALSE)
    if (!is.null(target$field) && !target$field %in% field_ids) {
      stop("Unknown business-rule field: ", target$field, call. = FALSE)
    }
    severity <- if (is.null(rule$severity)) "error" else rule$severity
    description <- if (is.null(rule$description)) id else rule$description
    parameters <- rule[setdiff(names(rule), c("severity", "description"))]
    if (!is.null(target$field)) parameters$field <- target$field
    rules[[length(rules) + 1L]] <- validation_rule(
      id, check, severity, description, parameters, stage = document$stage)
  }
  rules
}

#' Check declared sheet dimensions
#' @param sheets Named sheet dimension mappings.
#' @param path Source path for errors.
#' @return Invisible `NULL`, or an error.
#' @keywords internal
check_structure_sheets <- function(sheets, path) {
  named_list(sheets, paste(path, "sheets"))
  if (!length(sheets)) stop(path, ": sheets must not be empty", call. = FALSE)
  for (name in names(sheets)) {
    sheet <- sheets[[name]]
    named_list(sheet, paste(path, "sheet", name))
    if (!length(sheet) || length(setdiff(names(sheet), c("rows", "min_rows", "columns")))) {
      stop(path, ": sheet ", name, " must declare rows, min_rows and/or columns", call. = FALSE)
    }
    for (value in sheet) if (!is.numeric(value) || length(value) != 1L || is.na(value) || value < 0) {
      stop(path, ": sheet dimensions must be non-negative numbers", call. = FALSE)
    }
  }
  invisible(NULL)
}

#' Check generated rule entries
#' @param rules Expanded field-rule mappings.
#' @param path Source path for errors.
#' @return Invisible `NULL`, or an error.
#' @keywords internal
check_generated_rules <- function(rules, path) {
  for (id in names(rules)) {
    rule <- rules[[id]]
    named_list(rule, paste(path, id))
    allowed <- c("value", "severity", if (grepl("/(enum|reference)$", id)) "ignore_case")
    if (!"value" %in% names(rule) || length(setdiff(names(rule), allowed))) {
      stop(path, ": generated rule requires value and optional severity: ", id, call. = FALSE)
    }
    if ("ignore_case" %in% names(rule) &&
        (!is.logical(rule$ignore_case) || length(rule$ignore_case) != 1L || is.na(rule$ignore_case))) {
      stop(path, ": ignore_case must be true or false: ", id, call. = FALSE)
    }
    if (!is.null(rule$severity) && !rule$severity %in% c("error", "warning", "info")) {
      stop(path, ": generated severity must be error, warning, info or omitted: ", id, call. = FALSE)
    }
  }
  invisible(NULL)
}

#' Check structural and business rule entries
#' A field in the rule ID is an executable target and must exist in the model.
#' A `sheet`/`field` pair in the mapping is callback location metadata and may
#' identify an absent source declaration. The two field forms cannot be combined.
#' @param rules Expanded custom-rule mappings, or `NULL`.
#' @param path Source path for errors.
#' @return Invisible `NULL`, or an error.
#' @keywords internal
check_custom_rules <- function(rules, path) {
  for (id in names(rules)) {
    text_value(id, paste(path, "rule ID"))
    rule <- rules[[id]]
    named_list(rule, paste(path, "rule", id))
    target <- business_rule_target(id)
    if ("check" %in% names(rule)) {
      stop(path, ": callback identifiers belong in the rule ID: ", id, call. = FALSE)
    }
    if ("field" %in% names(rule)) {
      if (!is.null(target$field)) {
        stop(path, ": executable field targets belong in the rule ID; remove the duplicate field property: ", id, call. = FALSE)
      }
      text_value(rule$sheet, paste(path, id, "sheet"))
      text_value(rule$field, paste(path, id, "field"))
    }
    if (!is.null(rule$severity)) {
      text_value(rule$severity, paste(path, id, "severity"))
      if (!rule$severity %in% c("error", "warning", "info")) {
        stop(path, ": unsupported severity for ", id, call. = FALSE)
      }
    }
    if (!is.null(rule$description)) text_value(rule$description, paste(path, id, "description"))
  }
  invisible(NULL)
}

#' Check shared validation document metadata
#' @param document Parsed process or ruleset.
#' @param model Checked form model.
#' @param path Source path for errors.
#' @return Invisible `NULL`, or an error for unsupported or mismatched metadata.
#' @keywords internal
check_validation_header <- function(document, model, path) {
  for (key in c("schema_version", "rules_version")) text_value(document[[key]], paste(path, key))
  if (document$schema_version != "1.0.0") stop(path, ": unsupported schema_version", call. = FALSE)
  check_rule_keys(document$model, c("id", "version"), paste(path, "model"))
  if (!rules_equal(document$model, rules_model_identity(model))) {
    stop(path, ": model identity/version mismatch", call. = FALSE)
  }
  invisible(NULL)
}

#' Read a single YAML rule document without executing expressions
#' @param path Explicit UTF-8 YAML file path.
#' @return Parsed values, preserving sequences as lists and null values.
#' @keywords internal
read_rules_yaml <- function(path) {
  text_value(path, "path")
  if (!file.exists(path) || dir.exists(path)) stop("Missing YAML file: ", path, call. = FALSE)
  tryCatch({
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    if (any(grepl("^(---|\\.\\.\\.)([[:space:]]|$)", lines))) {
      stop("Document markers are unsupported; use one YAML document per file", call. = FALSE)
    }
    problem <- NULL
    handlers <- list(seq = identity, expr = function(x) {
      problem <<- "Expression tags are unsupported"
      x
    }, map = function(x) {
      keys <- attr(x, "keys")
      valid <- vapply(keys, function(k) is.character(k) && length(k) == 1L && !is.na(k), logical(1))
      if (!all(valid)) problem <<- "Mapping keys must be strings"
      if (all(valid) && anyDuplicated(unlist(keys))) problem <<- "Duplicate mapping keys are unsupported"
      attr(x, "keys") <- NULL
      if (all(valid)) names(x) <- vapply(keys, identity, character(1))
      x
    })
    result <- withCallingHandlers(yaml::yaml.load(paste(lines, collapse = "\n"),
      as.named.list = FALSE, handlers = handlers, eval.expr = FALSE),
      warning = function(w) stop(conditionMessage(w), call. = FALSE))
    if (!is.null(problem)) stop(problem, call. = FALSE)
    result
  }, error = function(e) stop(path, ": ", conditionMessage(e), call. = FALSE))
}

#' Check the exact keys of a schema mapping
#' @param x Mapping to check.
#' @param keys Required and allowed keys, as a vector or list.
#' @param label Diagnostic file and document location.
#' @return Invisibly `NULL`, or a load error.
#' @keywords internal
check_rule_keys <- function(x, keys, label) {
  named_list(x, label)
  keys <- unlist(keys, use.names = FALSE)
  if (!setequal(names(x), keys)) {
    stop(label, ": expected keys: ", paste(keys, collapse = ", "), call. = FALSE)
  }
  invisible(NULL)
}

#' Compare YAML values independently of mapping order and integer storage
#' @param x,y Parsed values; sequences remain ordered.
#' @return One logical value. Scalar types and sequence order are significant.
#' @keywords internal
rules_equal <- function(x, y) {
  if (is.list(x) && is.list(y)) {
    if (length(x) != length(y) || is.null(names(x)) != is.null(names(y))) return(FALSE)
    if (!is.null(names(x))) {
      if (!setequal(names(x), names(y))) return(FALSE)
      y <- y[names(x)]
    }
    return(all(vapply(seq_along(x), function(i) rules_equal(x[[i]], y[[i]]), logical(1))))
  }
  if (is.numeric(x) && is.numeric(y)) return(identical(as.double(x), as.double(y)))
  identical(x, y)
}

#' Read a business callback and optional field from its rule ID
#' @param id `<namespace>/<field>/<callback>` or `<namespace>/<callback>`.
#' @return Callback name and optional field ID.
#' @keywords internal
business_rule_target <- function(id) {
  if (!grepl("^[A-Za-z][A-Za-z0-9_.-]*/([A-Za-z][A-Za-z0-9_.-]*/)?[A-Za-z][A-Za-z0-9_.-]*$", id)) {
    stop("Business rule ID must be namespace/[field/]callback: ", id, call. = FALSE)
  }
  parts <- strsplit(id, "/", fixed = TRUE)[[1L]]
  list(check = parts[[length(parts)]], field = if (length(parts) == 3L) parts[[2L]] else NULL)
}

# Writing ---------------------------------------------------------

#' Serialize a generated parameter as a single-line YAML value
#' @param value Scalar or sequence of scalars from the checked form.
#' @return One flow-style YAML string, preserving scalar types and escaping text.
#' @keywords internal
flow_rule_value <- function(value) {
  if (is.null(value)) return("null")
  if (is.list(value)) {
    if (!is.null(names(value))) return(paste0("{", paste(paste0(
      vapply(names(value), flow_rule_value, character(1)), ": ",
      vapply(value, flow_rule_value, character(1))), collapse = ", "), "}"))
    return(paste0("[", paste(vapply(value, flow_rule_value, character(1)), collapse = ", "), "]"))
  }
  if (is.logical(value)) return(if (value) "true" else "false")
  if (is.character(value)) return(encodeString(value, quote = '"'))
  trimws(yaml::as.yaml(value, precision = 17))
}
