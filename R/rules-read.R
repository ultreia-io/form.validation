#' Inspect the validation YAML contract
#'
#' Return the installed contract for structure, generated field, and business
#' validation files.
#' @param version Exact schema version, currently `"1.0.0"`.
#' @return A nested list read from `schema/validation-1.0.0.yaml`.
#' @examples
#' validation_schema()$stages
#' @export
validation_schema <- function(version = "1.0.0") {
  text_value(version, "version")
  if (version != "1.0.0") stop("Unsupported validation schema version: ", version, call. = FALSE)
  read_rules_yaml(system.file("schema/validation-1.0.0.yaml", package = "model.validation", mustWork = TRUE))
}

#' Load a named model and validation process
#' @param name Prefix of `<name>-form.yaml` and `<name>-validation.yaml`.
#' @param directory Directory containing both files.
#' @param checks Named callback functions; see [read_validation_rules()].
#' @return A `validation_plan` containing the model and its checks.
#' @seealso [read_validation_process()] for a model and process stored separately.
#' @export
load_validation_plan <- function(name, directory, checks = list()) {
  model <- model.form::load_form_model(name, directory)
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
#' @return A `validation_plan` ready for [validate_data()].
#' @examples
#' directory <- system.file("examples", package = "model.validation")
#' model <- model.form::read_form_model("examples/survey-form.yaml", "model.form")
#' callbacks <- new.env(parent = baseenv())
#' sys.source(file.path(directory, "business.R"), callbacks)
#' plan <- read_validation_process(file.path(directory, "survey-validation.yaml"), model,
#'   checks = list(total_count = callbacks$total_count))
#' plan$rules_version
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
#' @return A `validation_plan` for [validate_data()].
#' @details Requires one structure and one field file, matching the model and rule version.
#'   Generated rule order, values and severity must match; stale files need regeneration.
#'   Structural and business files may declare callbacks as `namespace/[field/]callback`.
#'   Parameters reach callbacks through `config$rule`, including the optional field ID.
#'   Severity defaults to `error`, and description to the rule ID.
#'   YAML is parsed as data; callbacks must be supplied explicitly.
#' @seealso [read_validation_process()], [validation_schema()]
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
  expected <- generated_rule_entries(plan)
  if (!identical(names(fields$rules), names(expected)) || !rules_equal(fields$rules, expected)) {
    stop("Generated field validation differs from model; regenerate it", call. = FALSE)
  }
  for (i in seq_along(plan$rules)) {
    rule <- plan$rules[[i]]
    if (rule$stage == "fields") plan$rules[[i]]$value <- fields$rules[[rule$id]]$value
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
  field_ids <- names(model.form::form_fields(model))
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
    if (!"value" %in% names(rule) || length(setdiff(names(rule), c("value", "severity")))) {
      stop(path, ": generated rule requires value and optional severity: ", id, call. = FALSE)
    }
    if (!is.null(rule$severity) && !identical(rule$severity, "warning")) {
      stop(path, ": generated severity must be warning or omitted: ", id, call. = FALSE)
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
