#' Generate rules for a named model
#' @param name Prefix of `<name>-form.yaml`.
#' @param directory Directory containing the model.
#' @param version Version shared by all validation files.
#' @param overwrite Replace existing generated files; defaults to `FALSE`.
#' @return Structure and generated field-rule paths, invisibly. Business files are untouched.
#' @seealso [`write_validation_rules()`][form.validation::write_validation_rules], [`load_validation_plan()`][form.validation::load_validation_plan]
#' @export
generate_validation_rules <- function(name, directory, version = "1.0.0", overwrite = FALSE) {
  model <- form.model::load_form_model(name, directory)
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
  for (name in names(paths)) {
    path <- paths[[name]]
    if (!dir.exists(dirname(path))) stop("Missing output directory: ", dirname(path), call. = FALSE)
    if (file.exists(path)) {
      if (!overwrite) stop("File exists: ", path, call. = FALSE)
      previous <- read_rules_yaml(path)
      if (name == "structure") structure_rules <- previous[["rules"]]
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
    if (!is.null(rule$severity)) value <- paste0("{value: ", value, ", severity: ", rule$severity, "}")
    paste0("  ", id, ": ", value)
  }, character(1))
  yaml::write_yaml(structure, structure_path, fileEncoding = "UTF-8")
  writeLines(c(sub("\n$", "", header), if (length(rules)) "rules:" else "rules: {}", lines),
             generated_path, useBytes = TRUE)
  invisible(paths)
}

#' Serialize a generated parameter as a single-line YAML value
#' @param value Scalar or sequence of scalars from the checked form.
#' @return One flow-style YAML string, preserving scalar types and escaping text.
#' @keywords internal
flow_rule_value <- function(value) {
  if (is.null(value)) return("null")
  if (is.list(value)) return(paste0("[", paste(vapply(value, flow_rule_value, character(1)), collapse = ", "), "]"))
  if (is.logical(value)) return(if (value) "true" else "false")
  if (is.character(value)) return(encodeString(value, quote = '"'))
  trimws(yaml::as.yaml(value, precision = 17))
}
