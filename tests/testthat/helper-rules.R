rule_paths <- function() {
  directory <- tempfile()
  dir.create(directory)
  paths <- file.path(directory, c("survey-structure-validation.yaml",
                                  "survey-generated-validation.yaml"))
  write_validation_rules(fixture_model(), paths[[1L]], paths[[2L]], version = "1")
  paths
}
change_rules <- function(path, change) {
  document <- form.validation:::read_rules_yaml(path)
  yaml::write_yaml(change(document), path)
}
business_path <- function() {
  path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(list(schema_version = "1.0.0", rules_version = "1",
    model = list(id = "fixture", version = "1"), stage = "business",
    rules = list(`business/total_amount` = list(severity = "warning", maximum = 10))), path)
  path
}
limit_check <- function(model, data, references, config) {
  stopifnot(config$custom$caller == "test")
  values <- data$Observations$amount
  bad <- which(values > config$rule$maximum)
  if (length(bad)) do.call(rbind, lapply(bad, function(row) {
    validation_finding("Amount exceeds review limit", "Observations", "amount", row, 2,
      values[[row]], config$rule$maximum)
  }))
}
