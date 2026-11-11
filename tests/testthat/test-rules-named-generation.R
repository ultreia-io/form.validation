test_that("named generation preserves paths and reads the form family", {
  configuration <- form.references::read_reference_configuration(
    system.file("examples", "survey", "references.yaml", package = "form.references"))
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  file.copy(system.file("examples/survey/form.yaml", package = "form.model"),
            file.path(directory, "survey-form.yaml"))
  expect_error(load_validation_plan("survey", directory, lookup_configuration = configuration), "Missing YAML file")
  paths <- write_named_validation_rules("survey", directory, version = "1", lookup_configuration = configuration)
  expect_named(paths, c("structure", "generated"))
  expect_identical(basename(paths), c("survey-structure-validation.yaml",
                                      "survey-generated-validation.yaml"))
  process <- list(schema_version = "1.0.0", rules_version = "1",
                  model = list(id = "survey", version = "2026.2"),
                  rulesets = list(structure = basename(paths[["structure"]]),
                                  content = list(basename(paths[["generated"]]))))
  process_path <- file.path(directory, "survey-validation.yaml")
  yaml::write_yaml(process, process_path)
  plan <- load_validation_plan("survey", directory, lookup_configuration = configuration)
  expect_identical(form.model::form_definition(plan$model),
                   form.model::form_definition(upstream_model()))
  expect_true(validate_data(plan, upstream_data(), upstream_references())$valid)
  expect_error(write_named_validation_rules("survey", directory, lookup_configuration = configuration), "File exists")
  business <- file.path(directory, "survey-business-validation.yaml")
  file.copy(system.file("examples/survey/business-validation.yaml", package = "form.validation"), business)
  process$rulesets$content <- c(process$rulesets$content, list(basename(business)))
  yaml::write_yaml(process, process_path)
  expect_error(load_validation_plan("survey", directory, lookup_configuration = configuration), "Missing custom check")
  callbacks <- new.env(parent = baseenv())
  sys.source(system.file("examples/survey/business.R", package = "form.validation"), callbacks)
  checks <- list(total_count = callbacks$total_count)
  plan <- load_validation_plan("survey", directory, checks, lookup_configuration = configuration)
  expect_identical(tail(vapply(plan$rules, `[[`, character(1), "id"), 1), "business/total_count")
  before <- readLines(business)
  write_named_validation_rules("survey", directory, version = "1", overwrite = TRUE, lookup_configuration = configuration)
  expect_identical(readLines(business), before)
  writeLines("invalid: yaml", business)
  expect_error(load_validation_plan("survey", directory, checks, lookup_configuration = configuration), "missing required document keys")
  expect_error(load_validation_plan("../survey", directory), "expected a model name")
  expect_error(load_validation_plan("other", directory), "does not exist|not found")
})
