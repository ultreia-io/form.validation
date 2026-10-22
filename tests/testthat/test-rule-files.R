test_that("generated validation files are compact and round-trip the plan", {
  paths <- rule_paths()
  on.exit(unlink(dirname(paths[[1L]]), recursive = TRUE))
  structure <- form.validation:::read_rules_yaml(paths[[1L]])
  fields <- form.validation:::read_rules_yaml(paths[[2L]])
  expect_identical(structure$stage, "structure")
  expect_identical(names(structure$sheets), c("Cover", "Observations"))
  expect_identical(fields$stage, "fields")
  expect_identical(fields$rules[["field/Observations.amount/minimum"]], 0)
  expect_identical(fields$rules[["field/Observations.site/reference"]], list(value = "sites", severity = "warning"))
  expect_identical(fields$rules[["field/Observations.unit/enum"]], list("cm", "m"))
  plan <- read_validation_rules(stats::setNames(paths, c("structure", "generated")), fixture_model())
  actual <- validate_data(plan, fixture_data(), fixture_references())
  expected <- validate_data(validation_plan(fixture_model()), fixture_data(), fixture_references())
  expect_identical(actual$findings, expected$findings)
  expect_identical(actual$execution, expected$execution)
  expect_identical(actual$rules_version, "1")
})

test_that("compact business rules receive explicit values and defaults", {
  paths <- rule_paths()
  on.exit(unlink(dirname(paths[[1L]]), recursive = TRUE))
  business <- business_path()
  on.exit(unlink(business), add = TRUE)
  plan <- read_validation_rules(c(paths, business), fixture_model(),
    checks = list(total_amount = limit_check))
  data <- fixture_data()
  data$Observations$amount[[1L]] <- 11
  result <- validate_data(plan, data, fixture_references(),
    config = list(custom = list(caller = "test")))
  expect_identical(result$findings$rule_id, "business/total_amount")
  expect_identical(result$findings$severity, "warning")
  expect_equal(result$findings$expected[[1L]], 10)

  document <- form.validation:::read_rules_yaml(business)
  document$rules[[1L]]$severity <- NULL
  document$rules[[1L]]$description <- NULL
  yaml::write_yaml(document, business)
  defaulted <- read_validation_rules(c(paths, business), fixture_model(),
    checks = list(total_amount = limit_check))
  rule <- tail(defaulted$rules, 1L)[[1L]]
  expect_identical(rule$severity, "error")
  expect_identical(rule$description, rule$id)
})

test_that("rule files reject wrong versions, stages and stale definitions", {
  paths <- rule_paths()
  on.exit(unlink(dirname(paths[[1L]]), recursive = TRUE))
  expect_error(write_validation_rules(fixture_model(), paths[[1L]], paths[[2L]]), "File exists")
  expect_error(validation_schema("2"), "Unsupported")
  expect_error(read_validation_rules(paths[[2L]], fixture_model()), "structure")
  expect_error(read_validation_rules(rev(paths), fixture_model()), "ordered")
  changed <- fixture_definition()
  changed$form$version <- "2"
  expect_error(read_validation_rules(paths, form.model::as_form_model(changed)), "identity/version")
  original <- form.validation:::read_rules_yaml(paths[[1L]])
  for (change in list(
    function(d) { d$schema_version <- "2"; d },
    function(d) { d$sheets$Observations$columns <- 5L; d },
    function(d) { d$sheets <- rev(d$sheets); d }
  )) {
    yaml::write_yaml(change(original), paths[[1L]])
    expect_error(read_validation_rules(paths, fixture_model()), "unsupported schema_version|differs from model")
  }
})

test_that("validation process declares the complete ordered pipeline", {
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  file.copy(system.file("examples", c("survey-structure-validation.yaml",
    "survey-generated-validation.yaml", "survey-business-validation.yaml",
    "survey-validation.yaml"), package = "form.validation"), directory)
  model <- upstream_model()
  process <- file.path(directory, "survey-validation.yaml")
  plan <- read_validation_process(process, model, checks = upstream_checks())
  expect_identical(plan$rules_version, "1")
  expect_identical(vapply(plan$rules, `[[`, character(1), "stage") |> unique(),
                   c("structure", "fields", "business"))
  data <- upstream_data()
  expect_true(validate_data(plan, data, upstream_references())$valid)
  data$Observations[6:7, 2] <- list(600L, 700L)
  result <- validate_data(plan, data, upstream_references())
  expect_identical(result$findings$rule_id, "business/total_count")
  expect_identical(result$findings$value[[1]], 1300L)
  data$Observations[[6, 2]] <- "wrong"
  result <- validate_data(plan, data, upstream_references())
  expect_identical(result$status, "incomplete")
  expect_identical(result$findings$row, 6L)
  extra <- file.path(directory, "extra.yaml")
  document <- form.validation:::read_rules_yaml(file.path(directory, "survey-business-validation.yaml"))
  names(document$rules) <- "extra/total_count"
  yaml::write_yaml(document, extra)
  appended <- read_validation_process(process, model, checks = upstream_checks(), business = extra)
  expect_identical(tail(appended$rulesets$content, 2), c("business/total_count", "extra/total_count"))
  change_rules(process, function(d) { d$rulesets <- rev(d$rulesets); d })
  expect_error(read_validation_process(process, model,
    checks = upstream_checks()), "ordered")
  change_rules(process, function(d) { d$rulesets <- list(structure = "../outside.yaml", content = list("fields.yaml")); d })
  expect_error(read_validation_process(process, model), "process directory")
})

test_that("invalid YAML is data only and never drops duplicate keys", {
  path <- tempfile(fileext = ".yaml")
  marker <- tempfile()
  on.exit(unlink(c(path, marker)))
  writeLines("schema_version: '1'\nschema_version: '2'", path)
  expect_error(form.validation:::read_rules_yaml(path), "Duplicate")
  writeLines("schema_version: '1'\n---\nschema_version: '2'", path)
  expect_error(form.validation:::read_rules_yaml(path), "Document markers")
  writeLines(paste0("test: !expr file.create('", marker, "')"), path)
  expect_error(form.validation:::read_rules_yaml(path), "Expression tags")
  expect_false(file.exists(marker))
  writeLines("1: value", path)
  expect_error(form.validation:::read_rules_yaml(path), "Mapping keys must be strings")
})

test_that("changed model values require regenerating the concrete rules", {
  paths <- rule_paths()
  on.exit(unlink(dirname(paths[[1L]]), recursive = TRUE))
  changed <- fixture_definition()
  changed$sheets$Observations$fields$amount$minimum <- 1
  model <- form.model::as_form_model(changed)
  expect_error(read_validation_rules(paths, model), "differs from model")
  write_validation_rules(model, paths[[1L]], paths[[2L]], "1", overwrite = TRUE)
  plan <- read_validation_rules(paths, model)
  data <- fixture_data()
  data$Observations$amount[[1L]] <- 0
  expect_identical(validate_data(plan, data, fixture_references())$findings$rule_id,
                   "field/Observations.amount/minimum")
})

test_that("generated rule order, parameters and severities cannot silently drift", {
  paths <- rule_paths()
  on.exit(unlink(dirname(paths[[1L]]), recursive = TRUE))
  original <- form.validation:::read_rules_yaml(paths[[2L]])
  for (change in list(
    function(d) { d$rules <- d$rules[-1L]; d },
    function(d) { d$rules <- rev(d$rules); d },
    function(d) { d$rules[["field/Observations.amount/minimum"]] <- -1; d },
    function(d) { d$rules[["field/Observations.site/reference"]]$severity <- NULL; d }
  )) {
    yaml::write_yaml(change(original), paths[[2L]])
    expect_error(read_validation_rules(paths, fixture_model()), "differs from model")
  }
})

test_that("compact parameters preserve quoted text and numeric values", {
  d <- fixture_definition()
  d$sheets$Observations$fields$amount$minimum <- 0.12345678901234567
  d$sheets$Observations$fields$unit$enum <- list("01", "true", "a: b", "quote\"", "line\nbreak")
  paths <- rule_paths()
  on.exit(unlink(dirname(paths[[1L]]), recursive = TRUE))
  compact <- read_validation_rules(paths, fixture_model())
  expanded <- form.validation:::read_validation_document(paths[[2L]], compact$model)
  yaml::write_yaml(expanded, paths[[2L]])
  expect_identical(read_validation_rules(paths, compact$model)$rules, compact$rules)
  model <- form.model::as_form_model(d)
  write_validation_rules(model, paths[[1L]], paths[[2L]], "1", overwrite = TRUE)
  loaded <- read_validation_rules(paths, model)
  expected <- validation_plan(model)
  expect_identical(loaded$rules, expected$rules)
})

test_that("business IDs bind the callback and field without duplicate properties", {
  paths <- rule_paths()
  on.exit(unlink(dirname(paths[[1L]]), recursive = TRUE))
  business <- business_path()
  on.exit(unlink(business), add = TRUE)
  document <- form.validation:::read_rules_yaml(business)
  names(document$rules) <- "business/Observations.amount/inspect_amount"
  yaml::write_yaml(document, business)
  seen <- NULL
  inspect <- function(model, data, references, config) { seen <<- config$rule; NULL }
  plan <- read_validation_rules(c(paths, business), fixture_model(), list(inspect_amount = inspect))
  validate_data(plan, fixture_data(), fixture_references())
  expect_identical(seen$field, "Observations.amount")
  expect_equal(seen$maximum, 10)
  expect_false("check" %in% names(seen))
  expect_error(read_validation_rules(c(paths, business), fixture_model()), "Missing custom check: inspect_amount")
  for (property in c("check", "field")) {
    changed <- document; changed$rules[[1L]][[property]] <- "duplicate"
    yaml::write_yaml(changed, business)
    expect_error(read_validation_rules(c(paths, business), fixture_model()), "belong in the rule ID")
  }
  names(document$rules) <- "business/Observations.absent/inspect_amount"
  yaml::write_yaml(document, business)
  expect_error(read_validation_rules(c(paths, business), fixture_model(), list(inspect_amount = inspect)), "Unknown business-rule field")
  names(document$rules) <- "field/Observations.id/type"
  yaml::write_yaml(document, business)
  expect_error(read_validation_rules(c(paths, business), fixture_model(), list(type = inspect)), "Duplicate rule IDs")
})

test_that("forms with no fields round-trip an empty generated rules mapping", {
  d <- fixture_definition()
  for (name in names(d$sheets)) {
    d$sheets[[name]]$fields <- stats::setNames(list(), character())
    d$sheets[[name]]$layout <- list(kind = "table", column_count = 0L, start_row = 1L)
  }
  paths <- rule_paths()
  on.exit(unlink(dirname(paths[[1L]]), recursive = TRUE))
  model <- form.model::as_form_model(d)
  write_validation_rules(model, paths[[1L]], paths[[2L]], "1", overwrite = TRUE)
  expect_identical(read_validation_rules(paths, model)$rules, validation_plan(model)$rules)
})

test_that("business shortcuts preserve values and parameterless mappings", {
  paths <- rule_paths()
  on.exit(unlink(dirname(paths[[1L]]), recursive = TRUE))
  business <- business_path()
  on.exit(unlink(business), add = TRUE)
  document <- form.validation:::read_rules_yaml(business)
  header <- sub("\n$", "", yaml::as.yaml(document[setdiff(names(document), "rules")]))
  examples <- c('"01"', 'false', '0', 'null', '[]', '[cm, null]', '{value: {code: x}}', '{}')
  values <- list("01", FALSE, 0L, NULL, list(), list("cm", NULL), list(code = "x"), NULL)
  seen <- NULL
  inspect <- function(model, data, references, config) { seen <<- config$rule; NULL }
  for (i in seq_along(examples)) {
    writeLines(c(header, "rules:", paste0("  business/inspect: ", examples[[i]])), business)
    plan <- read_validation_rules(c(paths, business), fixture_model(), list(inspect = inspect))
    validate_data(plan, fixture_data(), fixture_references())
    expect_identical(seen$value, values[[i]])
    expect_identical("value" %in% names(seen), examples[[i]] != "{}")
    expect_identical(tail(plan$rules, 1L)[[1L]]$severity, "error")
  }
})

test_that("minimum cell extents round-trip and follow declared locations", {
  d <- fixture_definition()
  d$sheets$Cover$layout$row_count <- NULL
  d$sheets$Cover$fields$title$location <- "A3"
  model <- form.model::as_form_model(d)
  paths <- rule_paths()
  on.exit(unlink(dirname(paths[[1L]]), recursive = TRUE))
  write_validation_rules(model, paths[[1L]], paths[[2L]], overwrite = TRUE)
  document <- form.validation:::read_rules_yaml(paths[[1L]])
  expect_identical(document$sheets$Cover, list(min_rows = 3L, columns = 1L))
  plan <- read_validation_rules(paths, model)
  for (rows in c(3L, 4L)) {
    data <- fixture_data(); data$Cover <- matrix("Title", rows, 1)
    expect_true(validate_data(plan, data, fixture_references())$valid)
  }
  data$Cover <- data$Cover[1:2, , drop = FALSE]
  result <- validate_data(plan, data, fixture_references())
  expect_false(result$structure_passed)
  expect_identical(result$findings$message, "Expected at least 3 rows for declared cells; found: 2")
  change_rules(paths[[1L]], function(d) { d$sheets$Cover$min_rows <- 2L; d })
  expect_error(read_validation_rules(paths, model), "differs from model")
})

test_that("structure YAML binds explicit callbacks and survives regeneration", {
  paths <- rule_paths()
  on.exit(unlink(dirname(paths[[1L]]), recursive = TRUE))
  change_rules(paths[[1L]], function(d) {
    d$rules <- list("workbook/check_structure" = stats::setNames(list(), character()))
    d
  })
  expect_error(read_validation_rules(paths, fixture_model()), "Missing custom check: check_structure")
  plan <- read_validation_rules(paths, fixture_model(), list(check_structure = function(...) NULL))
  expect_true("workbook/check_structure" %in% plan$rulesets$structure)
  expect_identical(rle(vapply(plan$rules, `[[`, character(1), "stage"))$values, c("structure", "fields"))
  write_validation_rules(fixture_model(), paths[[1]], paths[[2]], "1", overwrite = TRUE)
  expect_true("workbook/check_structure" %in% read_validation_rules(paths, fixture_model(),
    list(check_structure = function(...) NULL))$rulesets$structure)
})
