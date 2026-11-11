row_constraint_model <- function(severity = "warning", primary = FALSE) {
  sheet <- list(layout = list(kind = "table", start_row = 2L),
    unique_key = list("parent", "category"),
    at_least_one = list(fields = list("category", "count", "confirmed")),
    fields = list(id = list(type = "string"), parent = list(type = "string"),
      category = list(type = "string"), count = list(type = "integer"), confirmed = list(type = "boolean")))
  if (!is.null(severity)) sheet$at_least_one$severity <- severity
  if (primary) sheet$primary_key <- list("id")
  form.model::as_form_model(list(schema_version = "1.0.0", form = list(id = "rows", version = "1"),
    references = list(), sheets = list(Details = sheet)))
}

row_constraint_data <- function() {
  list(Details = data.frame(
    id = c("ID", "1", "2", "3", "4", "5", "6", "7", NA),
    parent = c("PARENT", "P", "P", "P", "P", "Q", "P", "P", NA),
    category = c("CATEGORY", "A", "A", NA, "", "A", NA, NA, NA),
    count = I(list("COUNT", 2L, 3L, NULL, NULL, 1L, 0L, NULL, NULL)),
    confirmed = I(list("CONFIRMED", NULL, NULL, NULL, NULL, NULL, NULL, FALSE, NULL))))
}

test_that("optional keys skip incomplete rows and retain all duplicate occurrences", {
  result <- validate_data(create_validation_plan(row_constraint_model()), row_constraint_data(),
    config = list(blank_strings = TRUE))
  expect_true(result$complete)
  findings <- declared_findings(result)
  expect_identical(findings$row[findings$rule_id == "sheet/Details/unique_key"], c(2L, 3L))
  expect_identical(findings$severity[findings$rule_id == "sheet/Details/unique_key"], c("error", "error"))
  expect_false(any(grepl("primary_key", findings$rule_id)))
  expect_identical(findings$row[findings$rule_id == "sheet/Details/at_least_one"], c(4L, 5L))
  expect_identical(findings$severity[findings$rule_id == "sheet/Details/at_least_one"], c("warning", "warning"))
  literal_blanks <- validate_data(create_validation_plan(row_constraint_model()), row_constraint_data())
  expect_identical(literal_blanks$findings$row[literal_blanks$findings$rule_id == "sheet/Details/at_least_one"], 4L)
})

test_that("row completeness respects preparation and empty-row boundaries", {
  model <- row_constraint_model()
  data <- row_constraint_data()
  result <- validate_data(create_validation_plan(model), data, prepare_data = function(grids) {
    grids$Details$count[[4]] <- 0L
    grids$Details$confirmed[[5]] <- FALSE
    grids
  })
  expect_false(any(result$findings$rule_id == "sheet/Details/at_least_one"))
  empty <- validate_data(create_validation_plan(model), list(Details = data$Details[1, , drop = FALSE]))
  expect_true(empty$complete)
  expect_true(empty$valid)
  expect_false(any(grepl("unique_key|at_least_one", empty$findings$rule_id)))
  default <- validate_data(create_validation_plan(row_constraint_model(NULL)), data)
  expect_true(all(default$findings$severity[default$findings$rule_id == "sheet/Details/at_least_one"] == "error"))
})

test_that("primary and unique key caches remain independent", {
  data <- row_constraint_data()
  data$Details$id[2:3] <- "same"
  result <- validate_data(create_validation_plan(row_constraint_model(primary = TRUE)), data)
  expect_identical(result$findings$row[result$findings$rule_id == "sheet/Details/primary_key_unique"], c(2L, 3L))
  expect_identical(result$findings$row[result$findings$rule_id == "sheet/Details/unique_key"], c(2L, 3L))
  data$Details$category[3] <- "B"
  result <- validate_data(create_validation_plan(row_constraint_model(primary = TRUE)), data)
  expect_identical(result$findings$row[result$findings$rule_id == "sheet/Details/primary_key_unique"], c(2L, 3L))
  expect_false(any(result$findings$rule_id == "sheet/Details/unique_key"))
})

test_that("optional keys ignore incorrectly typed components", {
  data <- row_constraint_data()
  data$Details$category <- as.list(data$Details$category)
  data$Details$category[2:3] <- list(42L, 42L)
  result <- validate_data(create_validation_plan(row_constraint_model()), data)
  expect_false(any(result$findings$rule_id == "sheet/Details/unique_key"))
  expect_identical(result$findings$row[result$findings$rule_id == "field/Details.category/type"], c(2L, 3L))
})

test_that("row constraints round-trip generated YAML and reject stale definitions", {
  directory <- withr::local_tempdir()
  paths <- file.path(directory, c("structure.yaml", "generated.yaml"))
  model <- row_constraint_model()
  write_validation_rules(model, paths[[1]], paths[[2]], version = "1")
  loaded <- read_validation_plan_from_rules(paths, model)
  expect_identical(loaded$rules, create_validation_plan(model)$rules)
  expect_identical(validate_data(loaded, row_constraint_data())$findings,
    validate_data(create_validation_plan(model), row_constraint_data())$findings)
  d <- form.model::form_definition(model)
  d$sheets$Details$at_least_one$severity <- "error"
  expect_error(read_validation_plan_from_rules(paths, form.model::as_form_model(d)), "differs from model")
  d <- form.model::form_definition(model)
  d$sheets$Details$unique_key <- list("category")
  expect_error(read_validation_plan_from_rules(paths, form.model::as_form_model(d)), "differs from model")
})
