test_that("valid data returns stable empty findings and model identity", {
  result <- validate_fixture()
  expect_s3_class(result, "validation_result")
  expect_identical(result$status, "valid")
  expect_true(result$valid)
  expect_true(result$complete)
  expect_equal(nrow(result$findings), 0L)
  expect_true(is.list(result$findings$value))
  expect_identical(result$model$form$id, "fixture")
  expect_output(print(result), "valid")
})

test_that("sheet presence, order, dimensions, and extra sheets are checked", {
  empty <- validate_fixture(list())
  expect_identical(empty$findings$rule_id, c("sheet/Cover/presence", "sheet/Observations/presence"))
  data <- fixture_data()
  result <- validate_fixture(data[2])
  expect_identical(result$status, "invalid")
  expect_true(result$complete)
  expect_true("sheet/Cover/presence" %in% result$findings$rule_id)
  expect_identical(result$execution$status[result$execution$rule_id == "field/Cover.title/type"], "skipped")
  expect_true("workbook/sheets" %in% validate_fixture(data[2:1])$findings$rule_id)
  data$Cover <- matrix("Example", 2, 1)
  expect_true("sheet/Cover/layout" %in% validate_fixture(data)$findings$rule_id)
  data <- fixture_data()
  data$Extra <- matrix(1)
  expect_true(validate_fixture(data)$valid)
  expect_false(validate_fixture(data, config = list(allow_extra_sheets = FALSE))$valid)
})

test_that("multi-row preambles are ignored and data retains physical coordinates", {
  d <- fixture_definition()
  d$sheets$Observations$layout$start_row <- 3L
  data <- fixture_data()
  grid <- matrix(vector("list", 12), nrow = 3, ncol = 4)
  grid[1, ] <- list("First heading", "", "Merged heading", NULL)
  grid[2, ] <- list("Different label", "Units", NULL, "Notes")
  grid[3, ] <- list("S01", 2, "cm", "north")
  data$Observations <- grid
  model <- model.form::as_form_model(d)
  expect_true(validate_fixture(data, model = model)$valid)
  data$Observations[1:2, ] <- rep(list("Arbitrary preamble"), 8)
  expect_true(validate_fixture(data, model = model)$valid)
  data$Observations[[3, 2]] <- -1
  finding <- validate_fixture(data, model = model)$findings
  expect_identical(finding$rule_id, "field/Observations.amount/minimum")
  expect_identical(finding$row, 3L)
  expect_identical(finding$column, 2L)
  expect_identical(finding$value[[1]], -1)
  expect_identical(finding$source, "submission.xlsx")
  data$Observations <- grid[1, , drop = FALSE]
  result <- validate_fixture(data, model = model)
  expect_true(result$structure_passed)
  expect_true(result$valid)
})

test_that("mandatory fields require a filled value", {
  data <- fixture_data()
  data$Cover <- matrix(NA_character_, 1, 1)
  expect_identical(validate_fixture(data)$findings$rule_id, "field/Cover.title/mandatory")
  data$Cover <- matrix(character(), 0, 1)
  expect_identical(validate_fixture(data)$findings$rule_id, "sheet/Cover/layout")
  data$Cover <- matrix("", 1, 1)
  expect_true(validate_fixture(data)$valid)
  expect_identical(validate_fixture(data, config = list(blank_strings = TRUE))$findings$rule_id,
                   "field/Cover.title/mandatory")
  data$Observations <- data$Observations[FALSE, ]
  expect_true(validate_fixture(data, references = list())$valid)
})

test_that("all structure finishes before fields and business, and alone gates content", {
  calls <- 0L
  callback <- validation_rule("business/counter", function(...) { calls <<- calls + 1L; NULL })
  plan <- validation_plan(fixture_model(), list(callback))
  expect_identical(names(plan$rulesets), c("structure", "content"))
  expect_identical(tail(plan$rulesets$content, 1), "business/counter")
  stages <- vapply(plan$rules, `[[`, character(1), "stage")
  expect_identical(unique(stages), c("structure", "fields", "business"))
  data <- fixture_data()
  data$Cover <- matrix("wrong dimensions", 2, 1)
  data$Observations <- data$Observations[, 1:3]
  result <- validate_data(plan, data)
  expect_identical(result$findings$rule_id, c("sheet/Cover/layout", "sheet/Observations/layout"))
  expect_identical(result$findings$message, c("Expected row count: 1; found: 2",
                                             "Expected column count: 4; found: 3"))
  expect_true(all(result$execution$status[stages != "structure"] == "skipped"))
  expect_identical(calls, 0L)
  data <- fixture_data()
  data$Observations$amount[[1]] <- -1
  result <- validate_data(plan, data)
  expect_identical(result$status, "incomplete")
  expect_identical(calls, 1L)
})

test_that("failed or incomplete structural execution never invokes callbacks", {
  calls <- 0L
  callback <- validation_rule("business/counter", function(...) { calls <<- calls + 1L; NULL })
  original <- model.validation:::run_builtin
  for (problem in c("incomplete", "failed")) {
    local_mocked_bindings(run_builtin = function(rule, context) {
      if (rule$id == "workbook/sheets") {
        if (problem == "incomplete") validation_incomplete("Structure unavailable")
        stop("Structure failed")
      }
      original(rule, context)
    }, .package = "model.validation")
    result <- validate_data(validation_plan(fixture_model(), list(callback)), fixture_data())
    expect_identical(result$status, problem)
    expect_identical(calls, 0L)
    expect_identical(result$execution$status[result$execution$rule_id == "sheet/Observations/layout"], "completed")
    expect_true(all(result$execution$status[grepl("^(field/|business/)", result$execution$rule_id)] == "skipped"))
  }
})

test_that("A1 cell locations retain physical finding coordinates", {
  d <- fixture_definition()
  d$sheets$Cover$layout <- list(kind = "cells", row_count = 2L, column_count = 27L)
  d$sheets$Cover$fields$title$location <- "AA2"
  data <- fixture_data()
  data$Cover <- matrix(vector("list", 54), 2, 27)
  data$Cover[[2, 27]] <- 42
  result <- validate_fixture(data, model = model.form::as_form_model(d))
  expect_identical(result$findings$rule_id, "field/Cover.title/type")
  expect_identical(result$findings$row, 2L)
  expect_identical(result$findings$column, 27L)
  expect_identical(result$findings$value[[1]], 42)
})
