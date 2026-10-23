test_that("callbacks cannot change prepared values or reuse values from another input", {
  seen <- list()
  rule <- validation_rule("business/cached", function(model, data, references, config) {
    field <- config$field_values("Observations.amount")
    seen[[length(seen) + 1L]] <<- field
    field$original[[1L]] <- 999
    expect_identical(config$field_values("Observations.amount")$original[[1L]], data$Observations$amount[[1L]])
    NULL
  })
  data <- fixture_data()
  expect_true(validate_fixture(data, rules = list(rule))$valid)
  expect_identical(seen[[1L]]$rows, 1:2)
  expect_identical(seen[[1L]]$typed, c(TRUE, FALSE))
  expect_identical(seen[[1L]]$eligible, 1L)
  expect_identical(seen[[1L]]$values, 2)
  data$Observations$amount[[1L]] <- 3
  expect_true(validate_fixture(data, rules = list(rule))$valid)
  expect_identical(seen[[2L]]$original[[1L]], 3)
})

test_that("callback masks preserve blanks, invalid values and physical coordinates", {
  model <- field_model("string", mandatory = FALSE)
  data <- list(Values = matrix(list("", NA_character_, FALSE, "ok"), ncol = 1))
  seen <- NULL
  rule <- validation_rule("business/masks", function(model, data, references, config) {
    seen <<- config$field_values("Values.value")
    NULL
  })
  result <- validate_data(validation_plan(model, list(rule)), data, config = list(blank_strings = TRUE))
  expect_identical(seen$original, as.list(data$Values[, 1L]))
  expect_identical(seen$filled, c(FALSE, FALSE, TRUE, TRUE))
  expect_identical(seen$typed, c(FALSE, FALSE, FALSE, TRUE))
  expect_identical(seen$eligible, 4L)
  expect_identical(seen$rows, 1:4)
  expect_identical(declared_findings(result)$row, 3L)
  unknown <- validation_rule("business/unknown", function(model, data, references, config) {
    config$field_values("Values.absent")
  })
  result <- validate_data(validation_plan(model, list(unknown)), data)
  expect_identical(tail(result$execution$status, 1), "failed")
  expect_match(tail(result$execution$message, 1), "Unknown field")
})
