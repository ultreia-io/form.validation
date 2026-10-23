test_that("table observations count loaded rows and warn only about internal blanks", {
  definition <- form.model::form_definition(field_model("string", mandatory = FALSE))
  definition$sheets$Values$layout$start_row <- 2L
  model <- form.model::as_form_model(definition)
  plan <- validation_plan(model)
  data <- list(Values = matrix(c("Header", NA, "A", "", "B", NA), ncol = 1))
  result <- validate_data(plan, data, prepare_data = function(data) {
    data$Values[4, 1] <- "filled during preparation"
    data
  })
  expect_true(result$valid)
  expect_identical(result$findings$severity, c("info", "warning"))
  expect_identical(result$findings$value[[1]], 5L)
  expect_identical(result$findings$row, c(NA_integer_, 4L))
  frame <- validate_data(plan, list(Values = data.frame(value = data$Values[, 1])))
  expect_identical(frame$findings, result$findings)
  empty <- validate_data(plan, list(Values = matrix("Header", ncol = 1)))
  expect_true(empty$valid)
  expect_identical(empty$findings$value[[1]], 0L)
  expect_identical(empty$findings$severity, "info")
  blocked <- validate_data(plan, list())
  expect_false(any(blocked$findings$severity == "info"))
})
