test_that("explicit conversion preserves values outside the declared policy", {
  model <- field_model("string")
  values <- list(FALSE, 42, "001", " spaced ", "", NA_character_, NULL)
  data <- list(Values = data.frame(value = I(values)))
  result <- prepare_validation_data(data, model)
  expect_identical(unclass(result$Values$value), list("FALSE", "42", "001", " spaced ", "", NA_character_, NULL))
  expect_identical(data$Values$value[[1]], FALSE)
  expect_identical(prepare_validation_data(list(Values = matrix(FALSE, 1, 1)), model)$Values[[1]][[1]], "FALSE")
  expect_identical(dim(result$Values), dim(data$Values))
  expect_error(prepare_validation_data(list(Values = 1), model), "data frame or matrix")
})

test_that("date conversion retains time components and skips preambles", {
  definition <- form.model::form_definition(field_model("date", mandatory = FALSE))
  definition$sheets$Values$layout$start_row <- 2L
  model <- form.model::as_form_model(definition)
  midnight <- as.POSIXct("2024-01-01", tz = "UTC")
  values <- list(midnight, midnight, midnight + 3600, NA_real_, "2024-01-02")
  data <- list(Values = data.frame(value = I(values)))
  result <- prepare_validation_data(data, model)$Values$value
  expect_identical(result[[1]], midnight)
  expect_identical(result[[2]], as.Date("2024-01-01"))
  expect_identical(result[3:5], data$Values$value[3:5])
  plan <- validation_plan(model)
  expect_identical(validate_data(plan, data)$findings$row, c(2L, 3L))
  prepared <- validate_data(plan, data, prepare_data = function(data) prepare_validation_data(data, model))
  expect_identical(prepared$findings$row, 3L)
})
