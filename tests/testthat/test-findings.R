test_that("batch findings preserve the scalar API's values, coordinates and empty schema", {
  values <- list(NULL, NA_real_, as.Date("2024-02-29"), c("A", "B"))
  expected <- list(reference = "codes", matches = 1L)
  scalar <- bind_findings(lapply(seq_along(values), function(i) {
    validation_finding("Review", "Records", "code", i + 5L, 2L, values[[i]], expected)
  }))
  batch <- validation_findings("Review", row = 6:9, sheet = "Records", field = "code",
    column = 2, value = values, expected = list(expected))
  expect_identical(batch, scalar)
  expect_identical(check_findings(batch), scalar)
  class(batch) <- c("custom_findings", "data.frame")
  expect_identical(check_findings(batch), scalar)
  expect_identical(validation_findings("Review", integer()), scalar[FALSE, ])
})

test_that("column validation still rejects malformed callback findings", {
  good <- validation_findings(c("First", "Second"), row = c(2, 4))
  bad <- good; bad$message[[2]] <- NA_character_
  expect_error(check_findings(bad), "message")
  bad <- good; bad$row[[2]] <- 0
  expect_error(check_findings(bad), "positive integers")
  bad <- good; bad$field[[2]] <- ""
  expect_error(check_findings(bad), "field")
  expect_error(validation_findings("Review", 1:3, value = list(1, 2)), "length")
  expect_error(validation_findings("Review", 1, value = 1), "lists")
})

test_that("column extraction preserves mixed cells, missing values and physical rows", {
  plan <- validation_plan(field_model("string", mandatory = TRUE))
  values <- list("A", NA_character_, "", 5, list("X", "Y"), NULL)
  frame <- data.frame(value = I(values))
  matrix <- matrix(values, ncol = 1)
  a <- validate_data(plan, list(Values = frame), config = list(blank_strings = TRUE))
  b <- validate_data(plan, list(Values = matrix), config = list(blank_strings = TRUE))
  expect_identical(a, b)
  expect_identical(declared_findings(a)$row, c(2L, 3L, 6L, 4L, 5L))
  expect_identical(declared_findings(a)$value[[5]], list("X", "Y"))
  empty <- validate_data(plan, list(Values = matrix(list(), nrow = 0, ncol = 1)))
  expect_true(empty$valid)
  expect_equal(nrow(declared_findings(empty)), 0)
})
