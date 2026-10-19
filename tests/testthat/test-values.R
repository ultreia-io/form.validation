test_that("field constraints report relevant values without cascading type errors", {
  data <- fixture_data()
  data$Observations$id <- c("bad", "bad")
  data$Observations$amount <- c(-1, 101)
  data$Observations$unit <- c("km", NA_character_)
  result <- validate_fixture(data)
  expect_identical(result$status, "invalid")
  expect_true(result$complete)
  expect_true(all(c("field/Observations.id/pattern", "field/Observations.id/unique",
                    "field/Observations.amount/minimum", "field/Observations.amount/maximum",
                    "field/Observations.unit/enum", "field/Observations.unit/required_field") %in% result$findings$rule_id))
  duplicate <- result$findings[result$findings$rule_id == "field/Observations.id/unique", ]
  expect_identical(duplicate$row, 2L)
  expect_identical(duplicate$value[[1]], "bad")
  data <- fixture_data()
  data$Observations$amount <- c("wrong", "2")
  ids <- validate_fixture(data)$findings$rule_id
  expect_equal(sum(ids == "field/Observations.amount/type"), 2L)
  expect_false(any(ids %in% c("field/Observations.amount/minimum", "field/Observations.amount/maximum")))
})

test_that("integer, Boolean, date and datetime types have explicit representations", {
  check <- function(type, values, constraints = NULL) {
    validate_data(validation_plan(field_model(type, constraints)),
                   list(Values = data.frame(value = I(values))))
  }
  integer <- check("integer", list(1L, 2, 2.5, NaN, Inf))
  expect_identical(integer$findings$row, c(3L, 4L, 5L))
  expect_true(all(integer$findings$rule_id == "field/Values.value/type"))
  boolean <- check("boolean", list(TRUE, FALSE, "true"))
  expect_identical(boolean$findings$row, 3L)
  date <- check("date", list("2026-10-02", as.Date("2026-10-02"), "2026-02-30"),
                list(enum = list("2026-10-02")))
  expect_identical(date$findings$row, 3L)
  expect_identical(date$findings$rule_id, "field/Values.value/type")
  datetime <- check("datetime", list("2026-10-02T12:00:00Z", as.POSIXct("2026-10-02 12:00:00", tz = "UTC"),
                                     "2026-10-02 12:00:00", "2026-02-30T12:00:00Z"))
  expect_identical(datetime$findings$row, c(3L, 4L))
  expect_identical(check("string", list(factor("x")))$findings$rule_id, "field/Values.value/type")
})

test_that("enum membership is typed and empty optional values are ignored", {
  model <- field_model("integer", list(enum = list(1, 2)), mandatory = FALSE)
  result <- validate_data(validation_plan(model), list(Values = data.frame(value = c(1, 3, NA))))
  expect_identical(result$findings$row, 2L)
  expect_identical(result$findings$rule_id, "field/Values.value/enum")
})

test_that("required_field depends on the companion value in the same row", {
  data <- fixture_data()
  data$Observations$unit <- c(NA_character_, NA_character_)
  data$Observations$amount <- c(0, NA_real_)
  result <- validate_fixture(data)
  expect_identical(result$findings$rule_id, "field/Observations.unit/required_field")
  expect_identical(result$findings$row, 1L)
  expect_identical(result$findings$expected[[1L]], "amount")
  data$Observations$unit <- c("cm", "m")
  expect_true(validate_fixture(data)$valid)
})

test_that("vector uniqueness preserves mixed temporal representations and original coordinates", {
  model <- field_model("date", list(unique = TRUE), mandatory = FALSE)
  data <- list(Values = data.frame(value = I(list("2026-10-02", as.Date("2026-10-02"),
                                                NA, "bad", "2026-10-03"))))
  result <- validate_data(validation_plan(model), data)
  duplicate <- result$findings[result$findings$rule_id == "field/Values.value/unique", ]
  expect_identical(duplicate$row, 2L)
  expect_identical(duplicate$value[[1]], as.Date("2026-10-02"))
})

test_that("mandatory, type and reference precede remaining constraints regardless of mapping order", {
  d <- fixture_definition()
  site <- d$sheets$Observations$fields$site
  site$mandatory <- TRUE
  site <- c(list(pattern = "^[a-z]+$"), site)
  d$sheets$Observations$fields$site <- site
  plan <- validation_plan(model.form::as_form_model(d))
  rules <- Filter(function(rule) identical(rule$field, "Observations.site"), plan$rules)
  expect_identical(vapply(rules, `[[`, character(1), "kind"),
                   c("mandatory", "type", "reference", "pattern"))
  data <- fixture_data()
  data$Observations$site <- list(NA_character_, 42)
  result <- validate_data(plan, data)
  expect_true(result$complete)
  expect_identical(result$findings$rule_id,
                   c("field/Observations.site/mandatory", "field/Observations.site/type"))
  conditional <- Filter(function(rule) identical(rule$field, "Observations.unit"), plan$rules)
  expect_identical(vapply(conditional, `[[`, character(1), "kind"),
                   c("required_field", "type", "enum"))
})

test_that("numeric uniqueness uses value equality without coercing invalid or null cells", {
  data <- list(Values = data.frame(value = I(list(1L, 1, 0, -0, NA, "1", NaN))))
  result <- validate_data(validation_plan(field_model("number", list(unique = TRUE), FALSE)), data)
  expect_identical(result$findings$row[result$findings$rule_id == "field/Values.value/unique"], c(2L, 4L))
})

test_that("compact references default to errors and preserve explicit warnings", {
  d <- fixture_definition()
  d$sheets$Observations$fields$site <- list(mandatory = TRUE, type = "string", reference = "sites")
  data <- fixture_data()
  data$Observations$site <- c(NA_character_, "unknown")
  result <- validate_fixture(data, model = model.form::as_form_model(d))
  expect_identical(result$findings$rule_id,
                   c("field/Observations.site/mandatory", "field/Observations.site/reference"))
  expect_identical(result$findings$severity, c("error", "error"))
  d$sheets$Observations$fields$site$reference <- list(id = "sites", severity = "warning")
  result <- validate_fixture(data, model = model.form::as_form_model(d))
  expect_identical(result$findings$severity, c("error", "warning"))
})
