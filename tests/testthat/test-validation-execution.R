test_that("reference findings and missing execution are distinct", {
  data <- fixture_data()
  data$Observations$site[[1]] <- "unknown"
  warning <- validate_fixture(data)
  expect_true(warning$valid)
  expect_identical(declared_findings(warning)$severity, "warning")
  expect_identical(declared_findings(warning)$expected[[1]], list(reference = "code_lists/sites/code", matches = 1L))
  missing <- validate_fixture(references = list())
  expect_identical(missing$status, "incomplete")
  expect_true(is.na(missing$valid))
  expect_false(missing$complete)
  expect_equal(nrow(declared_findings(missing)), 0L)
  expect_match(missing$execution$message[missing$execution$status == "incomplete"], "Missing reference")
  refs <- fixture_references()
  refs[["code_lists/sites/code"]]$values <- list("north", 42)
  expect_identical(validate_fixture(references = refs)$status, "failed")
  data$Observations$amount[[1]] <- -1
  result <- validate_fixture(data, references = list())
  expect_false(result$valid)
  expect_identical(result$status, "incomplete")
})

test_that("custom rules receive explicit inputs and return portable findings", {
  rule <- create_validation_rule("custom/limit", function(model, data, references, config) {
    stopifnot(form.model::form_metadata(model)$form$id == "fixture")
    stopifnot(identical(references[["code_lists/sites/code"]]$values, c("north", "south")))
    values <- data$Observations$amount
    bad <- which(values > config$custom$limit)
    if (length(bad)) do.call(rbind, lapply(bad, function(row) {
      create_validation_finding("Amount exceeds configured limit", "Observations", "amount", row, 2,
                         values[[row]], config$custom$limit)
    }))
  })
  result <- validate_fixture(rules = list(rule), config = list(custom = list(limit = 1)))
  expect_identical(declared_findings(result)$rule_id, "custom/limit")
  expect_identical(declared_findings(result)$severity, "error")
  expect_identical(declared_findings(result)$row, 1L)
  expect_identical(declared_findings(result)$value[[1]], 2)
  expect_identical(declared_findings(result)$expected[[1]], 1)
  expect_identical(declared_findings(result)$source, "submission.xlsx")
  expect_identical(unserialize(serialize(result, NULL)), result)
})

test_that("rule errors, warnings, and malformed output do not abort other rules", {
  rules <- list(
    create_validation_rule("broken", function(...) stop("Calculation failed")),
    create_validation_rule("warning", function(...) warning("Partial computation")),
    create_validation_rule("missing", function(...) signal_validation_incomplete("No external prerequisite")),
    create_validation_rule("bad-output", function(...) TRUE),
    create_validation_rule("last", function(...) create_validation_finding("Review this submission"), severity = "info")
  )
  result <- validate_fixture(rules = rules)
  statuses <- stats::setNames(result$execution$status, result$execution$rule_id)
  expect_identical(unname(statuses[c("broken", "warning", "missing", "bad-output", "last")]),
                   c("failed", "failed", "incomplete", "failed", "completed"))
  expect_identical(result$status, "failed")
  expect_true(is.na(result$valid))
  expect_false(result$complete)
  expect_identical(declared_findings(result)$rule_id, "last")
  expect_identical(declared_findings(result)$severity, "info")
})

test_that("invalid API inputs fail before execution", {
  plan <- create_validation_plan(fixture_model())
  expect_error(validate_data(plan, list(1)), "unique nonempty names")
  expect_error(validate_data(plan, list(Cover = "wrong")), "data frame or matrix")
  expect_error(validate_data(plan, fixture_data(), config = list(unknown = TRUE)), "Unsupported configuration")
  expect_error(create_validation_finding("bad", row = 0), "positive integer")
  expect_error(create_validation_rule("x", function(...) NULL, severity = "fatal"), "severity")
})
