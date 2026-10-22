test_that("plans expose stable rule identities and preserve declared severity", {
  model <- fixture_model()
  before <- form.model::form_definition(model)
  plan <- validation_plan(model)
  ids <- vapply(plan$rules, function(rule) rule$id, character(1))
  expect_true(all(c("workbook/sheets", "field/Observations.unit/enum",
                    "field/Observations.unit/required_field",
                    "field/Observations.id/unique", "field/Observations.site/reference") %in% ids))
  expect_false(anyDuplicated(ids) > 0L)
  expect_identical(plan$rules[[match("field/Observations.site/reference", ids)]]$severity, "warning")
  expect_identical(form.model::form_definition(model), before)
  expect_identical(ids, vapply(validation_plan(model)$rules, function(rule) rule$id, character(1)))
})

test_that("unsupported declarations and rule collisions fail explicitly", {
  d <- fixture_definition()
  d$sheets[[1]]$fields[[1]]$unique <- TRUE
  expect_error(validation_plan(form.model::as_form_model(d)), "only supported for table")
  d <- fixture_definition()
  d$sheets[[2]]$fields[[2]]$unknown <- TRUE
  model <- fixture_model()
  model$sheets <- d$sheets
  expect_error(validation_plan(model), "unsupported property")
  duplicate <- validation_rule("field/Observations.id/type", function(...) NULL)
  expect_error(validation_plan(fixture_model(), list(duplicate)), "Duplicate rule IDs")
  expect_error(validation_plan(fixture_model(), list(list(id = "fake"))), "validation_rule")
})
