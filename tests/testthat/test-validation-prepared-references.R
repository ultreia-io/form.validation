test_that("prepared references preserve findings and respect changed snapshots", {
  refs <- fixture_references()
  refs[["code_lists/sites/code"]]$aliases <- c(old = "north")
  attr(refs, "provenance") <- "snapshot-sha256"
  data <- fixture_data()
  data$Observations$site <- c("old", "unknown")
  plan <- create_validation_plan(fixture_model())
  expected <- validate_data(plan, data, refs)
  prepared <- prepare_validation_reference_indexes(refs)
  expect_identical(prepared[["code_lists/sites/code"]], refs[["code_lists/sites/code"]])
  expect_identical(attr(prepared, "provenance"), "snapshot-sha256")
  expect_identical(validate_data(plan, data, prepared), expected)
  prepared[["code_lists/sites/code"]]$values <- c("north", "south", "unknown")
  expect_true(validate_data(plan, data, prepared)$valid)
  expect_equal(nrow(declared_findings(validate_data(plan, data, prepared))), 0L)
})

test_that("prepared missing and malformed snapshots retain deferred execution semantics", {
  plan <- create_validation_plan(fixture_model())
  blank <- fixture_data()
  blank$Observations$site <- c(NA_character_, NA_character_)
  gated <- fixture_data()
  gated$Cover <- NULL
  for (refs in list(list(), list("code_lists/sites/code" = list(values = 42)),
                   list("code_lists/sites/code" = list(values = "north", aliases = c(old = "absent"))))) {
    prepared <- prepare_validation_reference_indexes(refs, "sites")
    for (data in list(fixture_data(), blank, gated)) {
      expect_identical(validate_data(plan, data, prepared), validate_data(plan, data, refs))
    }
    prepared[["code_lists/sites/code"]] <- fixture_references()[["code_lists/sites/code"]]
    expect_true(validate_data(plan, fixture_data(), prepared)$valid)
  }
  refs <- list(sites = fixture_references()[["code_lists/sites/code"]], registry = list(values = 42, indexes = list()))
  expect_identical(prepare_validation_reference_indexes(refs, "sites")$registry, refs$registry)
  expect_error(prepare_validation_reference_indexes(refs, NA_character_), "nonempty reference names")
})

test_that("prepared text indexes are reused for reference matching", {
  references <- fixture_references()
  references[["code_lists/sites/code"]]$indexes <- list(
    form.references::reference_index(c("north", "south")))
  expect_true(validate_fixture(references = references)$valid)
  data <- fixture_data()
  data$Observations$site <- c("NORTH", "unknown")
  result <- validate_fixture(data, references)
  expect_equal(nrow(declared_findings(result)), 1L)
})
