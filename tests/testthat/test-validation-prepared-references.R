test_that("prepared references preserve findings and respect changed snapshots", {
  refs <- fixture_references()
  refs[["code_lists/sites/code"]] <- form.references::reference_index(c("north", "south"))
  attr(refs, "provenance") <- "snapshot-sha256"
  data <- fixture_data()
  data$Observations$site <- c("north", "unknown")
  plan <- create_validation_plan(fixture_model())
  expected <- validate_data(plan, data, refs)
  prepared <- prepare_validation_reference_indexes(refs)
  expect_identical(prepared[["code_lists/sites/code"]], refs[["code_lists/sites/code"]])
  expect_identical(attr(prepared, "provenance"), "snapshot-sha256")
  expect_identical(validate_data(plan, data, prepared), expected)
  prepared[["code_lists/sites/code"]] <- form.references::reference_index(c("north", "south", "unknown"))
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
                   list("code_lists/sites/code" = list(values = list(c("north", "south")))))) {
    prepared <- prepare_validation_reference_indexes(refs, "sites")
    for (data in list(fixture_data(), blank, gated)) {
      expect_identical(validate_data(plan, data, prepared), validate_data(plan, data, refs))
    }
    prepared[["code_lists/sites/code"]] <- fixture_references()[["code_lists/sites/code"]]
    expect_true(validate_data(plan, fixture_data(), prepared)$valid)
  }
  refs <- list(sites = fixture_references()[["code_lists/sites/code"]], registry = list(values = 42))
  expect_identical(prepare_validation_reference_indexes(refs, "sites")$registry, refs$registry)
  expect_error(prepare_validation_reference_indexes(refs, NA_character_), "nonempty reference names")
})

test_that("catalogue-prepared indexes retain aliases under rule case policies", {
  reference_directory <- system.file("examples/survey/catalogue/code_lists", package = "form.references")
  reference_cache <- tempfile("validation-references-")
  on.exit(unlink(reference_cache, recursive = TRUE), add = TRUE)
  reference_tables <- form.references::get_reference_tables("stations",
    form.references::reference_source(reference_directory, reference_cache))
  references <- fixture_references()
  references[["code_lists/sites/code"]] <- form.references::reference_index(
    reference_tables$data$stations, "code", ignore_case = FALSE)
  data <- fixture_data()
  data$Observations$site <- c("old-station", "002")
  expect_true(validate_fixture(data, references)$valid)
  data$Observations$site <- c("old-station", "unknown")
  result <- validate_fixture(data, references)
  expect_equal(nrow(declared_findings(result)), 1L)
})
