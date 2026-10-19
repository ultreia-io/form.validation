test_that("prepared references preserve findings and respect changed snapshots", {
  refs <- fixture_references()
  refs$sites$aliases <- c(old = "north")
  attr(refs, "provenance") <- "snapshot-sha256"
  data <- fixture_data()
  data$Observations$site <- c("old", "unknown")
  plan <- validation_plan(fixture_model())
  expected <- validate_data(plan, data, refs)
  prepared <- prepare_validation_references(refs)
  expect_identical(prepared$sites, refs$sites)
  expect_identical(attr(prepared, "provenance"), "snapshot-sha256")
  expect_identical(validate_data(plan, data, prepared), expected)
  prepared$sites$values <- c("north", "south", "unknown")
  expect_true(validate_data(plan, data, prepared)$valid)
  expect_equal(nrow(validate_data(plan, data, prepared)$findings), 0L)
})

test_that("prepared missing and malformed snapshots retain deferred execution semantics", {
  plan <- validation_plan(fixture_model())
  blank <- fixture_data()
  blank$Observations$site <- c(NA_character_, NA_character_)
  gated <- fixture_data()
  gated$Cover <- NULL
  for (refs in list(list(), list(sites = list(values = 42)),
                   list(sites = list(values = "north", aliases = c(old = "absent"))))) {
    prepared <- prepare_validation_references(refs, "sites")
    for (data in list(fixture_data(), blank, gated)) {
      expect_identical(validate_data(plan, data, prepared), validate_data(plan, data, refs))
    }
    prepared$sites <- fixture_references()$sites
    expect_true(validate_data(plan, fixture_data(), prepared)$valid)
  }
  refs <- list(sites = fixture_references()$sites, registry = list(values = 42, indexes = list()))
  expect_identical(prepare_validation_references(refs, "sites")$registry, refs$registry)
  expect_error(prepare_validation_references(refs, NA_character_), "nonempty reference names")
})
