test_that("registry keys retain scalar types and temporal equivalence", {
  expect_false(identical(registry_key("001"), registry_key(1L)))
  expect_identical(registry_key(1L), registry_key(1))
  expect_identical(registry_key(as.Date("2024-02-29")), registry_key("2024-02-29"))
})
