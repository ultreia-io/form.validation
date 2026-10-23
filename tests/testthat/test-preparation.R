test_that("preparation runs once after structure and refreshes cached field values", {
  events <- character()
  data <- fixture_data(); data$Cover <- matrix(FALSE, 1, 1)
  inspect <- validation_rule("custom/structure", function(model, data, references, config) {
    events <<- c(events, "structure")
    config$field_values("Cover.title")
    NULL
  }, stage = "structure")
  business <- validation_rule("custom/business", function(model, data, references, config) {
    events <<- c(events, "business")
    expect_identical(data$Cover[[1]], "FALSE")
    NULL
  })
  plan <- validation_plan(fixture_model(), list(inspect, business))
  result <- validate_data(plan, data, fixture_references(), prepare_data = function(data) {
    events <<- c(events, "prepare")
    data$Cover <- matrix(as.character(data$Cover), 1, 1)
    data
  })
  expect_identical(events, c("structure", "prepare", "business"))
  expect_true(result$valid)
  expect_identical(result$execution$rule_id[result$execution$rule_id %in% vapply(plan$rules, `[[`, character(1), "id")], vapply(plan$rules, `[[`, character(1), "id"))
  expect_identical(data$Cover[[1]], FALSE)
})

test_that("every structural defect finishes and blocks preparation and content", {
  for (problem in c("invalid", "incomplete", "failed")) {
    calls <- character()
    checks <- lapply(c("first", "last"), function(id) validation_rule(paste0("structure/", id),
      function(...) {
        calls <<- c(calls, id)
        switch(problem, invalid = validation_finding("Bad structure"),
          incomplete = validation_incomplete("Unavailable"), failed = stop("Failed"))
      }, stage = "structure"))
    checks <- c(checks, list(validation_rule("business/never", function(...) stop("Must not run"))))
    result <- validate_data(validation_plan(fixture_model(), checks), fixture_data(),
      prepare_data = function(data) { calls <<- c(calls, "prepare"); data })
    expect_identical(calls, c("first", "last"))
    expect_identical(result$status, problem)
    expect_true(all(result$execution$status[result$execution$stage != "structure"] == "skipped"))
    expect_false("data/prepare" %in% result$execution$rule_id)
  }
})

test_that("failed preparation preserves structural results and blocks content", {
  preparations <- list(function(data) stop("Conversion failed"),
    function(data) { warning("Conversion warning"); data },
    function(data) data[2:1], function(data) { data$Cover <- matrix(1, 2, 1); data })
  calls <- 0L
  plan <- validation_plan(fixture_model(), list(validation_rule("business/counter",
    function(...) { calls <<- calls + 1L; NULL })))
  for (prepare in preparations) {
    result <- validate_data(plan, fixture_data(), prepare_data = prepare)
    expect_true(result$structure_passed)
    expect_identical(result$status, "failed")
    expect_identical(result$execution$status[result$execution$rule_id == "data/prepare"], "failed")
    expect_true(all(result$execution$status[result$execution$stage != "structure" &
      result$execution$rule_id != "data/prepare" &
      !grepl("^sheet/.+/(row_count|empty_rows)$", result$execution$rule_id)] == "skipped"))
  }
  expect_identical(calls, 0L)
  expect_error(validate_data(plan, fixture_data(), prepare_data = TRUE), "prepare_data")
})
