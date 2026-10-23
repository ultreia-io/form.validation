test_that("report grouping matches single-input summaries without losing details", {
  data <- fixture_data()
  data$Observations$amount <- c(-1, -1)
  data$Observations$unit <- c("cm", "cm")
  data$Cover[1, 1] <- NA_character_
  result <- validate_fixture(data)
  report <- validation_report_data(result, fixture_model())
  expect_identical(report$summary$errors, 3L)
  expect_identical(vapply(report$sheets, `[[`, character(1), "name"), c("Cover", "Observations"))
  expect_identical(names(report$sheets[[1]]$summary), c("severity", "stage", "meta_name", "meta_value", "message"))
  expect_equal(nrow(report$sheets[[1]]$details), 1L)
  expect_identical(report$sheets[[1]]$details$stage, "Field")
  expect_identical(report$sheets[[2]]$summary$count, c(1L, 2L))
  expect_identical(report$sheets[[2]]$details$row, c(NA_integer_, 1L, 2L))
  expect_identical(report$sheets[[2]]$summary$column_name, c(NA_character_, "amount"))
  expect_identical(report$sheets[[2]]$summary$value, c("2", "-1"))
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE))
  paths <- write_validation_results(result, directory)
  expect_identical(utils::read.csv(paths[["summary"]])$errors, 3L)
  expect_equal(nrow(utils::read.csv(paths[["findings"]])), 4)
  expect_error(validation_report_data(result, upstream_model()), "differs")
})

test_that("reports retain the structure gate and incomplete or failed execution", {
  blocked <- validate_fixture(data = list())
  report <- validation_report_data(blocked, fixture_model())
  expect_false(report$structure_passed)
  expect_equal(nrow(report$execution), 0L)
  missing <- validation_report_data(validate_fixture(references = list()), fixture_model())
  expect_identical(missing$summary$status, "incomplete")
  expect_identical(missing$summary$errors, 0L)
  expect_identical(missing$summary$incomplete, 1L)
  broken <- validation_rule("test/failure", function(...) stop("Cannot finish"))
  whole <- validation_rule("test/workbook", function(...) validation_finding("Review workbook"), "info")
  failed <- validation_report_data(validate_fixture(rules = list(broken, whole)), fixture_model())
  expect_identical(failed$sheets[[2]]$name, "Form")
  expect_identical(failed$summary$status, "failed")
  expect_true("Cannot finish" %in% failed$execution$message)
  valid <- validation_report_data(validate_fixture(), fixture_model())
  expect_length(valid$sheets, 1)
  expect_identical(valid$summary$status, "valid")
})

test_that("HTML reports link local CSV and assets and escape page metadata", {
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE))
  rule <- validation_rule("review/text", function(...) validation_finding(
    "<script>alert(1)</script>", "Observations", "Observations.id", 1, 1, "001"), "warning")
  report <- validation_report_data(validate_fixture(rules = list(rule)), fixture_model())
  path <- file.path(directory, "report.html")
  write_validation_report(report, path, title = "<Title & review>", author = 'A "quoted" author')
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "&lt;Title &amp; review&gt;", fixed = TRUE)
  expect_match(html, "A &quot;quoted&quot; author", fixed = TRUE)
  links <- regmatches(html, gregexpr('(?:href|src)="[^"]+"', html, perl = TRUE))[[1]]
  targets <- sub('^[^=]+="(.*)"$', "\\1", links)
  expect_true(all(c("summary.csv", "findings.csv") %in% targets))
  expect_true(any(grepl("\\.js$", targets)))
  expect_true(any(grepl("\\.css$", targets)))
  expect_true(all(file.exists(file.path(directory, targets))))
  details <- utils::read.csv(file.path(directory, "findings.csv"), colClasses = "character")
  details <- details[details$rule_id == "review/text", ]
  expect_identical(details$message, "<script>alert(1)</script>")
  expect_identical(details$value, "001")
  expect_error(write_validation_report(report, path), "already exists")
})

test_that("summary grouping retains stage and messages include fixed-cell defects", {
  finding <- function(...) validation_finding("Review value", "Observations", "Observations.id", 1, 1, "S01")
  result <- validate_fixture(rules = list(
    validation_rule("review/structure", finding, "warning", stage = "structure"),
    validation_rule("review/business", finding, "warning")))
  before <- result
  report <- validation_report_data(result, fixture_model())
  expect_identical(report$sheets[[1]]$summary$stage, c("Structure", "Field", "Business"))
  expect_identical(report$sheets[[1]]$summary$count, c(1L, 1L, 1L))
  expect_identical(names(report$sheets[[1]]$details)[1:2], c("severity", "stage"))
  expect_identical(result, before)
  data <- fixture_data(); data$Cover <- matrix("x", 2, 1)
  structural <- validation_report_data(validate_fixture(data), fixture_model())
  expect_identical(structural$sheets[[1]]$details$stage, "Structure")
  expect_match(structural$sheets[[1]]$details$message, "row")
})

test_that("individual and aggregate reports retain informational counts and cards", {
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE))
  report <- validation_report_data(validate_fixture(), fixture_model())
  expect_identical(report$summary$infos, 1L)
  expect_identical(report$sheets[[1]]$infos, 1L)
  single <- write_validation_report(report, file.path(directory, "form", "index.html"))
  write_validation_aggregate(data.frame(directory = "form", report = "index.html"),
    directory, group_columns = character())
  totals <- jsonlite::read_json(file.path(directory, "aggregate.json"), simplifyVector = TRUE)$totals
  expect_identical(totals$infos, 1L)
  expect_identical(totals$errors, 0L)
  for (path in c(single, file.path(directory, "index.html"))) {
    html <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_match(html, "Information findings", fixed = TRUE)
    expect_match(html, 'data-(count|total)="infos"')
  }
})
