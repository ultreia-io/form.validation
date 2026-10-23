read_output <- function(path) utils::read.csv(path, colClasses = "character", check.names = FALSE,
                                                   na.strings = NULL, fileEncoding = "UTF-8")

test_that("a valid input writes a summary and informational findings", {
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE))
  result <- validate_fixture()
  paths <- write_validation_results(result, directory)
  summary <- read_output(paths[["summary"]])
  details <- read_output(paths[["findings"]])
  expect_identical(summary$input, "submission.xlsx")
  expect_identical(summary$status, "valid")
  expect_identical(summary$rules_version, "")
  expect_identical(summary$errors, "0")
  expect_identical(summary$infos, "1")
  expect_identical(details$severity, "info")
  expect_identical(details$value, "2")
  expect_identical(names(details), c("rule_id", "kind", "severity", "sheet", "field",
                                     "row", "column", "value", "expected", "message",
                                     "stage", "sheet_order", "layout", "field_label"))
  expect_error(write_validation_results(result, directory), "already exists")
})

test_that("CSV preserves values and combines validation and execution problems", {
  directory <- tempfile()
  rule_files <- rule_paths()
  on.exit({ unlink(directory, recursive = TRUE); unlink(dirname(rule_files[[1L]]), recursive = TRUE) })
  plan <- read_validation_rules(rule_files, fixture_model())
  plan$rules <- c(plan$rules, list(
    validation_rule("business/text", function(...) validation_finding('Review "é", please\nnext line',
      value = "001", expected = list("a,b", "m", NULL)), severity = "info"),
    validation_rule("business/broken", function(...) stop("Calculation failed"))
  ))
  data <- fixture_data()
  data$Observations$amount[[1]] <- -1
  result <- validate_data(plan, data, references = list(), source = "submission.xlsx")
  paths <- write_validation_results(result, directory)
  summary <- read_output(paths[["summary"]])
  details <- read_output(paths[["findings"]])
  expect_identical(summary$status, "failed")
  expect_identical(summary$rules_version, "1")
  expect_identical(summary$errors, "1")
  expect_identical(summary$incomplete, "1")
  expect_identical(summary$failed, "1")
  details <- details[details$rule_id != "sheet/Observations/row_count", ]
  expect_identical(details$kind, c("validation", "validation", "incomplete", "failed"))
  expect_identical(details$value[[2]], "001")
  expect_identical(details$message[[2]], 'Review "é", please\nnext line')
  expect_identical(yaml::yaml.load(details$expected[[2]], handlers = list(seq = identity)), list("a,b", "m", NULL))
  expect_identical(details$sheet[[3]], "Observations")
  expect_identical(details$field[[3]], "Observations.site")
  expect_identical(details$row[[3]], "")
  expect_identical(details$message[[4]], "Calculation failed")
  expect_identical(details$sheet[[4]], "")
})

test_that("both destinations are checked before overwriting either CSV", {
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  writeLines("keep", file.path(directory, "findings.csv"))
  expect_error(write_validation_results(validate_fixture(), directory), "already exists")
  expect_false(file.exists(file.path(directory, "summary.csv")))
  expect_identical(readLines(file.path(directory, "findings.csv")), "keep")
  write_validation_results(validate_fixture(), directory, overwrite = TRUE)
  expect_identical(read_output(file.path(directory, "summary.csv"))$status, "valid")
})


test_that("prepared exports retain findings and add report metadata", {
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE))
  rule <- validation_rule("review/value", function(...) validation_finding(
    'Review "é", please\nnext line', value = as.Date("2024-02-29"),
    expected = list("001", "a,b", NULL)), severity = "info")
  result <- validate_fixture(references = list(), rules = list(rule))
  original <- write_validation_results(result, file.path(directory, "original"))
  report <- validation_report_data(result, fixture_model())
  shared <- write_validation_results(report, file.path(directory, "shared"))
  raw <- read_output(original[["findings"]])
  prepared <- read_output(shared[["findings"]])
  metadata <- c("sheet_order", "layout", "field_label")
  expect_identical(prepared[setdiff(names(prepared), metadata)], raw[setdiff(names(raw), metadata)])
  expect_identical(prepared$sheet_order, c("2", "", "2"))
  expect_identical(prepared$layout, c("table", "table", "table"))
  expect_identical(prepared$field_label, c("", "", "site"))
  summary <- read_output(shared[["summary"]])
  expect_identical(summary$structure_passed, "TRUE")
  expect_identical(summary$generated_at, report$generated_at)
  repeated <- write_validation_results(report, file.path(directory, "repeated"))
  expect_identical(unname(tools::md5sum(shared)), unname(tools::md5sum(repeated)))
})
