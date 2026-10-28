# Build a small table with two grouping components and a numbered text field.
# @param type Declared sequence field type.
# @param start First expected number in each group.
# @return A checked synthetic form model.
sequence_model <- function(type = "string", start = 1L) {
  form.model::as_form_model(list(schema_version = "1.0.0",
    form = list(id = "sequence", version = "1"), references = list(),
    sheets = list(Items = list(layout = list(kind = "table", start_row = 1L),
      fields = list(batch = list(mandatory = TRUE, type = "string"),
        group = list(mandatory = TRUE, type = "string"),
        item = c(list(mandatory = TRUE, type = type),
          if (type == "string") list(pattern = "^[0-9]{4}$"),
          list(sequence = list(start = start, group_by = list("batch", "group")))))))))
}

# Select the sequence findings while retaining other findings in the result.
# @param result Completed validation result.
# @return Sequence finding rows with physical positions and expected numbers.
sequence_findings <- function(result) {
  result$findings[result$findings$rule_id == "field/Items.item/sequence", , drop = FALSE]
}

test_that("sequence follows worksheet order and rejects gaps and reversed values", {
  plan <- validation_plan(sequence_model())
  for (values in list(c("0002", "0001"), c("0001", "0003"))) {
    result <- validate_data(plan, list(Items = data.frame(batch = "A", group = "B", item = values)))
    rows <- sequence_findings(result)
    expect_true(result$complete)
    expect_equal(rows$row, if (values[1] == "0002") c(1L, 2L) else 2L)
    expect_equal(unlist(rows$expected), rows$row)
    expect_identical(rows$value, I(as.list(values[rows$row])))
    expect_true(all(rows$column == 3L))
  }
})

test_that("interleaved composite groups keep independent numbering without collisions", {
  data <- data.frame(batch = c("A|B", "A", "A|B", "A"),
    group = c("C", "B|C", "C", "B|C"), item = c("0001", "0001", "0002", "0002"))
  plan <- validation_plan(sequence_model())
  result <- validate_data(plan, list(Items = data))
  expect_true(result$valid)
  expect_equal(nrow(sequence_findings(result)), 0L)
  data$item[4] <- "0003"
  rows <- sequence_findings(validate_data(plan, list(Items = data)))
  expect_identical(rows$row, 4L)
  expect_equal(rows$expected[[1]], 2)
})

test_that("invalid sequence values keep their row positions without cascading findings", {
  data <- data.frame(batch = c(rep("A", 5), NA, "A"), group = "B",
    item = I(list("0001", "bad", NA_character_, 4L, "0005", "9999", "0006")))
  result <- validate_data(validation_plan(sequence_model()), list(Items = data))
  expect_true(result$complete)
  expect_equal(nrow(sequence_findings(result)), 0L)
  expect_true(all(c("field/Items.item/pattern", "field/Items.item/mandatory",
    "field/Items.item/type", "field/Items.batch/mandatory") %in% result$findings$rule_id))
})

test_that("integer sequences support another start and empty tables", {
  plan <- validation_plan(sequence_model("integer", 5L))
  data <- data.frame(batch = "A", group = "B", item = c(5L, 6L, 8L))
  result <- validate_data(plan, list(Items = data))
  expect_true(result$complete)
  rows <- sequence_findings(result)
  expect_identical(rows$row, 3L)
  expect_equal(rows$expected[[1]], 7)
  result <- validate_data(plan, list(Items = data[FALSE, ]))
  expect_true(result$complete)
  expect_equal(nrow(sequence_findings(result)), 0L)
})
