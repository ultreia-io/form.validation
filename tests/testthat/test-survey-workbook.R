test_that('the bundled survey exposes hierarchy and field-rule failures', {
  model <- form.model::load_form_model('survey', system.file('examples', package = 'form.model'))
  path <- form.model::form_resource('examples/survey.xlsx', 'form.model')
  plan <- validation_plan(model)
  references <- list(stations = list(values = c('ST01', 'ST02')),
    observers = list(values = c('observer-1', 'observer-2')),
    'contacts.email' = list(values = 'survey@example.org'))
  baseline <- validate_workbook(path, plan, references, log = function(...) NULL)
  expect_true(baseline$structure_passed)
  expect_true(baseline$complete)
  expect_identical(baseline$status, 'invalid')
  # Blank rows deliberately exercise the mandatory and primary-key findings.
  expect_setequal(baseline$findings$rule_id[baseline$findings$severity == 'error'],
    c('field/Observations.station/mandatory', 'field/Observations.count/mandatory',
      'field/Observations.active/mandatory', 'sheet/Observations/primary_key_present',
      'field/Samples.station/mandatory', 'field/Samples.observation_id/mandatory',
      'field/Samples.sample_no/mandatory', 'sheet/Samples/primary_key_present'))
  expect_setequal(baseline$findings$row[grepl('/empty_rows$', baseline$findings$rule_id)], c(7L, 8L))
  expect_false(any(baseline$execution$status %in% c('failed', 'incomplete')))

  data <- read_validation_workbook(path, function(stage, expression) force(expression))
  data$Samples[[6, 4]] <- 20
  data$Samples[[7, 1]] <- 'MISSING'
  data$Measurements[[9, 4]] <- 3
  data$Equipment[[7, 2]] <- 'SER-100'
  changed <- validate_data(plan, data, references,
    prepare_data = function(grids) prepare_validation_data(grids, model))
  errors <- changed$findings[changed$findings$severity == 'error', ]
  for (rule in c('field/Samples.minimum_length/maximum_field', 'sheet/Samples/parent_key',
                 'field/Measurements.reading_no/sequence', 'field/Equipment.serial/unique')) {
    expect_true(rule %in% errors$rule_id, info = rule)
  }
  expect_equal(errors$row[errors$rule_id == 'field/Samples.minimum_length/maximum_field'], 6L)
  expect_equal(errors$row[errors$rule_id == 'sheet/Samples/parent_key'], 7L)
  expect_equal(errors$row[errors$rule_id == 'field/Measurements.reading_no/sequence'], 9L)
  expect_equal(errors$row[errors$rule_id == 'field/Equipment.serial/unique'], 7L)
})
