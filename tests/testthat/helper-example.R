resolved_fixture_model <- function(definition) {
  model <- form.model::as_form_model(definition)
  configuration <- form.references::read_reference_configuration(
    system.file("examples", "survey", "references.yaml", package = "form.references"))
  configuration$catalogues$code_lists$tables$sites <- list(code = list(matching = "exact", ignore_case = TRUE))
  form.references::resolve_reference_lookups(model, configuration)
}

# Synthetic unit fixture; the installed example is owned exclusively by form.model.
fixture_model <- function() {
  resolved_fixture_model(list(schema_version = "1.0.0", form = list(id = "fixture", version = "1"),
    references = list(code_lists = list("sites")), sheets = list(
      Cover = list(layout = list(kind = "cells", row_count = 1L, column_count = 1L),
        fields = list(title = list(mandatory = TRUE, location = "A1", type = "string"))),
      Observations = list(layout = list(kind = "table", column_count = 4L, start_row = 1L),
        fields = list(
          id = list(mandatory = TRUE, type = "string", pattern = "^S[0-9]{2}$", unique = TRUE),
          amount = list(type = "number", minimum = 0, maximum = 100),
          unit = list(type = "string", required_field = "amount", enum = list("cm", "m")),
          site = list(mandatory = TRUE, type = "string", reference = list(catalogue = "code_lists", table = "sites", lookup = "code", severity = "warning")))))))
}
fixture_data <- function() {
  list(Cover = matrix("Example", 1, 1),
       Observations = data.frame(id = c("S01", "S02"), amount = c(2, NA_real_),
                                 unit = c("cm", NA_character_), site = c("north", "south")))
}
fixture_references <- function() list("code_lists/sites/code" = list(values = c("north", "south")))
fixture_definition <- function() form.model::form_definition(fixture_model())
field_model <- function(type, constraints = NULL, mandatory = TRUE) {
  d <- list(schema_version = "1.0.0", form = list(id = "values", version = "1"),
            references = list(), sheets = list(Values = list(
              fields = list(value = c(list(mandatory = mandatory, type = type), constraints)),
              layout = list(kind = "table", column_count = 1L, start_row = 1L))))
  resolved_fixture_model(d)
}
validate_fixture <- function(data = fixture_data(), references = fixture_references(),
                             model = fixture_model(), rules = list(), config = list()) {
  validate_data(create_validation_plan(model, rules), data, references, source = "submission.xlsx", config = config)
}

upstream_model <- function() form.references::resolve_reference_lookups(
  form.model::read_form_model("examples/survey/form.yaml", "form.model"),
  form.references::read_reference_configuration(system.file("examples", "survey", "references.yaml", package = "form.references")))
upstream_references <- function() list(
  "code_lists/observers/code" = list(values = "ALICE"),
  "code_lists/stations/code" = list(values = c("001", "002")),
  "registries/contacts/email" = list(values = "alice@example.org")
)
upstream_data <- function() {
  data <- list(Metadata = matrix(vector("list", 15), 5, 3),
               Observations = matrix(vector("list", 77), 7, 11),
               Samples = matrix(vector("list", 30), 5, 6),
               Measurements = matrix(vector("list", 25), 5, 5),
               Equipment = matrix(vector("list", 15), 5, 3),
               Notes = matrix(vector("list", 25), 5, 5))
  data$Metadata[[2, 3]] <- "001"
  data$Metadata[[3, 3]] <- "2026-10-03"
  data$Observations[5, ] <- as.list(c("STATION", "COUNT", "LENGTH", "UNIT", "ACTIVE", "OBSERVED_AT", "LATITUDE", "LONGITUDE", "OBSERVATION_ID", "QUALITY", "OBSERVER"))
  data$Observations[6, ] <- list("001", 2L, NULL, NULL, TRUE, "2026-10-03T12:00:00Z", -20.5, 55.5, "OBS-001", 1L, "ALICE")
  data$Observations[7, ] <- list("002", 3L, NULL, NULL, FALSE, "2026-10-03T13:00:00Z", -21, 56, "OBS-002", 2L, "ALICE")
  data
}
upstream_checks <- function() {
  callbacks <- new.env(parent = baseenv())
  sys.source(system.file("examples/survey/business.R", package = "form.validation"), callbacks)
  list(total_count = callbacks$total_count)
}

# Keep field/custom-rule assertions independent of automatic table observations.
declared_findings <- function(result) {
  rows <- result$findings
  rows[!grepl("^sheet/.+/(row_count|empty_rows)$", rows$rule_id), , drop = FALSE]
}
