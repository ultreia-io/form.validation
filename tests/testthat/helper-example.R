# Synthetic unit fixture; the installed example is owned exclusively by form.model.
fixture_model <- function() {
  form.model::as_form_model(list(schema_version = "1.0.0", form = list(id = "fixture", version = "1"),
    references = list("sites"), sheets = list(
      Cover = list(layout = list(kind = "cells", row_count = 1L, column_count = 1L),
        fields = list(title = list(mandatory = TRUE, location = "A1", type = "string"))),
      Observations = list(layout = list(kind = "table", column_count = 4L, start_row = 1L),
        fields = list(
          id = list(mandatory = TRUE, type = "string", pattern = "^S[0-9]{2}$", unique = TRUE),
          amount = list(type = "number", minimum = 0, maximum = 100),
          unit = list(type = "string", required_field = "amount", enum = list("cm", "m")),
          site = list(mandatory = TRUE, type = "string", reference = list(id = "sites", severity = "warning")))))))
}
fixture_data <- function() {
  list(Cover = matrix("Example", 1, 1),
       Observations = data.frame(id = c("S01", "S02"), amount = c(2, NA_real_),
                                 unit = c("cm", NA_character_), site = c("north", "south")))
}
fixture_references <- function() list(sites = list(values = c("north", "south")))
fixture_definition <- function() form.model::form_definition(fixture_model())
field_model <- function(type, constraints = NULL, mandatory = TRUE) {
  d <- list(schema_version = "1.0.0", form = list(id = "values", version = "1"),
            references = list(), sheets = list(Values = list(
              fields = list(value = c(list(mandatory = mandatory, type = type), constraints)),
              layout = list(kind = "table", column_count = 1L, start_row = 1L))))
  form.model::as_form_model(d)
}
validate_fixture <- function(data = fixture_data(), references = fixture_references(),
                             model = fixture_model(), rules = list(), config = list()) {
  validate_data(validation_plan(model, rules), data, references, source = "submission.xlsx", config = config)
}

upstream_model <- function() form.model::read_form_model("examples/survey-form.yaml", "form.model")
upstream_references <- function() list(observers = list(values = "ALICE"), stations = list(values = c("001", "002")))
upstream_data <- function() {
  data <- list(Metadata = matrix(vector("list", 15), 5, 3),
               Observations = matrix(vector("list", 77), 7, 11))
  data$Metadata[[2, 3]] <- "001"
  data$Metadata[[3, 3]] <- "2026-10-03"
  data$Observations[5, ] <- as.list(c("STATION", "COUNT", "LENGTH", "UNIT", "ACTIVE", "OBSERVED_AT", "LATITUDE", "LONGITUDE", "OBSERVATION_ID", "QUALITY", "OBSERVER"))
  data$Observations[6, ] <- list("001", 2L, NULL, NULL, TRUE, "2026-10-03T12:00:00Z", -20.5, 55.5, "OBS-001", 1L, "ALICE")
  data$Observations[7, ] <- list("002", 3L, NULL, NULL, FALSE, "2026-10-03T13:00:00Z", -21, 56, "OBS-002", 2L, "ALICE")
  data
}
upstream_checks <- function() {
  callbacks <- new.env(parent = baseenv())
  sys.source(system.file("examples/business.R", package = "form.validation"), callbacks)
  list(total_count = callbacks$total_count)
}
