# The caller explicitly loads this callback; YAML never sources code.
total_count <- function(model, data, references, config) {
  field <- config$field_values("Observations.count")
  if (any(field$filled & !field$typed)) {
    form.validation::signal_validation_incomplete("Total count needs numeric observations")
  }
  maximum <- config$rule$maximum
  if (!is.numeric(maximum) || length(maximum) != 1L || !is.finite(maximum)) {
    stop("Business parameter maximum must be one finite number")
  }
  total <- sum(field$values)
  if (total > maximum) {
    form.validation::create_validation_finding("Total count exceeds the budget",
      sheet = "Observations", field = "Observations.count", column = 2,
      value = total, expected = maximum)
  }
}
