# form.validation

[![Build](https://github.com/ultreia-io/form.validation/actions/workflows/Build.yaml/badge.svg?branch=develop)](https://github.com/ultreia-io/form.validation/actions/workflows/Build.yaml?query=branch%3Adevelop)
[![Test Coverage](https://github.com/ultreia-io/form.validation/actions/workflows/TestCoverage.yaml/badge.svg?branch=main)](https://github.com/ultreia-io/form.validation/actions/workflows/TestCoverage.yaml?query=branch%3Amain)
[![Coverage](https://img.shields.io/badge/dynamic/xml?url=https%3A%2F%2Fultreia-io.github.io%2Fform.validation%2Fcoverage%2Fcoverage.xml&query=round%28%2Fcoverage%2F%40line-rate+%2A+1000%29+div+10&label=coverage&suffix=%25&color=brightgreen)](https://ultreia-io.github.io/form.validation/articles/coverage.html)
[![Website](https://img.shields.io/badge/website-documentation-blue)](https://ultreia-io.github.io/form.validation/)
[![Latest
release](https://img.shields.io/github/v/release/ultreia-io/form.validation?display_name=tag&sort=semver)](https://github.com/ultreia-io/form.validation/releases/latest)
[![License](https://img.shields.io/github/license/ultreia-io/form.validation)](https://github.com/ultreia-io/form.validation/blob/main/LICENSE)

Validate submitted form data and produce CSV and HTML reports.

Requires [form.model](https://ultreia-io.github.io/form.model/)
for checked definitions and shared field-type checks.

## Installation

Install the latest released version with Excel support:

``` r
install.packages("pak") # Once
pak::pak("ultreia-io/form.validation@*release")
```

`pak` installs all required dependencies automatically: `form.model`, `yaml`, `readxl` and `cellranger`.

For the development version:

``` r
pak::pak("ultreia-io/form.validation@develop")
```

## Start here

Validate your workbook against its form definition:

``` r
library(form.validation)
model <- form.model::read_form_model("my-form.yaml")
plan <- validation_plan(model)
data <- read_validation_workbook("submission.xlsx")
result <- validate_data(plan, data)
result$findings
```

Supply reference snapshots when the model declares code lists.

The [Getting started
guide](https://ultreia-io.github.io/form.validation/articles/form.validation.html)
provides a complete survey example.

## What it does

- Check workbook structure before submitted values.
- Apply field types, constraints and reference membership.
- Run explicitly supplied custom checks.
- Record findings with physical cell locations.
- Export CSV results and HTML reports.
- Run batches and rebuild reports without repeating validation.
- Review aggregate findings and evidence for code lists, registries and conversion errors.

Accept a submission only when `result$complete` and
`isTRUE(result$valid)` are both true.

## Learn more

- [Authoring validation rules](https://ultreia-io.github.io/form.validation/articles/rules-authoring.html): generate checks and add custom rules.
- [Validation specification](https://ultreia-io.github.io/form.validation/articles/validation-specification.html): formats, execution and references.
- [Using validation in your package](https://ultreia-io.github.io/form.validation/articles/downstream-packages.html): integrate and run validation.
- [Validation
  reports](https://ultreia-io.github.io/form.validation/articles/validation-reports.html):
  CSV and HTML output.
- [Batch workflow](https://ultreia-io.github.io/form.validation/reference/validation_batch_plan.html): plan, validate and prepare reports for publication.
- [Aggregate reports](https://ultreia-io.github.io/form.validation/reference/write_validation_aggregate.html): combine reports and review evidence.
- [Function
  reference](https://ultreia-io.github.io/form.validation/reference/index.html).
- [Contributing](https://ultreia-io.github.io/form.validation/CONTRIBUTING.html) and [maintenance](https://ultreia-io.github.io/form.validation/DEVELOPMENT.html)

LGPL-2.1. See [LICENSE](https://github.com/ultreia-io/form.validation/blob/main/LICENSE).
