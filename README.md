# form.validation

[![Build](https://img.shields.io/github/actions/workflow/status/ultreia-io/form.validation/Build.yaml?branch=develop&label=Build&logo=githubactions&logoColor=white)](https://github.com/ultreia-io/form.validation/actions/workflows/Build.yaml?query=branch%3Adevelop)
[![Website](https://img.shields.io/badge/website-documentation-blue?logo=readthedocs&logoColor=white)](https://ultreia-io.github.io/form.validation/index.html)
[![License](https://img.shields.io/badge/license-LGPL--2.1-blue?logo=gnu&logoColor=white)](https://github.com/ultreia-io/form.validation/blob/main/LICENSE)

[![Latest release](https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fapi.github.com%2Frepos%2Fultreia-io%2Fform.validation%2Freleases%2Flatest&query=%24.tag_name&label=Latest%20release&color=blue&logo=github&logoColor=white)](https://github.com/ultreia-io/form.validation/releases/latest)
![CRAN release](https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fcrandb.r-pkg.org%2Fform.validation&query=%24.Version&label=CRAN%20release&color=brightgreen&logo=r&logoColor=white)

[![Coverage develop](https://img.shields.io/codecov/c/github/ultreia-io/form.validation/develop?label=coverage%20develop&logo=codecov)](https://app.codecov.io/gh/ultreia-io/form.validation/tree/develop)
[![Coverage main](https://img.shields.io/codecov/c/github/ultreia-io/form.validation/main?label=coverage%20main&logo=codecov)](https://app.codecov.io/gh/ultreia-io/form.validation/tree/main)

## About this project

> `form.validation` checks submitted forms against their form models and produces findings with precise locations.

Read the [Terminology](https://ultreia-io.github.io/form.validation/articles/terminology.html) for the concepts used throughout this documentation.

It prepares validation rules from form models and accepts explicit reference values and custom business checks.

Structural checks run first. Field and business checks run only when the form structure permits them.

Consumers can validate an Excel form or supply worksheet data directly, then inspect findings or produce reports.

Batch operations reuse prepared validation plans and references, and reports can be rebuilt from saved results.

## Related packages

- [form.model](https://github.com/ultreia-io/form.model) defines forms, their layouts and constraints.
- [form.references](https://github.com/ultreia-io/form.references) loads reference catalogues and resolves lookups.
- [form.tools](https://github.com/ultreia-io/form.tools) writes and splits forms using their layouts and keys.

## Installation

The first CRAN submission is pending. Once available, install the latest version from CRAN:

``` r
# Install form.validation from CRAN.
install.packages("form.validation")
```

Alternatively, install the latest GitHub release:

``` r
# Install the package installer once.
install.packages("pak")
# Install the latest released form.validation package.
pak::pak("ultreia-io/form.validation@*release")
```

`pak` downloads the package and installs its dependencies automatically.

For the development version:

``` r
# Install form.validation from the development branch.
pak::pak("ultreia-io/form.validation@develop")
```

## Getting started

Follow the [Getting started guide](https://ultreia-io.github.io/form.validation/articles/form.validation.html) for the runnable survey example and downloadable files.

## Guides

- [Authoring validation rules](https://ultreia-io.github.io/form.validation/articles/rules-authoring.html)
- [Validation specification](https://ultreia-io.github.io/form.validation/articles/validation-specification.html)
- [Downstream package integration](https://ultreia-io.github.io/form.validation/articles/downstream-packages.html)
- [Validation reports](https://ultreia-io.github.io/form.validation/articles/validation-reports.html)
- [API reference](https://ultreia-io.github.io/form.validation/reference/index.html)
- [Contributing](https://ultreia-io.github.io/form.validation/CONTRIBUTING.html) and [maintenance](https://ultreia-io.github.io/form.validation/DEVELOPMENT.html)
