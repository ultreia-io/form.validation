# Maintainer guide

## Code organization

| Area          | Files                                                                                   | Responsibility                                               |
| ------------- | --------------------------------------------------------------------------------------- | ------------------------------------------------------------ |
| Input         | `input-workbook.R`, `input-values.R`                                                    | Read worksheets and prepare submitted values.                |
| Rules         | `rules.R`, `rules-read.R`, `rules-write.R`                                              | Define plans and read or generate rule YAML.                 |
| Validation    | `validation.R`, `validation-checks.R`, `validation-findings.R`, `validation-registry.R` | Execute checks, match registry entries and collect findings. |
| Reporting     | `report.R`                                                                              | Prepare report data and write CSV and HTML.                  |
| Documentation | `model.validation-package.R`                                                            | Provide package-level documentation.                         |

## Verification and release

```sh
Rscript -e 'pkgload::load_all("../model.form"); roxygen2::roxygenise(); testthat::test_local()'
mkdir -p build
(cd build && R CMD build --no-manual ..)
R CMD check --no-manual --output=build build/model.validation_0.1.0.tar.gz
Rscript tools/build-site.R
```

The site includes a native coverage page at `docs/articles/coverage.html`, using the shared theme and navigation.

Coverage CSV and XML stay in `docs/coverage/`. Archives and check output stay in `build/`.

Run `Rscript tools/coverage.R` to refresh coverage data alone; rebuild the site to refresh its page.

Prepare DESCRIPTION and NEWS on `develop`, then commit and push.

Open **Actions → Perform Release → Run workflow**, select `develop`, enter the version, and run it.

The workflow checks the selected commit, creates its tag and GitHub Release, and uploads the archive and checksum.

It then deploys the website from the same commit.

Release `model.form` first; `model.validation` depends on its `v0.1.0` tag.

## Execution

Structural checks run first and block content checks when they fail or cannot finish.

Content checks reuse each field's extracted values, coordinates, and missing/type masks within one validation run.

Callbacks can access these through `config$field_values("Sheet.field")`.

`original` retains submitted values; `eligible` identifies typed, filled cells; `values` holds comparison values.

Plans and prepared reference snapshots can be reused across submissions. Field caches are specific to each run.

Use `prepare_validation_references()` for code lists and `registry_index()` for registries.

Only references needed by executed rules can cause incomplete or failed validation.
