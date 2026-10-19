
<!-- README.md is generated from README.Rmd. Edit this file. -->

# model.validation

[![Build](https://github.com/ultreia-io/model.validation/actions/workflows/Build.yaml/badge.svg?branch=develop)](https://github.com/ultreia-io/model.validation/actions/workflows/Build.yaml)
[![Test](https://github.com/ultreia-io/model.validation/actions/workflows/Test.yaml/badge.svg?branch=develop)](https://github.com/ultreia-io/model.validation/actions/workflows/Test.yaml)
[![Website](https://img.shields.io/badge/website-documentation-blue)](https://ultreia-io.github.io/model.validation/)
[![Coverage
reports](https://img.shields.io/badge/coverage-CI_reports-blue)](https://github.com/ultreia-io/model.validation/actions/workflows/Test.yaml)
[![Latest
release](https://img.shields.io/github/v/release/ultreia-io/model.validation?display_name=tag&sort=semver)](https://github.com/ultreia-io/model.validation/releases/latest)
[![License](https://img.shields.io/github/license/ultreia-io/model.validation)](https://github.com/ultreia-io/model.validation/blob/main/LICENSE)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

Validate submitted data against neutral `model.form` definitions, with
explicit reference snapshots and custom rules.

`model.form` checks definitions. `model.validation` checks submitted
values and records whether execution completed.

## Installation

``` r
# install.packages("pak")
pak::pak("ultreia-io/model.validation@develop")
```

For local development, install `model.form` first, then run
`R CMD INSTALL .` from this package’s directory.

The form definition belongs to `model.form`; this example reuses its
`examples/survey-form.yaml`.

This package stores no duplicate form.

Its validation files describe the process, structure, generated checks,
and business rules:

| File                               | Purpose                        |
|------------------------------------|--------------------------------|
| `survey-validation.yaml`           | Complete execution process     |
| `survey-structure-validation.yaml` | Sheet order and dimensions     |
| `survey-generated-validation.yaml` | Explicit generated field rules |
| `survey-business-validation.yaml`  | Handwritten callback rules     |

``` yaml
rulesets:
  structure: survey-structure-validation.yaml
  content:
  - survey-generated-validation.yaml
  - survey-business-validation.yaml
```

All structural checks finish first. Errors or unfinished structural
checks block the entire content ruleset.

Content merges generated checks followed by business checks. Only
structure gates execution.

## Generate, review, and execute

``` r
model <- model.form::read_form_model("examples/survey-form.yaml", "model.form")
directory <- system.file("examples", package = "model.validation")
callbacks <- new.env(parent = baseenv())
sys.source(file.path(directory, "business.R"), callbacks)
plan <- read_validation_process(file.path(directory, "survey-validation.yaml"), model,
  checks = list(total_count = callbacks$total_count))
metadata <- matrix(list(NULL), nrow = 5, ncol = 3)
metadata[2, 3] <- list("001")
metadata[3, 3] <- list("2026-10-03")
metadata[4, 3] <- list("alice@example.org")
observations <- matrix(list(NULL), nrow = 7, ncol = 11)
observations[5, ] <- as.list(c("STATION", "COUNT", "LENGTH", "UNIT", "ACTIVE", "OBSERVED_AT", "LATITUDE", "LONGITUDE", "OBSERVATION_ID", "QUALITY", "OBSERVER"))
observations[6, ] <- list("001", 2, 12.5, "cm", TRUE, "2026-10-03T12:00:00Z", -20.5, 55.5, "OBS-001", 1L, "ALICE")
observations[7, ] <- list("002", 3, NA_real_, NA_character_, FALSE, "2026-10-03T13:00:00Z", -21, 56, "OBS-002", 2L, "ALICE")
data <- list(Metadata = metadata, Observations = observations)
references <- list(observers = list(values = "ALICE"), stations = list(values = c("001", "002")))
result <- validate_data(plan, data, references, source = "submission.xlsx")
result
#> <validation_result> valid
#> 0 findings; 37 completed; 0 incomplete; 0 failed
output <- tempfile()
write_validation_results(result, output)
```

The process receives the authoritative form explicitly. Its listed files
supply the two ordered rulesets.

Generated rules are written one per line, using the value shortcut
whenever possible.

A scalar or sequence is shorthand for `{value: ...}`. Both forms are
accepted; severity defaults to error.

``` yaml
rules:
  field/Observations.count/mandatory: true
  field/Observations.count/type: integer
  field/Observations.count/minimum: 0
  field/Observations.unit/enum: [cm, m]
  field/Observations.station/reference: {value: stations, severity: warning}
```

Use a mapping when adding properties such as `severity`. Quote strings
that YAML could read as numbers or booleans.

Business rules accept the same shortcut: `business/check_total: 1000`
passes `config$rule$value = 1000`.

A mapping-valued parameter needs `{value: {key: data}}`; `{}` still
means a rule without parameters.

The ID identifies the field and check. Edit the form, then regenerate;
stale rules fail to load.

To generate compact checks from an already loaded form:

``` r
write_validation_rules(model, "rules/survey-structure-validation.yaml",
  "rules/survey-generated-validation.yaml", version = "1")
```

Use `overwrite = TRUE` to regenerate structure and field files. Form,
process, and business files remain untouched.

Supply data frames or matrices named by workbook sheet name, retaining
all physical preamble rows.

Fixed-cell sheets without `row_count` derive `min_rows` from their last
declared cell. Extra rows are allowed.

Tables ending before `start_row` are empty; blank preamble rows need not
be manufactured. Column counts still apply.

Findings include rule IDs, severity, source, field, row, column, and
actual/expected values.

Check `result$complete` and `isTRUE(result$valid)` before loading.
Missing prerequisites never silently pass.

Keep handwritten business rules in a separate YAML file. Regenerating
model rules leaves that file untouched.

Business YAML names an R check and its parameters. The caller supplies
the function when loading the rules.

Each input gets two reporting files:

- `summary.csv`: input identity, versions, overall status, and
  finding/execution counts.
- `findings.csv`: located validation findings and incomplete or failed
  checks, across all sheets.

Successful inputs have a summary and a header-only findings file.
Code-list snapshots are supplied explicitly.

## Alternate reference codes

Reference snapshots may supply explicit aliases alongside canonical
values:

``` r
references <- list(units = list(values = c("cm", "kg"), aliases = c(CM = "cm")))
```

Membership accepts canonical codes or alias names. Comparisons remain
exact and case-sensitive.

Alias targets must exist in `values`; malformed mappings fail execution.
Missing snapshots remain incomplete.

Submitted values are never rewritten. Duplicate aliases are allowed:
this checks membership, not unique identity.

Accepted keys are cached per validation, or shared across submissions
with `prepare_validation_references()`.

The caller supplies all mappings; no data is fetched.

## Registry name matching

Prepare a lookup once from supplied registry values, then reuse it
across submissions:

``` r
index <- registry_index(c("MARTIN Emile", "Jane Smith"), matching = "name",
  aliases = c("J. Smith" = "Jane Smith"))
registry_match_counts(index, c("Emile Martin", "J. Smith", "Unknown"))
# 1 1 0
```

Lookup tries exact values, explicit aliases, then normalized names.
Exactly one registry row must match.

Name keys ignore case, accents, selected punctuation and word order;
this is not fuzzy matching.

Exact duplicates and normalized collisions remain ambiguous. Missing
alias targets remain unmatched.

Alias chains are supported; cycles fail preparation. Input values and
duplicate registry rows are preserved.

`matching = "exact"` is the default. Name matching and aliases require
string fields.

The caller chooses matching options and supplies snapshots; these
helpers load no files or databases.

## Custom structural checks

Custom structural checks use
`validation_rule(..., stage = "structure")`. Structure YAML may include
a `rules` mapping using the same compact callback format as business
YAML. Callbacks are supplied explicitly through `checks`. All structural
checks finish; errors or unfinished checks block every content rule.
Regeneration preserves the structure file’s custom `rules` mapping.

Optional `validate_data(..., prepare_data = function(data) ...)`
prepares values once after structure passes. It must retain sheet names,
order and dimensions. Preparation errors block content checks and appear
as `data/prepare` execution failures.

`prepare_validation_data(data, model)` is an opt-in policy that converts
string fields to text and midnight timestamps to dates for date fields.
Other values remain unchanged. Pass it through `prepare_data` to use
this policy after the gate.

## Optional Excel input

`read_validation_workbook(path)` reads XLSX worksheets as named grids
using optional readxl and cellranger dependencies. It retains physical
positions, individual cell types, whitespace and textual leading zeros.
It does not apply form-specific conversions or execute validation rules.

``` r
data <- read_validation_workbook("submission.xlsx")
result <- validate_data(plan, data, references,
  prepare_data = function(data) prepare_validation_data(data, plan$model))
```

The engine still accepts supplied grids directly. Excel errors and
formulas without cached results become missing values; formulas are
never recalculated.

## Single-input HTML report

Prepare report data once, then write a CSV-backed HTML page:

``` r
report <- validation_report_data(result, model)
write_validation_results(report, "report")
write_validation_report(report, "report/report.html")
```

The Abstract shows the input, versions, status, and error/warning
counts. Zero counts and skipped checks are hidden.

Summary and Validation messages start with a severity icon and a
labelled stage icon (Structure, Field, Business). All sheets appear in
Validation messages, including fixed-cell sheets and workbook-level
problems. CSV exports retain plain severity and stage text; the CSV files include explicit report metadata.

Summary and Validation messages use sheet tabs with search, column
filters, pagination, and CSV buttons.

Table summaries group identical field, column, value, severity, stage,
and message combinations, with occurrence counts.

Fixed-cell findings appear individually in Summary. Validation messages
retains every finding for every sheet.

“Checks that could not finish” lists only incomplete or failed checks
and is hidden when empty.

An unfinished validation never appears as a successful report, even with
no error findings.

CSV and HTML reuse the prepared report’s formatted tables.

The HTML loads adjacent `summary.csv` and `findings.csv` over HTTP.
Publish both CSV files and the `report-assets` folder with the HTML.
Apache or a Shiny static resource directory can serve the files.
No rendering packages, Pandoc, or external CDN are required.
For collections, set `assets_directory` to a shared relative directory.

The original workbook is not copied. Existing report files require
explicit `overwrite = TRUE`.

`validation_report_data()` returns plain tables for reuse by a future
Shiny interface, without rendering or revalidation.

CSV exports include all recorded problems, explicit structure status and UTC generation time.
Prepared reports also export finding stages, field labels, sheet order and layout.

## API

- `generate_validation_rules()` writes rules using the named form-file
  convention.
- `load_validation_plan()` reads a named form and its complete process.
- `read_validation_process()` loads an explicit process, with optional
  additional business files.
- `write_validation_rules()` and `read_validation_rules()` also accept
  explicit paths.
- `validation_schema()` exposes the installed YAML contract.
- `write_validation_results()` writes the two reporting CSV files.
- `validation_plan()` remains available for direct in-memory use.
- `validate_data()` executes checks and returns findings plus execution
  records.
- `validation_rule()` and `validation_finding()` define custom checks
  and their findings.
- `validation_incomplete()` reports an unavailable prerequisite from a
  custom rule.

[Read the
guide](https://ultreia-io.github.io/model.validation/articles/model.validation.html).

## Execution cost

Each field extracts its column once and caches parallel value and
validity arrays.

Built-in checks construct finding tables in batches. Callback results
are checked by column without rebuilding rows.

Custom checks can use `validation_findings()` for batches;
`validation_finding()` remains the single-finding API.

Callbacks use `config$field_values("Sheet.field")` to share prepared
values and masks with generated checks.

`original` retains every submitted value; `eligible` indexes typed,
filled cells and `values` contains their comparison values.

Physical `rows` and `column` locate findings. Cached fields never cross
workbook boundaries.

Prepare code lists once with
`references <- prepare_validation_references(references, ids)`.

Pass them to every validation call. Keep custom registry IDs out of
`ids`; their indexes use `registry_index()`.

Missing or malformed snapshots only affect rules that run. Modified
snapshots cannot reuse stale prepared keys.

Per field, checks run mandatory, type, reference, then remaining
constraints.

Conditional requiredness runs with mandatory.

Each field is extracted once per run. Missing/type masks and normalized
comparison values are reused by its checks.

Bounds, patterns, membership, and uniqueness use vector operations. Use
`prepare_validation_references()` to prepare reference keys once per
batch.

Original mixed-type cells and physical coordinates remain intact.
Callbacks receive the original explicit inputs.

Reuse a loaded plan across inputs. Arbitrary business callbacks may
still scan data; their cost belongs to the callback.

## Development

### Code organization

| Area          | Files                                                                                   | Responsibility                                               |
| ------------- | --------------------------------------------------------------------------------------- | ------------------------------------------------------------ |
| Input         | `input-workbook.R`, `input-values.R`                                                    | Read worksheets and prepare submitted values.                |
| Rules         | `rules.R`, `rules-read.R`, `rules-write.R`                                              | Define plans and read or generate rule YAML.                 |
| Validation    | `validation.R`, `validation-checks.R`, `validation-findings.R`, `validation-registry.R` | Execute checks, match registry entries and collect findings. |
| Reporting     | `report.R`                                                                              | Prepare report data and write CSV and HTML.                  |
| Documentation | `model.validation-package.R`                                                            | Provide package-level documentation.                         |

### Verification and release

``` sh
Rscript -e 'pkgload::load_all("../model.form"); roxygen2::roxygenise(); testthat::test_local()'
mkdir -p build
(cd build && R CMD build --no-manual ..)
R CMD check --no-manual --output=build build/model.validation_0.1.0.tar.gz
Rscript tools/build-site.R
```

The site includes a native coverage page at
`docs/articles/coverage.html`, using the shared theme and navigation.

Coverage CSV and XML stay in `docs/coverage/`. Archives and check output
stay in `build/`.

Run `Rscript tools/coverage.R` to refresh coverage data alone; rebuild
the site to refresh its page.

Prepare DESCRIPTION and NEWS on `develop`, then commit and push.

Open **Actions → Perform Release → Run workflow**, select `develop`, enter the version, and run it.

The workflow checks the selected commit, creates its tag and GitHub Release, and uploads the archive and checksum.

It then deploys the website from the same commit.

Release `model.form` first; `model.validation` depends on its `v0.1.0` tag.

## Scope

Supports model schema `1.0.0`. Uniqueness requires a table field.

Unsupported declarations fail explicitly.

Workbook reading, database access, and reference fetching remain outside
this package.

HTML reporting is an optional pipeline step; executing validation never
invokes the renderer.

LGPL-2.1. See [LICENSE](LICENSE).
