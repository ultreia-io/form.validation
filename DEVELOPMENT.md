# Maintainer guide

`develop` is the integration branch. `main` contains releases. Implementation branches use `feature/`.

## Code organization

The files organize functions in one package namespace. You do not source each file separately.

Each functional area separates its public interface from its implementation helpers:

* `-api.R`: exported functions and their documentation, intended for package consumers.
* `-internal.R`: private helpers supporting the public interface.

| File                            | Responsibility                                                      |
| ------------------------------- | ------------------------------------------------------------------- |
| `input-api.R`                   | Read forms, prepare data and run workbook validation.               |
| `input-internal.R`              | Convert values, extract cells and handle workbook read failures.    |
| `rules-api.R`                   | Construct plans, read validation definitions and write rule YAML.   |
| `rules-internal.R`              | Build rule records, check definitions and bind loaded rules.        |
| `validation-api.R`              | Prepare references, execute plans and construct findings.           |
| `validation-internal.R`         | Run built-in checks, cache lookups and assemble findings.           |
| `report-api.R`                  | Prepare reports, write CSV and HTML, and aggregate results.         |
| `report-internal.R`             | Format result tables, render templates and copy report assets.      |
| `report-evidence-internal.R`    | Prepare and render reference, conversion and primary-key evidence.  |
| `batch-api.R`                   | Plan batches, validate inputs, render reports and build publications. |
| `batch-internal.R`              | Manage batch state, workers, integrity checks and navigation.       |
| `form.validation-package.R`     | Provide package-level documentation.                                |

## Local verification

Run from the package root with development dependencies and LaTeX installed:

```sh
Rscript tools/check.R
```

This generates documentation, runs tests, builds and checks the archive with `--as-cran`, then builds the website.

Local and CI checks share the CRAN mirror settings in `tools/check.Renviron`. All CRAN checks remain enabled.

The **Build** workflow measures coverage with `covr` on Linux after successful checks and uploads it to Codecov.

The README coverage badges link to the Codecov report.

Archives and check output stay in `build/`.

`roxygen2`, `rcmdcheck`, and `pkgload` are development tools, not runtime dependencies.

Review coverage gaps for missing behaviour. Check installed resources, runnable examples, and website navigation.

## Website

Run from the package root with website dependencies installed:

``` r
# Prepare example downloads and the licence from their source files.
source("tools/prepare-site-assets.R")
# Build the website with pkgdown.
pkgdown::build_site()
```

Generated copies in `pkgdown/assets/` are replaced before building. Example sources remain in `inst/examples/`.

## GitHub Release

1. **Prepare the release on `develop`:**

   1. **Prepare the release files:** update the version in `DESCRIPTION`, finalize `NEWS.md`, `cran-comments.md` and documentation.
   2. **Perform the [local checks](#local-verification).**
   3. **Commit the changes:** use `[form.validation] Release <version>`.
   4. **Push the release commit.**

2. **Start the release on GitHub:**

   1. Open **Actions → Start Release**.
   2. Click **Run workflow**, select `develop`, then click **Run workflow** to confirm.
   3. Open the release PR created by the workflow.

3. **Merge the release PR:**

   1. **Wait for the checks:** all checks on the release PR must be green.
   2. **Merge:** click **Merge pull request**, then **Confirm merge**.
   3. Merging automatically starts the **Publish Release** workflow.

The **Publish Release** workflow:

1. Publishes the package archive, checksum and documentation.
2. **Submits the same archive to CRAN**.

## Manual CRAN submission

Use this fallback only if the release has not already been submitted by CI.

Start from a clean working tree. Replace `version` with the release version to submit.

```sh
# Fetch the published release tags.
git fetch origin --tags
# Check out the release tag.
git switch --detach version
```

Run these commands in R from the package root, with `devtools` installed.

```r
# Build and run R CMD check --as-cran, including the PDF manual and incoming checks.
devtools::check(args = "--as-cran", manual = TRUE, remote = TRUE, error_on = "warning")

# After resolving errors and warnings and reviewing NOTES, submit the release to CRAN.
devtools::submit_cran()
```

This manual route rebuilds from the release tag. CI submits the existing GitHub archive unchanged.

Confirm CRAN's email, then wait for its checks and response.
