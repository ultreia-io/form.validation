# Maintainer guide

`develop` is the integration branch. `main` contains releases. Implementation branches use `feature/`.

## Code organization

| Area          | Files                                                                                   | Responsibility                                               |
| ------------- | --------------------------------------------------------------------------------------- | ------------------------------------------------------------ |
| Input         | `input-workbook.R`, `input-values.R`                                                    | Read worksheets and prepare submitted values.                |
| Rules         | `rules.R`, `rules-read.R`, `rules-write.R`                                              | Define plans and read or generate rule YAML.                 |
| Validation    | `validation.R`, `validation-checks.R`, `validation-findings.R`, `validation-registry.R` | Execute checks, match registry entries and collect findings. |
| Reporting     | `report.R`                                                                              | Prepare report data and write CSV and HTML.                  |
| Documentation | `form.validation-package.R`                                                            | Provide package-level documentation.                         |

## Local verification

Run these commands from the repository root with the development dependencies installed.

1. **Generate documentation and run tests:**

   ```sh
   Rscript -e 'roxygen2::roxygenise(); testthat::test_local()'
   ```

2. **Build and check the package:**

   ```sh
   mkdir -p build
   (cd build && R CMD build --no-manual ..)
   package_version=$(Rscript -e 'cat(read.dcf("DESCRIPTION")[1, "Version"])')
   R CMD check --no-manual --output=build "build/form.validation_${package_version}.tar.gz"
   ```

3. **Build the documentation website:**

   ```sh
   Rscript tools/build-site.R
   ```

## Release

1. **Prepare the release on `develop`:**

   1. **Prepare the release files:** update the version in `DESCRIPTION`, finalize `NEWS.md` and documentation.
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

**Publish Release** publishes the package, checksum and documentation automatically.

## Execution

Structural checks run first and block content checks when they fail or cannot finish.

Content checks reuse each field's extracted values, coordinates, and missing/type masks within one validation run.

Callbacks can access these through `config$field_values("Sheet.field")`.

`original` retains submitted values; `eligible` identifies typed, filled cells; `values` holds comparison values.

Plans and prepared reference snapshots can be reused across submissions. Field caches are specific to each run.

Use `prepare_validation_references()` for code lists and `registry_index()` for registries.

Only references needed by executed rules can cause incomplete or failed validation.
