# form.validation 0.3.0

* Add table row counts, internal blank-row warnings and Information findings cards.
* Add contextual evidence reports for individual forms and aggregates, reusing prepared evidence.
* Update dependency from `form.model` 0.3.0 to 0.3.1.
* Prepare for CRAN submission: include generated help and the vignette index, and update maintainer details.
* Improve website branding, documentation links and coverage reporting.
* Update Linux CI containers and website dependency caching.

# form.validation 0.2.1

* Install `form.model` from its release archive, including generated documentation.

# form.validation 0.2.0

* Rename package `model.validation` to `form.validation`.
* Update dependency to `form.model` 0.3.0.
* Add optional value conversion, whitespace trimming and missing-value handling.
* Separate batch validation, report generation and publication preparation; support resuming interrupted batches.
* Add aggregate reports and evidence reports for code lists, registries and conversion errors.
* Improve report navigation, filtering and performance for large batches.
* Simplify documentation and automate releases.

# model.validation 0.1.0

* Generate versioned structural and field rules from model.form definitions and reject stale rule files.
* Validate worksheet grids in stages, running content checks only after structural checks pass.
* Apply field types and constraints, code-list references, and explicitly registered custom rules.
* Return findings with source coordinates and distinguish validation errors from unfinished or failed checks.
* Support reusable reference snapshots, registry matching helpers, and optional data preparation.
* Read Excel workbooks while retaining cell positions and individual value types.
* Export summary and findings CSV files and interactive HTML reports that load them over HTTP.
* Share local report assets and support custom stylesheets and logos.
* Include an end-to-end survey validation example.
