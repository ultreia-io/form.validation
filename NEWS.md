# form.validation 0.2.0

* Rename the package from `model.validation` to `form.validation`; update installation references and namespace calls.

* Distinguish interactive overview cards with explicit action labels and IOTC-coloured hover and focus states.

* Convert valid day-first slash dates and year-first dotted dates with strict calendar checks.

* Show aggregate evidence reports as navigation cards ordered code lists, registries, then conversions.

* Add conversion evidence cards by severity and expected type, with fresh filters on each card selection.

* Accept caller-defined per-field missing-text tokens and optional case-insensitive matching during preparation.
* Adapt report page sizes to filtered row counts, offering All only up to 1000 matching rows, with a preferred default of 20; debounce text filters by 250 ms.
* Paint loading feedback before expensive evidence-table creation; reuse completed tabs without a loader.
* Show the selected submitted value above source evidence in all three evidence reports.
* Accept case-insensitive Y/N as boolean text, while retaining numeric booleans as invalid.
* Accept native and coerced datetimes with fractional seconds, preserving their precision during validation.
* Normalize complete ISO datetimes with explicit zones to UTC; reject ambiguous or partial temporal formats.
* Prepare numeric and textual boolean values by declared type, retaining ambiguous inputs as findings.
* Expose optional readxl whitespace trimming while preserving the default reader behaviour.
* Store optional plain-text batch comments in publication manifests.
* Add conversion evidence for saved numeric, boolean and temporal type failures, preserving values and source cells.
* Resolve sheet and field metadata once per report instead of once per finding.
* Carry caller metadata from validation plans into results and reports, including unreadable inputs.
* Display optional escaped report notes in the Warning findings card on individual and aggregate reports.
* Add registry evidence reports with errors, warnings, review reasons, source cells and simple CSV downloads.
* Enable the source-evidence tab only after selecting a case in either evidence report.
* Read, prepare and validate individual workbooks without generating reports, retaining typed reader failures and timings.
* Add a neutral batch plan with explicit inputs, jobs, frozen resources and caller-defined grouping.
* Save validation results with checksums, resume support and timings; rebuild reports independently.
* Prepare verified publication trees with ancestor navigation and portable plan metadata, without uploading.
* Write summary and findings CSV files with a UTF-8 BOM so Excel on Windows recognizes their encoding.
* Separate HTML templates, scripts and stylesheets, with shared content-hashed report assets.
* Support configurable HTML filenames, parent navigation and publication breadcrumbs.
* Aggregate completed reports by caller-defined directory levels, without rerunning validation.
* Reduce aggregation memory use by grouping each form before combining its findings.
* Retain structured sheet and field locations for incomplete checks and unresolved source declarations.
* Add review tabs, card shortcuts, editable filters, reset controls, page sizes and CSV exports.
* Load CSV data in a local worker and retain current tab content until its replacement is ready.
* Use consistent form terminology, status icons, count formatting and visible whitespace in reported values.
* Keep zero-count overview cards visible with muted styling and no navigation shortcut.
* Add code-list evidence reports with snapshot-specific cases, source cell ranges and downloadable CSV evidence.
* Preserve report title punctuation with portable Unicode escapes in R sources.

# model.validation 0.1.0

* Generate versioned structural and field rules from form.model definitions and reject stale rule files.
* Validate worksheet grids in stages, running content checks only after structural checks pass.
* Apply field types and constraints, code-list references, and explicitly registered custom rules.
* Return findings with source coordinates and distinguish validation errors from unfinished or failed checks.
* Support reusable reference snapshots, registry matching helpers, and optional data preparation.
* Read Excel workbooks while retaining cell positions and individual value types.
* Export summary and findings CSV files and interactive HTML reports that load them over HTTP.
* Share local report assets and support custom stylesheets and logos.
* Include an end-to-end survey validation example.
