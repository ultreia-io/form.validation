# model.validation 0.2.0


* Convert valid day-first slash dates and year-first dotted dates with strict calendar checks.



* Accept caller-defined per-field missing-text tokens and optional case-insensitive matching during preparation.
* Accept case-insensitive Y/N as boolean text, while retaining numeric booleans as invalid.
* Accept native and coerced datetimes with fractional seconds, preserving their precision during validation.
* Normalize complete ISO datetimes with explicit zones to UTC; reject ambiguous or partial temporal formats.
* Prepare numeric and textual boolean values by declared type, retaining ambiguous inputs as findings.
* Expose optional readxl whitespace trimming while preserving the default reader behaviour.
* Store optional plain-text batch comments in publication manifests.
* Carry caller metadata from validation plans into results and reports, including unreadable inputs.
* Read, prepare and validate individual workbooks without generating reports, retaining typed reader failures and timings.
* Add a neutral batch plan with explicit inputs, jobs, frozen resources and caller-defined grouping.
* Save validation results with checksums, resume support and timings; rebuild reports independently.
* Prepare verified publication trees with ancestor navigation and portable plan metadata, without uploading.

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
