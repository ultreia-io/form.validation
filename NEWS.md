# model.validation 0.1.0

* Generate structural and field checks from model.form schema 1.0.0.
* Validate supplied worksheet grids and explicit reference snapshots.
* Return located findings and separate incomplete or failed rule execution.
* Support custom rules with explicit data, references, and configuration.

* Generate versioned YAML rule files and check them against an installed schema.
* Load separate business rules with explicitly registered R callbacks and parameters.
* Export one summary.csv and findings.csv per input, including execution problems.
* Resolve model, generated, and optional business files from a shared model-name convention.

* Support explicitly supplied structural callbacks.
* Show severity, stage, and all located messages in reports; hide skipped checks and zero counts.
* Preserve generated table HTML outside Pandoc conversion.

- Prepare caller-owned data once after structure passes; preparation failures block content checks.
- Add an optional neutral XLSX reader; validation still accepts supplied grids directly.
