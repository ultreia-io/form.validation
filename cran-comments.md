## Check results

R CMD check --as-cran for form.validation on:

* Local computer: Linux (elementary OS 8), R 4.6.2.

Local results: 0 errors, 1 warning, 0 notes.

The incoming warning reports that the required form.references package is not yet available on CRAN.

The same check also reports the Remotes field.

## First submission

This is the first CRAN submission of form.validation.

The package uses form.model to generate validation rules and form.references for shared reference lookups.

It validates submitted forms and worksheet data, returning findings with cell locations and HTML or CSV reports.

Thank you for reviewing this submission and for supporting the R open-source community.
