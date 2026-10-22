# Security policy

Security fixes are provided for the latest release.

Report vulnerabilities privately through [GitHub advisories](https://github.com/ultreia-io/form.validation/security/advisories/new).

If private reporting is unavailable, contact Tony through his GitHub profile before disclosing details publicly.

Do not include credentials, personal data, or confidential workbooks in public issues.

YAML expressions are not evaluated. Custom R callbacks are supplied explicitly and must be trusted.

Resource parsing is not an isolation boundary for arbitrarily large hostile files. Callers should limit input sizes.
