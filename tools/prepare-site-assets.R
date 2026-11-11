# Require the package root before preparing generated website assets.
if (!file.exists("DESCRIPTION") || !file.exists("LICENSE"))
  stop("Run Rscript tools/prepare-site-assets.R from the package root.", call. = FALSE)

# Locate the survey files supplied by form.model.
example_directory <- system.file("examples", "survey", package = "form.model")
example_files <- file.path(example_directory, c("form.yaml", "form.xlsx", "form-empty.xlsx"))
if (!nzchar(example_directory) || !all(file.exists(example_files)))
  stop("Install form.model with its survey example files.", call. = FALSE)

# Replace generated assets so removed files cannot remain in the site input.
website_assets <- "pkgdown/assets"
unlink(website_assets, recursive = TRUE)
website_examples <- file.path(website_assets, "examples", "survey")
dir.create(website_examples, recursive = TRUE, showWarnings = FALSE)

# Copy the example files without modifying the installed resources.
if (!all(file.copy(example_files, website_examples, overwrite = TRUE)))
  stop("Could not prepare example downloads.", call. = FALSE)

# Copy this package's validation files alongside the survey form files.
validation_examples <- list.files("inst/examples/survey", full.names = TRUE)
if (!length(validation_examples) || !all(file.copy(validation_examples, website_examples,
    recursive = TRUE, overwrite = TRUE))) stop("Could not prepare validation downloads.", call. = FALSE)

# Copy the licence used by the website footer and maintainer guide.
if (!file.copy("LICENSE", file.path(website_assets, "LICENSE"), overwrite = TRUE))
  stop("Could not prepare the website licence.", call. = FALSE)
