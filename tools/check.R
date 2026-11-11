# Run each command synchronously and stop if it fails.
run_command <- function(command, arguments) {
  command_status <- system2(command, arguments)
  if (command_status != 0L) stop("Command failed: ", command, call. = FALSE)
}

# Require the package root as the working directory.
if (!file.exists("DESCRIPTION") || !file.exists("tools/check.Renviron"))
  stop("Run Rscript tools/check.R from the package root.", call. = FALSE)

# Locate the R executables and package metadata.
r_executable <- file.path(R.home("bin"), "R")
rscript_executable <- file.path(R.home("bin"), "Rscript")
package_description <- read.dcf("DESCRIPTION")
package_archive <- paste0(package_description[1L, "Package"], "_",
  package_description[1L, "Version"], ".tar.gz")

# Share the CRAN mirror configuration with CI without disabling any checks.
Sys.setenv(R_CHECK_ENVIRON = normalizePath("tools/check.Renviron", winslash = "/"))

# Generate documentation and run the package tests.
roxygen2::roxygenise()
testthat::test_local()

# Build the source archive in build/, restoring the package root afterwards.
dir.create("build", showWarnings = FALSE)
local({
  package_directory <- setwd("build")
  on.exit(setwd(package_directory))
  run_command(r_executable, c("CMD", "build", ".."))
})

# Check the archive with all CRAN checks, including the PDF manual.
run_command(r_executable, c("CMD", "check", "--as-cran", "--output=build",
  shQuote(file.path("build", package_archive))))

# Prepare example downloads and the licence for the website.
run_command(rscript_executable, "tools/prepare-site-assets.R")

# Build the website with standard pkgdown.
run_command(rscript_executable, c("-e", shQuote("pkgdown::build_site()")))
