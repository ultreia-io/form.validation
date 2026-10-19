options(covr.rstudio_source_markers = FALSE)
coverage <- covr::package_coverage()
print(coverage)
output <- "docs/coverage"
dir.create(output, recursive = TRUE, showWarnings = FALSE)
covr::to_cobertura(coverage, filename = file.path(output, "coverage.xml"))
utils::write.csv(covr::tally_coverage(coverage), file.path(output, "coverage.csv"), row.names = FALSE)
print(covr::zero_coverage(coverage))
