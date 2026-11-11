# Locate the check log produced by the workflow.
check_logs <- list.files("build", pattern = "^00check[.]log$", recursive = TRUE, full.names = TRUE)
if (length(check_logs) != 1L) stop("Expected exactly one R CMD check log.")
# Reject interrupted checks even when the check action reports success.
check_lines <- readLines(check_logs[[1L]], warn = FALSE)
if (!any(grepl("^\\* DONE$", check_lines))) stop("R CMD check did not finish.")
