pkgdown::init_site()
source("tools/coverage.R")
pkgdown::build_site(new_process = FALSE, install = TRUE, preview = FALSE)
