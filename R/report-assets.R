# Share content-addressed assets across individual and aggregate reports.
copy_report_assets <- function(sources, directory) {
  if (any(!file.exists(sources))) stop("Report asset does not exist", call. = FALSE)
  if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE))
    stop("Cannot create report asset directory", call. = FALSE)
  hashes <- unname(tools::md5sum(sources))
  filenames <- stats::setNames(paste0("report-", hashes, ".", tools::file_ext(sources)), names(sources))
  for (i in seq_along(sources)) {
    destination <- file.path(directory, filenames[[i]])
    if (file.exists(destination)) {
      if (!identical(unname(tools::md5sum(destination)), hashes[[i]]))
        stop("Conflicting report asset: ", destination, call. = FALSE)
    } else if (!file.copy(sources[[i]], destination)) stop("Cannot write report asset", call. = FALSE)
  }
  filenames
}
