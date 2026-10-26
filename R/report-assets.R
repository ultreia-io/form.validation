# Share content-addressed assets across individual and aggregate reports.
#' Copy content-addressed report assets
#' @param sources Named vector of existing source file paths.
#' @param directory Destination asset directory, created if needed.
#' @return Named filenames using an MD5 content hash and the original extension.
#' @details Reuses identical files and rejects conflicting contents. Returned names retain the source names for template lookup.
#' @keywords internal
copy_report_assets <- function(sources, directory) {
  if (any(!file.exists(sources))) stop("Report asset does not exist", call. = FALSE)
  if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE) && !dir.exists(directory))
    stop("Cannot create report asset directory", call. = FALSE)
  hashes <- unname(tools::md5sum(sources))
  filenames <- stats::setNames(paste0("report-", hashes, ".", tools::file_ext(sources)), names(sources))
  for (i in seq_along(sources)) {
    destination <- file.path(directory, filenames[[i]])
    if (file.exists(destination)) {
      if (!identical(unname(tools::md5sum(destination)), hashes[[i]]))
        stop("Conflicting report asset: ", destination, call. = FALSE)
    } else {
      # Complete the copy before another report worker can see the asset.
      temporary <- tempfile(".report-asset-", tmpdir = directory)
      if (!file.copy(sources[[i]], temporary)) stop("Cannot write report asset", call. = FALSE)
      moved <- file.rename(temporary, destination)
      if (!moved && (!file.exists(destination) ||
                     !identical(unname(tools::md5sum(destination)), hashes[[i]])))
        stop("Cannot write report asset", call. = FALSE)
      if (!moved) unlink(temporary)
    }
  }
  filenames
}
