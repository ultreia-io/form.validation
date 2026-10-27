# Render installed HTML templates with escaped scalar placeholders.
# NULL omits a complete template line; substitution never reprocesses inserted values.
#' Render an installed HTML template
#' @param name Filename under the installed `report` directory.
#' @param values Named list of scalar character values or `NULL`.
#' @return Character vector of rendered HTML lines.
#' @details Escapes HTML characters. A NULL placeholder removes its complete line. Unknown placeholders and invalid values fail; inserted text is never processed again.
#' @keywords internal
render_report_template <- function(name, values) {
  template <- readLines(system.file("report", name, package = "form.validation", mustWork = TRUE),
                        warn = FALSE, encoding = "UTF-8")
  # Escape one template value
  # @param value One nonmissing character string.
  # @return HTML-escaped scalar text.
  escape <- function(value) {
    if (!is.character(value) || length(value) != 1L || is.na(value)) stop("Template values must be scalar text")
    for (pair in list(c("&", "&amp;"), c("<", "&lt;"), c(">", "&gt;"), c('"', "&quot;")))
      value <- gsub(pair[[1]], pair[[2]], value, fixed = TRUE)
    value
  }
  unlist(lapply(template, function(line) {
    matches <- gregexpr("\\{\\{[a-z_]+\\}\\}", line, perl = TRUE)
    tokens <- regmatches(line, matches)[[1L]]
    if (!length(tokens)) return(line)
    keys <- substring(tokens, 3L, nchar(tokens) - 2L)
    if (any(!keys %in% names(values))) stop("Unknown template placeholder")
    if (any(vapply(values[keys], is.null, logical(1)))) return(character())
    regmatches(line, matches) <- list(vapply(values[keys], escape, character(1)))
    line
  }), use.names = FALSE)
}

#' Prepare counts for HTML cards
#' @param counts Named list or one-row data frame of numeric counts.
#' @return Scalar template values for formatted counts, raw values and empty states.
#' @keywords internal
report_count_values <- function(counts) {
  values <- list()
  for (key in names(counts)) {
    count <- as.numeric(counts[[key]])
    values[[paste0(key, "_count")]] <- format(count, big.mark = ",", scientific = FALSE, trim = TRUE)
    values[[paste0(key, "_value")]] <- format(count, scientific = FALSE, trim = TRUE)
    values[[paste0(key, "_empty")]] <- if (count == 0) "true" else "false"
  }
  values
}
