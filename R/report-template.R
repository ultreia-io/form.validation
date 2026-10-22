# Render installed HTML templates with escaped scalar placeholders.
# NULL omits a complete template line; substitution never reprocesses inserted values.
render_report_template <- function(name, values) {
  template <- readLines(system.file("report", name, package = "form.validation", mustWork = TRUE),
                        warn = FALSE, encoding = "UTF-8")
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
