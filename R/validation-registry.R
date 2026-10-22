#' Build order-independent comparison keys for personal or vessel names
#' @param x Character vector of names.
#' @return Character keys; missing or empty normalized names return `NA`.
#' @details Removes commas, squishes whitespace, uppercases and transliterates
#'   accents to ASCII, replaces apostrophes, hyphens, dots and parentheses with
#'   spaces, then sorts and concatenates words. This is not fuzzy matching or a
#'   cryptographic hash. Different names can share a key; retain duplicate rows.
#' @export
normalized_name_key <- function(x) {
  if (!is.character(x)) stop("Names must be character", call. = FALSE)
  x <- trimws(gsub("[[:space:]]+", " ", gsub(",", "", x, fixed = TRUE)))
  x <- toupper(iconv(x, to = "ASCII//TRANSLIT"))
  x <- gsub("[.'()\\-]", " ", x, perl = TRUE)
  vapply(strsplit(x, "[[:space:]]+"), function(words) {
    if (anyNA(words)) return(NA_character_)
    key <- paste0(sort(words[nzchar(words)]), collapse = "")
    if (nzchar(key)) key else NA_character_
  }, character(1), USE.NAMES = FALSE)
}

#' Encode a validated scalar registry key
#' @param x One nonmissing scalar accepted by [`value_type()`][form.validation::value_type].
#' @return A character key with a numeric, Boolean, or string prefix. Dates and
#'   timestamps use their canonical text representation; numeric zero is unsigned.
#' @keywords internal
registry_key <- function(x) {
  if (inherits(x, "Date")) x <- format(x, "%Y-%m-%d")
  if (inherits(x, "POSIXct")) x <- format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  type <- if (is.numeric(x)) "n" else if (is.logical(x)) "b" else "s"
  value <- if (is.numeric(x)) sprintf("%a", as.double(if (x == 0) 0 else x)) else enc2utf8(as.character(x))
  paste0(type, ":", value)
}

#' Look up registry match counts
#' @param counts Named vector or frequency table of match counts.
#' @param keys Character lookup keys, possibly missing or unknown.
#' @return Integer counts in key order; unmatched keys return zero.
#' @keywords internal
registry_count <- function(counts, keys) {
  result <- as.integer(counts[keys])
  result[is.na(result)] <- 0L
  result
}

#' Prepare a reusable registry lookup from supplied values
#' @param values Vector or list of scalar registry values. Retain duplicate rows.
#' @param type Declared field type, default `"string"`.
#' @param matching `"exact"` or `"name"` for normalized name fallback.
#' @param aliases Named character vector or named list mapping aliases to names.
#'   Aliases require string fields. Chains are allowed; cycles are errors.
#' @return A `validation_registry` list, reusable across fields and submissions.
#' @details Lookup tries exact values, then explicit aliases, then normalized
#'   names. Any exact match takes precedence, including ambiguous matches.
#'   Zero matches means unknown; more than one means ambiguous. Neither is valid.
#'   Alias targets absent from the supplied registry remain unmatched.
#'   No data is loaded, changed, or deduplicated. Prepare once per snapshot.
#' @examples
#' index <- registry_index(c("MARTIN Emile", "Jane Smith"), matching = "name")
#' registry_match_counts(index, c("Emile Martin", "Unknown"))
#' @export
registry_index <- function(values, type = "string", matching = "exact", aliases = character()) {
  type <- match.arg(type, c("string", "email", "boolean", "integer", "number", "latitude", "longitude", "date", "datetime"))
  matching <- match.arg(matching, c("exact", "name"))
  if (!(is.atomic(values) || is.list(values)) || is.data.frame(values) || !is.null(dim(values))) {
    stop("Registry values must be a vector or list of scalar keys", call. = FALSE)
  }
  values <- lapply(seq_along(values), function(i) values[[i]])
  if (any(!vapply(values, value_type, logical(1), type = type))) stop("Malformed registry keys", call. = FALSE)
  if (is.list(aliases)) {
    if (any(!vapply(aliases, value_type, logical(1), type = "string"))) stop("Invalid registry aliases", call. = FALSE)
    aliases <- vapply(aliases, identity, character(1))
  }
  if (!is.character(aliases) || (length(aliases) && (is.null(names(aliases)) ||
      anyNA(names(aliases)) || any(!nzchar(names(aliases))) || anyDuplicated(names(aliases)) ||
      anyNA(aliases) || any(!nzchar(aliases))))) stop("Invalid registry aliases", call. = FALSE)
  if (type != "string" && (matching == "name" || length(aliases))) {
    stop("Name matching and aliases require string fields", call. = FALSE)
  }
  exact <- table(vapply(values, registry_key, character(1)))
  normalized <- if (matching == "name") table(normalized_name_key(vapply(values, identity, character(1)))) else integer()
  resolve <- function(value, seen = character()) {
    count <- registry_count(exact, registry_key(value))
    if (count > 0L) return(count)
    if (value %in% seen) stop("Cyclic registry alias: ", value, call. = FALSE)
    if (value %in% names(aliases)) return(resolve(aliases[[value]], c(seen, value)))
    if (matching == "name") registry_count(normalized, normalized_name_key(value)) else 0L
  }
  alias_counts <- vapply(names(aliases), resolve, integer(1))
  structure(list(type = type, matching = matching, aliases = aliases, exact = exact,
    normalized = normalized, alias_counts = alias_counts), class = "validation_registry")
}

#' Count registry matches for submitted values
#' @param index Prepared lookup from [`registry_index()`][form.validation::registry_index].
#' @param values Vector or list of submitted scalars of the index's declared type.
#' @return Integer match counts in input order. Missing values return zero;
#'   malformed non-missing values are errors. Exactly one match is valid.
#' @details Repeated input values share one lookup. Only unmatched names are
#'   normalized. Original submitted values are never modified.
#' @export
registry_match_counts <- function(index, values) {
  if (!inherits(index, "validation_registry")) stop("Use registry_index() first", call. = FALSE)
  values <- lapply(seq_along(values), function(i) values[[i]])
  present <- !vapply(values, null_value, logical(1))
  if (any(!vapply(values[present], value_type, logical(1), type = index$type))) {
    stop("Malformed submitted registry keys", call. = FALSE)
  }
  result <- integer(length(values))
  values <- values[present]
  keys <- vapply(values, registry_key, character(1))
  first <- !duplicated(keys)
  distinct <- keys[first]
  counts <- registry_count(index$exact, distinct)
  if (index$type == "string") {
    submitted <- vapply(values[first], identity, character(1))
    aliased <- counts == 0L & submitted %in% names(index$aliases)
    counts[aliased] <- registry_count(index$alias_counts, submitted[aliased])
    fallback <- counts == 0L & !aliased & index$matching == "name"
    counts[fallback] <- registry_count(index$normalized, normalized_name_key(submitted[fallback]))
  }
  result[present] <- counts[match(keys, distinct)]
  result
}
