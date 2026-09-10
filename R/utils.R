# Small internal helpers ------------------------------------------------------
# Kept in base R on purpose: the package must not pull in the tidyverse, both
# to stay light and because attaching tidyverse packages inside an export step
# is what produced the noisy startup/conflict messages users complained about.

`%||%` <- function(x, y) if (is.null(x)) y else x

# NULL, NA and "" all mean "not supplied" for documentation fields.
is_blank <- function(x) {
  is.null(x) || length(x) == 0L || (length(x) == 1L && (is.na(x) || !nzchar(trimws(as.character(x)))))
}

# Drop NULL elements of a list (recursively at one level, which is all we need).
compact <- function(x) x[!vapply(x, is.null, logical(1))]

# Zero-pad an integer to a common width, minimum 2 (01, 02, ... 10).
pad_id <- function(i, width = NULL) {
  i <- as.integer(i)
  width <- width %||% max(2L, nchar(as.character(max(c(1L, i), na.rm = TRUE))))
  formatC(i, width = width, flag = "0", format = "d")
}

# A single UTC-ish local timestamp string, used throughout the documentation.
dr_timestamp <- function(time = Sys.time()) {
  format(time, "%Y-%m-%d %H:%M:%S", tz = Sys.timezone())
}

# Turn an arbitrary object name into something safe on every file system:
# transliterate diacritics, collapse whitespace and drop reserved characters.
# Czech object titles used to produce unopenable files on Windows shares.
sanitise_filename <- function(x, max_nchar = 80L) {
  x <- as.character(x)
  out <- vapply(x, function(one) {
    if (is.na(one)) return("NA")
    one <- translit(one)
    one <- gsub("[[:space:]]+", "_", trimws(one))
    one <- gsub("[^A-Za-z0-9._-]+", "_", one)
    one <- gsub("_{2,}", "_", one)
    one <- gsub("^[._-]+|[._-]+$", "", one)
    if (!nzchar(one)) one <- "unnamed"
    if (nchar(one) > max_nchar) one <- substr(one, 1L, max_nchar)
    one
  }, character(1), USE.NAMES = FALSE)
  out
}

# iconv() to ASCII is locale-dependent on Windows, so map the characters we
# actually meet (Czech, Slovak, German, Polish) explicitly and fall back to
# iconv for the rest.
translit <- function(x) {
  from <- "\u00e1\u010d\u010f\u00e9\u011b\u00ed\u0148\u00f3\u0159\u0161\u0165\u00fa\u016f\u00fd\u017e\u00e4\u00f6\u00fc\u00df\u0142\u0105\u0119\u0107\u0144\u015b\u017c\u017a\u00e5\u00f8\u00e6"
  to   <- c("a","c","d","e","e","i","n","o","r","s","t","u","u","y","z","a","o","u","ss","l","a","e","c","n","s","z","z","a","o","ae")
  chars <- strsplit(from, "")[[1]]
  for (i in seq_along(chars)) {
    x <- gsub(chars[i], to[i], x, fixed = TRUE)
    x <- gsub(toupper(chars[i]), toupper(to[i]), x, fixed = TRUE)
  }
  conv <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  ifelse(is.na(conv), x, conv)
}

# Split a "a;b;c" string (or a c("a","b") vector) into a clean character vector.
split_spec <- function(x, sep = ";") {
  if (is.null(x) || length(x) == 0L) return(character(0))
  out <- unlist(strsplit(as.character(x), sep, fixed = TRUE), use.names = FALSE)
  out <- trimws(out)
  out[nzchar(out)]
}

# Normalise a file extension to ".ext" in lower case.
normalise_ext <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- sub("^[.]*", ".", x)
  x
}

# Join path components with forward slashes and no doubled separators.
# Vectorised over its arguments, which are recycled to a common length: it is
# used both for single paths and for whole columns of them.
path_join <- function(...) {
  parts <- list(...)
  parts <- parts[lengths(parts) > 0L]
  if (!length(parts)) return(character(0))
  parts <- lapply(parts, function(p) {
    p <- as.character(p)
    p[is.na(p)] <- ""
    p
  })
  n <- max(lengths(parts))
  parts <- lapply(parts, rep_len, n)

  vapply(seq_len(n), function(i) {
    pieces <- vapply(parts, function(p) p[[i]], character(1))
    pieces <- pieces[nzchar(pieces)]
    if (!length(pieces)) return("")
    one <- paste(pieces, collapse = "/")
    # Preserve a leading "//" (a UNC share) while collapsing every other run.
    lead <- if (startsWith(one, "//")) "//" else ""
    paste0(lead, gsub("/{2,}", "/", substring(one, nchar(lead) + 1L)))
  }, character(1))
}

# Wrap text to a width, with an optional hanging indent, using base strwrap.
wrap_text <- function(x, width = 75L, indent = 0L, exdent = 0L) {
  if (is_blank(x)) return(character(0))
  x <- as.character(x)
  paste(strwrap(x, width = width, indent = indent, exdent = exdent), collapse = "\n")
}

# Convert length units to inches, which is what all graphics devices take.
to_inches <- function(x, units = c("cm", "in", "mm", "px"), dpi = 300) {
  units <- match.arg(units)
  switch(units,
    "in" = x,
    "cm" = x / 2.54,
    "mm" = x / 25.4,
    "px" = x / as.numeric(dpi)
  )
}

# Devices accept "retina"/"print"/"screen" as dpi; everything else is numeric.
resolve_dpi <- function(dpi) {
  if (is_blank(dpi)) return(300)
  if (is.numeric(dpi)) return(as.numeric(dpi))
  key <- tolower(as.character(dpi)[1])
  named <- c(retina = 320, print = 300, screen = 72)
  if (key %in% names(named)) return(unname(named[[key]]))
  num <- suppressWarnings(as.numeric(key))
  if (is.na(num)) 300 else num
}

# Write text as UTF-8 regardless of the session locale. writeLines(useBytes)
# on an explicitly UTF-8 connection is the only combination that behaves the
# same on Windows and on Linux.
write_utf8 <- function(text, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  text <- enc2utf8(paste(text, collapse = "\n"))
  writeLines(text, con = con, useBytes = TRUE)
  invisible(path)
}

# Named-vector helpers used by the documentation formatters -------------------

# A documentation field may be given as c(x = "Speed"), list(x = "Speed") or
# "Speed". Normalise all three to a named character vector.
as_named_chr <- function(x) {
  if (is.null(x) || length(x) == 0L) return(stats::setNames(character(0), character(0)))
  x <- unlist(x, use.names = TRUE)
  nms <- names(x) %||% rep("", length(x))
  nms[is.na(nms)] <- ""
  stats::setNames(as.character(x), nms)
}

# Collapse a named vector to "name = value; name = value", tolerating missing
# names on either side.
collapse_named <- function(x, sep = "; ", eq = " = ") {
  x <- as_named_chr(x)
  if (length(x) == 0L) return("")
  nms <- names(x)
  parts <- ifelse(nzchar(nms), paste0(nms, eq, unname(x)), unname(x))
  paste(parts, collapse = sep)
}

# Warn once about paths that Windows will refuse to open.
check_path_length <- function(paths, limit = getOption("documenteR.max_path", 250L)) {
  too_long <- paths[nchar(paths) > limit]
  if (length(too_long)) {
    cli::cli_warn(c(
      "!" = "{length(too_long)} output path{?s} exceed{?s/} {limit} characters.",
      "i" = "Windows cannot open paths longer than 260 characters.",
      "i" = "Shorten {.arg output_dir}, {.arg subfolder} or the object names.",
      "*" = "{.path {too_long[1]}}"
    ))
  }
  invisible(too_long)
}
