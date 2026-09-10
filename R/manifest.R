# Export manifest -------------------------------------------------------------
#
# A machine-readable index of everything an export produced. This is the piece
# that makes an output folder auditable: which file came from which object,
# what it was documented as, how big it is and what its checksum is - so a
# later version can be diffed against it and a corrupted transfer detected.

# Make manifest paths relative to the version folder, so a manifest stays
# valid when the export is moved or shared.
relativise <- function(paths, root) {
  # normalizePath(winslash = "/") already returns forward slashes on Windows,
  # so the relative path is a plain prefix strip - no pattern matching needed.
  root_norm <- normalizePath(root, winslash = "/", mustWork = FALSE)
  paths_norm <- normalizePath(paths, winslash = "/", mustWork = FALSE)
  prefix <- paste0(sub("/+$", "", root_norm), "/")
  rel <- ifelse(
    startsWith(paths_norm, prefix),
    substring(paths_norm, nchar(prefix) + 1L),
    basename(paths_norm)
  )
  # normalizePath() gives up on files that do not exist; fall back to the name.
  ifelse(nzchar(rel), rel, basename(paths))
}


# Attach the documentation of each output to its manifest rows, so the
# manifest alone answers "what is this file?".
enrich_manifest <- function(manifest, store, root) {
  if (is.null(manifest) || !nrow(manifest)) return(empty_manifest())

  doc_of <- function(type, name) {
    entry <- store[[type]][[name]]
    if (is.null(entry)) return(list())
    entry$documentation %||% list()
  }

  manifest$file <- relativise(manifest$file, root)
  manifest$title <- vapply(seq_len(nrow(manifest)), function(i) {
    as.character(doc_of(manifest$type[i], manifest$name[i])$title %||% NA_character_)[1]
  }, character(1))
  manifest$desc <- vapply(seq_len(nrow(manifest)), function(i) {
    as.character(doc_of(manifest$type[i], manifest$name[i])$desc %||% NA_character_)[1]
  }, character(1))
  manifest$source <- vapply(seq_len(nrow(manifest)), function(i) {
    d <- doc_of(manifest$type[i], manifest$name[i])
    if (is_blank(d$source)) NA_character_ else collapse_named(d$source)
  }, character(1))
  manifest$created <- vapply(seq_len(nrow(manifest)), function(i) {
    as.character(doc_of(manifest$type[i], manifest$name[i])$timestamp %||% NA_character_)[1]
  }, character(1))

  manifest[, c("type", "name", "title", "file", "format", "status",
               "bytes", "md5", "desc", "source", "created", "message")]
}

write_manifest <- function(manifest, dir, meta = list(), gpars = list()) {
  paths <- character(0)
  csv <- path_join(dir, "00_MANIFEST.csv")
  write_data_file(manifest, csv, ".csv", gpars = gpars, name = "manifest")
  paths <- c(paths, csv)

  if (requireNamespace("jsonlite", quietly = TRUE)) {
    json <- path_join(dir, "00_MANIFEST.json")
    payload <- c(meta, list(files = manifest))
    write_utf8(
      jsonlite::toJSON(payload, dataframe = "rows", auto_unbox = TRUE,
                       na = "null", pretty = TRUE),
      json
    )
    paths <- c(paths, json)
  }
  invisible(paths)
}

#' Read the manifest of a saved export
#'
#' @description
#' Returns the `00_MANIFEST.csv` of an export as a data frame: every file
#' written, which appended object it came from, its documentation, size and
#' MD5 checksum.
#'
#' @param output_dir The project output folder, or a specific version folder.
#' @param version Version to read, e.g. `"0_3"`. Defaults to the most recent.
#'
#' @return A data frame.
#' @seealso [save_outputs()], [dr_versions()]
#' @export
dr_manifest <- function(output_dir, version = NULL) {
  dir <- resolve_version_dir(output_dir, version)
  path <- path_join(dir, "00_MANIFEST.csv")
  if (!file.exists(path)) {
    cli::cli_abort(c(
      "No manifest found at {.path {path}}.",
      "i" = "Manifests are written by {.fun save_outputs} from version 0.9.0 onwards."
    ))
  }
  utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
}

#' Verify that a saved export is intact
#'
#' @description
#' Re-computes the MD5 checksum of every file in an export's manifest and
#' reports anything missing or changed. Worth running after copying an export
#' across a network share, or before handing it to someone else.
#'
#' @inheritParams dr_manifest
#'
#' @return A data frame of problems, invisibly; empty if everything checks
#'   out. A summary is printed.
#' @seealso [dr_manifest()], [save_outputs()]
#' @export
dr_verify <- function(output_dir, version = NULL) {
  dir <- resolve_version_dir(output_dir, version)
  man <- dr_manifest(dir)
  man <- man[man$status == "ok", , drop = FALSE]

  full <- path_join(dir, man$file)
  exists <- file.exists(full)
  current <- rep(NA_character_, length(full))
  current[exists] <- unname(tools::md5sum(full[exists]))

  problem <- ifelse(
    !exists, "missing",
    ifelse(!is.na(man$md5) & !is.na(current) & man$md5 != current, "changed", NA_character_)
  )
  bad <- data.frame(
    file = man$file, problem = problem,
    expected = man$md5, found = current,
    stringsAsFactors = FALSE
  )
  bad <- bad[!is.na(bad$problem), , drop = FALSE]

  if (!nrow(bad)) {
    cli::cli_alert_success("All {nrow(man)} file{?s} in {.path {basename(dir)}} match the manifest.")
  } else {
    cli::cli_alert_danger("{nrow(bad)} file{?s} do{?es/} not match the manifest:")
    for (i in seq_len(min(10L, nrow(bad)))) {
      cli::cli_bullets(c("x" = "{bad$problem[i]}: {.path {bad$file[i]}}"))
    }
    if (nrow(bad) > 10L) cli::cli_bullets(c(" " = "... and {nrow(bad) - 10L} more."))
  }
  invisible(bad)
}
