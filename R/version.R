# Version handling ------------------------------------------------------------
#
# A project output folder holds one folder per export, named "<major>_<minor>".
# save_outputs() adds a minor version by default and a major one on request.

.dr_version_pattern <- "^([0-9]+)_([0-9]+)$"

# All version folders present, as a data frame ordered oldest first.
scan_versions <- function(output_dir) {
  empty <- data.frame(
    version = character(0), major = integer(0), minor = integer(0),
    path = character(0), stringsAsFactors = FALSE
  )
  if (!dir.exists(output_dir)) return(empty)

  dirs <- list.dirs(output_dir, full.names = FALSE, recursive = FALSE)
  dirs <- dirs[grepl(.dr_version_pattern, dirs)]
  if (!length(dirs)) return(empty)

  major <- as.integer(sub(.dr_version_pattern, "\\1", dirs))
  minor <- as.integer(sub(.dr_version_pattern, "\\2", dirs))
  ord <- order(major, minor)

  data.frame(
    version = dirs[ord],
    major = major[ord],
    minor = minor[ord],
    path = path_join(output_dir, dirs[ord]),
    stringsAsFactors = FALSE
  )
}

# Pull a "key: value" field out of a 00_DOC.txt.
doc_field <- function(lines, field) {
  hit <- grep(paste0("^", field, ": "), lines)
  if (!length(hit)) return(NA_character_)
  sub(paste0("^", field, ": "), "", lines[hit[1]])
}

#' List the versions of an export
#'
#' @description
#' Reads the `00_DOC.txt` of every version folder in an output directory and
#' returns one row per version. Useful for finding out what changed when, or
#' which version a figure in a report came from.
#'
#' @param output_dir The project output folder passed to [save_outputs()].
#'
#' @return A data frame with columns `version`, `major`, `minor`,
#'   `project_title`, `version_name`, `notes`, `saved` and `path`. Zero rows
#'   if nothing has been exported yet.
#' @seealso [save_outputs()], [dr_log()]
#' @export
#' @examples
#' dr_versions(tempdir())
dr_versions <- function(output_dir) {
  vers <- scan_versions(output_dir)
  if (!nrow(vers)) return(cbind(vers, data.frame(
    project_title = character(0), version_name = character(0),
    notes = character(0), saved = character(0), stringsAsFactors = FALSE
  )))

  meta <- lapply(vers$path, function(p) {
    doc <- path_join(p, "00_DOC.txt")
    if (!file.exists(doc)) {
      return(data.frame(
        project_title = NA_character_, version_name = NA_character_,
        notes = NA_character_, saved = NA_character_, stringsAsFactors = FALSE
      ))
    }
    lines <- readLines(doc, warn = FALSE, encoding = "UTF-8")
    data.frame(
      project_title = doc_field(lines, "project title"),
      version_name = doc_field(lines, "version name"),
      notes = doc_field(lines, "version notes"),
      saved = doc_field(lines, "saved"),
      stringsAsFactors = FALSE
    )
  })
  out <- cbind(vers, do.call(rbind, meta))
  rownames(out) <- NULL
  out
}

# Turn "the output folder" plus an optional version into a concrete path,
# accepting either the project folder or a version folder directly.
resolve_version_dir <- function(output_dir, version = NULL) {
  if (!is.null(version)) {
    candidate <- path_join(output_dir, version)
    if (dir.exists(candidate)) return(candidate)
    cli::cli_abort(c(
      "Version {.val {version}} not found in {.path {output_dir}}.",
      "i" = "Available: {.val {scan_versions(output_dir)$version}}."
    ))
  }
  if (grepl(.dr_version_pattern, basename(output_dir)) && dir.exists(output_dir)) {
    return(output_dir)
  }
  vers <- scan_versions(output_dir)
  if (!nrow(vers)) {
    cli::cli_abort("No version folders found in {.path {output_dir}}.")
  }
  vers$path[nrow(vers)]
}


# Verbal version names --------------------------------------------------------
#
# Kept from the original because a memorable name makes "which version was
# that figure from?" a conversation people can actually have. The word lists
# are Czech and deliberately silly; replace them by passing your own `parts`.

.dr_name_parts <- list(
  whose = c("Na\u0161i", "Roztomil\u00ED", "Po\u0165ouchl\u00ED", "Vysm\u00E1t\u00ED",
            "Zv\u00EDdav\u00ED", "Hedv\u00E1bn\u00ED"),
  who = c("u\u0161\u00E1\u010Dci", "hop\u00E1lci", "z\u00E1jov\u00E9", "zaj\u00ED\u010Dci",
          "zajdul\u00E1nci", "kr\u00E1li\u010D\u00ED kamar\u00E1di", "kr\u00E1l\u00ED\u010Dci",
          "bident\u00E1ln\u00ED pr\u0165ousov\u00E9"),
  named = c("Karotka", "Popelka", "Miniz\u00E1ja", "Prezident Z\u00E1j\u00EDnek",
            "D\u011Bda Zaj\u00EDc"),
  how = c("r\u00E1di", "s oblibou", "p\u0159\u00EDle\u017Eitostn\u011B", "vesele"),
  what = c(
    "jezd\u00ED na kole",
    "absolvuj\u00ED \u0161kolen\u00ED o vytv\u00E1\u0159en\u00ED bezpe\u010Dn\u00E9ho pracovn\u00EDho prost\u0159ed\u00ED",
    "slav\u00ED Den veter\u00E1n\u016F zaje\u010D\u00EDch v\u00E1lek",
    "nos\u00ED legra\u010Dn\u00ED \u010Depice",
    "okusujou tenisky",
    "provozuj\u00ED st\u00E1nek s limon\u00E1dou",
    "volaj\u00ED telefonem z mrkve",
    "pou\u017E\u00EDvaj\u00ED moj\u00ED kreditku k n\u00E1kup\u016Fm na internetu",
    "okusuj\u00ED om\u00EDtku",
    "jezd\u00ED elektrick\u00FDm aut\u00ED\u010Dkem",
    "komentuj\u00ED krizi pozdn\u00EDho kapitalismu teoretickou optikou Frankfurtsk\u00E9 \u0161koly",
    "papaj\u00ED ban\u00E1nek"
  )
)

#' Generate a memorable name for a version
#'
#' @description
#' Builds a short whimsical sentence used as a version's human-readable name,
#' avoiding names already used in the same project.
#'
#' @param used Character vector of names already taken.
#' @param parts A list of word vectors with components `whose`, `who`,
#'   `named`, `how` and `what`. Defaults to the built-in Czech word lists;
#'   supply your own to change the flavour.
#' @param max_tries How many times to retry before giving up on uniqueness.
#'
#' @return A single string. Falls back to a timestamp-based name if the
#'   vocabulary is exhausted, so this function never loops indefinitely.
#' @seealso [save_outputs()]
#' @export
#' @examples
#' set.seed(1)
#' dr_version_name()
dr_version_name <- function(used = character(0),
                            parts = .dr_name_parts,
                            max_tries = 200L) {
  needed <- c("whose", "who", "named", "how", "what")
  if (!all(needed %in% names(parts)) ||
      any(vapply(parts[needed], length, integer(1)) == 0L)) {
    cli::cli_abort("{.arg parts} must have non-empty {.field {needed}} components.")
  }
  n_named <- min(3L, length(parts$named))

  one <- function() {
    who2 <- sample(parts$named, n_named, replace = FALSE)
    joined <- if (n_named >= 2L) {
      paste0(
        paste(utils::head(who2, n_named - 1L), collapse = ", "),
        " a ", who2[n_named]
      )
    } else {
      who2[1]
    }
    paste0(
      paste(sample(parts$whose, 1), sample(parts$who, 1), "-", joined, "-",
            sample(parts$how, 1), sample(parts$what, 1)),
      "."
    )
  }

  for (i in seq_len(max_tries)) {
    candidate <- one()
    if (!candidate %in% used) return(candidate)
  }
  # The original spun forever here once the vocabulary ran out.
  cli::cli_warn(c(
    "!" = "Could not find an unused version name after {max_tries} tries.",
    "i" = "Falling back to a timestamped name; extend {.arg parts} for more variety."
  ))
  paste0("Verze ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ".")
}


#' Build the documentation text for a new version
#'
#' @description
#' Works out the next version number, invents a name for it, and assembles the
#' text written to `00_DOC.txt`. Called by [save_outputs()]; exported mainly
#' so the version-numbering rule is inspectable.
#'
#' @param output_dir The project output folder.
#' @param project_title Project title. If empty, the title of the most recent
#'   version is carried forward.
#' @param version_notes Free text describing what changed in this version.
#' @param new_version If `TRUE`, bump the major version and reset the minor
#'   one (`0_3` becomes `1_0`); otherwise bump the minor version (`0_3`
#'   becomes `0_4`).
#' @param version_name Supply a name instead of generating one.
#'
#' @return A list with `version` (e.g. `"0_4"`), `name` and `text`.
#' @seealso [save_outputs()], [dr_versions()]
#' @export
#' @examples
#' d <- file.path(tempdir(), "vers_demo")
#' dir.create(d, showWarnings = FALSE)
#' v <- get_version_DOC(d, project_title = "Demo", version_notes = "First")
#' v$version
get_version_DOC <- function(output_dir,
                            project_title = "",
                            version_notes = "",
                            new_version = FALSE,
                            version_name = NULL) {
  existing <- dr_versions(output_dir)

  if (nrow(existing)) {
    last <- existing[nrow(existing), ]
    major <- last$major
    minor <- last$minor
    prior_titles <- existing$project_title[!is.na(existing$project_title) &
                                             nzchar(existing$project_title)]
    carried_title <- if (length(prior_titles)) utils::tail(prior_titles, 1) else ""
    used_names <- stats::na.omit(existing$version_name)
  } else {
    major <- 0L
    minor <- 0L
    carried_title <- ""
    used_names <- character(0)
  }

  if (isTRUE(new_version)) {
    major <- major + 1L
    minor <- 0L
  } else {
    minor <- minor + 1L
  }
  version <- paste0(major, "_", minor)

  if (is.null(version_name)) {
    version_name <- if (isTRUE(getOption("documenteR.version_names", TRUE))) {
      dr_version_name(used = as.character(used_names))
    } else {
      ""
    }
  }
  name <- as.character(version_name)[1]

  title <- if (is_blank(project_title)) carried_title else project_title

  text <- paste(
    c(
      paste0("saved: ", dr_timestamp()),
      "",
      paste0("project title: ", title),
      paste0("version: ", version),
      paste0("version name: ", name),
      "",
      paste0("version notes: ", version_notes),
      "",
      paste0("R version: ", R.version.string),
      paste0("platform: ", R.version$platform),
      paste0("documenteR: ", as.character(utils::packageVersion("documenteR"))),
      "",
      "session info:",
      utils::capture.output(utils::sessionInfo())
    ),
    collapse = "\n"
  )

  list(version = version, name = name, text = text)
}
