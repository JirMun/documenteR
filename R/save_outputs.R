#' Export everything in an output collection
#'
#' @description
#' Writes a complete, self-describing export of a collection into a new
#' version folder: the files themselves, a README per output type, a
#' machine-readable manifest with checksums, a variable codebook for the
#' data, a typeset PDF catalogue of the plots, a copy of the code, and a log
#' of everything that happened along the way.
#'
#' @section What gets written:
#' ```
#' <output_dir>/0_4/
#'   00_DOC.txt         version number, name, notes, session info
#'   00_LOG.txt         every message and warning raised during the export
#'   00_MANIFEST.csv    one row per file: object, documentation, size, MD5
#'   00_MANIFEST.json   the same, plus export metadata
#'   01_output/         the collection object and a copy of the code
#'   02_data/           datasets + 00_codebook.csv + 00_README.txt
#'   03_plots/          plots + 00_01_catalogue.pdf + 00_README.txt
#'   04_tables/         tables + 00_README.txt
#'   05_stats/          01_stats.csv + 00_README.txt
#' ```
#' Folders are only created for output types that contain something.
#'
#' @section Console output and the log:
#' Messages and warnings raised by other packages during an export are
#' captured into `00_LOG.txt` rather than printed, so the progress report
#' stays readable. Warnings are then summarised at the end — nothing is
#' silently dropped. A single output that cannot be written is recorded as
#' `failed` in the manifest and the export continues, so one broken plot does
#' not cost you the other forty files.
#'
#' @param output_dir Project output folder. A version subfolder is created
#'   inside it.
#' @param output_list The collection to export. Defaults to `list_outputs` if
#'   visible, otherwise the collection created by the last [init_outputs()].
#' @param project_title Project title recorded in `00_DOC.txt`. If empty, the
#'   title of the previous version is carried forward.
#' @param version_notes What changed in this version. Worth filling in.
#' @param new_version If `TRUE`, bump the major version (`0_3` becomes
#'   `1_0`); otherwise bump the minor version (`0_3` becomes `0_4`).
#' @param update_latest_version If `TRUE`, overwrite the most recent version
#'   folder instead of creating a new one. For fixing a typo, not for new
#'   results.
#' @param version_name Override the generated version name.
#' @param exclude Output types to leave out, e.g. `c("data")`. Their folder
#'   still gets a README recording that they were excluded, so the omission is
#'   documented rather than invisible.
#' @param code_location Paths to script files to copy into the export. Give
#'   every script the analysis needs, in run order.
#' @param catalogue Build the PDF plot catalogue. See [dr_catalogue_pars()].
#' @param rerun_code If `TRUE`, run `code_location` in a clean R subprocess
#'   first and refuse to export if it fails. See [dr_check_code()].
#' @param folders Folder names per output type. See [dr_folders()].
#' @param dry_run If `TRUE`, report exactly what would be written and write
#'   nothing.
#' @param quiet Suppress progress output. The log is still written.
#'
#' @return A `documenteR_export` object (invisibly): a list with the version,
#'   the path written to, the manifest and the log.
#' @seealso [init_outputs()], [append_plot()], [dr_versions()],
#'   [dr_manifest()], [dr_verify()], [dr_log()]
#' @export
#' @examples
#' out <- init_outputs(default = FALSE, quiet = TRUE)
#' data_cars <- cars
#' append_data(data_cars, output_list = out, desc = "Built-in cars data",
#'             quiet = TRUE)
#' plt_cars <- function() plot(cars, pch = 19)
#' append_plot(plt_cars, output_list = out, desc = "Speed vs distance",
#'             quiet = TRUE)
#'
#' dir <- file.path(tempdir(), "documenteR_example")
#' res <- save_outputs(dir, out, project_title = "Example", quiet = TRUE)
#' res$version
#' list.files(res$path)
save_outputs <- function(output_dir,
                         output_list = NULL,
                         project_title = "",
                         version_notes = "",
                         new_version = FALSE,
                         update_latest_version = FALSE,
                         version_name = NULL,
                         exclude = character(0),
                         code_location = character(0),
                         catalogue = TRUE,
                         rerun_code = FALSE,
                         folders = dr_folders(),
                         dry_run = FALSE,
                         quiet = NULL) {
  quiet <- quiet %||% getOption("documenteR.quiet", FALSE)
  store <- resolve_outputs(output_list, env = parent.frame())

  if (missing(output_dir) || is_blank(output_dir)) {
    cli::cli_abort("{.arg output_dir} is required: it is where the version folders go.")
  }
  output_dir <- gsub("\\\\", "/", as.character(output_dir)[1])

  exclude <- as.character(exclude)
  unknown <- setdiff(exclude[nzchar(exclude)], c(dr_output_types(), "output"))
  if (length(unknown)) {
    cli::cli_abort(c(
      "Cannot exclude {.val {unknown}}.",
      "i" = "Excludable: {.val {c('output', dr_output_types())}}."
    ))
  }

  present <- dr_output_types()[
    vapply(dr_output_types(), function(ty) length(store[[ty]]) > 0L, logical(1))
  ]
  if (!length(present)) {
    cli::cli_warn(c(
      "!" = "The collection is empty; only version documentation will be written.",
      "i" = "Append outputs with {.fun append_plot}, {.fun append_data}, {.fun append_table} or {.fun append_stats}."
    ))
  }

  # -- dry run -------------------------------------------------------------
  if (isTRUE(dry_run)) {
    report <- build_dry_run(store, output_dir, present, exclude, catalogue)
    if (!quiet) print(report)
    return(invisible(report))
  }

  # -- optional reproducibility check --------------------------------------
  if (isTRUE(rerun_code)) {
    check <- dr_check_code(code_location, quiet = quiet)
    if (!isTRUE(check$ok)) {
      cli::cli_abort(c(
        "Re-running the code in a clean session failed; nothing was exported.",
        "x" = "{check$failed} of {check$total} script{?s} errored.",
        "i" = "See the output above, or pass {.code rerun_code = FALSE} to export anyway."
      ))
    }
  }

  log <- new_dr_log(paste0("save_outputs(", output_dir, ")"))
  started <- Sys.time()

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  if (!dir.exists(output_dir)) {
    cli::cli_abort(c(
      "Could not create {.path {output_dir}}.",
      "i" = "Check the drive is mounted and you have write permission."
    ))
  }

  # -- version folder ------------------------------------------------------
  if (isTRUE(update_latest_version)) {
    existing <- scan_versions(output_dir)
    if (!nrow(existing)) {
      cli::cli_abort(c(
        "{.code update_latest_version = TRUE} but {.path {output_dir}} has no versions yet.",
        "i" = "Run once with the default {.code update_latest_version = FALSE}."
      ))
    }
    version <- existing$version[nrow(existing)]
    dir_version <- existing$path[nrow(existing)]
    version_doc <- NULL
    if (!quiet) cli::cli_alert_info("Updating existing version {.strong {version}} in place.")
  } else {
    version_doc <- get_version_DOC(
      output_dir = output_dir,
      project_title = project_title,
      version_notes = version_notes,
      new_version = new_version,
      version_name = version_name
    )
    version <- version_doc$version
    dir_version <- path_join(output_dir, version)
    dir.create(dir_version, showWarnings = FALSE, recursive = TRUE)
    write_utf8(version_doc$text, path_join(dir_version, "00_DOC.txt"))
  }

  project_label <- paste(c(
    if (!is_blank(project_title)) project_title,
    paste0("v", version)
  ), collapse = " \u00B7 ")

  if (!quiet) {
    cli::cli_h2("Exporting to {.path {dir_version}}")
    if (!is.null(version_doc) && nzchar(version_doc$name)) {
      cli::cli_text("{.emph version name} {version_doc$name}")
    }
  }

  subdirs <- folder_setup(store, dir_version, folders)
  manifest <- empty_manifest()

  # -- the collection object and the code ----------------------------------
  if ("output" %in% exclude) {
    write_utf8(
      c(paste0("saved: ", dr_timestamp()), "", "EXCLUDED FROM THIS UPLOAD"),
      path_join(subdirs[["output"]], "00_README.txt")
    )
  } else {
    with_dr_log(log, {
      write_utf8(
        get_output_README(store, "output"),
        path_join(subdirs[["output"]], "00_README.txt")
      )
      rds <- path_join(subdirs[["output"]], "01_list_outputs.rds")
      # Saved as a plain list rather than the environment, so it can be read
      # back by any R session and by as_outputs() without this package.
      saveRDS(as.list(store), file = rds)
      manifest <- rbind(manifest, manifest_row(
        "output", "list_outputs", rds, ".rds", "ok", NA_character_, store$gpars
      ))
      manifest <- rbind(manifest, copy_code(code_location, subdirs[["output"]], store$gpars, log))
    }, where = "output", quiet = quiet)
  }

  # -- each output type ----------------------------------------------------
  savers <- list(
    data = function(dir) save_data(store, dir, log = log, quiet = quiet),
    plots = function(dir) {
      save_plots(store, dir, catalogue = catalogue, project = project_label,
                 log = log, quiet = quiet)
    },
    tables = function(dir) save_tables(store, dir, log = log, quiet = quiet),
    stats = function(dir) save_stats(store, dir, log = log, quiet = quiet)
  )

  for (ty in present) {
    dir_ty <- subdirs[[ty]]
    readme <- path_join(dir_ty, "00_README.txt")

    if (ty %in% exclude) {
      write_utf8(
        c(paste0("saved: ", dr_timestamp()), "", "EXCLUDED FROM THIS UPLOAD"),
        readme
      )
      if (!quiet) cli::cli_alert_info("Skipped {.field {ty}} (excluded).")
      next
    }

    if (!quiet) cli::cli_alert("Saving {.field {ty}} ({length(store[[ty]])})")

    with_dr_log(log, {
      write_utf8(get_output_README(store, ty), readme)
    }, where = paste0(ty, "/README"), quiet = quiet)

    rows <- savers[[ty]](dir_ty)
    if (!is.null(rows) && nrow(rows)) manifest <- rbind(manifest, rows)
  }

  # -- manifest and log ----------------------------------------------------
  meta <- list(
    project_title = if (is_blank(project_title)) NA_character_ else project_title,
    version = version,
    version_name = if (is.null(version_doc)) NA_character_ else version_doc$name,
    version_notes = version_notes,
    saved = dr_timestamp(started),
    documenteR = as.character(utils::packageVersion("documenteR")),
    r_version = R.version.string
  )
  full_manifest <- enrich_manifest(manifest, store, dir_version)
  with_dr_log(log, {
    write_manifest(full_manifest, dir_version, meta = meta, gpars = store$gpars)
  }, where = "manifest", quiet = quiet)

  counts <- log_report(log, quiet = quiet)
  log_write(
    log,
    path_join(dir_version, "00_LOG.txt"),
    header = c(
      paste0("version : ", version),
      paste0("files   : ", nrow(full_manifest)),
      paste0("failed  : ", sum(full_manifest$status != "ok"))
    )
  )

  n_ok <- sum(full_manifest$status == "ok")
  n_bad <- sum(full_manifest$status != "ok")
  if (!quiet) {
    if (n_bad == 0L) {
      cli::cli_alert_success(
        "Version {.strong {version}}: {n_ok} file{?s} written to {.path {dir_version}}"
      )
    } else {
      cli::cli_alert_warning(
        "Version {.strong {version}}: {n_ok} file{?s} written, {n_bad} failed. See {.path 00_LOG.txt}."
      )
    }
  }

  invisible(structure(
    list(
      version = version,
      path = dir_version,
      version_name = meta$version_name,
      manifest = full_manifest,
      log = log,
      n_written = n_ok,
      n_failed = n_bad,
      warnings = unname(counts[["warnings"]])
    ),
    class = "documenteR_export"
  ))
}

# print.documenteR_export() lives in print.R.


# Helpers ---------------------------------------------------------------------

# Copy the analysis scripts into the export, numbered in the order given.
copy_code <- function(code_location, dir, gpars, log) {
  code_location <- as.character(code_location)
  code_location <- code_location[nzchar(code_location) & !is.na(code_location)]
  if (!length(code_location)) return(empty_manifest())

  missing <- code_location[!file.exists(code_location)]
  if (length(missing)) {
    cli::cli_warn(c(
      "!" = "{length(missing)} file{?s} in {.arg code_location} do{?es/} not exist and {?was/were} skipped.",
      "*" = "{.path {missing}}"
    ))
  }
  code_location <- code_location[file.exists(code_location)]
  if (!length(code_location)) return(empty_manifest())

  rows <- lapply(seq_along(code_location), function(i) {
    src <- code_location[i]
    target <- path_join(dir, paste0("02_", pad_id(i), "_", sanitise_filename(basename(src))))
    res <- try_item(log, {
      lines <- readLines(src, warn = FALSE, encoding = "UTF-8")
      write_utf8(c(paste0("# copied by documenteR: ", dr_timestamp(), " from ", src), lines), target)
    }, where = paste0("output/code/", basename(src)))
    if (item_failed(res)) {
      manifest_row("output", basename(src), target, ".R", "failed", res$message, gpars)
    } else {
      manifest_row("output", basename(src), target, ".R", "ok", NA_character_, gpars)
    }
  })
  do.call(rbind, rows)
}

# Build the dry-run report. Display is left entirely to
# print.documenteR_dry_run(), so what save_outputs() shows and what you see
# when you print a stored report can never drift apart.
build_dry_run <- function(store, output_dir, present, exclude, catalogue) {
  vers <- scan_versions(output_dir)
  next_version <- if (nrow(vers)) {
    paste0(vers$major[nrow(vers)], "_", vers$minor[nrow(vers)] + 1L)
  } else {
    "0_1"
  }

  planned <- list()
  for (ty in present) {
    if (ty %in% exclude) {
      # NA marks "excluded" as distinct from "no files".
      planned[[ty]] <- NA_character_
      next
    }
    plan <- plan_outputs(store[[ty]], store$gpars, ty)
    files <- unlist(lapply(seq_len(nrow(plan)), function(i) {
      paste0(path_join(plan$dir[i], plan$stem[i]), split_spec(plan$formats[i]))
    }))
    if (identical(ty, "stats")) {
      files <- paste0("01_stats", split_spec(store$gpars$formats$stats %||% ".csv"))
    }
    if (identical(ty, "plots") && isTRUE(catalogue)) {
      files <- c(files, "00_01_catalogue.pdf")
    }
    if (identical(ty, "data") && isTRUE(store$gpars$codebook %||% TRUE)) {
      files <- c(files, "00_codebook.csv")
    }
    planned[[ty]] <- files
  }

  structure(
    list(
      version = next_version,
      path = path_join(output_dir, next_version),
      planned = planned
    ),
    class = "documenteR_dry_run"
  )
}


#' Run analysis scripts in a clean R session
#'
#' @description
#' Sources each script in a fresh R subprocess, in the order given, and
#' reports which ones failed. A cheap reproducibility check: it catches the
#' script that only works because of an object left over in your workspace.
#'
#' @details
#' The subprocess has the environment variable `DOCUMENTER_RERUN` set to
#' `"1"`. Guard the export at the end of a script with [dr_is_rerun()] so the
#' check does not itself write an export:
#'
#' ```r
#' if (!dr_is_rerun()) {
#'   save_outputs(output_dir, list_outputs)
#' }
#' ```
#'
#' @param code_location Paths to `.R` files, in run order.
#' @param quiet Suppress progress output.
#'
#' @return A list with `ok`, `total`, `failed` and a `results` data frame,
#'   invisibly.
#' @seealso [save_outputs()], [dr_is_rerun()]
#' @export
dr_check_code <- function(code_location, quiet = FALSE) {
  code_location <- as.character(code_location)
  code_location <- code_location[nzchar(code_location) & !is.na(code_location)]
  exists <- file.exists(code_location)
  if (!length(code_location) || !any(exists)) {
    cli::cli_abort(c(
      "No existing {.field .R} files were given in {.arg code_location}.",
      "i" = "A reproducibility check needs the scripts to run."
    ))
  }
  if (any(!exists)) {
    cli::cli_warn("Skipping {sum(!exists)} missing file{?s}: {.path {code_location[!exists]}}")
    code_location <- code_location[exists]
  }

  rscript <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )
  old_flag <- Sys.getenv("DOCUMENTER_RERUN", unset = NA)
  Sys.setenv(DOCUMENTER_RERUN = "1")
  on.exit({
    if (is.na(old_flag)) Sys.unsetenv("DOCUMENTER_RERUN") else Sys.setenv(DOCUMENTER_RERUN = old_flag)
  }, add = TRUE)

  results <- lapply(code_location, function(src) {
    if (!quiet) cli::cli_alert("Running {.path {basename(src)}} in a clean session")
    driver <- tempfile("documenteR_rerun_", fileext = ".R")
    on.exit(unlink(driver), add = TRUE)
    write_utf8(
      sprintf('source("%s", encoding = "UTF-8")', gsub("\\\\", "/", normalizePath(src, winslash = "/"))),
      driver
    )
    out <- suppressWarnings(system2(
      rscript, args = c("--vanilla", shQuote(driver)),
      stdout = TRUE, stderr = TRUE
    ))
    status <- attr(out, "status") %||% 0L
    if (status != 0L && !quiet) {
      cli::cli_alert_danger("{.path {basename(src)}} failed:")
      cli::cli_verbatim(utils::tail(out, 15L))
    }
    data.frame(file = src, status = as.integer(status),
               ok = status == 0L, stringsAsFactors = FALSE)
  })
  results <- do.call(rbind, results)
  failed <- sum(!results$ok)

  if (!quiet) {
    if (failed == 0L) {
      cli::cli_alert_success("All {nrow(results)} script{?s} ran cleanly.")
    } else {
      cli::cli_alert_danger("{failed} of {nrow(results)} script{?s} failed.")
    }
  }
  invisible(list(ok = failed == 0L, total = nrow(results), failed = failed,
                 results = results))
}

#' Is this session a documenteR reproducibility re-run?
#'
#' @description
#' `TRUE` when the current R session was started by [dr_check_code()]. Use it
#' to skip the export step when a script is being re-run purely as a check.
#'
#' @return A logical scalar.
#' @seealso [dr_check_code()]
#' @export
#' @examples
#' dr_is_rerun()
dr_is_rerun <- function() {
  nzchar(Sys.getenv("DOCUMENTER_RERUN"))
}
