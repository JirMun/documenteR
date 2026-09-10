#' Append every prefixed object a script creates
#'
#' @description
#' Scans a script for top-level assignments whose names carry a known prefix
#' (`plt_`, `tab_`, `data_`, `stat_`) and appends the corresponding objects
#' from your workspace. Useful when a naming convention is already in place
#' and writing an `append_*()` call per object would be busywork.
#'
#' @details
#' The trade-off is documentation: objects picked up this way get only a
#' title (their name) and a timestamp. Anything you want described,
#' attributed or sized needs its own `append_*()` call. A reasonable middle
#' course is to call this first and then re-append the outputs that matter
#' with full documentation — `append_*()` replaces an existing entry of the
#' same name, so ordering is not a problem.
#'
#' Objects are read from `env` (the caller's environment by default), not
#' from the script — the script is only read to find out *which* names to
#' look for. It is never sourced or evaluated.
#'
#' @param code_location Path to one or more `.R` files to scan.
#' @param output_list The collection to append to. See [append_plot()].
#' @param prefixes Named character vector mapping output type to name prefix.
#' @param exclude Output types to skip. Defaults to `c("data", "stats")`,
#'   on the grounds that datasets and headline numbers deserve real
#'   documentation.
#' @param skip_appended Leave objects already in the collection untouched.
#' @param env Environment to look the objects up in.
#' @param quiet Suppress per-object messages.
#'
#' @return The collection, invisibly.
#' @seealso [append_plot()], [save_outputs()]
#' @export
#' @examples
#' script <- tempfile(fileext = ".R")
#' writeLines(c("plt_a <- function() plot(1:10)", "tab_b <- head(cars)"), script)
#' plt_a <- function() plot(1:10)
#' tab_b <- head(cars)
#'
#' out <- init_outputs(default = FALSE, quiet = TRUE)
#' append_from_script(script, output_list = out, quiet = TRUE)
#' names(out$plots)
#' names(out$tables)
append_from_script <- function(code_location,
                               output_list = NULL,
                               prefixes = c(data = "data_", plots = "plt_",
                                            tables = "tab_", stats = "stat_"),
                               exclude = c("data", "stats"),
                               skip_appended = TRUE,
                               env = parent.frame(),
                               quiet = NULL) {
  quiet <- quiet %||% getOption("documenteR.quiet", FALSE)
  store <- resolve_outputs(output_list, env = parent.frame())

  code_location <- as.character(code_location)
  missing_files <- code_location[!file.exists(code_location)]
  if (length(missing_files)) {
    cli::cli_abort(c(
      "{cli::qty(length(missing_files))}Script{?s} not found: {.path {missing_files}}."
    ))
  }

  unknown <- setdiff(names(prefixes), dr_output_types())
  if (length(unknown)) {
    cli::cli_abort(c(
      "{.arg prefixes} names must be output types.",
      "x" = "Unknown: {.val {unknown}}.",
      "i" = "Valid: {.val {dr_output_types()}}."
    ))
  }
  bad_prefix <- prefixes[!grepl("^[A-Za-z0-9._]+$", prefixes)]
  if (length(bad_prefix)) {
    cli::cli_abort(c(
      "Prefixes must be made of letters, digits, {.val _} and {.val .}.",
      "x" = "Offending: {.val {bad_prefix}}."
    ))
  }

  lines <- unlist(lapply(code_location, readLines, warn = FALSE, encoding = "UTF-8"))
  appenders <- list(data = append_data, plots = append_plot,
                    tables = append_table, stats = append_stats)

  types <- setdiff(names(prefixes), exclude)
  appended <- character(0)

  for (ty in types) {
    prefix <- prefixes[[ty]]
    # Top-level assignment only: the name must start the line, so a mention
    # inside a function body or a comment is not picked up.
    pattern <- paste0("^(", prefix, "[A-Za-z0-9._]*)[[:space:]]*(<-|=[^=])")
    # Take the captured name via regmatches(): sub() would only replace the
    # part of the line the pattern matched, leaving the right-hand side of the
    # assignment attached to the name.
    matched <- regmatches(lines, regexec(pattern, lines))
    hits <- vapply(
      matched,
      function(m) if (length(m) >= 2L) m[[2L]] else NA_character_,
      character(1)
    )
    hits <- unique(hits[!is.na(hits)])
    if (!length(hits)) next

    if (isTRUE(skip_appended)) {
      hits <- setdiff(hits, names(store[[ty]]))
    }

    exists_in_env <- vapply(hits, function(nm) {
      !is.null(get0(nm, envir = env, inherits = TRUE))
    }, logical(1))
    if (any(!exists_in_env)) {
      cli::cli_warn(c(
        "!" = "{sum(!exists_in_env)} {ty} object{?s} named in the script {?is/are} not in the environment and {?was/were} skipped.",
        "*" = "{.val {hits[!exists_in_env]}}",
        "i" = "Run the script first, or pass {.arg env}."
      ))
      hits <- hits[exists_in_env]
    }

    for (nm in hits) {
      obj <- get0(nm, envir = env, inherits = TRUE)
      res <- tryCatch(
        {
          appenders[[ty]](obj, object_name = nm, output_list = store, quiet = TRUE)
          TRUE
        },
        error = function(e) {
          cli::cli_warn(c(
            "!" = "Could not append {.val {nm}} as {ty}: {conditionMessage(e)}"
          ))
          FALSE
        }
      )
      if (isTRUE(res)) appended <- c(appended, paste0(ty, "/", nm))
    }
  }

  if (!quiet) {
    if (length(appended)) {
      cli::cli_alert_success("Appended {length(appended)} object{?s} found in the script:")
      cli::cli_ul(appended)
    } else {
      cli::cli_alert_info("Nothing new to append.")
    }
  }
  invisible(store)
}
