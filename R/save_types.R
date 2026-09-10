# Writing each output type ----------------------------------------------------
#
# Every save_* function here takes a resolved collection, a target folder, an
# export plan and a log, writes files, and returns manifest rows. They all
# route each individual file through try_item(), so a single failure is
# recorded and skipped instead of aborting the whole export.

# One manifest row per file written.
manifest_row <- function(type, name, file, format, status = "ok",
                         message = NA_character_, gpars = list()) {
  size <- if (identical(status, "ok") && file.exists(file)) file.size(file) else NA_real_
  hash <- if (isTRUE(gpars$hash %||% TRUE) && identical(status, "ok") && file.exists(file)) {
    unname(tools::md5sum(file))
  } else {
    NA_character_
  }
  data.frame(
    type = type, name = name, file = file, format = format,
    status = status, message = message, bytes = size, md5 = hash,
    stringsAsFactors = FALSE
  )
}

empty_manifest <- function() {
  data.frame(
    type = character(0), name = character(0), file = character(0),
    format = character(0), status = character(0), message = character(0),
    bytes = numeric(0), md5 = character(0), stringsAsFactors = FALSE
  )
}

# Shared driver for data and tables, which differ only in where the object
# lives on the entry and whether a codebook is produced.
save_rectangular <- function(entries, dir, plan, gpars, type, slot, log,
                             quiet = FALSE, progress = TRUE) {
  rows <- list()
  if (!nrow(plan)) return(empty_manifest())

  bar <- if (progress && !quiet && nrow(plan) > 1L) {
    cli::cli_progress_bar(
      paste0("Writing ", type), total = nrow(plan), .envir = environment()
    )
  } else {
    NULL
  }

  for (i in seq_len(nrow(plan))) {
    nm <- plan$name[i]
    entry <- entries[[nm]]
    obj <- entry[[slot]]
    stem <- path_join(dir, plan$dir[i], plan$stem[i])
    formats <- split_spec(plan$formats[i])

    for (fmt in formats) {
      target <- paste0(stem, fmt)
      check_path_length(target)
      res <- try_item(
        log,
        write_data_file(obj, target, fmt, gpars = gpars, name = nm),
        where = paste0(type, "/", nm, fmt),
        quiet = quiet
      )
      rows[[length(rows) + 1L]] <- if (item_failed(res)) {
        manifest_row(type, nm, target, fmt, "failed", res$message, gpars)
      } else {
        manifest_row(type, nm, target, fmt, "ok", NA_character_, gpars)
      }
      if (isTRUE(getOption("documenteR.verbose", FALSE)) && !quiet) {
        cli::cli_alert("{.path {basename(target)}}")
      }
    }
    if (!is.null(bar)) cli::cli_progress_update(id = bar)
  }
  if (!is.null(bar)) cli::cli_progress_done(id = bar)

  do.call(rbind, rows)
}

#' Write the appended datasets
#'
#' @description
#' Writes every appended dataset in the requested formats and, unless
#' switched off, a combined codebook. Called by [save_outputs()].
#'
#' @param output_list The collection. See [append_plot()].
#' @param output_dir_data Folder to write into.
#' @param log Internal log object; created automatically when omitted.
#' @param quiet Suppress progress output.
#'
#' @return A manifest data frame of the files written, invisibly.
#' @seealso [save_outputs()], [dr_codebook()]
#' @export
save_data <- function(output_list = NULL, output_dir_data, log = NULL, quiet = FALSE) {
  store <- resolve_outputs(output_list, env = parent.frame())
  log <- log %||% new_dr_log("save_data")
  entries <- store$data
  gpars <- store$gpars
  plan <- plan_outputs(entries, gpars, "data")

  rows <- save_rectangular(entries, output_dir_data, plan, gpars,
                           "data", "data", log, quiet = quiet)

  if (isTRUE(gpars$codebook %||% TRUE)) {
    cb <- try_item(log, build_combined_codebook(entries, plan),
                   where = "data/codebook", quiet = quiet)
    if (!item_failed(cb) && !is.null(cb)) {
      target <- path_join(output_dir_data, "00_codebook.csv")
      res <- try_item(log, write_data_file(cb, target, ".csv", gpars = gpars,
                                           name = "codebook"),
                      where = "data/codebook", quiet = quiet)
      rows <- rbind(rows, if (item_failed(res)) {
        manifest_row("data", "codebook", target, ".csv", "failed", res$message, gpars)
      } else {
        manifest_row("data", "codebook", target, ".csv", "ok", NA_character_, gpars)
      })
    }
  }
  invisible(rows)
}

#' Write the appended tables
#'
#' @inheritParams save_data
#' @param output_dir_tables Folder to write into.
#' @return A manifest data frame of the files written, invisibly.
#' @seealso [save_outputs()]
#' @export
save_tables <- function(output_list = NULL, output_dir_tables, log = NULL, quiet = FALSE) {
  store <- resolve_outputs(output_list, env = parent.frame())
  log <- log %||% new_dr_log("save_tables")
  plan <- plan_outputs(store$tables, store$gpars, "tables")
  invisible(save_rectangular(store$tables, output_dir_tables, plan, store$gpars,
                             "tables", "table", log, quiet = quiet))
}

#' Write the appended plots
#'
#' @description
#' Writes every appended plot in the requested formats at its configured
#' physical size, and optionally builds the PDF catalogue. Called by
#' [save_outputs()].
#'
#' @inheritParams save_data
#' @param output_dir_plots Folder to write into.
#' @param catalogue Build the PDF catalogue as well.
#' @param project Label printed in the catalogue footer.
#'
#' @return A manifest data frame of the files written, invisibly.
#' @seealso [save_outputs()], [dr_catalogue()], [dr_save_plot()]
#' @export
save_plots <- function(output_list = NULL, output_dir_plots, catalogue = TRUE,
                       project = NULL, log = NULL, quiet = FALSE) {
  store <- resolve_outputs(output_list, env = parent.frame())
  log <- log %||% new_dr_log("save_plots")
  entries <- store$plots
  gpars <- store$gpars
  plan <- plan_outputs(entries, gpars, "plots")
  if (!nrow(plan)) return(invisible(empty_manifest()))

  rows <- list()
  cat_items <- list()

  bar <- if (!quiet && nrow(plan) > 1L) {
    cli::cli_progress_bar("Writing plots", total = nrow(plan), .envir = environment())
  } else {
    NULL
  }

  for (i in seq_len(nrow(plan))) {
    nm <- plan$name[i]
    entry <- entries[[nm]]
    pars <- resolve_plot_pars(entry, gpars)
    plot_obj <- entry$plot

    if (isTRUE(entry$wrap_labels)) {
      wrapped <- try_item(log, wrap_plot_labels(plot_obj, pars$textwidths),
                          where = paste0("plots/", nm, " (wrap)"), quiet = quiet)
      if (!item_failed(wrapped)) plot_obj <- wrapped
    }

    stem <- path_join(output_dir_plots, plan$dir[i], plan$stem[i])
    for (fmt in split_spec(plan$formats[i])) {
      target <- paste0(stem, fmt)
      check_path_length(target)
      res <- try_item(
        log,
        dr_save_plot(plot_obj, target, W = pars$W, H = pars$H,
                     units = pars$units, dpi = pars$dpi, bg = pars$bg),
        where = paste0("plots/", nm, fmt),
        quiet = quiet
      )
      rows[[length(rows) + 1L]] <- if (item_failed(res)) {
        manifest_row("plots", nm, target, fmt, "failed", res$message, gpars)
      } else {
        manifest_row("plots", nm, target, fmt, "ok", NA_character_, gpars)
      }
      if (isTRUE(getOption("documenteR.verbose", FALSE)) && !quiet) {
        cli::cli_alert("{.path {basename(target)}}")
      }
    }

    cat_items[[length(cat_items) + 1L]] <- list(
      name = nm,
      label = plan_label(plan[i, ], formats = TRUE),
      plot = plot_obj,
      documentation = entry$documentation %||% list(),
      plot_w = to_inches(pars$W, pars$units),
      plot_h = to_inches(pars$H, pars$units),
      dpi = resolve_dpi(pars$dpi),
      bg = pars$bg
    )
    if (!is.null(bar)) cli::cli_progress_update(id = bar)
  }
  if (!is.null(bar)) cli::cli_progress_done(id = bar)

  if (isTRUE(catalogue)) {
    target <- path_join(output_dir_plots, "00_01_catalogue.pdf")
    res <- try_item(
      log,
      dr_catalogue(store, path = target, project = project, items = cat_items),
      where = "plots/catalogue",
      quiet = quiet
    )
    rows[[length(rows) + 1L]] <- if (item_failed(res)) {
      manifest_row("plots", "catalogue", target, ".pdf", "failed", res$message, gpars)
    } else {
      manifest_row("plots", "catalogue", target, ".pdf", "ok", NA_character_, gpars)
    }
  }

  invisible(do.call(rbind, rows))
}


# In-text statistics ----------------------------------------------------------

# Substitute variable labels into a formula string.
#
# The original built this with case_when() over operator-split fragments,
# which raised "`..2 (right)` must be a vector, not NULL" as soon as a
# statistic had a formula but no documented vars. Whole-word substitution,
# longest name first, is both simpler and impossible to break that way.
formula_to_text <- function(formula, vars) {
  if (is_blank(formula)) return("")
  out <- as.character(formula)[1]
  vars <- as_named_chr(vars)
  vars <- vars[nzchar(names(vars))]
  if (!length(vars)) return(out)

  # Pick out the identifier-shaped runs and swap in any that name a variable.
  # Operating on tokens rather than on a constructed pattern means the labels
  # are inserted literally, whatever punctuation they contain.
  m <- gregexpr("[A-Za-z0-9_.]+", out)
  tokens <- regmatches(out, m)[[1]]
  if (!length(tokens)) return(out)
  hit <- tokens %in% names(vars)
  tokens[hit] <- unname(vars[tokens[hit]])
  regmatches(out, m) <- list(tokens)
  out
}

# Format one statistic for human consumption.
#
# The original passed `accuracy = 1 + 10^(-stat_round)` to scales::comma(),
# which rounds to the nearest 1.001 - so a proportion of 0.5 came out as "0".
# Here `stat_round` means what it says: the number of decimal places.
format_stat <- function(value, type = "char", digits = 3, big_mark = " ") {
  if (is_blank(value)) return("")
  if (identical(type, "char") || !is.numeric(value)) {
    return(as.character(value))
  }
  digits <- as.integer(digits %||% 3L)
  num <- as.numeric(value)
  switch(type,
    "integer" = formatC(round(num), format = "d", big.mark = big_mark),
    "percent" = formatC(num * 100, format = "f", digits = digits, big.mark = big_mark),
    "float"   = formatC(num, format = "f", digits = digits, big.mark = big_mark),
    as.character(value)
  )
}

# Assemble the statistics table. One row per appended statistic, with the raw
# value, the formatted value, and the filled-in sentence.
build_stats_table <- function(entries, gpars) {
  if (!length(entries)) return(NULL)
  default_round <- as.integer(gpars$stat_round %||% 3L)
  big_mark <- gpars$big_mark %||% " "

  rows <- lapply(seq_along(entries), function(i) {
    nm <- names(entries)[i]
    entry <- entries[[i]]
    doc <- entry$documentation %||% list()
    type <- as.character(doc$type %||% "char")[1]
    digits <- if (is_blank(doc$stat_round)) default_round else as.integer(doc$stat_round)

    raw <- entry$stat
    formatted <- format_stat(raw, type = type, digits = digits, big_mark = big_mark)
    ftext <- formula_to_text(doc$formula, doc$vars)
    text <- as.character(doc$text %||% "")
    filled <- if (nzchar(text)) gsub("*", formatted, text, fixed = TRUE) else ""

    data.frame(
      ord = i,
      name = nm,
      title = as.character(doc$title %||% nm),
      section = as.character(doc$section %||% ""),
      page = as.character(doc$page %||% ""),
      stat = formatted,
      stat_raw = as.character(raw),
      type = type,
      stat_round = digits,
      desc = as.character(doc$desc %||% ""),
      text = text,
      text_fill = filled,
      formula = as.character(doc$formula %||% ""),
      formula_text = ftext,
      vars = collapse_named(doc$vars),
      source = collapse_named(doc$source),
      notes = as.character(doc$notes %||% ""),
      timestamp = as.character(doc$timestamp %||% ""),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Write the appended statistics
#'
#' @description
#' Collects every appended statistic into one table and writes it in the
#' requested formats. Called by [save_outputs()].
#'
#' @inheritParams save_data
#' @param output_dir_stats Folder to write into.
#'
#' @return A manifest data frame of the files written, invisibly.
#' @seealso [append_stats()], [save_outputs()]
#' @export
#' @examples
#' out <- init_outputs(default = FALSE, quiet = TRUE)
#' stat_n <- 1234
#' append_stats(stat_n, output_list = out, type = "integer", quiet = TRUE)
#' d <- file.path(tempdir(), "stats_demo")
#' dir.create(d, showWarnings = FALSE)
#' save_stats(out, d)
#' list.files(d)
save_stats <- function(output_list = NULL, output_dir_stats, log = NULL, quiet = FALSE) {
  store <- resolve_outputs(output_list, env = parent.frame())
  log <- log %||% new_dr_log("save_stats")
  gpars <- store$gpars

  tbl <- try_item(log, build_stats_table(store$stats, gpars),
                  where = "stats/table", quiet = quiet)
  if (item_failed(tbl) || is.null(tbl)) {
    return(invisible(empty_manifest()))
  }

  rows <- list()
  for (fmt in split_spec(gpars$formats$stats %||% ".csv")) {
    target <- path_join(output_dir_stats, paste0("01_stats", fmt))
    res <- try_item(log, write_data_file(tbl, target, fmt, gpars = gpars, name = "stats"),
                    where = paste0("stats/01_stats", fmt), quiet = quiet)
    rows[[length(rows) + 1L]] <- if (item_failed(res)) {
      manifest_row("stats", "stats", target, fmt, "failed", res$message, gpars)
    } else {
      manifest_row("stats", "stats", target, fmt, "ok", NA_character_, gpars)
    }
  }
  invisible(do.call(rbind, rows))
}
