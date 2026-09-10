# Display methods -------------------------------------------------------------
#
# All console rendering lives here so that the layout of the various tables
# stays consistent.
#
# The aligned rows are emitted with cli::cli_verbatim() rather than
# cli::cli_text(): cli_text() interpolates `{}` and re-wraps, both of which
# would corrupt a table whose columns have already been padded. Colour is
# applied with the cli::col_*() / cli::style_*() functions, which write ANSI
# directly and are no-ops when the console does not support them.

# Pad or truncate to an exact display width. nchar(type = "width") is used
# throughout because accented and wide characters otherwise break alignment.
pad_width <- function(x, w) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  vapply(x, function(s) {
    if (nchar(s, type = "width") > w) {
      s <- paste0(substr(s, 1L, max(1L, w - 1L)), "\u2026")
    }
    paste0(s, strrep(" ", max(0L, w - nchar(s, type = "width"))))
  }, character(1), USE.NAMES = FALSE)
}

# Console width, clamped so output stays readable in a very wide terminal and
# does not wrap in a narrow one.
display_width <- function(min = 60L, max = 110L) {
  w <- tryCatch(cli::console_width(), error = function(e) 80L)
  if (!is.numeric(w) || !is.finite(w)) w <- 80L
  as.integer(min(max(w, min), max))
}

# A file's formats, shown compactly: ".png .svg"
fmt_formats <- function(x) paste(split_spec(x), collapse = " ")

# append_*() defaults the title to the object name, so showing both would just
# repeat it. Blank the title in that case, which also makes the rows carrying
# no real documentation easy to spot.
display_title <- function(doc, name) {
  title <- as.character(doc$title %||% "")[1]
  if (is.na(title) || identical(title, name)) "" else title
}

# How completely is one entry documented? Counts the fields a reader would
# actually want, which is what makes the coverage line in print() meaningful.
doc_fields_present <- function(entry) {
  doc <- entry$documentation %||% list()
  sum(!vapply(
    list(doc$desc, doc$vars, doc$source, doc$notes),
    is_blank, logical(1)
  ))
}

# The "16 x 12.35 cm" / "50 x 2" / value column.
entry_size_label <- function(entry, type, gpars = NULL) {
  doc <- entry$documentation %||% list()
  if (type %in% c("data", "tables")) {
    if (is_blank(doc$nrow)) return("")
    return(paste0(format(doc$nrow, big.mark = " "), " x ", doc$ncol))
  }
  if (identical(type, "plots")) {
    pars <- if (is.null(gpars)) NULL else resolve_plot_pars(entry, gpars)
    if (is.null(pars)) {
      if (is_blank(entry$H) || is_blank(entry$W)) return("")
      return(paste0(entry$W, " x ", entry$H, " ", entry$units %||% "cm"))
    }
    return(paste0(pars$W, " x ", pars$H, " ", pars$units))
  }
  if (identical(type, "stats")) {
    return(if (is_blank(entry$stat)) "" else as.character(entry$stat))
  }
  ""
}


#' Print an output collection
#'
#' @description
#' Shows what a collection currently holds: how many outputs of each type,
#' the file name each will be exported under, its title, and how completely
#' it is documented. The listing uses the same numbering [save_outputs()]
#' will use, so it doubles as a preview of the export.
#'
#' @param x A `documenteR_outputs` object.
#' @param n Maximum number of outputs to list per type. Use `Inf`, or
#'   `detail = TRUE`, to list all of them.
#' @param detail List every output and show the description column.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#' @seealso [summary.documenteR_outputs()] for the full inventory as a data
#'   frame, [init_outputs()], [save_outputs()]
#' @export
#' @examples
#' out <- init_outputs(default = FALSE, quiet = TRUE)
#' data_cars <- cars
#' append_data(data_cars, output_list = out, desc = "Built-in data",
#'             quiet = TRUE)
#' plt_cars <- function() plot(cars)
#' append_plot(plt_cars, output_list = out, type = "s_wide", quiet = TRUE)
#'
#' out
#' print(out, detail = TRUE)
print.documenteR_outputs <- function(x, n = 8L, detail = FALSE, ...) {
  types <- dr_output_types()
  counts <- vapply(types, function(ty) length(x[[ty]]), integer(1))
  total <- sum(counts)
  width <- display_width()

  cli::cli_h1("documenteR output collection")

  if (total == 0L) {
    cli::cli_alert_info(
      "Empty. Append outputs with {.fun append_plot}, {.fun append_data}, {.fun append_table} or {.fun append_stats}."
    )
    print_gpars_line(x)
    return(invisible(x))
  }

  if (isTRUE(detail)) n <- Inf

  # Column layout: id | name | title (| desc) | size | formats
  labels <- unlist(lapply(types, function(ty) {
    entries <- x[[ty]]
    if (!length(entries)) return(character(0))
    sub <- vapply(entries, function(e) as.character(e$subfolder %||% "")[1], character(1))
    ifelse(nzchar(sub), paste0(sub, "/", names(entries)), names(entries))
  }))
  w_id <- 6L
  w_name <- min(28L, max(8L, max(nchar(labels, type = "width"), 8L)))
  w_size <- 14L
  w_fmt <- 14L
  w_rest <- width - (2L + w_id + 1L + w_name + 1L + w_size + 1L + w_fmt)
  if (isTRUE(detail)) {
    w_title <- max(12L, floor(w_rest * 0.45))
    w_desc <- max(12L, w_rest - w_title - 1L)
  } else {
    w_title <- max(12L, w_rest)
    w_desc <- 0L
  }

  header <- paste0(
    "  ", pad_width("id", w_id), " ", pad_width("name", w_name), " ",
    pad_width("title", w_title), " ",
    if (w_desc > 0L) paste0(pad_width("description", w_desc), " ") else "",
    pad_width("size", w_size), " ", "formats"
  )
  cli::cli_verbatim(cli::col_grey(header))

  n_undocumented <- 0L

  for (ty in types) {
    if (counts[[ty]] == 0L) next
    entries <- x[[ty]]
    plan <- plan_outputs(entries, x$gpars, ty)

    cli::cli_verbatim(paste0(
      cli::style_bold(ty), cli::col_grey(paste0(" (", counts[[ty]], ")"))
    ))

    show <- min(nrow(plan), n)
    for (i in seq_len(show)) {
      nm <- plan$name[i]
      entry <- entries[[nm]]
      doc <- entry$documentation %||% list()
      if (doc_fields_present(entry) == 0L) n_undocumented <- n_undocumented + 1L

      # Statistics all share one file, so a per-entry format column is noise.
      fmt <- if (identical(ty, "stats")) "" else fmt_formats(plan$formats[i])
      label <- if (nzchar(plan$subfolder[i])) {
        paste0(plan$subfolder[i], "/", nm)
      } else {
        nm
      }

      # Whichever column ends the row is left unpadded, so no row carries
      # trailing whitespace (statistics have no per-entry format).
      size <- entry_size_label(entry, ty, x$gpars)
      tail_cells <- if (nzchar(fmt)) {
        paste0(cli::col_grey(pad_width(size, w_size)), " ", cli::col_grey(fmt))
      } else {
        cli::col_grey(size)
      }
      row <- paste0(
        "  ",
        cli::col_grey(pad_width(plan$id[i], w_id)), " ",
        pad_width(label, w_name), " ",
        pad_width(display_title(doc, nm), w_title), " ",
        if (w_desc > 0L) paste0(cli::col_grey(pad_width(doc$desc %||% "", w_desc)), " ") else "",
        tail_cells
      )
      cli::cli_verbatim(row)
    }
    if (nrow(plan) > show) {
      cli::cli_verbatim(cli::col_grey(sprintf(
        "  ... and %d more (print(x, detail = TRUE) to see all)",
        nrow(plan) - show
      )))
    }
    # Anything not listed still counts towards the coverage line.
    if (nrow(plan) > show) {
      for (i in seq(from = show + 1L, to = nrow(plan))) {
        if (doc_fields_present(entries[[plan$name[i]]]) == 0L) {
          n_undocumented <- n_undocumented + 1L
        }
      }
    }
  }

  cli::cli_text("")
  if (n_undocumented > 0L) {
    cli::cli_alert_warning(
      "{n_undocumented} of {total} output{?s} {?has/have} no description, variables, source or notes."
    )
  } else {
    cli::cli_alert_success("All {total} output{?s} carry some documentation.")
  }
  print_gpars_line(x)
  cli::cli_alert_info(
    "{.fun summary} for the full inventory; {.fun save_outputs} to export."
  )
  invisible(x)
}

print_gpars_line <- function(x) {
  fmt <- x$gpars$formats
  cli::cli_verbatim(cli::col_grey(paste0(
    "formats  data ", fmt$data, "  plots ", fmt$plots,
    "  tables ", fmt$tables, "  stats ", fmt$stats
  )))
  invisible(NULL)
}

#' @export
format.documenteR_outputs <- function(x, ...) {
  counts <- vapply(dr_output_types(), function(ty) length(x[[ty]]), integer(1))
  sprintf(
    "<documenteR_outputs: %s>",
    paste(sprintf("%s=%d", names(counts), counts), collapse = ", ")
  )
}


#' The full inventory of an output collection
#'
#' @description
#' One row per appended output, with the file it will be exported to, its
#' documentation and how completely it is documented. Unlike
#' [print.documenteR_outputs()] nothing is truncated, so this is what to use
#' to find the outputs that still need attention:
#'
#' ```r
#' s <- summary(list_outputs)
#' s[!s$has_desc, c("type", "name")]
#' ```
#'
#' @param object A `documenteR_outputs` object.
#' @param ... Unused.
#'
#' @return A data frame with class `documenteR_summary`, carrying columns
#'   `type`, `id`, `name`, `title`, `file`, `formats`, `size`, `desc`,
#'   `source`, `subfolder`, `has_desc`, `has_vars`, `has_source` and
#'   `doc_fields`. It prints as an aligned table and subsets like any data
#'   frame.
#' @seealso [print.documenteR_outputs()], [save_outputs()]
#' @export
#' @examples
#' out <- init_outputs(default = FALSE, quiet = TRUE)
#' d <- cars
#' append_data(d, output_list = out, desc = "Documented", quiet = TRUE)
#' t1 <- head(cars)
#' append_table(t1, output_list = out, quiet = TRUE)
#'
#' s <- summary(out)
#' s
#' # which outputs still need a description?
#' s$name[!s$has_desc]
summary.documenteR_outputs <- function(object, ...) {
  rows <- lapply(dr_output_types(), function(ty) {
    entries <- object[[ty]]
    if (length(entries) == 0L) return(NULL)
    plan <- plan_outputs(entries, object$gpars, ty)

    do.call(rbind, lapply(seq_len(nrow(plan)), function(i) {
      nm <- plan$name[i]
      entry <- entries[[nm]]
      doc <- entry$documentation %||% list()
      file <- if (identical(ty, "stats")) {
        "01_stats"
      } else {
        path_join(plan$dir[i], plan$stem[i])
      }
      data.frame(
        type = ty,
        id = plan$id[i],
        name = nm,
        title = {
          shown <- display_title(doc, nm)
          if (nzchar(shown)) shown else NA_character_
        },
        file = file,
        formats = if (identical(ty, "stats")) {
          fmt_formats(object$gpars$formats$stats)
        } else {
          fmt_formats(plan$formats[i])
        },
        size = entry_size_label(entry, ty, object$gpars),
        desc = if (is_blank(doc$desc)) NA_character_ else as.character(doc$desc)[1],
        source = if (is_blank(doc$source)) NA_character_ else collapse_named(doc$source),
        subfolder = plan$subfolder[i],
        has_desc = !is_blank(doc$desc),
        has_vars = !is_blank(doc$vars),
        has_source = !is_blank(doc$source),
        doc_fields = doc_fields_present(entry),
        stringsAsFactors = FALSE
      )
    }))
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]

  if (length(rows) == 0L) {
    out <- data.frame(
      type = character(0), id = character(0), name = character(0),
      title = character(0), file = character(0), formats = character(0),
      size = character(0), desc = character(0), source = character(0),
      subfolder = character(0), has_desc = logical(0), has_vars = logical(0),
      has_source = logical(0), doc_fields = integer(0),
      stringsAsFactors = FALSE
    )
  } else {
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
  }
  class(out) <- c("documenteR_summary", "data.frame")
  out
}

#' @rdname summary.documenteR_outputs
#' @param x A `documenteR_summary` object.
#' @export
print.documenteR_summary <- function(x, ...) {
  if (nrow(x) == 0L) {
    cli::cli_alert_info("No outputs appended yet.")
    return(invisible(x))
  }
  width <- display_width(max = 120L)

  w_type <- 6L
  w_file <- min(34L, max(10L, max(nchar(x$file, type = "width"))))
  w_size <- min(16L, max(4L, max(nchar(x$size, type = "width"))))
  w_fmt <- min(16L, max(7L, max(nchar(x$formats, type = "width"))))
  w_title <- max(
    12L,
    width - (w_type + 1L + w_file + 1L + w_size + 1L + w_fmt + 5L)
  )

  cli::cli_verbatim(cli::col_grey(paste0(
    pad_width("type", w_type), " ", pad_width("file", w_file), " ",
    pad_width("title", w_title), " ", pad_width("size", w_size), " ",
    pad_width("formats", w_fmt), " ", "doc"
  )))

  tick <- cli::symbol$tick
  dot <- cli::symbol$dot
  for (i in seq_len(nrow(x))) {
    flags <- paste0(
      if (x$has_desc[i]) tick else dot,
      if (x$has_vars[i]) tick else dot,
      if (x$has_source[i]) tick else dot
    )
    cli::cli_verbatim(paste0(
      cli::col_grey(pad_width(x$type[i], w_type)), " ",
      pad_width(x$file[i], w_file), " ",
      pad_width(x$title[i], w_title), " ",
      cli::col_grey(pad_width(x$size[i], w_size)), " ",
      cli::col_grey(pad_width(x$formats[i], w_fmt)), " ",
      flags
    ))
  }
  cli::cli_verbatim(cli::col_grey(paste0(
    "doc columns: ", tick, "/", dot, " for description, variables, source"
  )))
  invisible(x)
}


#' Print one appended output
#'
#' @description
#' Renders the documentation of a single appended output, so that inspecting
#' one shows what you recorded about it rather than dumping the plot object
#' or the whole data frame:
#'
#' ```r
#' list_outputs$plots$plt_cars
#' ```
#'
#' @param x A `dr_entry` object — one element of `x$plots`, `x$data`,
#'   `x$tables` or `x$stats`.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#' @seealso [append_plot()], [print.documenteR_outputs()]
#' @export
#' @examples
#' out <- init_outputs(default = FALSE, quiet = TRUE)
#' plt_cars <- function() plot(cars)
#' append_plot(plt_cars, output_list = out, type = "m_wide",
#'             desc = "Speed against braking distance",
#'             vars = c(x = "Speed", y = "Distance"),
#'             source = c(datasets = "R built-in"), quiet = TRUE)
#'
#' out$plots$plt_cars
print.dr_entry <- function(x, ...) {
  type <- attr(x, "dr_type") %||% "output"
  name <- attr(x, "dr_name") %||% ""
  label <- switch(type,
    plots = "plot", data = "dataset", tables = "table", stats = "statistic",
    type
  )
  width <- display_width(max = 90L)

  cli::cli_rule(left = "{label} {.strong {name}}")
  block <- format_doc_block(
    x$documentation %||% list(),
    extra = entry_extra(x, type),
    width = width - 2L
  )
  if (length(block)) {
    cli::cli_verbatim(block)
  } else {
    cli::cli_alert_info("No documentation recorded.")
  }
  invisible(x)
}

#' @export
format.dr_entry <- function(x, ...) {
  sprintf(
    "<%s: %s>",
    attr(x, "dr_type") %||% "output",
    attr(x, "dr_name") %||% "unnamed"
  )
}

# The per-type facts shown under the documentation of a single entry. Unlike
# readme_extra() this works without gpars, using only what the entry itself
# carries, so it is honest about what was set explicitly.
entry_extra <- function(entry, type) {
  doc <- entry$documentation %||% list()
  switch(type,
    "plots" = compact(list(
      type = if (!is_blank(entry$type)) as.character(entry$type),
      size = if (!is_blank(entry$H) && !is_blank(entry$W)) {
        paste0(entry$W, " x ", entry$H, " ", entry$units %||% "cm")
      },
      dpi = if (!is_blank(entry$dpi)) as.character(entry$dpi),
      formats = if (!is_blank(entry$device)) fmt_formats(entry$device),
      folder = entry$subfolder
    )),
    "data" = ,
    "tables" = compact(list(
      size = if (!is_blank(doc$nrow)) {
        paste0(format(doc$nrow, big.mark = " "), " rows x ", doc$ncol, " columns")
      },
      formats = if (!is_blank(entry$format)) fmt_formats(entry$format),
      folder = entry$subfolder
    )),
    "stats" = compact(list(
      value = if (!is_blank(entry$stat)) as.character(entry$stat),
      format = doc$type,
      decimals = if (!is_blank(doc$stat_round)) as.character(doc$stat_round),
      used_in = paste(compact(list(
        if (!is_blank(doc$section)) paste0("section ", doc$section),
        if (!is_blank(doc$page)) paste0("page ", doc$page)
      )), collapse = ", "),
      sentence = doc$text
    )),
    list()
  )
}


#' Print the result of an export
#'
#' @param x A `documenteR_export` object, as returned by [save_outputs()].
#' @param ... Unused.
#' @return `x`, invisibly.
#' @seealso [save_outputs()]
#' @export
print.documenteR_export <- function(x, ...) {
  cli::cli_h2("documenteR export {.strong {x$version}}")
  cli::cli_text("{.path {x$path}}")
  if (!is.na(x$version_name) && nzchar(x$version_name)) {
    cli::cli_text("{.emph {x$version_name}}")
  }

  man <- x$manifest
  if (!is.null(man) && nrow(man)) {
    by_type <- table(man$type[man$status == "ok"])
    if (length(by_type)) {
      cli::cli_verbatim(cli::col_grey(paste(
        sprintf("%s %d", names(by_type), as.integer(by_type)),
        collapse = "   "
      )))
    }
  }

  if (x$n_failed > 0L) {
    cli::cli_alert_warning("{x$n_written} file{?s} written, {x$n_failed} failed.")
    failed <- man$file[man$status != "ok"]
    for (f in utils::head(failed, 5L)) cli::cli_bullets(c("x" = "{.path {f}}"))
    if (length(failed) > 5L) {
      cli::cli_bullets(c(" " = "... and {length(failed) - 5L} more."))
    }
  } else {
    cli::cli_alert_success("{x$n_written} file{?s} written.")
  }
  if (x$warnings > 0L) {
    cli::cli_alert_info("{x$warnings} warning{?s} recorded in {.path 00_LOG.txt}.")
  }
  invisible(x)
}


#' Print a run log
#'
#' @param x A `documenteR_log` object.
#' @param n Number of recent entries to show.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @seealso [dr_log()], [save_outputs()]
#' @export
print.documenteR_log <- function(x, n = 10L, ...) {
  levels_seen <- log_levels(x)
  cli::cli_h3("documenteR log")
  if (!length(levels_seen)) {
    cli::cli_alert_info("Nothing recorded.")
    return(invisible(x))
  }

  counts <- table(factor(
    levels_seen,
    levels = c("progress", "message", "warning", "error", "stdout")
  ))
  counts <- counts[counts > 0L]
  cli::cli_verbatim(cli::col_grey(paste(
    sprintf("%s %d", names(counts), as.integer(counts)),
    collapse = "   "
  )))

  # Problems first: they are the reason anyone opens a log.
  notable <- x$entries[vapply(x$entries, function(e) {
    e$level %in% c("error", "warning")
  }, logical(1))]
  shown <- utils::tail(if (length(notable)) notable else x$entries, n)

  for (e in shown) {
    marker <- switch(e$level, error = "x", warning = "!", "i")
    where <- if (is.na(e$where)) "" else paste0("[", e$where, "] ")
    cli::cli_bullets(stats::setNames(
      paste0(where, gsub("\n", " ", trimws(e$text))),
      marker
    ))
  }
  total <- length(if (length(notable)) notable else x$entries)
  if (total > length(shown)) {
    cli::cli_verbatim(cli::col_grey(sprintf("... and %d more", total - length(shown))))
  }
  invisible(x)
}


#' Print a dry-run report
#'
#' @param x A `documenteR_dry_run` object, as returned by
#'   `save_outputs(dry_run = TRUE)`.
#' @param n Maximum number of files to list per output type.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @seealso [save_outputs()]
#' @export
print.documenteR_dry_run <- function(x, n = 12L, ...) {
  cli::cli_h2("Dry run: nothing will be written")
  cli::cli_text("Target: {.path {x$path}}")

  if (!length(x$planned)) {
    cli::cli_alert_info("Nothing to export.")
    return(invisible(x))
  }
  for (ty in names(x$planned)) {
    files <- x$planned[[ty]]
    if (identical(files, NA_character_)) {
      cli::cli_alert_info("{.field {ty}}: excluded")
      next
    }
    cli::cli_alert_success("{.field {ty}}: {length(files)} file{?s}")
    cli::cli_ul(utils::head(files, n))
    if (length(files) > n) {
      cli::cli_text("... and {length(files) - n} more")
    }
  }
  invisible(x)
}
