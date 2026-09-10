# Shared machinery for the append_*() family ----------------------------------

# The documentation fields every output type understands. Keeping them in one
# place means READMEs, the manifest and the codebook all agree on field order.
.dr_doc_fields <- c(
  "title", "desc", "formula", "vars", "source", "notes", "timestamp"
)

# Work out the name to store an output under. Using the caller's expression is
# what makes `append_plot(plt_cars)` work, but it produces nonsense for an
# inline expression, so we detect that and ask for a name instead.
infer_object_name <- function(expr, object_name, arg = "output_object",
                              call = rlang::caller_env()) {
  if (!is.null(object_name)) {
    if (!is.character(object_name) || length(object_name) != 1L || !nzchar(object_name)) {
      cli::cli_abort("{.arg object_name} must be a single non-empty string.", call = call)
    }
    return(object_name)
  }
  if (is.symbol(expr)) return(as.character(expr))
  cli::cli_abort(c(
    "Cannot work out a name for {.arg {arg}}.",
    "x" = "You passed an expression ({.code {deparse(expr)[1]}}), not a named object.",
    "i" = "Either assign it to a variable first, or pass {.arg object_name}."
  ), call = call)
}

# Assemble the documentation sub-list, dropping unsupplied fields so that
# downstream formatters can simply test for presence.
build_documentation <- function(title, timestamp, desc = NULL, vars = NULL,
                                source = NULL, formula = NULL, notes = NULL,
                                ...) {
  doc <- list(
    title = title,
    desc = desc,
    formula = formula,
    vars = if (is_blank(vars)) NULL else as_named_chr(vars),
    source = if (is_blank(source)) NULL else as_named_chr(source),
    notes = notes,
    timestamp = timestamp
  )
  extra <- list(...)
  doc <- c(doc, extra[!vapply(extra, is.null, logical(1))])
  compact(doc)
}

# Store one entry, warning when it replaces an existing one of the same name.
store_entry <- function(store, type, name, entry, quiet = FALSE, replace = TRUE) {
  existing <- names(store[[type]])
  if (name %in% existing) {
    if (!replace) {
      cli::cli_abort(c(
        "{.val {name}} is already in {.field {type}}.",
        "i" = "Pass {.code replace = TRUE} to overwrite it, or use a different {.arg object_name}."
      ))
    }
    if (!quiet) {
      cli::cli_alert_info("Replacing existing {type} entry {.val {name}}.")
    }
  }
  lst <- store[[type]]
  lst[[name]] <- entry
  store[[type]] <- lst
  if (!quiet) cli::cli_alert_success("Appended {.val {name}} to {.field {type}}.")
  invisible(store)
}

# A subfolder may be a nested path ("maps/regional"); normalise separators and
# reject anything that would climb out of the output folder.
check_subfolder <- function(subfolder, call = rlang::caller_env()) {
  if (is_blank(subfolder)) return(NULL)
  sf <- gsub("\\\\", "/", as.character(subfolder)[1])
  sf <- gsub("^/+|/+$", "", sf)
  if (grepl("(^|/)\\.\\.(/|$)", sf)) {
    cli::cli_abort(
      "{.arg subfolder} may not contain {.val ..} ({.val {subfolder}}).",
      call = call
    )
  }
  if (!nzchar(sf)) return(NULL)
  sf
}

# Validate a requested format list against what the package can write.
check_formats <- function(format, type, call = rlang::caller_env()) {
  fmt <- normalise_ext(split_spec(format))
  if (!length(fmt)) return(NULL)
  ok <- dr_formats()[[type]]
  bad <- setdiff(fmt, ok)
  if (length(bad)) {
    cli::cli_abort(c(
      "Unsupported {type} format{?s}: {.val {bad}}.",
      "i" = "Supported: {.val {ok}}."
    ), call = call)
  }
  fmt
}


#' Append a plot to an output collection
#'
#' @description
#' Marks a plot for export and records its documentation and export geometry.
#' Any plotting system is accepted, not just `ggplot2` — see
#' *Supported plot objects*.
#'
#' @section Supported plot objects:
#' - `ggplot2` objects, including `patchwork` and `ggarrange` compositions.
#' - `lattice` / `trellis` objects.
#' - `grid` grobs, `gtable`s and anything with a `grid.draw()` method.
#' - A **function of no arguments** that draws with base graphics, e.g.
#'   `function() { plot(x, y); abline(h = 0) }`. This is the way to export
#'   base-R plots, which cannot be captured as objects.
#' - A recorded base plot from `grDevices::recordPlot()`.
#' - Anything else with a `print()` or `plot()` method, as a last resort.
#'
#' See [dr_draw()] to add support for another plotting system.
#'
#' @param output_object The plot. Must be a named object, or pass
#'   `object_name`.
#' @param object_name Name to store and export the plot under. Defaults to
#'   the name of `output_object`.
#' @param output_list The collection to append to. Defaults to `list_outputs`
#'   if visible, otherwise the collection created by the last
#'   [init_outputs()]. May be an object, or the name of one.
#' @param title Documentation: a human-readable title. Defaults to
#'   `object_name`.
#' @param time_stamp Documentation: when the output was produced. Defaults to
#'   the current time.
#' @param desc Documentation: a free-text description.
#' @param vars Documentation: the variables shown, ideally named, e.g.
#'   `c(x = "Speed", y = "Braking distance")`.
#' @param source Documentation: data sources, ideally named, e.g.
#'   `c("CZSO" = "Census 2021")`.
#' @param notes Documentation: caveats, to-dos, anything else worth recording.
#' @param device File format(s) to export to, e.g. `".png"` or
#'   `c(".png", ".svg")`. Defaults to `gpars$formats$plots`.
#' @param dpi Resolution for raster formats: a number, or one of
#'   `"retina"` (320), `"print"` (300), `"screen"` (72).
#' @param type Name of a size preset in `gpars$sizes$type` (`"s"`, `"m_wide"`,
#'   ...). Sets `H`, `W` and `units` unless you give them explicitly.
#' @param H,W Height and width. Override `type`.
#' @param units Units for `H` and `W`: `"cm"` (default), `"mm"`, `"in"`, `"px"`.
#' @param bg Background colour, e.g. `"white"` or `"transparent"`.
#' @param subfolder Subfolder within the plots folder. May be nested
#'   (`"maps/regional"`).
#' @param wrap_labels If `TRUE`, wrap the plot's title, subtitle and axis
#'   labels to the character widths in `gpars$textwidths` before export.
#'   Only affects `ggplot2` objects. Default `FALSE`.
#' @param replace Overwrite an existing entry of the same name. Default `TRUE`.
#' @param quiet Suppress the confirmation message.
#'
#' @return The collection, invisibly. Called for its side effect.
#' @seealso [append_data()], [append_table()], [append_stats()],
#'   [preview_plot()], [save_outputs()]
#' @export
#' @examples
#' out <- init_outputs(default = FALSE, quiet = TRUE)
#'
#' # A base-R plot, passed as a drawing function
#' plt_cars <- function() plot(cars$speed, cars$dist, pch = 19)
#' append_plot(plt_cars, output_list = out, title = "Braking distance",
#'             vars = c(x = "Speed (mph)", y = "Distance (ft)"),
#'             type = "m_wide", quiet = TRUE)
#'
#' names(out$plots)
append_plot <- function(output_object,
                        object_name = NULL,
                        output_list = NULL,
                        title = NULL,
                        time_stamp = NULL,
                        desc = NULL,
                        vars = NULL,
                        source = NULL,
                        notes = NULL,
                        device = NULL,
                        dpi = NULL,
                        type = NULL,
                        H = NULL,
                        W = NULL,
                        units = NULL,
                        bg = NULL,
                        subfolder = NULL,
                        wrap_labels = FALSE,
                        replace = TRUE,
                        quiet = NULL) {
  quiet <- quiet %||% getOption("documenteR.quiet", FALSE)
  store <- resolve_outputs(output_list, env = parent.frame())
  name <- infer_object_name(substitute(output_object), object_name)

  if (!dr_can_draw(output_object)) {
    cli::cli_abort(c(
      "Cannot draw {.val {name}} ({.cls {class(output_object)[1]}}).",
      "i" = "Pass a {.pkg ggplot2}/{.pkg lattice} object, a grob, or a function of no arguments that draws a base plot.",
      "i" = "See {.help documenteR::dr_draw} to support another plotting system."
    ))
  }

  if (!is_blank(type)) {
    known <- names(store$gpars$sizes$type)
    if (!as.character(type)[1] %in% known) {
      cli::cli_warn(c(
        "!" = "Size preset {.val {type}} is not defined in {.code gpars$sizes$type}.",
        "i" = "Defined preset{?s}: {.val {known}}. Falling back to {.code gpars$sizes$default}."
      ))
    }
  }
  if (!is_blank(H) && !is.numeric(H)) cli::cli_abort("{.arg H} must be numeric.")
  if (!is_blank(W) && !is.numeric(W)) cli::cli_abort("{.arg W} must be numeric.")

  entry <- compact(list(
    plot = output_object,
    type = type,
    device = check_formats(device, "plots"),
    dpi = dpi,
    H = H,
    W = W,
    units = units,
    bg = bg,
    subfolder = check_subfolder(subfolder),
    wrap_labels = isTRUE(wrap_labels),
    documentation = build_documentation(
      title = title %||% name,
      timestamp = time_stamp %||% dr_timestamp(),
      desc = desc, vars = vars, source = source, notes = notes
    )
  ))

  store_entry(store, "plots", name, entry, quiet = quiet, replace = replace)
}


#' Append a dataset to an output collection
#'
#' @description
#' Marks a dataset for export. Unlike [append_table()], appended *data* also
#' gets a variable-level codebook in the export (see [dr_codebook()]); the
#' distinction is intent — `data` is the analysis-ready dataset you want
#' others to reuse, `tables` are presentation tables.
#'
#' @inheritParams append_plot
#' @param output_object A data frame, or any object coercible with
#'   [as.data.frame()].
#' @param format File format(s): any of `.csv`, `.csv2`, `.tsv`, `.xlsx`,
#'   `.rds`, `.parquet`, `.json`. Defaults to `gpars$formats$data`.
#' @param codebook Write a codebook for this dataset. Defaults to
#'   `gpars$codebook`.
#' @param labels Optional named character vector of variable labels, e.g.
#'   `c(speed = "Speed in mph")`. Used in the codebook. Variable labels
#'   already stored in a column's `label` attribute (as `haven` does) are
#'   picked up automatically.
#'
#' @return The collection, invisibly.
#' @seealso [append_table()], [dr_codebook()], [save_outputs()]
#' @export
#' @examples
#' out <- init_outputs(default = FALSE, quiet = TRUE)
#' data_cars <- cars
#' append_data(data_cars, output_list = out, format = c(".csv", ".rds"),
#'             labels = c(speed = "Speed (mph)", dist = "Distance (ft)"),
#'             quiet = TRUE)
#' names(out$data)
append_data <- function(output_object,
                        object_name = NULL,
                        output_list = NULL,
                        title = NULL,
                        time_stamp = NULL,
                        desc = NULL,
                        vars = NULL,
                        source = NULL,
                        notes = NULL,
                        format = NULL,
                        subfolder = NULL,
                        codebook = NULL,
                        labels = NULL,
                        replace = TRUE,
                        quiet = NULL) {
  quiet <- quiet %||% getOption("documenteR.quiet", FALSE)
  store <- resolve_outputs(output_list, env = parent.frame())
  name <- infer_object_name(substitute(output_object), object_name)

  output_object <- coerce_rectangular(output_object, name, "append_data")

  entry <- compact(list(
    data = output_object,
    format = check_formats(format, "data"),
    subfolder = check_subfolder(subfolder),
    codebook = codebook,
    labels = if (is_blank(labels)) NULL else as_named_chr(labels),
    documentation = build_documentation(
      title = title %||% name,
      timestamp = time_stamp %||% dr_timestamp(),
      desc = desc, vars = vars, source = source, notes = notes,
      nrow = nrow(output_object), ncol = ncol(output_object)
    )
  ))

  store_entry(store, "data", name, entry, quiet = quiet, replace = replace)
}


#' Append a presentation table to an output collection
#'
#' @description
#' Marks a table for export. Identical to [append_data()] except that tables
#' land in the tables folder and get no codebook.
#'
#' @inheritParams append_data
#' @param format File format(s): any of `.csv`, `.csv2`, `.tsv`, `.xlsx`,
#'   `.rds`, `.parquet`, `.json`. Defaults to `gpars$formats$tables`.
#'
#' @return The collection, invisibly.
#' @seealso [append_data()], [save_outputs()]
#' @export
#' @examples
#' out <- init_outputs(default = FALSE, quiet = TRUE)
#' tab_speed <- aggregate(dist ~ speed > 15, data = cars, FUN = mean)
#' append_table(tab_speed, output_list = out, title = "Mean distance by speed",
#'              quiet = TRUE)
#' names(out$tables)
append_table <- function(output_object,
                         object_name = NULL,
                         output_list = NULL,
                         title = NULL,
                         time_stamp = NULL,
                         desc = NULL,
                         vars = NULL,
                         source = NULL,
                         notes = NULL,
                         format = NULL,
                         subfolder = NULL,
                         replace = TRUE,
                         quiet = NULL) {
  quiet <- quiet %||% getOption("documenteR.quiet", FALSE)
  store <- resolve_outputs(output_list, env = parent.frame())
  name <- infer_object_name(substitute(output_object), object_name)

  output_object <- coerce_rectangular(output_object, name, "append_table")

  entry <- compact(list(
    table = output_object,
    format = check_formats(format, "tables"),
    subfolder = check_subfolder(subfolder),
    documentation = build_documentation(
      title = title %||% name,
      timestamp = time_stamp %||% dr_timestamp(),
      desc = desc, vars = vars, source = source, notes = notes,
      nrow = nrow(output_object), ncol = ncol(output_object)
    )
  ))

  store_entry(store, "tables", name, entry, quiet = quiet, replace = replace)
}


#' Append an in-text statistic to an output collection
#'
#' @description
#' Marks a single number (or short string) for export, so that figures quoted
#' in a report can be traced back to the code that produced them. All
#' appended statistics are exported to one table, with both the raw value and
#' a formatted version.
#'
#' @section Formula and text templates:
#' `formula` records how the statistic was computed, in terms of the names in
#' `vars`. In the export, a readable version is derived by substituting the
#' labels: `formula = "deaths/population"` with
#' `vars = c(deaths = "Number of deaths", population = "Population")` yields
#' `formula_text = "Number of deaths/Population"`.
#'
#' `text` is the sentence the statistic appears in, with `*` marking where the
#' number goes: `text = "Mortality reached * per cent."` produces a
#' `text_fill` column with the formatted value substituted in, ready to paste
#' into a report.
#'
#' @inheritParams append_plot
#' @param output_object A length-1 numeric or character value.
#' @param type How to format the value: `"integer"` (thousands separator, no
#'   decimals), `"percent"` (multiplied by 100), `"float"` (as-is, rounded),
#'   or `"char"` (left alone). Default `"char"`.
#' @param stat_round Number of decimal places for `"percent"` and `"float"`.
#'   Defaults to `gpars$stat_round`.
#' @param formula Documentation: how the number was computed. See
#'   *Formula and text templates*.
#' @param text Documentation: the sentence the number appears in, with `*` as
#'   a placeholder. See *Formula and text templates*.
#' @param section,page Documentation: where the statistic is used in the
#'   report.
#'
#' @return The collection, invisibly.
#' @seealso [append_data()], [save_outputs()]
#' @export
#' @examples
#' out <- init_outputs(default = FALSE, quiet = TRUE)
#' stat_share <- 0.4567
#' append_stats(stat_share, output_list = out, type = "percent", stat_round = 1,
#'              formula = "affected/total",
#'              vars = c(affected = "Affected households",
#'                       total = "All households"),
#'              text = "* per cent of households were affected.",
#'              quiet = TRUE)
#' names(out$stats)
append_stats <- function(output_object,
                         object_name = NULL,
                         output_list = NULL,
                         title = NULL,
                         time_stamp = NULL,
                         desc = NULL,
                         vars = NULL,
                         source = NULL,
                         notes = NULL,
                         text = NULL,
                         section = NULL,
                         page = NULL,
                         type = c("char", "integer", "percent", "float"),
                         stat_round = NULL,
                         formula = NULL,
                         replace = TRUE,
                         quiet = NULL) {
  quiet <- quiet %||% getOption("documenteR.quiet", FALSE)
  type <- match.arg(type)
  store <- resolve_outputs(output_list, env = parent.frame())
  name <- infer_object_name(substitute(output_object), object_name)

  if (length(output_object) != 1L) {
    cli::cli_abort(c(
      "A statistic must be a single value; {.val {name}} has length {length(output_object)}.",
      "i" = "Use {.fun append_table} for a table of numbers."
    ))
  }
  if (!is.numeric(output_object) && !is.character(output_object) &&
      !is.logical(output_object)) {
    cli::cli_abort(
      "A statistic must be numeric, character or logical, not {.cls {class(output_object)[1]}}."
    )
  }
  if (type != "char" && !is.numeric(output_object)) {
    cli::cli_abort(c(
      "{.val {name}} is {.cls {class(output_object)[1]}} but {.arg type} is {.val {type}}.",
      "i" = "Numeric formatting needs a numeric value; use {.code type = \"char\"} otherwise."
    ))
  }
  if (!is_blank(stat_round) &&
      (!is.numeric(stat_round) || stat_round < 0 || stat_round != round(stat_round))) {
    cli::cli_abort("{.arg stat_round} must be a non-negative whole number.")
  }
  if (!is_blank(text) && !grepl("*", text, fixed = TRUE)) {
    cli::cli_warn(c(
      "!" = "{.arg text} for {.val {name}} contains no {.val *} placeholder.",
      "i" = "The formatted value will not appear in the {.field text_fill} column."
    ))
  }

  entry <- compact(list(
    stat = output_object,
    documentation = build_documentation(
      title = title %||% name,
      timestamp = time_stamp %||% dr_timestamp(),
      desc = desc, vars = vars, source = source, notes = notes,
      formula = formula,
      text = text,
      section = section,
      page = page,
      type = type,
      stat_round = stat_round
    )
  ))

  store_entry(store, "stats", name, entry, quiet = quiet, replace = replace)
}


#' Remove appended outputs
#'
#' @description
#' Drops entries from a collection, either by name or an entire output type.
#' Useful when re-running part of a script interactively.
#'
#' @param name Names of entries to remove. `NULL` removes every entry of the
#'   given `type`(s).
#' @param type Output type(s) to remove from. Defaults to all.
#' @param output_list The collection. See [append_plot()].
#' @param quiet Suppress the confirmation message.
#'
#' @return The collection, invisibly.
#' @export
#' @examples
#' out <- init_outputs(default = FALSE, quiet = TRUE)
#' d1 <- cars
#' append_data(d1, output_list = out, quiet = TRUE)
#' remove_outputs("d1", output_list = out, quiet = TRUE)
#' length(out$data)
remove_outputs <- function(name = NULL, type = dr_output_types(),
                           output_list = NULL, quiet = NULL) {
  quiet <- quiet %||% getOption("documenteR.quiet", FALSE)
  store <- resolve_outputs(output_list, env = parent.frame())
  type <- intersect(type, dr_output_types())
  removed <- character(0)

  for (ty in type) {
    lst <- store[[ty]]
    if (length(lst) == 0L) next
    drop <- if (is.null(name)) names(lst) else intersect(name, names(lst))
    if (!length(drop)) next
    lst[drop] <- NULL
    store[[ty]] <- lst
    removed <- c(removed, paste0(ty, "/", drop))
  }

  if (!is.null(name)) {
    missing <- setdiff(name, sub("^[^/]+/", "", removed))
    if (length(missing)) {
      cli::cli_warn("No entr{?y/ies} named {.val {missing}} found in {.field {type}}.")
    }
  }
  if (!quiet) {
    if (length(removed)) {
      cli::cli_alert_success("Removed {length(removed)} entr{?y/ies}: {.val {removed}}.")
    } else {
      cli::cli_alert_info("Nothing to remove.")
    }
  }
  invisible(store)
}
