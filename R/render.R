#' Draw a plot object on the current graphics device
#'
#' @description
#' The single point of extension for plot support. `documenteR` never calls
#' `ggsave()`; it opens a graphics device itself and asks `dr_draw()` to draw
#' the object on it. That is what makes the package work with any plotting
#' system, and it is what guarantees that a plot in the PDF catalogue has
#' exactly the same geometry as the standalone file.
#'
#' @details
#' Methods are provided for `ggplot2` objects (including `patchwork`
#' compositions), `lattice`/`trellis` objects, `grid` grobs and `gtable`s,
#' recorded base plots, plain drawing functions, and unevaluated calls or
#' expressions. The default method falls back to `print()` and then `plot()`.
#'
#' To support another plotting system, define a method:
#'
#' ```r
#' dr_draw.my_plot_class <- function(x, ...) {
#'   my_render_function(x)
#'   invisible(x)
#' }
#' ```
#'
#' A method must draw on the *current* device and must not open or close one.
#'
#' @param x The plot object.
#' @param ... Passed to methods.
#'
#' @return `x`, invisibly.
#' @seealso [append_plot()], [dr_save_plot()]
#' @export
#' @examples
#' f <- tempfile(fileext = ".png")
#' grDevices::png(f, width = 400, height = 300)
#' dr_draw(function() plot(1:10))
#' grDevices::dev.off()
#' file.exists(f)
dr_draw <- function(x, ...) UseMethod("dr_draw")

#' @rdname dr_draw
#' @export
dr_draw.default <- function(x, ...) {
  drawn <- FALSE
  if (is.object(x) || isS4(x)) {
    ok <- try(print(x), silent = TRUE)
    drawn <- !inherits(ok, "try-error")
  }
  if (!drawn) {
    ok <- try(graphics::plot(x), silent = TRUE)
    drawn <- !inherits(ok, "try-error")
  }
  if (!drawn) {
    cli::cli_abort(c(
      "Do not know how to draw an object of class {.cls {class(x)[1]}}.",
      "i" = "Define a {.fun dr_draw} method for it, or wrap the drawing code in a function of no arguments."
    ))
  }
  invisible(x)
}

#' @rdname dr_draw
#' @export
dr_draw.ggplot <- function(x, ...) {
  print(x)
  invisible(x)
}

#' @rdname dr_draw
#' @export
dr_draw.trellis <- function(x, ...) {
  print(x)
  invisible(x)
}

#' @rdname dr_draw
#' @export
dr_draw.grob <- function(x, ...) {
  # No grid.newpage() here: the caller owns page management, so that the same
  # method works for a standalone file and for a catalogue page.
  grid::grid.draw(x)
  invisible(x)
}

#' @rdname dr_draw
#' @export
dr_draw.gTree <- dr_draw.grob

#' @rdname dr_draw
#' @export
dr_draw.gtable <- dr_draw.grob

#' @rdname dr_draw
#' @export
dr_draw.recordedplot <- function(x, ...) {
  grDevices::replayPlot(x)
  invisible(x)
}

#' @rdname dr_draw
#' @export
dr_draw.function <- function(x, ...) {
  fmls <- formals(x)
  no_default <- vapply(fmls, function(f) identical(f, quote(expr = )), logical(1))
  required <- setdiff(names(fmls)[no_default], "...")
  if (length(required)) {
    cli::cli_abort(c(
      "A plotting function must take no required arguments.",
      "x" = "This one requires {.arg {required}}.",
      "i" = "Wrap it: {.code function() my_plot(x = {required[1]})}."
    ))
  }
  x()
  invisible(x)
}

#' @rdname dr_draw
#' @export
dr_draw.call <- function(x, ...) {
  eval(x, envir = attr(x, "dr_env") %||% parent.frame())
  invisible(x)
}

#' @rdname dr_draw
#' @export
dr_draw.expression <- function(x, ...) {
  eval(x, envir = attr(x, "dr_env") %||% parent.frame())
  invisible(x)
}

#' @rdname dr_draw
#' @export
dr_draw.formula <- function(x, ...) {
  eval(x[[length(x)]], envir = environment(x) %||% parent.frame())
  invisible(x)
}

#' Can this object be drawn?
#'
#' @description
#' Used by [append_plot()] to reject objects it could not export, at the
#' moment they are appended rather than half an hour later during
#' [save_outputs()].
#'
#' @param x Any object.
#' @return `TRUE` or `FALSE`.
#' @seealso [dr_draw()]
#' @export
#' @examples
#' dr_can_draw(function() plot(1:10))
#' dr_can_draw(cars)
dr_can_draw <- function(x) {
  if (is.null(x)) return(FALSE)
  if (is.function(x)) return(TRUE)
  known <- c(
    "ggplot", "gg", "patchwork", "ggmatrix", "ggarrange",
    "trellis", "recordedplot", "grob", "gTree", "gDesc", "gtable",
    "Heatmap", "HeatmapList", "plotly", "ggsurvplot", "venn", "upset"
  )
  if (inherits(x, known)) return(TRUE)
  if (is.call(x) || is.expression(x) || inherits(x, "formula")) return(TRUE)
  if (is.data.frame(x)) return(FALSE)
  if (is.atomic(x) && !is.object(x)) return(FALSE)
  # Anything else is drawable if it has a print or plot method of its own.
  any(vapply(class(x), function(cl) {
    !is.null(utils::getS3method("print", cl, optional = TRUE)) ||
      !is.null(utils::getS3method("plot", cl, optional = TRUE))
  }, logical(1)))
}


# Device dispatch -------------------------------------------------------------

# Map an extension to a function that opens a device of the requested physical
# size. `ragg` and `svglite` are used when available because they produce
# better text rendering and honour font families more reliably than the base
# devices; both are optional.
open_plot_device <- function(path, ext, width_in, height_in, dpi = 300,
                             bg = "white", family = "") {
  ext <- normalise_ext(ext)
  res <- resolve_dpi(dpi)
  fam <- if (nzchar(family)) family else ""

  raster <- function(agg_fun, base_fun, base_extra = list()) {
    if (requireNamespace("ragg", quietly = TRUE)) {
      args <- list(
        filename = path, width = width_in, height = height_in,
        units = "in", res = res, background = bg
      )
      do.call(getExportedValue("ragg", agg_fun), args)
    } else {
      args <- c(list(
        filename = path, width = width_in, height = height_in,
        units = "in", res = res, bg = bg
      ), base_extra)
      if (capabilities("cairo")) args$type <- "cairo"
      do.call(base_fun, args)
    }
  }

  switch(ext,
    ".png"  = raster("agg_png", grDevices::png),
    ".jpg"  = raster("agg_jpeg", grDevices::jpeg, list(quality = 95)),
    ".jpeg" = raster("agg_jpeg", grDevices::jpeg, list(quality = 95)),
    ".tiff" = raster("agg_tiff", grDevices::tiff, list(compression = "lzw")),
    ".svg"  = if (requireNamespace("svglite", quietly = TRUE)) {
      svglite::svglite(path, width = width_in, height = height_in, bg = bg)
    } else {
      grDevices::svg(path, width = width_in, height = height_in, bg = bg)
    },
    ".pdf"  = if (capabilities("cairo")) {
      grDevices::cairo_pdf(path, width = width_in, height = height_in, bg = bg)
    } else {
      grDevices::pdf(path, width = width_in, height = height_in, bg = bg,
                     onefile = TRUE, useDingbats = FALSE)
    },
    ".eps"  = if (capabilities("cairo")) {
      grDevices::cairo_ps(path, width = width_in, height = height_in, bg = bg)
    } else {
      grDevices::postscript(path, width = width_in, height = height_in,
                            paper = "special", horizontal = FALSE, bg = bg)
    },
    cli::cli_abort(c(
      "Cannot write plots to {.val {ext}}.",
      "i" = "Supported: {.val {dr_formats()$plots}}."
    ))
  )
  invisible(TRUE)
}

#' Save a plot to a file at an exact physical size
#'
#' @description
#' Opens a graphics device of exactly `W` x `H` `units`, draws `plot` on it
#' with [dr_draw()] and closes it. This is the low-level workhorse behind
#' [save_outputs()]; it is exported because it is genuinely useful on its own,
#' and because it is the function to test against when a plot does not come
#' out the size you expect.
#'
#' @param plot A plot object — anything [dr_draw()] handles.
#' @param path Output file path. Its extension selects the format.
#' @param W,H Width and height.
#' @param units Units for `W` and `H`: `"cm"`, `"mm"`, `"in"` or `"px"`.
#' @param dpi Resolution for raster formats, or `"retina"`/`"print"`/`"screen"`.
#' @param bg Background colour.
#' @param family Font family; `""` uses the device default.
#'
#' @return `path`, invisibly.
#' @seealso [dr_draw()], [preview_plot()]
#' @export
#' @examples
#' p <- function() barplot(c(a = 3, b = 5, c = 2))
#' f <- tempfile(fileext = ".png")
#' dr_save_plot(p, f, W = 12, H = 8, units = "cm")
#' file.exists(f)
dr_save_plot <- function(plot, path, W = 16, H = 12.35, units = "cm",
                         dpi = 300, bg = "white", family = "") {
  ext <- normalise_ext(tools::file_ext(path))
  if (!nzchar(ext) || identical(ext, ".")) {
    cli::cli_abort("{.arg path} needs a file extension, e.g. {.path plot.png}.")
  }
  if (!is.numeric(W) || !is.numeric(H) || W <= 0 || H <= 0) {
    cli::cli_abort("{.arg W} and {.arg H} must be positive numbers (got {W} x {H}).")
  }

  res <- resolve_dpi(dpi)
  width_in <- to_inches(W, units, dpi = res)
  height_in <- to_inches(H, units, dpi = res)

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  before <- grDevices::dev.cur()
  open_plot_device(path, ext, width_in, height_in, dpi = res, bg = bg, family = family)
  opened <- grDevices::dev.cur()
  # Close the device we opened even if drawing fails, so a single broken plot
  # cannot leave a locked file handle behind and derail the whole export.
  on.exit({
    if (grDevices::dev.cur() == opened && opened != before) grDevices::dev.off(opened)
  }, add = TRUE)

  dr_draw(plot)
  invisible(path)
}

# ggplot2 label wrapping ------------------------------------------------------

# Wrap the title/subtitle/axis labels of a ggplot to the character widths in
# gpars$textwidths. Only touched when the user asks for it, because silently
# rewriting labels surprises people.
wrap_plot_labels <- function(plot, widths) {
  if (!inherits(plot, "ggplot") || !length(widths)) return(plot)
  labs <- plot$labels
  if (!length(labs)) return(plot)
  for (nm in names(labs)) {
    width <- widths[[nm]] %||% widths$labs
    if (is_blank(width) || !is.character(labs[[nm]]) || length(labs[[nm]]) != 1L) next
    wrapped <- try(
      paste(strwrap(enc2utf8(labs[[nm]]), width = as.numeric(width)), collapse = "\n"),
      silent = TRUE
    )
    if (!inherits(wrapped, "try-error") && nzchar(wrapped)) plot$labels[[nm]] <- wrapped
  }
  plot
}


#' Preview a plot at its export size
#'
#' @description
#' Saves a plot to a temporary file using exactly the geometry
#' [save_outputs()] would use, and opens it in the system viewer. The quickest
#' way to check whether a chosen `type` or `H`/`W` actually suits a plot,
#' before committing to a full export.
#'
#' @param plot A plot object, or the name of an already-appended plot.
#' @param output_list The collection whose `gpars` supply the defaults. See
#'   [append_plot()].
#' @param type,H,W,units,dpi,device,bg Geometry overrides; anything omitted
#'   comes from `type`, then from `gpars$sizes$default`.
#' @param open Open the file in the system viewer. Default `TRUE`.
#' @param quiet Suppress the confirmation message.
#'
#' @return The path to the temporary file, invisibly.
#' @seealso [append_plot()], [dr_save_plot()]
#' @export
#' @examples
#' out <- init_outputs(default = FALSE, quiet = TRUE)
#' p <- function() plot(cars)
#' f <- preview_plot(p, output_list = out, type = "s_wide", open = FALSE)
#' basename(f)
preview_plot <- function(plot,
                         output_list = NULL,
                         type = NULL,
                         H = NULL,
                         W = NULL,
                         units = NULL,
                         dpi = NULL,
                         device = NULL,
                         bg = NULL,
                         open = TRUE,
                         quiet = NULL) {
  quiet <- quiet %||% getOption("documenteR.quiet", FALSE)
  store <- resolve_outputs(output_list, env = parent.frame())

  # Allow naming an already-appended plot.
  if (is.character(plot) && length(plot) == 1L && plot %in% names(store$plots)) {
    entry <- store$plots[[plot]]
    label <- plot
    plot <- entry$plot
    type <- type %||% entry$type
    H <- H %||% entry$H
    W <- W %||% entry$W
    units <- units %||% entry$units
    dpi <- dpi %||% entry$dpi
    device <- device %||% entry$device
    bg <- bg %||% entry$bg
  } else {
    label <- "preview"
  }

  entry <- compact(list(type = type, H = H, W = W, units = units, dpi = dpi,
                        device = device, bg = bg))
  pars <- resolve_plot_pars(entry, store$gpars)
  ext <- normalise_ext(pars$device[1] %||% ".png")

  path <- tempfile(paste0("documenteR_", sanitise_filename(label), "_"), fileext = ext)
  dr_save_plot(plot, path, W = pars$W, H = pars$H, units = pars$units,
               dpi = pars$dpi, bg = pars$bg)

  if (!quiet) {
    cli::cli_alert_success(
      "Preview: {.val {pars$W}} x {.val {pars$H}} {pars$units} ({pars$type}) at {.path {path}}"
    )
  }
  if (isTRUE(open)) {
    ok <- try(utils::browseURL(path), silent = TRUE)
    if (inherits(ok, "try-error")) {
      cli::cli_alert_info("Could not open a viewer; the file is at {.path {path}}.")
    }
  }
  invisible(path)
}
