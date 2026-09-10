# PDF plot catalogue ----------------------------------------------------------
#
# Why this is written from scratch rather than composing ggplot objects:
#
#   * The old catalogue stacked a text "plot" above the real plot with
#     ggpubr::ggarrange() and saved each page at height H + header, so every
#     page had a *different* size. pdftools::pdf_combine() then produced a PDF
#     with mixed page sizes, which every viewer rescales page by page - that
#     is what made the plots look distorted and inconsistently sized.
#   * Stacking with relative heights also stretched the plot panel itself, so
#     a plot in the catalogue no longer matched the standalone export.
#
# Here, one device with one fixed page size is opened for the whole document.
# Each plot is drawn into a viewport of its exact physical size, scaled down
# proportionally only if it genuinely does not fit. Aspect ratio is therefore
# always preserved, and every page is the same size.

# Estimate how many characters of a proportional font fit across a width.
# 0.5 em per character is a good average for Latin text; being slightly
# conservative is fine because the result is only used for line breaking.
chars_per_line <- function(width_in, fontsize_pt) {
  max(12L, floor(width_in * 72 / (fontsize_pt * 0.5)))
}

# Height of one line of text, in inches.
line_height_in <- function(fontsize_pt, leading = 1.35) {
  fontsize_pt * leading / 72
}

# Build the label/value rows shown in a catalogue header, in a fixed order,
# skipping anything undocumented.
catalogue_fields <- function(doc) {
  fields <- list(
    desc    = doc$desc,
    formula = doc$formula,
    vars    = if (!is_blank(doc$vars)) collapse_named(doc$vars) else NULL,
    source  = if (!is_blank(doc$source)) collapse_named(doc$source) else NULL,
    notes   = doc$notes,
    created = doc$timestamp
  )
  fields <- fields[!vapply(fields, is_blank, logical(1))]
  lapply(fields, function(v) paste(as.character(v), collapse = " "))
}

# Measure the height a header would need, so the plot area can be sized before
# anything is drawn.
measure_header <- function(title, fields, content_w, cp) {
  title_lh <- line_height_in(cp$title_fontsize)
  body_lh <- line_height_in(cp$fontsize)
  label_w <- min(1.05, content_w * 0.16)
  value_w <- content_w - label_w - 0.06

  title_lines <- length(strwrap(title, width = chars_per_line(content_w, cp$title_fontsize)))
  rows <- lapply(names(fields), function(nm) {
    lines <- strwrap(fields[[nm]], width = chars_per_line(value_w, cp$fontsize))
    if (!length(lines)) lines <- ""
    list(label = nm, lines = lines, h = length(lines) * body_lh)
  })

  height <- title_lines * title_lh + 0.10 +          # title + rule
    sum(vapply(rows, function(r) r$h, numeric(1))) +
    max(0, length(rows) - 1) * body_lh * 0.35 +      # inter-row spacing
    0.06
  list(height = height, title_lines = title_lines, rows = rows,
       label_w = label_w, value_w = value_w,
       title_lh = title_lh, body_lh = body_lh)
}

# Draw a measured header inside the content viewport. `top` is the y position
# of the top of the header, in inches from the bottom of the content area.
draw_header <- function(title, subtitle, hdr, content_w, top, cp) {
  fam <- cp$fontfamily
  y <- top

  title_lines <- strwrap(title, width = chars_per_line(content_w, cp$title_fontsize))
  grid::grid.text(
    paste(title_lines, collapse = "\n"),
    x = grid::unit(0, "in"), y = grid::unit(y, "in"),
    just = c("left", "top"),
    gp = grid::gpar(fontsize = cp$title_fontsize, fontface = "bold",
                    fontfamily = fam, col = "grey15", lineheight = 1.2)
  )
  y <- y - length(title_lines) * hdr$title_lh

  if (!is_blank(subtitle)) {
    grid::grid.text(
      subtitle,
      x = grid::unit(content_w, "in"), y = grid::unit(top, "in"),
      just = c("right", "top"),
      gp = grid::gpar(fontsize = cp$fontsize * 0.9, fontfamily = fam, col = "grey45")
    )
  }

  # Hairline rule under the title.
  grid::grid.lines(
    x = grid::unit(c(0, content_w), "in"),
    y = grid::unit(rep(y - 0.045, 2), "in"),
    gp = grid::gpar(col = "grey75", lwd = 0.7)
  )
  y <- y - 0.10

  for (r in hdr$rows) {
    grid::grid.text(
      r$label,
      x = grid::unit(0, "in"), y = grid::unit(y, "in"),
      just = c("left", "top"),
      gp = grid::gpar(fontsize = cp$fontsize * 0.85, fontface = "bold",
                      fontfamily = fam, col = "grey45")
    )
    grid::grid.text(
      paste(r$lines, collapse = "\n"),
      x = grid::unit(hdr$label_w, "in"), y = grid::unit(y, "in"),
      just = c("left", "top"),
      gp = grid::gpar(fontsize = cp$fontsize, fontfamily = fam,
                      col = "grey10", lineheight = 1.35)
    )
    y <- y - r$h - hdr$body_lh * 0.35
  }
  invisible(y)
}

# Is this object drawn by the grid graphics engine? Grid plots can be placed
# in a viewport and stay vector; anything else has to be rasterised first,
# because base graphics and grid cannot share a page.
is_grid_plot <- function(x) {
  inherits(x, c("ggplot", "gg", "patchwork", "ggmatrix", "trellis",
                "grob", "gTree", "gtable", "gDesc"))
}

# Render a non-grid plot off-screen and read it back as a raster, so it can be
# placed on the catalogue page like any other grob. Base graphics and grid
# cannot share a page, so for base plots this is the only way to combine a
# typeset header with the plot.
#
# dev.capture() would avoid the round trip through a file, but it is
# unsupported on the Windows PNG devices, so reading the file with the (tiny,
# dependency-free) png package is tried first.
capture_plot_raster <- function(plot, width_in, height_in, res = 200, bg = "white") {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  before <- grDevices::dev.cur()
  ok <- try(
    open_plot_device(tmp, ".png", max(width_in, 0.5), max(height_in, 0.5),
                     dpi = res, bg = bg),
    silent = TRUE
  )
  if (inherits(ok, "try-error")) {
    if (before > 1) try(grDevices::dev.set(before), silent = TRUE)
    return(NULL)
  }
  opened <- grDevices::dev.cur()

  captured <- NULL
  drawn <- try({
    dr_draw(plot)
    if (requireNamespace("png", quietly = TRUE)) {
      NULL
    } else {
      captured <- suppressWarnings(grDevices::dev.capture(native = TRUE))
      NULL
    }
  }, silent = TRUE)

  if (grDevices::dev.cur() == opened) grDevices::dev.off(opened)
  if (before > 1) try(grDevices::dev.set(before), silent = TRUE)

  if (inherits(drawn, "try-error")) return(NULL)
  if (!is.null(captured)) return(captured)

  if (requireNamespace("png", quietly = TRUE) && file.exists(tmp)) {
    img <- try(png::readPNG(tmp), silent = TRUE)
    if (!inherits(img, "try-error")) return(img)
  }
  NULL
}

# Place one plot inside a region of the current page, at true size, centred.
# Returns the scale factor actually used (1 = true size).
draw_plot_in_region <- function(plot, region_w, region_h, region_top,
                                plot_w, plot_h, x_centre,
                                scale_to_fit = TRUE,
                                res = 200, bg = "white") {
  scale <- 1
  if (isTRUE(scale_to_fit)) {
    scale <- min(1, region_w / plot_w, region_h / plot_h)
  }
  w <- plot_w * scale
  h <- plot_h * scale
  # Hang the plot from the top of its region rather than centring it in the
  # leftover space: plots then start at the same height on every page, however
  # much documentation sits above them.
  y_centre <- region_top - h / 2

  vp <- grid::viewport(
    x = grid::unit(x_centre, "in"), y = grid::unit(y_centre, "in"),
    width = grid::unit(w, "in"), height = grid::unit(h, "in"),
    just = c("centre", "centre"), name = "dr_plot_region", clip = "on"
  )

  if (is_grid_plot(plot)) {
    grid::pushViewport(vp)
    on.exit(grid::upViewport(0), add = TRUE)
    if (inherits(plot, c("grob", "gTree", "gtable", "gDesc"))) {
      grid::grid.draw(plot)
    } else {
      # ggplot2 and lattice both honour newpage = FALSE and draw into the
      # viewport that is current when print() is called.
      print(plot, newpage = FALSE)
    }
  } else {
    raster <- capture_plot_raster(plot, w, h, res = res, bg = bg)
    if (is.null(raster)) {
      grid::pushViewport(vp)
      on.exit(grid::upViewport(0), add = TRUE)
      grid::grid.text(
        paste(
          "[this plot could not be placed in the catalogue]",
          "install the png package to include base-graphics plots",
          sep = "\n"
        ),
        gp = grid::gpar(fontsize = 8, col = "grey50", fontface = "italic")
      )
      cli::cli_warn(c(
        "!" = "Could not rasterise a base-graphics plot for the catalogue.",
        "i" = 'Install the {.pkg png} package: {.run install.packages("png")}.'
      ))
      return(NA_real_)
    }
    grid::grid.raster(
      raster,
      x = grid::unit(x_centre, "in"), y = grid::unit(y_centre, "in"),
      width = grid::unit(w, "in"), height = grid::unit(h, "in"),
      interpolate = FALSE
    )
  }
  scale
}

# Decide one page orientation for the whole document. Mixed orientations are
# deliberately not offered: they would reintroduce the mixed-page-size problem
# this rewrite exists to fix.
choose_orientation <- function(items, requested) {
  if (requested != "auto") return(requested)
  if (!length(items)) return("portrait")
  ratios <- vapply(items, function(it) it$plot_w / it$plot_h, numeric(1))
  # Landscape only if plots are clearly wide on average; a portrait page suits
  # a header-above-plot layout better otherwise.
  if (mean(ratios, na.rm = TRUE) > 1.45) "landscape" else "portrait"
}

draw_footer <- function(content_w, y, left_text, right_text, cp) {
  fam <- cp$fontfamily
  grid::grid.lines(
    x = grid::unit(c(0, content_w), "in"),
    y = grid::unit(rep(y + 0.11, 2), "in"),
    gp = grid::gpar(col = "grey85", lwd = 0.6)
  )
  if (!is_blank(left_text)) {
    grid::grid.text(
      left_text, x = grid::unit(0, "in"), y = grid::unit(y, "in"),
      just = c("left", "bottom"),
      gp = grid::gpar(fontsize = cp$fontsize * 0.8, fontfamily = fam, col = "grey50")
    )
  }
  if (!is_blank(right_text)) {
    grid::grid.text(
      right_text, x = grid::unit(content_w, "in"), y = grid::unit(y, "in"),
      just = c("right", "bottom"),
      gp = grid::gpar(fontsize = cp$fontsize * 0.8, fontfamily = fam, col = "grey50")
    )
  }
  invisible(NULL)
}

draw_toc <- function(items, content_w, content_h, cp, project) {
  fam <- cp$fontfamily
  lh <- line_height_in(cp$fontsize)
  grid::grid.text(
    "Contents",
    x = grid::unit(0, "in"), y = grid::unit(content_h, "in"),
    just = c("left", "top"),
    gp = grid::gpar(fontsize = cp$title_fontsize * 1.25, fontface = "bold",
                    fontfamily = fam, col = "grey15")
  )
  if (!is_blank(project)) {
    grid::grid.text(
      project, x = grid::unit(content_w, "in"), y = grid::unit(content_h, "in"),
      just = c("right", "top"),
      gp = grid::gpar(fontsize = cp$fontsize, fontfamily = fam, col = "grey45")
    )
  }
  y <- content_h - line_height_in(cp$title_fontsize * 1.25) - 0.16

  # Two columns once the list is too long for one.
  per_col <- max(1L, floor((y - 0.2) / lh))
  n_col <- if (length(items) > per_col) 2L else 1L
  col_w <- (content_w - 0.25 * (n_col - 1)) / n_col
  page_no_w <- 0.38

  for (i in seq_along(items)) {
    col <- ((i - 1L) %/% per_col) + 1L
    if (col > n_col) break
    row <- (i - 1L) %% per_col
    x0 <- (col - 1L) * (col_w + 0.25)
    yy <- y - row * lh
    label <- items[[i]]$label
    title <- items[[i]]$title
    if (!is_blank(title) && !identical(title, items[[i]]$name)) {
      label <- paste0(label, "  -  ", title)
    }
    label <- truncate_to_width(label, col_w - page_no_w - 0.08, cp$fontsize)
    grid::grid.text(
      label, x = grid::unit(x0, "in"), y = grid::unit(yy, "in"),
      just = c("left", "top"),
      gp = grid::gpar(fontsize = cp$fontsize, fontfamily = fam, col = "grey10")
    )
    grid::grid.text(
      as.character(items[[i]]$page),
      x = grid::unit(x0 + col_w, "in"), y = grid::unit(yy, "in"),
      just = c("right", "top"),
      gp = grid::gpar(fontsize = cp$fontsize, fontfamily = fam, col = "grey50")
    )
  }
  if (length(items) > per_col * n_col) {
    grid::grid.text(
      sprintf("... and %d more; see 00_README.txt", length(items) - per_col * n_col),
      x = grid::unit(0, "in"), y = grid::unit(0.1, "in"),
      just = c("left", "bottom"),
      gp = grid::gpar(fontsize = cp$fontsize * 0.85, fontfamily = fam,
                      col = "grey50", fontface = "italic")
    )
  }
  invisible(NULL)
}

truncate_to_width <- function(x, width_in, fontsize_pt) {
  n <- chars_per_line(width_in, fontsize_pt)
  if (nchar(x) <= n) return(x)
  paste0(substr(x, 1L, max(1L, n - 1L)), "\u2026")
}

#' Build a PDF catalogue of plots
#'
#' @description
#' Writes one PDF in which every plot gets a page: its documentation typeset
#' at the top, the plot itself below at its true physical size. Called
#' automatically by [save_outputs()] when `catalogue = TRUE`; exported so a
#' catalogue can be regenerated on its own.
#'
#' @details
#' All pages share one size and orientation, and plots keep their aspect
#' ratio — a plot only ever gets *smaller* than its export size, never
#' stretched. `ggplot2`, `lattice` and `grid` plots are embedded as vectors;
#' base-graphics plots are rasterised at `gpars$catalogue` resolution,
#' because base and grid graphics cannot share a page.
#'
#' @param output_list The collection whose plots to catalogue. See
#'   [append_plot()].
#' @param path Output PDF path.
#' @param project Optional project/version label printed in the footer.
#' @param items Optional pre-built item list, used internally by
#'   [save_outputs()] so that catalogue entries carry the same numbering as
#'   the exported files. Leave as `NULL`.
#'
#' @return `path`, invisibly.
#' @seealso [dr_catalogue_pars()], [save_outputs()]
#' @export
#' @examples
#' out <- init_outputs(default = FALSE, quiet = TRUE)
#' p1 <- function() plot(cars, pch = 19)
#' append_plot(p1, output_list = out, desc = "Speed against distance",
#'             type = "m_wide", quiet = TRUE)
#' f <- tempfile(fileext = ".pdf")
#' dr_catalogue(out, f)
#' file.exists(f)
dr_catalogue <- function(output_list = NULL, path, project = NULL, items = NULL) {
  store <- resolve_outputs(output_list, env = parent.frame())
  gpars <- store$gpars
  cp <- merge_lists(dr_catalogue_pars(), gpars$catalogue %||% list())

  if (is.null(items)) {
    items <- lapply(names(store$plots), function(nm) {
      entry <- store$plots[[nm]]
      pars <- resolve_plot_pars(entry, gpars)
      list(
        name = nm,
        label = nm,
        plot = entry$plot,
        documentation = entry$documentation %||% list(),
        plot_w = to_inches(pars$W, pars$units),
        plot_h = to_inches(pars$H, pars$units),
        dpi = resolve_dpi(pars$dpi),
        bg = pars$bg
      )
    })
  }
  if (!length(items)) {
    cli::cli_warn("No plots to catalogue; {.path {basename(path)}} not written.")
    return(invisible(NULL))
  }

  paper <- resolve_paper(cp$paper, cp$units)
  orientation <- choose_orientation(items, cp$orientation)
  page_w <- if (orientation == "landscape") max(paper) else min(paper)
  page_h <- if (orientation == "landscape") min(paper) else max(paper)

  m <- to_inches(cp$margin, cp$units)          # top, right, bottom, left
  content_w <- page_w - m[2] - m[4]
  content_h <- page_h - m[1] - m[3]
  if (content_w <= 1 || content_h <= 1) {
    cli::cli_abort(c(
      "Catalogue margins leave no room for content.",
      "i" = "Reduce {.code gpars$catalogue$margin} (currently {cp$margin} {cp$units})."
    ))
  }

  footer_h <- if (isTRUE(cp$page_numbers) || !is_blank(project)) 0.30 else 0

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  before <- grDevices::dev.cur()
  if (capabilities("cairo")) {
    grDevices::cairo_pdf(path, width = page_w, height = page_h, onefile = TRUE, bg = "white")
  } else {
    grDevices::pdf(path, width = page_w, height = page_h, onefile = TRUE,
                   bg = "white", useDingbats = FALSE)
  }
  opened <- grDevices::dev.cur()
  on.exit({
    if (grDevices::dev.cur() == opened) grDevices::dev.off(opened)
    if (before > 1) try(grDevices::dev.set(before), silent = TRUE)
  }, add = TRUE)

  content_vp <- function() {
    grid::viewport(
      x = grid::unit(m[4], "in"), y = grid::unit(m[3], "in"),
      width = grid::unit(content_w, "in"), height = grid::unit(content_h, "in"),
      just = c("left", "bottom"), name = "dr_content"
    )
  }

  n_plot_pages <- length(items)
  toc <- isTRUE(cp$include_toc) && n_plot_pages > 1L
  offset <- if (toc) 1L else 0L
  total_pages <- n_plot_pages + offset

  if (toc) {
    grid::grid.newpage()
    grid::pushViewport(content_vp())
    toc_items <- lapply(seq_along(items), function(i) {
      list(
        label = items[[i]]$label,
        name = items[[i]]$name,
        title = as.character(items[[i]]$documentation$title %||% "")[1],
        page = i + offset
      )
    })
    draw_toc(toc_items, content_w, content_h, cp, project)
    if (isTRUE(cp$page_numbers)) {
      draw_footer(content_w, 0, project, sprintf("%d / %d", 1L, total_pages), cp)
    }
    grid::upViewport(0)
  }

  scales <- numeric(n_plot_pages)
  for (i in seq_len(n_plot_pages)) {
    it <- items[[i]]
    grid::grid.newpage()
    grid::pushViewport(content_vp())

    fields <- catalogue_fields(it$documentation)
    title <- as.character(it$documentation$title %||% it$name)
    hdr <- measure_header(title, fields, content_w, cp)

    max_header <- content_h * cp$header_max
    if (hdr$height > max_header) {
      # Trim the least essential rows from the bottom until the header fits,
      # rather than shrinking the plot.
      while (hdr$height > max_header && length(fields) > 1L) {
        fields <- fields[-length(fields)]
        hdr <- measure_header(title, fields, content_w, cp)
      }
      hdr$truncated <- TRUE
    }
    header_h <- min(hdr$height, max_header)

    draw_header(
      title,
      subtitle = it$label,
      hdr = hdr, content_w = content_w,
      top = content_h, cp = cp
    )

    gap <- 0.14
    region_h <- content_h - header_h - gap - footer_h
    region_w <- content_w
    if (region_h < 0.6) {
      cli::cli_warn(c(
        "!" = "Header for {.val {it$name}} leaves under 1.5 cm for the plot.",
        "i" = "Lower {.code gpars$catalogue$header_max} or shorten the description."
      ))
      region_h <- max(region_h, 0.6)
    }

    scales[i] <- draw_plot_in_region(
      it$plot,
      region_w = region_w, region_h = region_h,
      region_top = footer_h + region_h,
      plot_w = it$plot_w, plot_h = it$plot_h,
      x_centre = content_w / 2,
      scale_to_fit = isTRUE(cp$scale_to_fit),
      res = it$dpi %||% 200, bg = it$bg %||% "white"
    )

    if (footer_h > 0) {
      right <- if (isTRUE(cp$page_numbers)) {
        sprintf("%d / %d", i + offset, total_pages)
      } else {
        NULL
      }
      left <- paste(c(
        project,
        if (!is.na(scales[i]) && scales[i] < 0.999) {
          sprintf("shown at %d%% of export size", round(scales[i] * 100))
        }
      ), collapse = "  \u00B7  ")
      draw_footer(content_w, 0, left, right, cp)
    }
    grid::upViewport(0)
  }

  shrunk <- sum(!is.na(scales) & scales < 0.999)
  if (shrunk > 0) {
    cli::cli_inform(c(
      "i" = "{shrunk} plot{?s} {?was/were} scaled down to fit the catalogue page (aspect ratio preserved).",
      "i" = "The standalone plot files are unaffected."
    ))
  }
  invisible(path)
}
