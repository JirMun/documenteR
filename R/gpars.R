#' Default general parameters for an output collection
#'
#' @description
#' Every output collection carries a list of *general parameters* under
#' `gpars`, which supply the defaults used when an individual output does not
#' specify its own. `dr_gpars()` returns that list, so you can inspect the
#' defaults or build a modified set to hand to [init_outputs()].
#'
#' @details
#' The list has the following components.
#'
#' \describe{
#'   \item{`formats`}{Default file formats per output type. See
#'     [dr_formats()] for what is supported.}
#'   \item{`sizes`}{Plot dimensions. `sizes$default` is used when an output
#'     names no `type`; `sizes$type` holds named presets (`"s"`, `"m_wide"`,
#'     ...) that outputs can select with `type =`. Each entry may set
#'     `units`, `H` (height), `W` (width), `dpi` and `bg` (background).
#'     The defaults are built for an A4 page with 2.5 cm margins, so 16 cm
#'     is the usable text width and 12.35 cm is half the usable height.}
#'   \item{`textwidths`}{Character widths used to wrap plot titles, subtitles
#'     and axis labels, per `type`. Only applied when
#'     `append_plot(wrap_labels = TRUE)`.}
#'   \item{`catalogue`}{Appearance of the PDF plot catalogue. See
#'     [dr_catalogue_pars()].}
#'   \item{`readme_head`}{Title and introductory body text for each output
#'     type's `00_README.txt`.}
#'   \item{`readme_w`}{Width in characters to which README text is wrapped.}
#'   \item{`encoding`}{Encoding for text output. Only `"UTF-8"` is really
#'     supported and it is the default.}
#'   \item{`units`}{Default length unit (`"cm"`, `"mm"`, `"in"`).}
#'   \item{`stat_round`}{Default number of decimal places for statistics.}
#'   \item{`csv_delim`}{Field separator for `.csv`. `.csv2` always uses
#'     `";"` with a comma decimal mark, the convention Czech and German
#'     Excel expects.}
#'   \item{`csv_bom`}{Write a UTF-8 byte-order mark at the start of CSV
#'     files. Makes Excel open accented characters correctly, but may
#'     confuse naive parsers. Default `FALSE`.}
#'   \item{`big_mark`}{Thousands separator used when formatting statistics.
#'     Default `" "`, the Czech and SI convention.}
#'   \item{`codebook`}{Write a variable-level codebook for appended data.
#'     Default `TRUE`.}
#'   \item{`hash`}{Record an MD5 checksum of every written file in the
#'     manifest. Default `TRUE`.}
#' }
#'
#' @param ... Named values to override. Overrides are merged *recursively*,
#'   so `dr_gpars(formats = list(plots = ".svg"))` changes the plot format
#'   and leaves the other formats at their defaults.
#'
#' @return A named list of general parameters.
#' @seealso [init_outputs()], [dr_catalogue_pars()], [dr_formats()]
#' @export
#' @examples
#' gp <- dr_gpars(formats = list(plots = c(".png", ".svg")), stat_round = 2)
#' gp$formats$plots
#' gp$formats$data # untouched default
dr_gpars <- function(...) {
  base <- .dr_gpars_default()
  overrides <- list(...)
  if (length(overrides)) {
    if (is.null(names(overrides)) || any(!nzchar(names(overrides)))) {
      cli::cli_abort("All arguments to {.fun dr_gpars} must be named.")
    }
    unknown <- setdiff(names(overrides), names(base))
    if (length(unknown)) {
      cli::cli_warn(c(
        "!" = "Unknown general parameter{?s}: {.field {unknown}}.",
        "i" = "Known parameters are {.field {names(base)}}."
      ))
    }
    base <- merge_lists(base, overrides)
  }
  base
}

# Recursive list merge: values in `y` win, but a named list in `y` is merged
# into the corresponding list in `x` rather than replacing it wholesale. This
# is what lets a user override gpars$formats$plots without silently losing
# gpars$formats$data.
merge_lists <- function(x, y) {
  for (nm in names(y)) {
    if (is.list(y[[nm]]) && is.list(x[[nm]]) && !is.null(names(y[[nm]]))) {
      x[[nm]] <- merge_lists(x[[nm]], y[[nm]])
    } else {
      x[[nm]] <- y[[nm]]
    }
  }
  x
}

.dr_gpars_default <- function() {
  list(
    # -- file formats ------------------------------------------------------
    formats = list(
      data   = ".csv",
      plots  = ".png",
      tables = ".csv",
      stats  = ".csv"
    ),

    # -- plot geometry -----------------------------------------------------
    # Sized for A4 (21 x 29.7 cm) with 2.5 cm margins: 16 cm usable width,
    # 24.7 cm usable height, 12.35 cm = half the usable height.
    sizes = list(
      default = list(units = "cm", H = 12.35, W = 16, dpi = "retina", bg = "white"),
      type = list(
        s           = list(units = "cm", H = 8,     W = 8),
        s_wide      = list(units = "cm", H = 8,     W = 16),
        s_tall      = list(units = "cm", H = 16,    W = 8),
        m           = list(units = "cm", H = 12.35, W = 12.35),
        m_wide      = list(units = "cm", H = 12.35, W = 16),
        m_tall      = list(units = "cm", H = 24.7,  W = 12.35),
        l           = list(units = "cm", H = 16,    W = 24.7),
        l_landscape = list(units = "cm", H = 24.7,  W = 16)
      )
    ),

    # -- label wrapping widths (characters) --------------------------------
    textwidths = list(
      default = list(title = 40, subtitle = 45, labs = 12, x = 35, y = 35),
      type = list(
        s           = list(title = 20, subtitle = 20,  labs = 10, x = 20,  y = 20),
        s_wide      = list(title = 55, subtitle = 60,  labs = 10, x = 60,  y = 20),
        s_tall      = list(title = 20, subtitle = 20,  labs = 10, x = 20,  y = 40),
        m           = list(title = 60, subtitle = 70,  labs = 12, x = 60,  y = 35),
        m_wide      = list(title = 55, subtitle = 60,  labs = 10, x = 60,  y = 35),
        m_tall      = list(title = 40, subtitle = 45,  labs = 12, x = 35,  y = 35),
        l           = list(title = 55, subtitle = 70,  labs = 10, x = 60,  y = 60),
        l_landscape = list(title = 85, subtitle = 100, labs = 20, x = 100, y = 75)
      )
    ),

    # -- PDF catalogue -----------------------------------------------------
    catalogue = dr_catalogue_pars(),

    # -- README text -------------------------------------------------------
    readme_head = list(
      output = list(
        title = "Output",
        body  = "This folder contains the output collection object and a copy of the code that produced it."
      ),
      data   = list(title = "Data",       body = "Data contained in this folder:"),
      plots  = list(title = "Plots",      body = "Plots contained in this folder:"),
      tables = list(title = "Tables",     body = "Tables contained in this folder:"),
      stats  = list(title = "Statistics", body = "Statistics contained in this folder:")
    ),
    readme_w = 75,

    # -- misc --------------------------------------------------------------
    encoding   = "UTF-8",
    units      = "cm",
    stat_round = 3,
    csv_delim  = ",",
    csv_bom    = FALSE,
    big_mark   = " ",
    codebook   = TRUE,
    hash       = TRUE
  )
}

#' Appearance of the PDF plot catalogue
#'
#' @description
#' The catalogue is a single PDF in which every appended plot gets one page,
#' drawn at its true physical size with its documentation typeset above it.
#' This function builds the parameter list controlling its appearance; pass
#' the result as the `catalogue` component of `gpars`.
#'
#' @param paper Page size: one of `"a5"`, `"a4"`, `"a3"`, `"letter"`,
#'   `"legal"`, or a numeric `c(width, height)` in `units`.
#' @param orientation `"auto"` (default) picks portrait or landscape per page
#'   from the plot's aspect ratio; `"portrait"` and `"landscape"` force one.
#'   Under `"auto"` every page still has the *same* dimensions, only rotated,
#'   so PDF viewers no longer rescale each page by a different factor.
#' @param units Units for `paper` and `margin`.
#' @param margin Page margin: a single value, or `c(top, right, bottom, left)`.
#' @param header_max Maximum share of page height (strictly between 0 and 1)
#'   that the documentation header may occupy. The header shrinks to fit its
#'   content but never grows past this, so the plot is never squeezed away.
#' @param fontsize Base font size in points for header body text.
#' @param title_fontsize Font size in points for the page title.
#' @param fontfamily Font family name. `""` uses the device default, which is
#'   the safest choice for characters outside Latin-1.
#' @param scale_to_fit If `TRUE` (default) a plot larger than the available
#'   area is scaled down *proportionally*, preserving its aspect ratio. If
#'   `FALSE` it is drawn at full size and clipped.
#' @param page_numbers Print "n / N" in the page footer.
#' @param include_toc Begin the catalogue with a contents page.
#'
#' @return A named list of catalogue parameters.
#' @seealso [dr_gpars()], [save_outputs()]
#' @export
#' @examples
#' dr_catalogue_pars(paper = "a3", orientation = "landscape")
dr_catalogue_pars <- function(paper = "a4",
                              orientation = c("auto", "portrait", "landscape"),
                              units = "cm",
                              margin = 1.5,
                              header_max = 0.32,
                              fontsize = 8.5,
                              title_fontsize = 12,
                              fontfamily = "",
                              scale_to_fit = TRUE,
                              page_numbers = TRUE,
                              include_toc = TRUE) {
  orientation <- match.arg(orientation)
  if (length(margin) == 1L) margin <- rep(margin, 4L)
  if (length(margin) != 4L) {
    cli::cli_abort(
      "{.arg margin} must have length 1 or 4 ({.code c(top, right, bottom, left)})."
    )
  }
  if (!is.numeric(header_max) || length(header_max) != 1L ||
      header_max <= 0 || header_max >= 1) {
    cli::cli_abort("{.arg header_max} must be a single number strictly between 0 and 1.")
  }
  list(
    paper = paper,
    orientation = orientation,
    units = units,
    margin = margin,
    header_max = header_max,
    fontsize = fontsize,
    title_fontsize = title_fontsize,
    fontfamily = fontfamily,
    scale_to_fit = scale_to_fit,
    page_numbers = page_numbers,
    include_toc = include_toc
  )
}

# Paper sizes in centimetres, portrait orientation.
.dr_paper_sizes <- list(
  a5     = c(14.8, 21.0),
  a4     = c(21.0, 29.7),
  a3     = c(29.7, 42.0),
  letter = c(21.59, 27.94),
  legal  = c(21.59, 35.56)
)

# Returns c(width, height) in inches.
resolve_paper <- function(paper, units = "cm") {
  if (is.numeric(paper)) {
    if (length(paper) != 2L) {
      cli::cli_abort("A numeric {.arg paper} must be {.code c(width, height)}.")
    }
    return(to_inches(paper, units))
  }
  key <- tolower(as.character(paper)[1])
  if (!key %in% names(.dr_paper_sizes)) {
    cli::cli_abort(c(
      "Unknown paper size {.val {key}}.",
      "i" = "Use one of {.val {names(.dr_paper_sizes)}}, or a numeric {.code c(width, height)}."
    ))
  }
  to_inches(.dr_paper_sizes[[key]], "cm")
}

# Resolve the size and label parameters for one plot entry: values set on the
# entry win over the named `type`, which wins over `default`. This replaces
# the original three-way tibble join, which was hard to follow and silently
# produced NA when a type was unknown.
resolve_plot_pars <- function(entry, gpars) {
  sizes <- gpars$sizes %||% gpars$ggplot %||% list()
  tw <- gpars$textwidths %||% list()

  pars <- sizes$default %||% list()
  labs <- tw$default %||% list()

  type <- entry$type
  if (!is_blank(type)) {
    type <- as.character(type)[1]
    if (!is.null(sizes$type[[type]])) pars <- merge_lists(pars, sizes$type[[type]])
    if (!is.null(tw$type[[type]])) labs <- merge_lists(labs, tw$type[[type]])
  } else {
    type <- "default"
  }

  for (nm in c("units", "H", "W", "dpi", "bg")) {
    if (!is_blank(entry[[nm]])) pars[[nm]] <- entry[[nm]]
  }

  pars$device <- if (!is_blank(entry$device)) {
    split_spec(entry$device)
  } else {
    split_spec(gpars$formats$plots)
  }
  pars$type <- type
  pars$textwidths <- labs
  pars$units <- pars$units %||% gpars$units %||% "cm"
  pars$bg <- pars$bg %||% "white"
  pars$H <- as.numeric(pars$H %||% 12.35)
  pars$W <- as.numeric(pars$W %||% 16)
  pars
}
