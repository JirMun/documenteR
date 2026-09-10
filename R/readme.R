# README generation -----------------------------------------------------------
#
# Each output folder gets a plain-text README describing everything in it. The
# layout is a two-column definition list with a fixed label column, so the
# files stay readable in any editor and diff cleanly between versions.

.dr_label_width <- 11L

# One "label   value" block, wrapping the value with a hanging indent so
# continuation lines line up under the first one.
doc_line <- function(label, value, width = 75L, label_w = .dr_label_width) {
  if (is_blank(value)) return(character(0))
  value <- paste(as.character(value), collapse = " ")
  lines <- strwrap(value, width = max(20L, width - label_w))
  if (!length(lines)) return(character(0))
  pad <- strrep(" ", label_w)
  first <- paste0(formatC(label, width = -label_w), lines[1])
  if (length(lines) == 1L) return(first)
  c(first, paste0(pad, lines[-1]))
}

# A named vector rendered as a numbered sub-list, which is much easier to read
# than one long semicolon-separated run when there are many variables.
doc_named_block <- function(label, x, width = 75L, label_w = .dr_label_width) {
  x <- as_named_chr(x)
  if (!length(x)) return(character(0))
  nms <- names(x)
  nms[!nzchar(nms)] <- paste0("V", seq_along(x))[!nzchar(nms)]
  items <- paste0(seq_along(x), ". ", nms, ": ", unname(x))

  out <- character(0)
  pad <- strrep(" ", label_w)
  for (i in seq_along(items)) {
    lines <- strwrap(items[i], width = max(20L, width - label_w),
                     exdent = nchar(nms[i]) + nchar(as.character(i)) + 4L)
    prefix <- if (i == 1L) formatC(label, width = -label_w) else pad
    out <- c(out, paste0(prefix, lines[1]))
    # paste0() treats a zero-length argument as "", so guard the continuation
    # lines explicitly: paste0(pad, character(0)) is `pad`, i.e. a blank line.
    if (length(lines) > 1L) out <- c(out, paste0(pad, lines[-1]))
  }
  out
}

# The documentation block for a single output.
format_doc_block <- function(doc, extra = list(), width = 75L) {
  c(
    doc_line("title", doc$title, width),
    doc_line("desc", doc$desc, width),
    doc_line("formula", doc$formula, width),
    doc_line("formula=", doc$formula_text, width),
    doc_named_block("vars", doc$vars, width),
    doc_named_block("source", doc$source, width),
    doc_line("notes", doc$notes, width),
    unlist(lapply(names(extra), function(nm) doc_line(nm, extra[[nm]], width))),
    doc_line("created", doc$timestamp, width)
  )
}

# Per-type extra facts worth recording: dimensions for data, geometry for
# plots. These answer the questions people actually ask of an export.
readme_extra <- function(type, entry, gpars) {
  doc <- entry$documentation %||% list()
  switch(type,
    "data" = ,
    "tables" = compact(list(
      size = if (!is_blank(doc$nrow)) {
        sprintf("%s rows x %s columns", format(doc$nrow, big.mark = " "), doc$ncol)
      }
    )),
    "plots" = {
      pars <- resolve_plot_pars(entry, gpars)
      compact(list(
        size = sprintf(
          "%g x %g %s%s",
          pars$W, pars$H, pars$units,
          if (!is_blank(pars$type) && pars$type != "default") {
            paste0(" (type \"", pars$type, "\")")
          } else {
            ""
          }
        ),
        dpi = if (!is_blank(pars$dpi)) as.character(pars$dpi)
      ))
    },
    "stats" = compact(list(
      value = if (!is_blank(entry$stat)) as.character(entry$stat),
      format = if (!is_blank(doc$type)) as.character(doc$type),
      used_in = paste(compact(list(
        if (!is_blank(doc$section)) paste0("section ", doc$section),
        if (!is_blank(doc$page)) paste0("page ", doc$page)
      )), collapse = ", "),
      sentence = doc$text
    )),
    list()
  )
}

#' Build the README text for one output type
#'
#' @description
#' Renders the documentation of every appended output of one type into the
#' plain-text README written alongside the files. Called by [save_outputs()];
#' exported so you can preview what an export will document before running it.
#'
#' @param output_list The collection. See [append_plot()].
#' @param output_type One of `"output"`, `"data"`, `"plots"`, `"tables"`,
#'   `"stats"`.
#' @param plan Optional export plan, used internally so that README headings
#'   match the numbered file names exactly. Leave as `NULL`.
#'
#' @return A single string containing the README text.
#' @seealso [save_outputs()]
#' @export
#' @examples
#' out <- init_outputs(default = FALSE, quiet = TRUE)
#' d <- cars
#' append_data(d, output_list = out, desc = "Speed and stopping distance",
#'             source = c(datasets = "R built-in"), quiet = TRUE)
#' cat(get_output_README(out, "data"))
get_output_README <- function(output_list = NULL, output_type, plan = NULL) {
  store <- resolve_outputs(output_list, env = parent.frame())
  gpars <- store$gpars
  width <- as.integer(gpars$readme_w %||% 75L)

  head_spec <- gpars$readme_head[[output_type]] %||%
    list(title = output_type, body = "")

  title <- toupper(as.character(head_spec$title %||% output_type))
  rule1 <- strrep("=", width)
  rule2 <- strrep("-", width)

  body <- unlist(strsplit(as.character(head_spec$body %||% ""), "\n", fixed = TRUE))
  body <- unlist(lapply(body, function(b) strwrap(b, width = width)))

  header <- c(rule1, title, rule1, "", body)

  if (identical(output_type, "output") || !output_type %in% dr_output_types()) {
    return(paste(c(header, ""), collapse = "\n"))
  }

  entries <- store[[output_type]]
  if (!length(entries)) {
    return(paste(c(header, "", "(nothing of this kind was exported)", ""), collapse = "\n"))
  }

  plan <- plan %||% plan_outputs(entries, gpars, output_type)
  # Statistics all land in one file, so listing per-entry formats is noise.
  show_formats <- !identical(output_type, "stats")

  blocks <- lapply(seq_len(nrow(plan)), function(i) {
    nm <- plan$name[i]
    entry <- entries[[nm]]
    heading <- if (identical(output_type, "stats")) nm else plan_label(plan[i, ], formats = show_formats)
    c(
      rule2,
      heading,
      rule2,
      format_doc_block(
        entry$documentation %||% list(),
        extra = readme_extra(output_type, entry, gpars),
        width = width
      ),
      ""
    )
  })

  paste(c(header, "", unlist(blocks)), collapse = "\n")
}
