#' Initialise an output collection
#'
#' @description
#' Creates the collection that [append_plot()], [append_data()],
#' [append_table()] and [append_stats()] add to, and that [save_outputs()]
#' exports. Call it once near the top of an analysis script.
#'
#' @details
#' The collection is an environment with class `documenteR_outputs`. Because
#' environments have reference semantics, `append_*()` adds to it *in place*
#' — you never have to write `list_outputs <- append_plot(list_outputs, p)`.
#' It otherwise behaves like the nested list it replaces:
#'
#' ```r
#' length(out$plots)                    # number of appended plots
#' out$gpars$formats$plots <- ".svg"    # change a general parameter
#' names(out$data)                      # names of appended datasets
#' ```
#'
#' With `default = TRUE` the collection is additionally assigned to
#' `list_outputs` in the calling environment, which for a script run at top
#' level is the global environment. That is what allows the rest of the
#' `append_*()` calls to omit `output_list`.
#'
#' @param default If `TRUE` (default), assign the new collection to
#'   `list_outputs` in the calling environment *and* register it as the
#'   active collection, so `append_*()` and [save_outputs()] find it without
#'   being told. If `FALSE`, only return it.
#' @param gpars General parameters. Defaults to [dr_gpars()]; pass
#'   `dr_gpars(...)` to override individual settings.
#' @param quiet Suppress the confirmation message.
#'
#' @return A `documenteR_outputs` object, invisibly when `default = TRUE`.
#' @seealso [dr_gpars()], [append_plot()], [save_outputs()]
#' @export
#' @examples
#' # A self-contained collection, not registered anywhere
#' out <- init_outputs(default = FALSE, quiet = TRUE)
#' out
#'
#' # Custom defaults
#' out2 <- init_outputs(
#'   default = FALSE,
#'   gpars = dr_gpars(formats = list(plots = c(".png", ".pdf"))),
#'   quiet = TRUE
#' )
#' out2$gpars$formats$plots
init_outputs <- function(default = TRUE, gpars = dr_gpars(), quiet = NULL) {
  quiet <- quiet %||% getOption("documenteR.quiet", FALSE)
  if (!is.list(gpars)) {
    cli::cli_abort(c(
      "{.arg gpars} must be a list of general parameters.",
      "i" = "Build one with {.fun dr_gpars}."
    ))
  }

  store <- new.env(parent = emptyenv())
  store$data <- list()
  store$plots <- list()
  store$tables <- list()
  store$stats <- list()
  store$gpars <- merge_lists(.dr_gpars_default(), gpars)
  store$.created <- dr_timestamp()
  class(store) <- c("documenteR_outputs", "environment")

  if (isTRUE(default)) {
    .dr_state$active <- store
    assign("list_outputs", store, envir = parent.frame())
    if (!quiet) {
      cli::cli_alert_success(
        "Output collection initialised as {.var list_outputs}."
      )
    }
    return(invisible(store))
  }
  store
}

#' The output types a collection holds
#'
#' @return A character vector of output type names.
#' @export
#' @examples
#' dr_output_types()
dr_output_types <- function() c("data", "plots", "tables", "stats")

#' @export
print.documenteR_outputs <- function(x, ...) {
  cli::cli_h1("documenteR output collection")
  counts <- vapply(dr_output_types(), function(ty) length(x[[ty]]), integer(1))
  if (sum(counts) == 0L) {
    cli::cli_alert_info("Empty. Append outputs with {.fun append_plot} and friends.")
  } else {
    for (ty in dr_output_types()) {
      if (counts[[ty]] == 0L) next
      nms <- names(x[[ty]])
      cli::cli_text("{.strong {ty}} ({counts[[ty]]}): {.val {nms}}")
    }
  }
  fmt <- x$gpars$formats
  cli::cli_text("")
  cli::cli_text(
    "{.emph formats} data {.val {fmt$data}} \u00B7 plots {.val {fmt$plots}} \u00B7 ",
    "tables {.val {fmt$tables}} \u00B7 stats {.val {fmt$stats}}"
  )
  if (!is.null(x$.created)) cli::cli_text("{.emph created} {x$.created}")
  invisible(x)
}

#' @export
format.documenteR_outputs <- function(x, ...) {
  counts <- vapply(dr_output_types(), function(ty) length(x[[ty]]), integer(1))
  sprintf(
    "<documenteR_outputs: %s>",
    paste(sprintf("%s=%d", names(counts), counts), collapse = ", ")
  )
}

#' @export
length.documenteR_outputs <- function(x) {
  sum(vapply(dr_output_types(), function(ty) length(x[[ty]]), integer(1)))
}

#' @export
summary.documenteR_outputs <- function(object, ...) {
  rows <- lapply(dr_output_types(), function(ty) {
    entries <- object[[ty]]
    if (length(entries) == 0L) return(NULL)
    data.frame(
      type = ty,
      name = names(entries),
      title = vapply(entries, function(e) {
        as.character(e$documentation$title %||% NA_character_)[1]
      }, character(1)),
      subfolder = vapply(entries, function(e) {
        as.character(e$subfolder %||% "")[1]
      }, character(1)),
      documented = vapply(entries, function(e) {
        !is_blank(e$documentation$desc)
      }, logical(1)),
      stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) {
    return(data.frame(
      type = character(0), name = character(0), title = character(0),
      subfolder = character(0), documented = logical(0),
      stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Coerce to an output collection
#'
#' @description
#' Converts a plain nested list — for example a `list_outputs.rds` written by
#' an older version of the package, or by the original standalone script —
#' into a `documenteR_outputs` object. Legacy `gpars$ggplot` is mapped to
#' `gpars$sizes`, and any missing general parameters are filled in from
#' [dr_gpars()].
#'
#' @param x A `documenteR_outputs` object (returned unchanged) or a list with
#'   `data`, `plots`, `tables`, `stats` and/or `gpars` components.
#' @param ... Unused, for method consistency.
#'
#' @return A `documenteR_outputs` object.
#' @export
#' @examples
#' legacy <- list(plots = list(), data = list(), gpars = list(stat_round = 1))
#' out <- as_outputs(legacy)
#' out$gpars$stat_round
#' out$gpars$formats$data # filled in from the defaults
as_outputs <- function(x, ...) UseMethod("as_outputs")

#' @export
as_outputs.documenteR_outputs <- function(x, ...) x

#' @export
as_outputs.list <- function(x, ...) {
  gpars <- x$gpars %||% list()
  # The original script kept plot geometry under gpars$ggplot; the package
  # calls it gpars$sizes because plots are no longer ggplot-only.
  if (!is.null(gpars$ggplot) && is.null(gpars$sizes)) {
    gpars$sizes <- gpars$ggplot
    gpars$ggplot <- NULL
  }
  store <- init_outputs(default = FALSE, gpars = gpars, quiet = TRUE)
  for (ty in dr_output_types()) {
    entries <- x[[ty]] %||% list()
    if (!is.list(entries)) {
      cli::cli_abort("{.field {ty}} must be a list of output entries, not {.cls {class(entries)}}.")
    }
    store[[ty]] <- entries
  }
  store
}

#' @export
as_outputs.default <- function(x, ...) {
  cli::cli_abort(c(
    "Cannot use {.cls {class(x)[1]}} as an output collection.",
    "i" = "Create one with {.fun init_outputs}, or pass a list with {.field data}/{.field plots}/{.field tables}/{.field stats}."
  ))
}

#' @export
as.list.documenteR_outputs <- function(x, ...) {
  out <- lapply(dr_output_types(), function(ty) x[[ty]])
  names(out) <- dr_output_types()
  out$gpars <- x$gpars
  out
}

# Internal: find the collection to operate on ---------------------------------
#
# Resolution order, deliberately explicit so that error messages can say what
# was tried:
#   1. `output_list` given as an object                -> use it
#   2. `output_list` given as a name (character)       -> look it up
#   3. `list_outputs` visible from the caller          -> use it
#   4. the collection registered by the last init_outputs() -> use it
resolve_outputs <- function(output_list = NULL,
                            env = parent.frame(2L),
                            arg = "output_list",
                            call = rlang::caller_env()) {
  if (inherits(output_list, "documenteR_outputs")) return(output_list)

  if (is.character(output_list) && length(output_list) == 1L) {
    found <- get0(output_list, envir = env, inherits = TRUE)
    if (is.null(found)) {
      cli::cli_abort(c(
        "No object named {.val {output_list}} was found.",
        "i" = "Did you run {.fun init_outputs}?"
      ), call = call)
    }
    return(as_outputs(found))
  }

  if (is.list(output_list)) return(as_outputs(output_list))

  if (!is.null(output_list)) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a {.cls documenteR_outputs} object, a list, or the name of one.",
      "x" = "You supplied {.cls {class(output_list)[1]}}."
    ), call = call)
  }

  found <- get0("list_outputs", envir = env, inherits = TRUE)
  if (inherits(found, "documenteR_outputs")) return(found)
  if (is.list(found)) return(as_outputs(found))

  if (inherits(.dr_state$active, "documenteR_outputs")) return(.dr_state$active)

  cli::cli_abort(c(
    "No output collection found.",
    "i" = "Run {.run documenteR::init_outputs()} first, or pass one as {.arg {arg}}."
  ), call = call)
}
