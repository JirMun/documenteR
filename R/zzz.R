# Package-level mutable state -------------------------------------------------

# `active` holds the collection most recently created by init_outputs(), so
# that append_*() and save_outputs() can be called without naming it.
.dr_state <- new.env(parent = emptyenv())
.dr_state$active <- NULL

.dr_default_options <- list(
  documenteR.quiet = FALSE,
  documenteR.verbose = FALSE,
  documenteR.version_names = TRUE,
  documenteR.max_path = 250L
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  unset <- !(names(.dr_default_options) %in% names(op))
  if (any(unset)) options(.dr_default_options[unset])
  invisible()
}

#' Package options
#'
#' `documenteR` reads a small number of global options. Set them with
#' [options()], usually once at the top of a project's setup script.
#'
#' \describe{
#'   \item{`documenteR.quiet`}{Logical. Suppress all progress messages from
#'     [save_outputs()] and `append_*()`. The run log is still written.
#'     Default `FALSE`.}
#'   \item{`documenteR.verbose`}{Logical. Print one line per file written
#'     instead of a progress bar per output type. Useful when debugging a
#'     failing export. Default `FALSE`.}
#'   \item{`documenteR.version_names`}{Logical. Generate a whimsical verbal
#'     name for each version (see [dr_version_name()]). Default `TRUE`.}
#'   \item{`documenteR.max_path` }{Integer. Warn when a file path exceeds
#'     this many characters. Windows has a 260-character limit that shared
#'     network and cloud drives hit easily. Default `250`.}
#' }
#'
#' @name documenteR-options
NULL
