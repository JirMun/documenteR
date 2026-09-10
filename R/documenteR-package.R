#' documenteR: collect, document and export analysis outputs
#'
#' @description
#' `documenteR` turns the scattered `ggsave()` / `write.csv()` / `saveRDS()`
#' calls of a typical analysis script into a single, documented export step.
#'
#' The workflow has three stages:
#'
#' 1. **Initialise** a collection with [init_outputs()].
#' 2. **Append** outputs as they are produced with [append_plot()],
#'    [append_data()], [append_table()] and [append_stats()], documenting
#'    each one inline (title, description, variables, sources, formula).
#' 3. **Export** everything at once with [save_outputs()], which writes a
#'    versioned folder containing the files themselves plus README files, a
#'    machine-readable manifest, a data codebook, a typeset PDF plot
#'    catalogue and a complete run log.
#'
#' @section Design notes:
#' An output collection is an *environment* with class
#' `documenteR_outputs`. Environments have reference semantics, so
#' `append_*()` can add to the collection in place without reassigning it,
#' which is what makes the "mark things as you go" workflow ergonomic. The
#' collection still behaves like the nested list it replaces: `x$plots`,
#' `x$gpars$formats$plots <- ".svg"` and `length(x$data)` all work as
#' expected.
#'
#' The package deliberately depends only on `cli`, `rlang` and base R.
#' Writers for specific file formats (`.xlsx`, `.parquet`, ...) and
#' renderers for specific plot systems (`ggplot2`, `lattice`, ...) are
#' optional and checked for at the point of use, with an actionable error
#' if a package is missing.
#'
#' @seealso [init_outputs()], [save_outputs()], [dr_gpars()]
#' @keywords internal
#' @importFrom rlang caller_env
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
