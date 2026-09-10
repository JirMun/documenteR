# File naming and folder planning --------------------------------------------
#
# Every output type is numbered the same way, so that a folder listing sorts
# in the order things were appended:
#
#   01_plt_overview.png          <- root-level outputs, numbered first
#   02_plt_detail.png
#   03_maps/03_01_plt_cz.png     <- then one number per subfolder
#   03_maps/03_02_plt_regions.png
#
# The original code derived these numbers with a chain of dplyr mutates on a
# tibble; doing it explicitly here makes the intent visible and removes the
# tidyverse dependency from the hot path.

# Build the export plan for one output type. Returns a data frame with one row
# per output, in the order it will be written.
plan_outputs <- function(entries, gpars, type) {
  n <- length(entries)
  if (n == 0L) {
    return(data.frame(
      name = character(0), subfolder = character(0), dir = character(0),
      stem = character(0), formats = character(0), orig_id = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  nms <- names(entries)
  subfolder <- vapply(entries, function(e) as.character(e$subfolder %||% "")[1], character(1))
  subfolder[is.na(subfolder)] <- ""

  # Per-entry format, falling back to the type default.
  default_fmt <- paste(normalise_ext(split_spec(gpars$formats[[type]])), collapse = ";")
  formats <- vapply(entries, function(e) {
    own <- e$format %||% e$device
    if (is_blank(own)) default_fmt else paste(normalise_ext(split_spec(own)), collapse = ";")
  }, character(1))
  formats[!nzchar(formats)] <- default_fmt

  is_root <- !nzchar(subfolder)
  n_root <- sum(is_root)

  # Subfolders are numbered in order of first appearance, continuing from the
  # last root-level file number.
  folder_order <- unique(subfolder[!is_root])
  folder_id <- stats::setNames(
    pad_id(n_root + seq_along(folder_order)),
    folder_order
  )

  width <- max(2L, nchar(as.character(n_root + length(folder_order))))
  stem <- character(n)
  dir <- character(n)

  root_counter <- 0L
  folder_counter <- stats::setNames(integer(length(folder_order)), folder_order)

  for (i in seq_len(n)) {
    safe_name <- sanitise_filename(nms[i])
    if (is_root[i]) {
      root_counter <- root_counter + 1L
      stem[i] <- paste0(pad_id(root_counter, width), "_", safe_name)
      dir[i] <- ""
    } else {
      sf <- subfolder[i]
      folder_counter[[sf]] <- folder_counter[[sf]] + 1L
      fid <- folder_id[[sf]]
      stem[i] <- paste0(fid, "_", pad_id(folder_counter[[sf]], 2L), "_", safe_name)
      safe_sf <- paste(sanitise_filename(strsplit(sf, "/", fixed = TRUE)[[1]]), collapse = "/")
      dir[i] <- paste0(fid, "_", safe_sf)
    }
  }

  out <- data.frame(
    name = nms,
    subfolder = subfolder,
    dir = dir,
    stem = stem,
    formats = formats,
    orig_id = seq_len(n),
    stringsAsFactors = FALSE
  )
  # Order rows the way the numbering reads, so the writing order, the README
  # order and a folder listing all agree.
  out <- out[order(out$dir, out$stem), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Full relative path (without extension) of a planned output.
plan_stem_path <- function(plan_row, root) {
  path_join(root, plan_row$dir, plan_row$stem)
}

# The label shown for an output in READMEs and the catalogue: numbering,
# name, and the formats it was written in.
plan_label <- function(plan_row, formats = TRUE) {
  lbl <- path_join(plan_row$dir, plan_row$stem)
  if (isTRUE(formats) && nzchar(plan_row$formats)) {
    lbl <- paste0(lbl, " [", plan_row$formats, "]")
  }
  lbl
}


#' Set up the folder structure for an export
#'
#' @description
#' Creates the per-type subfolders inside a version folder and returns their
#' paths. Only folders for output types that actually contain something are
#' created, so an export with no tables has no empty tables folder.
#'
#' @param output_list The collection. See [append_plot()].
#' @param output_dir The version folder to create the structure inside.
#' @param folders Named character vector mapping output type to folder name.
#'   Must contain `output` plus any of `data`, `plots`, `tables`, `stats`.
#'
#' @return A named character vector of created folder paths.
#' @seealso [save_outputs()]
#' @export
#' @examples
#' out <- init_outputs(default = FALSE, quiet = TRUE)
#' d <- cars
#' append_data(d, output_list = out, quiet = TRUE)
#' dirs <- folder_setup(out, file.path(tempdir(), "demo_export"))
#' basename(dirs)
folder_setup <- function(output_list = NULL,
                         output_dir = NULL,
                         folders = dr_folders()) {
  store <- resolve_outputs(output_list, env = parent.frame())
  output_dir <- output_dir %||% getwd()

  if (!"output" %in% names(folders)) {
    cli::cli_abort(c(
      "{.arg folders} must include an {.field output} entry.",
      "i" = "Start from {.fun dr_folders}."
    ))
  }

  present <- dr_output_types()[
    vapply(dr_output_types(), function(ty) length(store[[ty]]) > 0L, logical(1))
  ]
  wanted <- c("output", present)
  missing_names <- setdiff(wanted, names(folders))
  if (length(missing_names)) {
    cli::cli_abort("{.arg folders} has no entry for {.field {missing_names}}.")
  }

  paths <- stats::setNames(
    vapply(wanted, function(ty) path_join(output_dir, folders[[ty]]), character(1)),
    wanted
  )
  for (p in paths) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  paths
}

#' Default folder names for an export
#'
#' @return A named character vector.
#' @export
#' @examples
#' dr_folders()
dr_folders <- function() {
  c(output = "01_output",
    data   = "02_data",
    plots  = "03_plots",
    tables = "04_tables",
    stats  = "05_stats")
}
