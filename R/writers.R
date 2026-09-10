#' File formats documenteR can write
#'
#' @description
#' Lists the file extensions accepted by `format =` in [append_data()] /
#' [append_table()] and by `device =` in [append_plot()].
#'
#' @details
#' Some formats need an optional package, which is only required when you
#' actually ask for that format:
#'
#' | Format | Package |
#' |--------|---------|
#' | `.xlsx` | `openxlsx`, or `writexl` |
#' | `.parquet` | `arrow` |
#' | `.json` | `jsonlite` |
#' | `.svg` | `svglite` (falls back to `grDevices::svg()`) |
#' | `.png`, `.jpg`, `.tiff` | `ragg` if installed, otherwise base graphics |
#'
#' The PDF catalogue additionally uses `png` to place base-graphics plots on
#' a page alongside their documentation.
#'
#' `.csv` uses `gpars$csv_delim` as its separator; `.csv2` always uses a
#' semicolon with a comma decimal mark, which is what Czech and German
#' installations of Excel expect.
#'
#' @return A named list with character vectors for each output type.
#' @export
#' @examples
#' dr_formats()$data
#' dr_formats()$plots
dr_formats <- function() {
  rect <- c(".csv", ".csv2", ".tsv", ".xlsx", ".rds", ".parquet", ".json")
  list(
    data   = rect,
    tables = rect,
    stats  = rect,
    plots  = c(".png", ".jpg", ".jpeg", ".tiff", ".svg", ".pdf", ".eps")
  )
}

# Coerce an appended object to a data frame, with an error that names the
# offending object rather than failing deep inside as.data.frame().
coerce_rectangular <- function(x, name, fun, call = rlang::caller_env()) {
  if (is.data.frame(x)) return(x)
  if (is.null(x)) {
    cli::cli_abort("{.val {name}} is {.code NULL}; there is nothing to save.", call = call)
  }
  converted <- try(as.data.frame(x, stringsAsFactors = FALSE), silent = TRUE)
  if (inherits(converted, "try-error") || !is.data.frame(converted)) {
    cli::cli_abort(c(
      "{.fun {fun}} needs a data frame; {.val {name}} is {.cls {class(x)[1]}}.",
      "i" = "Convert it first, e.g. with {.fun as.data.frame}.",
      "i" = "For a plot use {.fun append_plot}; for a single number use {.fun append_stats}."
    ), call = call)
  }
  if (!is.matrix(x) && !inherits(x, "table")) {
    cli::cli_inform(c("i" = "Coerced {.val {name}} from {.cls {class(x)[1]}} to a data frame."))
  }
  converted
}

# Ensure a data frame can survive a flat text format: list columns and other
# exotica get flattened to strings, with a note in the log.
flatten_for_text <- function(df, name, format) {
  is_list_col <- vapply(df, function(col) is.list(col) && !is.data.frame(col), logical(1))
  if (any(is_list_col)) {
    cols <- names(df)[is_list_col]
    cli::cli_warn(c(
      "!" = "{.val {name}}: list column{?s} {.field {cols}} cannot be written to {.val {format}}.",
      "i" = "Collapsed to {.val ;}-separated text. Use {.val .rds} to keep the structure."
    ))
    for (cn in cols) {
      df[[cn]] <- vapply(
        df[[cn]],
        function(v) paste(as.character(unlist(v)), collapse = ";"),
        character(1)
      )
    }
  }
  # Nested data frames are equally unwritable.
  is_df_col <- vapply(df, is.data.frame, logical(1))
  if (any(is_df_col)) {
    df <- do.call(data.frame, c(as.list(df), stringsAsFactors = FALSE, check.names = FALSE))
  }
  df
}

# Write one data frame in one format. Returns the path written.
#
# The `row.names = FALSE` in the CSV branch is deliberate and important:
# utils::write.csv() writes row names by default as an unnamed first column,
# which readr then reads back as a phantom `...1` variable. That is where the
# stray index column in earlier exports came from.
write_data_file <- function(df, path, format, gpars = list(), name = basename(path)) {
  format <- normalise_ext(format)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  switch(format,
    ".csv" = write_delim_file(
      flatten_for_text(df, name, format), path,
      sep = gpars$csv_delim %||% ",", dec = ".", bom = isTRUE(gpars$csv_bom)
    ),
    ".csv2" = write_delim_file(
      flatten_for_text(df, name, format), path,
      sep = ";", dec = ",", bom = isTRUE(gpars$csv_bom)
    ),
    ".tsv" = write_delim_file(
      flatten_for_text(df, name, format), path,
      sep = "\t", dec = ".", bom = isTRUE(gpars$csv_bom)
    ),
    ".rds" = saveRDS(df, file = path),
    ".xlsx" = write_xlsx_file(flatten_for_text(df, name, format), path),
    ".parquet" = {
      require_pkg("arrow", "write .parquet files")
      arrow::write_parquet(df, path)
    },
    ".json" = {
      require_pkg("jsonlite", "write .json files")
      write_utf8(
        jsonlite::toJSON(df, dataframe = "rows", auto_unbox = TRUE,
                         na = "null", pretty = TRUE),
        path
      )
    },
    cli::cli_abort(c(
      "Cannot write {.val {format}}.",
      "i" = "Supported: {.val {dr_formats()$data}}."
    ))
  )
  invisible(path)
}

# utils::write.table with the settings we always want: UTF-8, no row names,
# and quoting only where needed. Strings are marked UTF-8 and the connection
# is opened with an explicit encoding, which is the only combination that
# behaves identically on Windows and on Linux.
write_delim_file <- function(df, path, sep = ",", dec = ".", bom = FALSE) {
  names(df) <- enc2utf8(as.character(names(df)))
  for (i in seq_along(df)) {
    if (is.character(df[[i]])) {
      df[[i]] <- enc2utf8(df[[i]])
    } else if (is.factor(df[[i]])) {
      levels(df[[i]]) <- enc2utf8(levels(df[[i]]))
    }
  }
  if (isTRUE(bom)) {
    bcon <- file(path, open = "wb")
    writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), bcon)
    close(bcon)
  }
  con <- file(path, open = if (isTRUE(bom)) "ab" else "wb", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  utils::write.table(
    df, file = con, sep = sep, dec = dec,
    row.names = FALSE, col.names = TRUE, qmethod = "double", na = ""
  )
  invisible(path)
}

write_xlsx_file <- function(df, path) {
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    openxlsx::write.xlsx(df, file = path, rowNames = FALSE, overwrite = TRUE)
  } else if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(df, path = path)
  } else {
    cli::cli_abort(c(
      "Writing {.val .xlsx} needs the {.pkg openxlsx} or {.pkg writexl} package.",
      "i" = 'Install one with {.run install.packages("openxlsx")}.'
    ))
  }
  invisible(path)
}

# A single place for "you need package X to do Y" errors.
require_pkg <- function(pkg, purpose, call = rlang::caller_env()) {
  if (requireNamespace(pkg, quietly = TRUE)) return(invisible(TRUE))
  cli::cli_abort(c(
    "The {.pkg {pkg}} package is required to {purpose}.",
    "i" = 'Install it with {.run install.packages("{pkg}")}.'
  ), call = call)
}
