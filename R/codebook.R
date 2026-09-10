#' Describe the variables of a dataset
#'
#' @description
#' Builds a variable-level codebook: one row per column, with its class, how
#' much of it is missing, how many distinct values it takes and a compact
#' summary of its range or most frequent categories.
#'
#' @details
#' [save_outputs()] writes a codebook covering every appended dataset to
#' `00_codebook.csv` in the data folder, which is usually the first thing
#' someone receiving an export wants to read. Set `gpars$codebook = FALSE`, or
#' `append_data(codebook = FALSE)` for one dataset, to skip it.
#'
#' Variable labels are taken from `labels`, falling back to a column's
#' `label` attribute — which is what `haven` sets when reading SPSS or Stata
#' files, so labelled survey data documents itself.
#'
#' @param data A data frame.
#' @param labels Optional named character vector of variable labels.
#' @param max_levels How many category values to list in the summary before
#'   truncating.
#'
#' @return A data frame with columns `variable`, `label`, `class`, `n`,
#'   `n_missing`, `pct_missing`, `n_unique` and `summary`.
#' @seealso [append_data()], [save_outputs()]
#' @export
#' @examples
#' dr_codebook(head(iris), labels = c(Species = "Iris species"))
dr_codebook <- function(data, labels = NULL, max_levels = 5L) {
  if (!is.data.frame(data)) {
    data <- coerce_rectangular(data, "data", "dr_codebook")
  }
  labels <- as_named_chr(labels)

  if (ncol(data) == 0L) {
    return(data.frame(
      variable = character(0), label = character(0), class = character(0),
      n = integer(0), n_missing = integer(0), pct_missing = numeric(0),
      n_unique = integer(0), summary = character(0), stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(names(data), function(nm) {
    col <- data[[nm]]
    attr_label <- attr(col, "label", exact = TRUE)
    label <- if (nm %in% names(labels)) {
      labels[[nm]]
    } else if (!is.null(attr_label)) {
      as.character(attr_label)[1]
    } else {
      NA_character_
    }
    n_missing <- sum(is.na(col))
    data.frame(
      variable = nm,
      label = label,
      class = paste(class(col), collapse = "/"),
      n = length(col),
      n_missing = as.integer(n_missing),
      pct_missing = if (length(col)) round(100 * n_missing / length(col), 2) else NA_real_,
      n_unique = length(unique(col[!is.na(col)])),
      summary = summarise_column(col, max_levels = max_levels),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# A one-line description of a column, chosen by type. Kept deliberately short:
# the codebook is meant to be skimmed.
summarise_column <- function(col, max_levels = 5L) {
  if (is.list(col) && !is.data.frame(col)) {
    return(sprintf("list column, lengths %s",
                   paste(range(lengths(col)), collapse = "-")))
  }
  ok <- col[!is.na(col)]
  if (!length(ok)) return("all missing")

  if (inherits(col, c("Date", "POSIXct", "POSIXt"))) {
    return(sprintf("%s to %s", format(min(ok)), format(max(ok))))
  }
  if (is.logical(col)) {
    return(sprintf("TRUE %d, FALSE %d", sum(ok), sum(!ok)))
  }
  if (is.numeric(col)) {
    q <- stats::quantile(as.numeric(ok), c(0, 0.5, 1), names = FALSE, na.rm = TRUE)
    return(sprintf(
      "min %s, median %s, max %s, mean %s",
      fmt_num(q[1]), fmt_num(q[2]), fmt_num(q[3]), fmt_num(mean(as.numeric(ok)))
    ))
  }
  # Factors and character: list the most common values.
  tab <- sort(table(as.character(ok)), decreasing = TRUE)
  shown <- utils::head(tab, max_levels)
  txt <- paste(sprintf("%s (%d)", names(shown), as.integer(shown)), collapse = ", ")
  if (length(tab) > length(shown)) {
    txt <- paste0(txt, sprintf(", ... +%d more", length(tab) - length(shown)))
  }
  txt
}

fmt_num <- function(x) {
  if (!is.finite(x)) return(as.character(x))
  if (x == round(x) && abs(x) < 1e15) {
    format(x, big.mark = " ", scientific = FALSE, trim = TRUE)
  } else {
    format(signif(x, 4), big.mark = " ", scientific = FALSE, trim = TRUE)
  }
}

# Codebook covering every appended dataset, with a `dataset` column so the
# whole export is described by one file.
build_combined_codebook <- function(entries, plan) {
  if (!length(entries)) return(NULL)
  parts <- lapply(seq_len(nrow(plan)), function(i) {
    nm <- plan$name[i]
    entry <- entries[[nm]]
    if (identical(entry$codebook, FALSE)) return(NULL)
    cb <- dr_codebook(entry$data, labels = entry$labels)
    if (!nrow(cb)) return(NULL)
    cbind(
      data.frame(
        dataset = nm,
        file = plan$stem[i],
        stringsAsFactors = FALSE
      ),
      cb
    )
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) return(NULL)
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
}
