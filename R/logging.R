# Run logging -----------------------------------------------------------------
#
# An export runs a lot of third-party code - graphics devices, Excel writers,
# whatever the user's plotting code pulls in - and all of it is entitled to
# emit messages and warnings. Printed straight to the console they bury the
# progress report under noise that means nothing to the person exporting.
#
# So every condition raised during save_outputs() is captured here with a
# timestamp and the output it came from, written to 00_LOG.txt beside the
# export, and summarised in a few readable lines at the end. Nothing is
# discarded; it just stops competing for the console.

new_dr_log <- function(context = NULL) {
  log <- new.env(parent = emptyenv())
  log$entries <- list()
  log$context <- context
  log$started <- Sys.time()
  class(log) <- c("documenteR_log", "environment")
  log
}

log_add <- function(log, level, text, where = NULL) {
  if (!inherits(log, "documenteR_log")) return(invisible(NULL))
  text <- trimws(paste(as.character(text), collapse = " "))
  if (!nzchar(text)) return(invisible(NULL))
  log$entries[[length(log$entries) + 1L]] <- list(
    time = Sys.time(),
    level = level,
    where = where %||% NA_character_,
    text = text
  )
  invisible(NULL)
}

log_levels <- function(log) {
  if (!length(log$entries)) return(character(0))
  vapply(log$entries, function(e) e$level, character(1))
}

log_count <- function(log, level) sum(log_levels(log) == level)

log_texts <- function(log, level) {
  keep <- vapply(log$entries, function(e) e$level == level, logical(1))
  vapply(log$entries[keep], function(e) {
    if (is.na(e$where)) e$text else paste0("[", e$where, "] ", e$text)
  }, character(1))
}

# Run `expr`, routing conditions into `log`.
#
# cli messages are our own progress output and stay on the console (unless
# quiet); plain messages and warnings from anywhere else are captured and
# muffled, then reported in aggregate by log_report().
with_dr_log <- function(log, expr, where = NULL, quiet = FALSE) {
  withCallingHandlers(
    expr,
    message = function(m) {
      txt <- conditionMessage(m)
      if (inherits(m, "cliMessage")) {
        log_add(log, "progress", txt, where)
        if (isTRUE(quiet)) invokeRestart("muffleMessage")
      } else {
        log_add(log, "message", txt, where)
        invokeRestart("muffleMessage")
      }
    },
    warning = function(w) {
      log_add(log, "warning", conditionMessage(w), where)
      invokeRestart("muffleWarning")
    }
  )
}

# Run one unit of work (writing a single file, building the catalogue, ...) so
# that its failure is recorded and the rest of the export still happens. One
# unwritable plot should not cost you the other forty outputs.
try_item <- function(log, expr, where, quiet = FALSE) {
  stdout_txt <- NULL
  result <- tryCatch(
    withCallingHandlers(
      {
        stdout_txt <- utils::capture.output(value <- force(expr))
        value
      },
      message = function(m) {
        txt <- conditionMessage(m)
        if (inherits(m, "cliMessage")) {
          log_add(log, "progress", txt, where)
          if (isTRUE(quiet)) invokeRestart("muffleMessage")
        } else {
          log_add(log, "message", txt, where)
          invokeRestart("muffleMessage")
        }
      },
      warning = function(w) {
        log_add(log, "warning", conditionMessage(w), where)
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      log_add(log, "error", conditionMessage(e), where)
      structure(list(message = conditionMessage(e)), class = "dr_item_error")
    }
  )
  if (length(stdout_txt)) {
    log_add(log, "stdout", paste(stdout_txt, collapse = "\n"), where)
  }
  result
}

item_failed <- function(x) inherits(x, "dr_item_error")

# Print a short, readable account of what the log collected.
log_report <- function(log, quiet = FALSE) {
  n_warn <- log_count(log, "warning")
  n_err <- log_count(log, "error")
  n_msg <- log_count(log, "message")

  if (isTRUE(quiet)) return(invisible(c(warnings = n_warn, errors = n_err)))

  if (n_err > 0L) {
    txts <- unique(log_texts(log, "error"))
    cli::cli_h3("{n_err} output{?s} could not be written")
    for (t in utils::head(txts, 5L)) cli::cli_bullets(c("x" = "{t}"))
    if (length(txts) > 5L) cli::cli_bullets(c(" " = "... and {length(txts) - 5L} more; see the log."))
  }
  if (n_warn > 0L) {
    txts <- unique(log_texts(log, "warning"))
    cli::cli_h3("{n_warn} warning{?s} ({length(txts)} distinct)")
    for (t in utils::head(txts, 4L)) cli::cli_bullets(c("!" = "{t}"))
    if (length(txts) > 4L) cli::cli_bullets(c(" " = "... and {length(txts) - 4L} more; see the log."))
  }
  if (n_msg > 0L && isTRUE(getOption("documenteR.verbose", FALSE))) {
    cli::cli_alert_info("{n_msg} message{?s} from other packages were captured in the log.")
  }
  invisible(c(warnings = n_warn, errors = n_err))
}

# Serialise the log to a plain-text file next to the export.
log_write <- function(log, path, header = NULL) {
  levels_seen <- log_levels(log)
  counts <- table(factor(levels_seen, levels = c("progress", "message", "warning", "error", "stdout")))

  lines <- c(
    "documenteR run log",
    strrep("=", 70),
    paste0("started : ", dr_timestamp(log$started)),
    paste0("finished: ", dr_timestamp()),
    paste0("duration: ", format(round(difftime(Sys.time(), log$started, units = "secs"), 1))),
    if (!is.null(log$context)) paste0("context : ", log$context),
    "",
    paste0(
      "entries : ",
      paste(sprintf("%s=%d", names(counts), as.integer(counts)), collapse = "  ")
    ),
    ""
  )

  if (!is.null(header)) lines <- c(lines, header, "")

  lines <- c(lines, strrep("-", 70))
  if (!length(log$entries)) {
    lines <- c(lines, "(nothing recorded)")
  } else {
    body <- vapply(log$entries, function(e) {
      sprintf(
        "%s  %-8s  %-28s  %s",
        format(e$time, "%H:%M:%S"),
        e$level,
        if (is.na(e$where)) "-" else substr(e$where, 1L, 28L),
        gsub("\n", "\n" , trimws(e$text))
      )
    }, character(1))
    lines <- c(lines, body)
  }

  write_utf8(lines, path)
}

#' @export
print.documenteR_log <- function(x, ...) {
  counts <- table(log_levels(x))
  cli::cli_h2("documenteR log")
  if (!length(counts)) {
    cli::cli_alert_info("Empty.")
  } else {
    for (nm in names(counts)) cli::cli_text("{.strong {nm}}: {counts[[nm]]}")
  }
  invisible(x)
}

#' Read the log of a saved export
#'
#' @description
#' Returns the contents of a version's `00_LOG.txt` as a character vector.
#' A convenience for looking at what the last export captured without
#' hunting for the file.
#'
#' @param output_dir The project output folder, or a specific version folder.
#' @param version Version to read, e.g. `"0_3"`. Defaults to the most recent.
#'
#' @return The log lines, invisibly; printed to the console.
#' @seealso [save_outputs()], [dr_versions()]
#' @export
dr_log <- function(output_dir, version = NULL) {
  dir <- resolve_version_dir(output_dir, version)
  path <- path_join(dir, "00_LOG.txt")
  if (!file.exists(path)) {
    cli::cli_abort(c(
      "No log found at {.path {path}}.",
      "i" = "Logs are written by {.fun save_outputs} from version 0.9.0 onwards."
    ))
  }
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  cat(lines, sep = "\n")
  invisible(lines)
}
