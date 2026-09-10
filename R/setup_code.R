#' Create a starter analysis script
#'
#' @description
#' Writes a script pre-wired for the `documenteR` workflow: it initialises a
#' collection, leaves a body section for the analysis, and ends with a
#' [save_outputs()] call guarded by [dr_is_rerun()].
#'
#' @param script_file Name of the script to create, without the `.R`
#'   extension.
#' @param code_folder Folder to create it in, relative to `wd`.
#' @param project_title Project title written into the `save_outputs()` call.
#' @param output_dir Output folder expression written into the script. The
#'   default builds a path from `output_kind` and the script name.
#' @param output_kind Sub-folder of the analysis output tree, e.g.
#'   `"01_data"`.
#' @param packages_file Optional script to `source()` for library calls.
#' @param custom_funs Optional script of project-specific functions to
#'   `source()`.
#' @param template Path to a template file. Defaults to the one shipped with
#'   the package; `documenteR:::.dr_template_path()` shows where that is.
#' @param overwrite Overwrite an existing file. When `FALSE` (default) a
#'   `_new` suffix is used instead.
#' @param open Open the new file in RStudio, if available.
#' @param quiet Suppress the confirmation message.
#' @param wd Project root.
#'
#' @return The path to the script written, invisibly.
#' @seealso [init_outputs()], [save_outputs()]
#' @export
#' @examples
#' d <- file.path(tempdir(), "proj")
#' dir.create(file.path(d, "04_code"), recursive = TRUE, showWarnings = FALSE)
#' f <- setup_code("01_01_data", wd = d, open = FALSE)
#' basename(f)
setup_code <- function(script_file = "01_01_data",
                       code_folder = "04_code",
                       project_title = "Project",
                       output_dir = NULL,
                       output_kind = "01_data",
                       packages_file = NULL,
                       custom_funs = NULL,
                       template = NULL,
                       overwrite = FALSE,
                       open = TRUE,
                       quiet = NULL,
                       wd = getwd()) {
  quiet <- quiet %||% getOption("documenteR.quiet", FALSE)
  template <- template %||% .dr_template_path()
  if (!file.exists(template)) {
    cli::cli_abort("Template not found at {.path {template}}.")
  }

  target_dir <- path_join(wd, code_folder)
  if (!dir.exists(target_dir)) {
    cli::cli_abort(c(
      "{.path {target_dir}} does not exist.",
      "i" = "Create it, or pass a different {.arg code_folder}."
    ))
  }

  script_file <- sub("\\.[Rr]$", "", script_file)
  path <- path_join(target_dir, paste0(script_file, ".R"))
  if (file.exists(path) && !isTRUE(overwrite)) {
    path <- path_join(target_dir, paste0(script_file, "_new.R"))
    cli::cli_warn(c(
      "!" = "{.path {script_file}.R} already exists.",
      "i" = "Writing {.path {basename(path)}} instead; pass {.code overwrite = TRUE} to replace it."
    ))
  }

  output_dir <- output_dir %||% sprintf(
    'file.path("05_analyzy", "%s", this_file)', output_kind
  )

  sources <- c(
    if (!is_blank(packages_file)) sprintf('source("%s", encoding = "UTF-8")', packages_file),
    if (!is_blank(custom_funs)) sprintf('source("%s", encoding = "UTF-8")', custom_funs)
  )

  text <- readLines(template, warn = FALSE, encoding = "UTF-8")
  text <- gsub("{{SCRIPT_FILE}}", script_file, text, fixed = TRUE)
  text <- gsub("{{PROJECT_TITLE}}", project_title, text, fixed = TRUE)
  text <- gsub("{{OUTPUT_DIR}}", output_dir, text, fixed = TRUE)
  text <- gsub(
    "{{SOURCES}}",
    if (length(sources)) paste(sources, collapse = "\n") else "# (no extra scripts to source)",
    text,
    fixed = TRUE
  )

  write_utf8(text, path)
  if (!quiet) cli::cli_alert_success("Created {.path {path}}")

  if (isTRUE(open) && requireNamespace("rstudioapi", quietly = TRUE) &&
      isTRUE(try(rstudioapi::isAvailable(), silent = TRUE))) {
    try(rstudioapi::navigateToFile(path), silent = TRUE)
  }
  invisible(path)
}

.dr_template_path <- function() {
  system.file("templates", "analysis_script.R", package = "documenteR")
}
