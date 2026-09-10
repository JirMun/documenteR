demo_collection <- function() {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  d <- cars
  append_data(d, object_name = "data_cars", output_list = out,
              desc = "Built-in cars data", quiet = TRUE)
  append_plot(function() plot(cars), object_name = "plt_cars",
              output_list = out, desc = "Speed vs distance",
              type = "s_wide", quiet = TRUE)
  append_table(head(cars), object_name = "tab_head", output_list = out,
               quiet = TRUE)
  append_stats(50L, object_name = "stat_n", output_list = out,
               type = "integer", quiet = TRUE)
  out
}

test_that("save_outputs writes a complete version folder", {
  root <- withr::local_tempdir()
  res <- save_outputs(root, demo_collection(), project_title = "T", quiet = TRUE)

  expect_s3_class(res, "documenteR_export")
  expect_equal(res$version, "0_1")
  expect_equal(res$n_failed, 0L)

  files <- list.files(res$path, recursive = TRUE)
  expect_true(all(c(
    "00_DOC.txt", "00_LOG.txt", "00_MANIFEST.csv",
    "01_output/00_README.txt", "01_output/01_list_outputs.rds",
    "02_data/00_README.txt", "02_data/01_data_cars.csv", "02_data/00_codebook.csv",
    "03_plots/00_README.txt", "03_plots/01_plt_cars.png",
    "04_tables/01_tab_head.csv",
    "05_stats/01_stats.csv"
  ) %in% files))
})

test_that("the catalogue is built and all its pages are the same size", {
  skip_if_not_installed("pdftools")
  root <- withr::local_tempdir()
  out <- init_outputs(default = FALSE, quiet = TRUE)
  # Deliberately mismatched shapes: under the old implementation these
  # produced a PDF with three different page sizes.
  append_plot(function() plot(1:10), object_name = "p_wide",
              output_list = out, type = "s_wide", quiet = TRUE)
  append_plot(function() plot(1:10), object_name = "p_tall",
              output_list = out, type = "s_tall", quiet = TRUE)
  append_plot(function() plot(1:10), object_name = "p_sq",
              output_list = out, type = "s", quiet = TRUE)

  res <- save_outputs(root, out, quiet = TRUE)
  pdf <- file.path(res$path, "03_plots", "00_01_catalogue.pdf")
  expect_true(file.exists(pdf))

  sizes <- pdftools::pdf_pagesize(pdf)
  expect_equal(length(unique(sizes$width)), 1L)
  expect_equal(length(unique(sizes$height)), 1L)
  # One page per plot, plus the contents page.
  expect_equal(nrow(sizes), 4L)
})

test_that("catalogue = FALSE skips the PDF", {
  root <- withr::local_tempdir()
  res <- save_outputs(root, demo_collection(), catalogue = FALSE, quiet = TRUE)
  expect_false(file.exists(file.path(res$path, "03_plots", "00_01_catalogue.pdf")))
})

test_that("versions increment, and new_version bumps the major number", {
  root <- withr::local_tempdir()
  out <- demo_collection()
  expect_equal(save_outputs(root, out, quiet = TRUE)$version, "0_1")
  expect_equal(save_outputs(root, out, quiet = TRUE)$version, "0_2")
  expect_equal(save_outputs(root, out, new_version = TRUE, quiet = TRUE)$version, "1_0")
  expect_equal(save_outputs(root, out, quiet = TRUE)$version, "1_1")
})

test_that("update_latest_version writes into the existing folder", {
  root <- withr::local_tempdir()
  out <- demo_collection()
  first <- save_outputs(root, out, quiet = TRUE)
  again <- save_outputs(root, out, update_latest_version = TRUE, quiet = TRUE)
  expect_equal(again$version, first$version)
  expect_equal(nrow(dr_versions(root)), 1L)
})

test_that("update_latest_version errors when there is nothing to update", {
  root <- withr::local_tempdir()
  expect_error(
    save_outputs(root, demo_collection(), update_latest_version = TRUE, quiet = TRUE),
    "no versions yet"
  )
})

test_that("dr_versions reads back the version metadata", {
  root <- withr::local_tempdir()
  out <- demo_collection()
  save_outputs(root, out, project_title = "Proj", version_notes = "first", quiet = TRUE)
  save_outputs(root, out, version_notes = "second", quiet = TRUE)

  vers <- dr_versions(root)
  expect_equal(nrow(vers), 2L)
  expect_equal(vers$version, c("0_1", "0_2"))
  expect_equal(vers$notes, c("first", "second"))
  # The project title carries forward when not repeated.
  expect_equal(vers$project_title, c("Proj", "Proj"))
  expect_true(all(nzchar(vers$version_name)))
})

test_that("the manifest describes every file and verifies clean", {
  root <- withr::local_tempdir()
  res <- save_outputs(root, demo_collection(), quiet = TRUE)

  man <- dr_manifest(root)
  expect_true(all(man$status == "ok"))
  expect_true(all(file.exists(file.path(res$path, man$file))))
  expect_true(all(nzchar(man$md5)))
  # Paths are relative, so the export can be moved.
  expect_false(any(grepl("^([A-Za-z]:|/)", man$file)))

  problems <- suppressMessages(dr_verify(root))
  expect_equal(nrow(problems), 0L)
})

test_that("dr_verify notices a changed file", {
  root <- withr::local_tempdir()
  res <- save_outputs(root, demo_collection(), quiet = TRUE)
  cat("tampered\n", file = file.path(res$path, "05_stats", "01_stats.csv"), append = TRUE)
  problems <- suppressMessages(dr_verify(root))
  expect_true("changed" %in% problems$problem)
})

test_that("excluding a type documents the omission instead of hiding it", {
  root <- withr::local_tempdir()
  res <- save_outputs(root, demo_collection(), exclude = "data", quiet = TRUE)
  readme <- readLines(file.path(res$path, "02_data", "00_README.txt"), warn = FALSE)
  expect_true(any(grepl("EXCLUDED", readme)))
  expect_false(file.exists(file.path(res$path, "02_data", "01_data_cars.csv")))
})

test_that("an unknown exclude value is refused", {
  root <- withr::local_tempdir()
  expect_error(
    save_outputs(root, demo_collection(), exclude = "banana", quiet = TRUE),
    "Cannot exclude"
  )
})

test_that("dry_run writes nothing", {
  root <- withr::local_tempdir()
  res <- suppressMessages(save_outputs(root, demo_collection(), dry_run = TRUE))
  expect_s3_class(res, "documenteR_dry_run")
  expect_equal(length(list.files(root)), 0L)
  expect_true("plots" %in% names(res$planned))
})

test_that("code files are copied and missing ones warned about", {
  root <- withr::local_tempdir()
  script <- file.path(root, "script.R")
  writeLines("x <- 1", script)
  res <- suppressWarnings(save_outputs(
    root, demo_collection(),
    code_location = c(script, file.path(root, "nope.R")),
    quiet = TRUE
  ))
  copied <- list.files(file.path(res$path, "01_output"), pattern = "^02_")
  expect_length(copied, 1L)
  expect_true(any(grepl("x <- 1", readLines(file.path(res$path, "01_output", copied)))))
})

test_that("one failing output does not abort the export", {
  root <- withr::local_tempdir()
  out <- init_outputs(default = FALSE, quiet = TRUE)
  append_plot(function() stop("this plot is broken"), object_name = "plt_bad",
              output_list = out, quiet = TRUE)
  append_plot(function() plot(1:10), object_name = "plt_good",
              output_list = out, quiet = TRUE)
  d <- cars
  append_data(d, output_list = out, quiet = TRUE)

  res <- save_outputs(root, out, catalogue = FALSE, quiet = TRUE)
  expect_gte(res$n_failed, 1L)
  expect_true(file.exists(file.path(res$path, "03_plots", "02_plt_good.png")))
  expect_true(file.exists(file.path(res$path, "02_data", "01_d.csv")))

  man <- dr_manifest(root)
  expect_equal(man$status[man$name == "plt_bad"], "failed")
  expect_true(grepl("broken", man$message[man$name == "plt_bad"]))

  # And the reason is in the log.
  log <- readLines(file.path(res$path, "00_LOG.txt"), warn = FALSE)
  expect_true(any(grepl("this plot is broken", log)))
})

test_that("noisy third-party messages go to the log, not the console", {
  root <- withr::local_tempdir()
  out <- init_outputs(default = FALSE, quiet = TRUE)
  append_plot(
    function() {
      message("a very chatty package writes this")
      warning("and warns about that")
      plot(1:10)
    },
    object_name = "plt_noisy", output_list = out, quiet = TRUE
  )

  res <- save_outputs(root, out, catalogue = FALSE, quiet = TRUE)
  log <- readLines(file.path(res$path, "00_LOG.txt"), warn = FALSE)
  expect_true(any(grepl("chatty package", log)))
  expect_true(any(grepl("warns about that", log)))
  # The file was still written.
  expect_equal(res$n_failed, 0L)
})

test_that("an empty collection warns but still records a version", {
  root <- withr::local_tempdir()
  out <- init_outputs(default = FALSE, quiet = TRUE)
  expect_warning(save_outputs(root, out, quiet = TRUE), "collection is empty")
  vers <- dr_versions(root)
  expect_equal(nrow(vers), 1L)
  expect_true(file.exists(file.path(vers$path, "00_DOC.txt")))
})

test_that("save_outputs insists on an output_dir", {
  expect_error(save_outputs(output_list = demo_collection()), "required")
  expect_error(save_outputs("", demo_collection(), quiet = TRUE), "required")
})

test_that("the saved collection object can be read back", {
  root <- withr::local_tempdir()
  res <- save_outputs(root, demo_collection(), quiet = TRUE)
  back <- readRDS(file.path(res$path, "01_output", "01_list_outputs.rds"))
  expect_type(back, "list")
  restored <- as_outputs(back)
  expect_s3_class(restored, "documenteR_outputs")
  expect_named(restored$data, "data_cars")
})

test_that("dr_log reads the log of the latest version", {
  root <- withr::local_tempdir()
  save_outputs(root, demo_collection(), quiet = TRUE)
  lines <- utils::capture.output(dr_log(root))
  expect_true(any(grepl("documenteR run log", lines)))
})

test_that("dr_is_rerun reflects the environment variable", {
  withr::with_envvar(c(DOCUMENTER_RERUN = "1"), expect_true(dr_is_rerun()))
  withr::with_envvar(c(DOCUMENTER_RERUN = NA), expect_false(dr_is_rerun()))
})
