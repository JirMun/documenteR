# cli writes some output to stdout and some as messages to stderr, so capture
# both and strip ANSI before matching. Deliberately not snapshot tests: the
# exact layout depends on console width, which varies between machines and CI.
grab <- function(expr) {
  msgs <- character(0)
  out <- utils::capture.output(
    withCallingHandlers(
      expr,
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
  )
  cli::ansi_strip(paste(c(out, msgs), collapse = "\n"))
}

demo <- function() {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  d <- cars
  append_data(d, object_name = "data_cars", output_list = out,
              desc = "Speed and stopping distance",
              source = c(datasets = "R built-in"),
              format = c(".csv", ".rds"), quiet = TRUE)
  append_plot(function() plot(cars), object_name = "plt_cars",
              output_list = out, title = "Braking distance",
              vars = c(x = "Speed"), type = "m_wide", quiet = TRUE)
  append_plot(function() plot(1:3), object_name = "plt_map",
              output_list = out, subfolder = "maps", quiet = TRUE)
  append_table(head(cars), object_name = "tab_head", output_list = out,
               quiet = TRUE)
  append_stats(50L, object_name = "stat_n", output_list = out,
               type = "integer", desc = "Sample size", quiet = TRUE)
  out
}

setup_quiet_cli <- function() withr::local_options(cli.num_colors = 1, .local_envir = parent.frame())


test_that("print() lists the outputs with their export ids and titles", {
  setup_quiet_cli()
  txt <- grab(print(demo()))

  expect_true(grepl("documenteR output collection", txt, fixed = TRUE))
  # One section per non-empty type, with counts.
  for (bit in c("data (1)", "plots (2)", "tables (1)", "stats (1)")) {
    expect_true(grepl(bit, txt, fixed = TRUE), info = bit)
  }
  # Names, export ids and a real title.
  for (bit in c("data_cars", "plt_cars", "maps/plt_map", "Braking distance")) {
    expect_true(grepl(bit, txt, fixed = TRUE), info = bit)
  }
  # Geometry and formats.
  expect_true(grepl("16 x 12.35 cm", txt, fixed = TRUE))
  expect_true(grepl(".csv .rds", txt, fixed = TRUE))
})

test_that("print() reports the documentation coverage", {
  setup_quiet_cli()
  # plt_map and tab_head carry nothing beyond a title.
  txt <- grab(print(demo()))
  expect_true(grepl("2 of 5 outputs have no description", txt, fixed = TRUE))

  out <- init_outputs(default = FALSE, quiet = TRUE)
  d <- cars
  append_data(d, output_list = out, desc = "Documented", quiet = TRUE)
  expect_true(grepl("All 1 output carry", grab(print(out)), fixed = TRUE) ||
                grepl("carry some documentation", grab(print(out)), fixed = TRUE))
})

test_that("print() on an empty collection says so and suggests what to do", {
  setup_quiet_cli()
  txt <- grab(print(init_outputs(default = FALSE, quiet = TRUE)))
  expect_true(grepl("Empty", txt, fixed = TRUE))
  expect_true(grepl("append_plot", txt, fixed = TRUE))
})

test_that("print() truncates long collections and detail = TRUE does not", {
  setup_quiet_cli()
  out <- init_outputs(default = FALSE, quiet = TRUE)
  for (i in 1:12) {
    append_stats(i, object_name = sprintf("stat_%02d", i), output_list = out,
                 type = "integer", quiet = TRUE)
  }
  short <- grab(print(out, n = 5L))
  expect_true(grepl("... and 7 more", short, fixed = TRUE))
  expect_false(grepl("stat_12", short, fixed = TRUE))

  full <- grab(print(out, detail = TRUE))
  expect_true(grepl("stat_12", full, fixed = TRUE))
  expect_false(grepl("and 7 more", full, fixed = TRUE))
})

test_that("outputs hidden by truncation still count towards coverage", {
  setup_quiet_cli()
  out <- init_outputs(default = FALSE, quiet = TRUE)
  for (i in 1:6) {
    append_stats(i, object_name = paste0("s", i), output_list = out,
                 type = "integer", quiet = TRUE)
  }
  txt <- grab(print(out, n = 2L))
  expect_true(grepl("6 of 6 outputs have no description", txt, fixed = TRUE))
})

test_that("a title that merely repeats the object name is not shown twice", {
  setup_quiet_cli()
  out <- init_outputs(default = FALSE, quiet = TRUE)
  data_cars <- cars
  append_data(data_cars, output_list = out, quiet = TRUE)
  txt <- grab(print(out))
  # The name appears once, not as both name and title.
  expect_equal(
    length(gregexpr("data_cars", txt, fixed = TRUE)[[1]]),
    1L
  )
})

test_that("printed rows carry no trailing whitespace", {
  setup_quiet_cli()
  lines <- strsplit(grab(print(demo())), "\n", fixed = TRUE)[[1]]
  expect_false(any(grepl("[ ]+$", lines)))
})

test_that("format() gives a one-line summary", {
  expect_match(format(demo()), "^<documenteR_outputs:")
  expect_match(format(demo()), "plots=2")
})


test_that("summary() returns one row per output with documentation flags", {
  s <- summary(demo())
  expect_s3_class(s, "documenteR_summary")
  expect_s3_class(s, "data.frame")
  expect_equal(nrow(s), 5L)
  expect_true(all(c("type", "id", "name", "title", "file", "formats", "size",
                    "desc", "source", "subfolder", "has_desc", "has_vars",
                    "has_source", "doc_fields") %in% names(s)))

  expect_true(s$has_desc[s$name == "data_cars"])
  expect_true(s$has_source[s$name == "data_cars"])
  expect_false(s$has_desc[s$name == "plt_map"])
  expect_true(s$has_vars[s$name == "plt_cars"])
  expect_equal(s$doc_fields[s$name == "plt_map"], 0L)
})

test_that("summary() file paths match what save_outputs writes", {
  root <- withr::local_tempdir()
  out <- demo()
  s <- summary(out)
  res <- save_outputs(root, out, catalogue = FALSE, quiet = TRUE)

  written <- sub("\\.[^.]+$", "", dr_manifest(root)$file)
  for (i in which(s$type != "stats")) {
    # summary() gives the path within the type folder; the manifest prefixes
    # the type folder itself.
    expect_true(
      any(endsWith(written, s$file[i])),
      info = s$file[i]
    )
  }
  expect_true(all(res$manifest$status == "ok"))
})

test_that("summary() supports the 'what still needs documenting' workflow", {
  s <- summary(demo())
  # plt_cars carries `vars` but no `desc`, so it needs one too.
  expect_setequal(s$name[!s$has_desc], c("plt_cars", "plt_map", "tab_head"))
  expect_setequal(s$name[s$doc_fields == 0L], c("plt_map", "tab_head"))
  # Subsetting keeps working like a data frame.
  sub <- s[!s$has_desc, c("type", "name")]
  expect_equal(nrow(sub), 3L)
})

test_that("summary() prints an aligned table", {
  setup_quiet_cli()
  txt <- grab(print(summary(demo())))
  expect_true(grepl("01_data_cars", txt, fixed = TRUE))
  expect_true(grepl("02_maps/02_01_plt_map", txt, fixed = TRUE))
  expect_true(grepl("doc columns", txt, fixed = TRUE))
})

test_that("summary() of an empty collection has zero rows and prints cleanly", {
  setup_quiet_cli()
  s <- summary(init_outputs(default = FALSE, quiet = TRUE))
  expect_equal(nrow(s), 0L)
  expect_true(grepl("No outputs appended", grab(print(s)), fixed = TRUE))
})


test_that("appended entries are tagged so a single one prints its docs", {
  out <- demo()
  entry <- out$plots$plt_cars
  expect_s3_class(entry, "dr_entry")
  expect_equal(attr(entry, "dr_type"), "plots")
  expect_equal(attr(entry, "dr_name"), "plt_cars")
})

test_that("print() of one entry shows the documentation, not the object", {
  setup_quiet_cli()
  txt <- grab(print(demo()$plots$plt_cars))
  expect_true(grepl("plot plt_cars", txt, fixed = TRUE))
  expect_true(grepl("Braking distance", txt, fixed = TRUE))
  expect_true(grepl("1. x: Speed", txt, fixed = TRUE))
  expect_true(grepl("m_wide", txt, fixed = TRUE))
  # Not a dump of the plotting function.
  expect_false(grepl("function()", txt, fixed = TRUE))
})

test_that("print() of a statistic entry shows its value and template", {
  setup_quiet_cli()
  out <- init_outputs(default = FALSE, quiet = TRUE)
  append_stats(0.4567, object_name = "stat_share", output_list = out,
               type = "percent", stat_round = 1,
               formula = "fast/total",
               vars = c(fast = "Fast", total = "All"),
               text = "* per cent.", section = "2.1", quiet = TRUE)
  txt <- grab(print(out$stats$stat_share))
  expect_true(grepl("statistic stat_share", txt, fixed = TRUE))
  expect_true(grepl("0.4567", txt, fixed = TRUE))
  expect_true(grepl("percent", txt, fixed = TRUE))
  expect_true(grepl("1. fast: Fast", txt, fixed = TRUE))
  expect_true(grepl("section 2.1", txt, fixed = TRUE))
})

test_that("print() of a data entry shows its dimensions", {
  setup_quiet_cli()
  txt <- grab(print(demo()$data$data_cars))
  expect_true(grepl("dataset data_cars", txt, fixed = TRUE))
  expect_true(grepl("50 rows x 2 columns", txt, fixed = TRUE))
  expect_true(grepl(".csv .rds", txt, fixed = TRUE))
})

test_that("format() of an entry is a one-liner", {
  expect_equal(format(demo()$data$data_cars), "<data: data_cars>")
})

test_that("an undocumented entry prints without error", {
  setup_quiet_cli()
  out <- init_outputs(default = FALSE, quiet = TRUE)
  append_plot(function() plot(1), object_name = "p", output_list = out,
              quiet = TRUE)
  expect_true(nzchar(grab(print(out$plots$p))))
})

test_that("entry tagging does not disturb the export", {
  root <- withr::local_tempdir()
  res <- save_outputs(root, demo(), quiet = TRUE)
  expect_equal(res$n_failed, 0L)
  # And a collection round-tripped through .rds still works.
  back <- as_outputs(readRDS(file.path(res$path, "01_output", "01_list_outputs.rds")))
  expect_named(back$plots, c("plt_cars", "plt_map"))
})


test_that("print() of an export summarises the files by type", {
  setup_quiet_cli()
  root <- withr::local_tempdir()
  res <- save_outputs(root, demo(), catalogue = FALSE, quiet = TRUE)
  txt <- grab(print(res))
  expect_true(grepl("documenteR export 0_1", txt, fixed = TRUE))
  expect_true(grepl("files written", txt, fixed = TRUE) ||
                grepl("file written", txt, fixed = TRUE))
  expect_true(grepl("plots", txt, fixed = TRUE))
})

test_that("print() of an export names the failures", {
  setup_quiet_cli()
  root <- withr::local_tempdir()
  out <- init_outputs(default = FALSE, quiet = TRUE)
  append_plot(function() stop("broken"), object_name = "plt_bad",
              output_list = out, quiet = TRUE)
  res <- save_outputs(root, out, catalogue = FALSE, quiet = TRUE)
  txt <- grab(print(res))
  expect_true(grepl("failed", txt, fixed = TRUE))
  expect_true(grepl("plt_bad", txt, fixed = TRUE))
})

test_that("print() of a dry run lists the planned files", {
  setup_quiet_cli()
  root <- withr::local_tempdir()
  res <- suppressMessages(save_outputs(root, demo(), dry_run = TRUE))
  txt <- grab(print(res))
  expect_true(grepl("Dry run", txt, fixed = TRUE))
  expect_true(grepl("01_data_cars.csv", txt, fixed = TRUE))
  expect_true(grepl("00_01_catalogue.pdf", txt, fixed = TRUE))
  # Nothing was written.
  expect_equal(length(list.files(root)), 0L)
})

test_that("a dry run marks excluded types as excluded", {
  setup_quiet_cli()
  root <- withr::local_tempdir()
  res <- suppressMessages(
    save_outputs(root, demo(), exclude = "data", dry_run = TRUE)
  )
  expect_true(identical(res$planned$data, NA_character_))
  expect_true(grepl("excluded", grab(print(res)), fixed = TRUE))
})

test_that("print() of a log leads with the problems", {
  setup_quiet_cli()
  log <- new_dr_log("test")
  for (i in 1:5) log_add(log, "progress", paste("step", i), "x")
  log_add(log, "warning", "something odd", "plots/p")
  log_add(log, "error", "could not write", "plots/q")

  txt <- grab(print(log))
  expect_true(grepl("something odd", txt, fixed = TRUE))
  expect_true(grepl("could not write", txt, fixed = TRUE))
  expect_true(grepl("progress 5", txt, fixed = TRUE))
  # Routine progress lines are not what you want to read in a log summary.
  expect_false(grepl("step 3", txt, fixed = TRUE))
})

test_that("print() of an empty log says so", {
  setup_quiet_cli()
  expect_true(grepl("Nothing recorded", grab(print(new_dr_log())), fixed = TRUE))
})


test_that("pad_width pads, truncates and handles accented characters", {
  expect_equal(pad_width("ab", 5L), "ab   ")
  expect_equal(nchar(pad_width("abcdefgh", 5L), type = "width"), 5L)
  expect_equal(nchar(pad_width("Plzeň", 8L), type = "width"), 8L)
  expect_equal(pad_width(NA, 3L), "   ")
})

test_that("display_title hides a title equal to the object name", {
  expect_equal(display_title(list(title = "x"), "x"), "")
  expect_equal(display_title(list(title = "A real title"), "x"), "A real title")
  expect_equal(display_title(list(), "x"), "")
})

test_that("doc_fields_present counts only substantive fields", {
  expect_equal(doc_fields_present(list(documentation = list(title = "t"))), 0L)
  expect_equal(
    doc_fields_present(list(documentation = list(desc = "d", source = c(a = "b")))),
    2L
  )
})

test_that("the export plan exposes the id used in printing", {
  entries <- list(a = list(subfolder = ""), b = list(subfolder = "sub"))
  plan <- plan_outputs(entries, dr_gpars(), "data")
  expect_true("id" %in% names(plan))
  expect_equal(plan$id[plan$name == "a"], "01")
  expect_equal(plan$id[plan$name == "b"], "02_01")
})
