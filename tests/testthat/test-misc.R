test_that("dr_version_name produces a unique name and never loops forever", {
  set.seed(42)
  n1 <- dr_version_name()
  expect_type(n1, "character")
  expect_length(n1, 1L)
  expect_true(nzchar(n1))
  expect_false(n1 %in% dr_version_name(used = n1))
})

test_that("dr_version_name falls back rather than hanging when exhausted", {
  # A vocabulary with exactly one possible name, already used.
  parts <- list(whose = "A", who = "B", named = "C", how = "D", what = "E")
  only <- dr_version_name(parts = parts)
  expect_warning(
    fallback <- dr_version_name(used = only, parts = parts, max_tries = 5L),
    "Could not find an unused"
  )
  expect_true(nzchar(fallback))
  expect_false(identical(fallback, only))
})

test_that("dr_version_name validates its word lists", {
  expect_error(dr_version_name(parts = list(whose = "A")), "non-empty")
  expect_error(
    dr_version_name(parts = list(whose = character(0), who = "B", named = "C",
                                 how = "D", what = "E")),
    "non-empty"
  )
})

test_that("get_version_DOC numbers versions and records session info", {
  root <- withr::local_tempdir()
  v <- get_version_DOC(root, project_title = "P", version_notes = "N")
  expect_equal(v$version, "0_1")
  expect_true(grepl("project title: P", v$text, fixed = TRUE))
  expect_true(grepl("version notes: N", v$text, fixed = TRUE))
  expect_true(grepl("R version", v$text, fixed = TRUE))
})

test_that("dr_versions on an empty or missing folder returns zero rows", {
  expect_equal(nrow(dr_versions(withr::local_tempdir())), 0L)
  expect_equal(nrow(dr_versions(file.path(tempdir(), "does-not-exist"))), 0L)
})

test_that("resolve_version_dir accepts a project or a version folder", {
  root <- withr::local_tempdir()
  out <- init_outputs(default = FALSE, quiet = TRUE)
  d <- cars
  append_data(d, output_list = out, quiet = TRUE)
  res <- save_outputs(root, out, quiet = TRUE)

  expect_equal(normalizePath(resolve_version_dir(root)), normalizePath(res$path))
  expect_equal(normalizePath(resolve_version_dir(res$path)), normalizePath(res$path))
  expect_equal(normalizePath(resolve_version_dir(root, "0_1")), normalizePath(res$path))
  expect_error(resolve_version_dir(root, "9_9"), "not found")
})


test_that("append_from_script picks up prefixed objects", {
  script <- tempfile(fileext = ".R")
  writeLines(c(
    "# a comment mentioning plt_ignored",
    "plt_one <- function() plot(1:10)",
    "tab_two = head(cars)",
    "data_three <- cars",
    "stat_four <- 1",
    "  plt_indented <- function() plot(1)"    # not top-level, so ignored
  ), script)

  plt_one <- function() plot(1:10)
  tab_two <- head(cars)
  data_three <- cars
  stat_four <- 1

  out <- init_outputs(default = FALSE, quiet = TRUE)
  append_from_script(script, output_list = out, quiet = TRUE)

  expect_named(out$plots, "plt_one")
  expect_named(out$tables, "tab_two")
  # data and stats are excluded by default.
  expect_length(out$data, 0L)
  expect_length(out$stats, 0L)
})

test_that("append_from_script can include every type", {
  script <- tempfile(fileext = ".R")
  writeLines(c("data_x <- cars", "stat_y <- 1"), script)
  data_x <- cars
  stat_y <- 1

  out <- init_outputs(default = FALSE, quiet = TRUE)
  append_from_script(script, output_list = out, exclude = character(0), quiet = TRUE)
  expect_named(out$data, "data_x")
  expect_named(out$stats, "stat_y")
})

test_that("append_from_script warns about objects that are not in scope", {
  script <- tempfile(fileext = ".R")
  writeLines("plt_absent <- function() plot(1)", script)
  out <- init_outputs(default = FALSE, quiet = TRUE)
  expect_warning(
    append_from_script(script, output_list = out, env = new.env(parent = emptyenv()),
                       quiet = TRUE),
    "not in the environment"
  )
})

test_that("append_from_script errors on a missing script and bad prefixes", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  expect_error(
    append_from_script(file.path(tempdir(), "nope.R"), output_list = out),
    "not found"
  )
  script <- tempfile(fileext = ".R")
  writeLines("plt_a <- 1", script)
  expect_error(
    append_from_script(script, output_list = out, prefixes = c(plots = "p*")),
    "letters, digits"
  )
})


test_that("setup_code writes a runnable starter script", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "04_code"), recursive = TRUE)
  f <- setup_code("01_test", wd = root, project_title = "Demo", open = FALSE,
                  quiet = TRUE)
  expect_true(file.exists(f))

  txt <- readLines(f, warn = FALSE)
  expect_true(any(grepl("init_outputs()", txt, fixed = TRUE)))
  expect_true(any(grepl("Demo", txt, fixed = TRUE)))
  expect_true(any(grepl("dr_is_rerun()", txt, fixed = TRUE)))
  # No template placeholders left behind.
  expect_false(any(grepl("{{", txt, fixed = TRUE)))
  # And it parses.
  expect_silent(parse(text = txt))
})

test_that("setup_code does not clobber an existing script by default", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "04_code"), recursive = TRUE)
  setup_code("01_test", wd = root, open = FALSE, quiet = TRUE)
  expect_warning(
    setup_code("01_test", wd = root, open = FALSE, quiet = TRUE),
    "already exists"
  )
  f2 <- suppressWarnings(setup_code("01_test", wd = root, open = FALSE, quiet = TRUE))
  expect_equal(basename(f2), "01_test_new.R")
})

test_that("setup_code errors when the code folder is missing", {
  root <- withr::local_tempdir()
  expect_error(setup_code("x", wd = root, open = FALSE, quiet = TRUE), "does not exist")
})


test_that("the log records levels and writes a readable file", {
  log <- new_dr_log("test")
  log_add(log, "warning", "something odd", "here")
  log_add(log, "message", "chatter", "there")
  log_add(log, "error", "broke", "elsewhere")

  expect_equal(log_count(log, "warning"), 1L)
  expect_equal(log_count(log, "error"), 1L)

  f <- file.path(withr::local_tempdir(), "log.txt")
  log_write(log, f)
  lines <- readLines(f, warn = FALSE)
  expect_true(any(grepl("something odd", lines)))
  expect_true(any(grepl("warning=1", lines)))
})

test_that("try_item captures an error instead of propagating it", {
  log <- new_dr_log()
  res <- try_item(log, stop("nope"), where = "unit")
  expect_true(item_failed(res))
  expect_equal(log_count(log, "error"), 1L)
  expect_true(grepl("nope", log_texts(log, "error")))
})

test_that("try_item captures stray stdout", {
  log <- new_dr_log()
  res <- try_item(log, {
    cat("printed by some package\n")
    42
  }, where = "unit")
  expect_false(item_failed(res))
  expect_equal(res, 42)
  expect_equal(log_count(log, "stdout"), 1L)
})

test_that("dr_gpars warns about unknown parameters", {
  expect_warning(dr_gpars(not_a_parameter = 1), "Unknown general parameter")
  expect_error(dr_gpars(1), "must be named")
})

test_that("dr_catalogue_pars validates its inputs", {
  expect_error(dr_catalogue_pars(margin = c(1, 2)), "length 1 or 4")
  expect_error(dr_catalogue_pars(header_max = 0), "between 0 and 1")
  expect_error(dr_catalogue_pars(header_max = 1), "between 0 and 1")
  expect_equal(length(dr_catalogue_pars(margin = 2)$margin), 4L)
})

test_that("resolve_paper knows its sizes and rejects nonsense", {
  expect_equal(resolve_paper("a4", "cm"), c(21, 29.7) / 2.54)
  expect_equal(resolve_paper(c(10, 20), "in"), c(10, 20))
  expect_error(resolve_paper("a9"), "Unknown paper size")
  expect_error(resolve_paper(c(1, 2, 3)), "c\\(width, height\\)")
})

test_that("check_path_length warns about paths Windows cannot open", {
  expect_warning(
    check_path_length(strrep("x", 300)),
    "exceed"
  )
  expect_silent(check_path_length("short/path.csv"))
})
