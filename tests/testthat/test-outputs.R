test_that("init_outputs creates an empty collection with defaults", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  expect_s3_class(out, "documenteR_outputs")
  expect_length(out, 0L)
  for (ty in dr_output_types()) expect_length(out[[ty]], 0L)
  expect_equal(out$gpars$formats$data, ".csv")
})

test_that("init_outputs(default = TRUE) assigns into the caller's frame", {
  f <- function() {
    init_outputs(quiet = TRUE)
    exists("list_outputs", inherits = FALSE)
  }
  expect_true(f())
})

test_that("gpars can be overridden without losing the other defaults", {
  out <- init_outputs(
    default = FALSE, quiet = TRUE,
    gpars = dr_gpars(formats = list(plots = ".svg"), stat_round = 1)
  )
  expect_equal(out$gpars$formats$plots, ".svg")
  expect_equal(out$gpars$formats$data, ".csv")
  expect_equal(out$gpars$stat_round, 1)
})

test_that("the collection has reference semantics, so append_* mutates it", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  d <- cars
  append_data(d, output_list = out, quiet = TRUE)
  # No reassignment: the caller's `out` sees the new entry.
  expect_named(out$data, "d")
})

test_that("gpars stay assignable through $", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  out$gpars$formats$plots <- ".svg"
  expect_equal(out$gpars$formats$plots, ".svg")
})

test_that("append_* infer the object name and default the title to it", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  plt_demo <- function() plot(1:10)
  append_plot(plt_demo, output_list = out, quiet = TRUE)
  expect_named(out$plots, "plt_demo")
  expect_equal(out$plots$plt_demo$documentation$title, "plt_demo")
})

test_that("an inline expression is refused with an actionable error", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  expect_error(
    append_data(head(cars), output_list = out, quiet = TRUE),
    "Cannot work out a name"
  )
  # ... and accepted once a name is given.
  expect_silent(
    append_data(head(cars), object_name = "d", output_list = out, quiet = TRUE)
  )
})

test_that("append_plot rejects things it could not draw", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  x <- cars
  expect_error(append_plot(x, output_list = out, quiet = TRUE), "Cannot draw")
  y <- 1:10
  expect_error(append_plot(y, output_list = out, quiet = TRUE), "Cannot draw")
})

test_that("append_data refuses a non-rectangular object", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  x <- function() NULL
  expect_error(append_data(x, output_list = out, quiet = TRUE), "needs a data frame")
})

test_that("append_stats validates length, class and type agreement", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  v <- 1:5
  expect_error(append_stats(v, output_list = out, quiet = TRUE), "single value")
  s <- "abc"
  expect_error(
    append_stats(s, type = "percent", output_list = out, quiet = TRUE),
    "Numeric formatting"
  )
  n <- 3
  expect_error(
    append_stats(n, stat_round = -1, output_list = out, quiet = TRUE),
    "non-negative whole number"
  )
})

test_that("an unknown size preset warns but still appends", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  p <- function() plot(1:10)
  expect_warning(
    append_plot(p, output_list = out, type = "nonexistent", quiet = TRUE),
    "not defined"
  )
  expect_named(out$plots, "p")
})

test_that("subfolders may not escape the output folder", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  d <- cars
  expect_error(
    append_data(d, output_list = out, subfolder = "../evil", quiet = TRUE),
    "may not contain"
  )
  expect_silent(
    append_data(d, output_list = out, subfolder = "nested/ok", quiet = TRUE)
  )
})

test_that("an unsupported format is refused at append time", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  d <- cars
  expect_error(
    append_data(d, output_list = out, format = ".dta", quiet = TRUE),
    "Unsupported data format"
  )
})

test_that("re-appending replaces, and can be refused", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  d <- cars
  append_data(d, output_list = out, quiet = TRUE)
  append_data(d, output_list = out, title = "second", quiet = TRUE)
  expect_length(out$data, 1L)
  expect_equal(out$data$d$documentation$title, "second")
  expect_error(
    append_data(d, output_list = out, replace = FALSE, quiet = TRUE),
    "already in"
  )
})

test_that("remove_outputs drops entries by name and by type", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  d1 <- cars
  d2 <- cars
  append_data(d1, output_list = out, quiet = TRUE)
  append_data(d2, output_list = out, quiet = TRUE)
  remove_outputs("d1", output_list = out, quiet = TRUE)
  expect_named(out$data, "d2")
  remove_outputs(type = "data", output_list = out, quiet = TRUE)
  expect_length(out$data, 0L)
})

test_that("as_outputs converts a legacy list and maps gpars$ggplot", {
  legacy <- list(
    plots = list(), data = list(), tables = list(), stats = list(),
    gpars = list(
      stat_round = 1,
      ggplot = list(default = list(units = "cm", H = 5, W = 5))
    )
  )
  out <- as_outputs(legacy)
  expect_s3_class(out, "documenteR_outputs")
  expect_equal(out$gpars$stat_round, 1)
  expect_equal(out$gpars$sizes$default$H, 5)
  # Missing parameters are filled in from the defaults.
  expect_equal(out$gpars$formats$data, ".csv")
})

test_that("as_outputs refuses something that is not a collection", {
  expect_error(as_outputs(42), "Cannot use")
})

test_that("resolve_outputs gives a useful error when nothing is available", {
  f <- function() {
    # An empty environment chain and no registered collection.
    resolve_outputs(NULL, env = new.env(parent = emptyenv()))
  }
  old <- .dr_state$active
  .dr_state$active <- NULL
  on.exit(.dr_state$active <- old, add = TRUE)
  expect_error(f(), "No output collection found")
})

test_that("summary() reports what has been appended", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  d <- cars
  append_data(d, output_list = out, desc = "documented", quiet = TRUE)
  s <- summary(out)
  expect_s3_class(s, "data.frame")
  expect_equal(s$name, "d")
  expect_true(s$has_desc)
  expect_equal(s$doc_fields, 1L)
})
