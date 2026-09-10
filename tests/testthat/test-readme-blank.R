test_that("README blocks contain no blank continuation lines", {
  # Regression: doc_named_block() built continuation lines with
  # paste0(pad, lines[-1]), and paste0() treats a zero-length argument as "",
  # so a single-line entry emitted one line of trailing spaces.
  out <- init_outputs(default = FALSE, quiet = TRUE)
  d <- cars
  append_data(d, output_list = out,
              desc = "Short",
              source = c(datasets = "R built-in dataset"),
              vars = c(speed = "Speed"),
              quiet = TRUE)

  lines <- strsplit(get_output_README(out, "data"), "\n", fixed = TRUE)[[1]]
  # A line of nothing but spaces is never intentional.
  expect_false(any(grepl("^[[:space:]]+$", lines)))
})

test_that("multi-line documentation values still get their continuations", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  d <- cars
  append_data(d, output_list = out,
              vars = c(a = paste(rep("long label", 20), collapse = " ")),
              quiet = TRUE)
  lines <- strsplit(get_output_README(out, "data"), "\n", fixed = TRUE)[[1]]
  # The wrapped value spans several lines, indented under the first.
  wrapped <- grep("long label", lines, value = TRUE)
  expect_gt(length(wrapped), 1L)
  expect_false(any(grepl("^[[:space:]]+$", lines)))
})
