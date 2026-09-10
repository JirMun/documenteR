test_that("get_output_README documents each output", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  d <- cars
  append_data(d, output_list = out,
              title = "Cars", desc = "Speed and stopping distance",
              vars = c(speed = "Speed (mph)", dist = "Distance (ft)"),
              source = c(datasets = "R base"),
              notes = "A note", quiet = TRUE)

  txt <- get_output_README(out, "data")
  expect_type(txt, "character")
  expect_length(txt, 1L)
  for (bit in c("DATA", "Cars", "Speed and stopping distance",
                "Speed (mph)", "datasets: R base", "A note",
                "50 rows x 2 columns")) {
    expect_true(grepl(bit, txt, fixed = TRUE), info = bit)
  }
})

test_that("undocumented fields are omitted rather than shown empty", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  d <- cars
  append_data(d, output_list = out, quiet = TRUE)
  txt <- get_output_README(out, "data")
  expect_false(grepl("desc", txt, fixed = TRUE))
  expect_false(grepl("source", txt, fixed = TRUE))
  expect_true(grepl("title", txt, fixed = TRUE))
})

test_that("README lines respect the configured width", {
  out <- init_outputs(default = FALSE, quiet = TRUE,
                      gpars = dr_gpars(readme_w = 60))
  d <- cars
  append_data(d, output_list = out,
              desc = paste(rep("a long description", 20), collapse = " "),
              quiet = TRUE)
  lines <- strsplit(get_output_README(out, "data"), "\n", fixed = TRUE)[[1]]
  expect_lte(max(nchar(lines)), 62L)
})

test_that("plot READMEs record the export geometry", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  append_plot(function() plot(1:10), object_name = "p", output_list = out,
              type = "s_wide", quiet = TRUE)
  txt <- get_output_README(out, "plots")
  expect_true(grepl("16 x 8 cm", txt, fixed = TRUE))
  expect_true(grepl('type "s_wide"', txt, fixed = TRUE))
})

test_that("the README for an empty type says so", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  txt <- get_output_README(out, "tables")
  expect_true(grepl("nothing of this kind", txt, fixed = TRUE))
})

test_that("non-ASCII documentation survives into the README", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  d <- cars
  append_data(d, output_list = out, desc = "Údaje za Plzeňský kraj", quiet = TRUE)
  txt <- get_output_README(out, "data")
  expect_true(grepl("Plzeňský", txt, fixed = TRUE))
})


test_that("dr_codebook describes every column", {
  cb <- dr_codebook(iris)
  expect_equal(nrow(cb), ncol(iris))
  expect_setequal(cb$variable, names(iris))
  expect_true(all(c("class", "n", "n_missing", "n_unique", "summary") %in% names(cb)))
  expect_equal(cb$n, rep(nrow(iris), ncol(iris)))
})

test_that("dr_codebook counts missing values", {
  df <- data.frame(x = c(1, NA, 3), y = c("a", "b", NA), stringsAsFactors = FALSE)
  cb <- dr_codebook(df)
  expect_equal(cb$n_missing, c(1L, 1L))
  expect_equal(cb$pct_missing, c(33.33, 33.33), tolerance = 0.01)
})

test_that("dr_codebook picks up labels from arguments and attributes", {
  df <- data.frame(a = 1:3, b = 4:6)
  attr(df$b, "label") <- "From an attribute"
  cb <- dr_codebook(df, labels = c(a = "From the argument"))
  expect_equal(cb$label, c("From the argument", "From an attribute"))
})

test_that("dr_codebook summarises each column type sensibly", {
  df <- data.frame(
    num = c(1, 2, 3),
    chr = c("a", "a", "b"),
    lgl = c(TRUE, FALSE, TRUE),
    dte = as.Date(c("2020-01-01", "2020-06-01", "2020-12-31")),
    stringsAsFactors = FALSE
  )
  cb <- dr_codebook(df)
  expect_true(grepl("median", cb$summary[cb$variable == "num"]))
  expect_true(grepl("a (2)", cb$summary[cb$variable == "chr"], fixed = TRUE))
  expect_true(grepl("TRUE 2", cb$summary[cb$variable == "lgl"], fixed = TRUE))
  expect_true(grepl("2020-01-01 to 2020-12-31", cb$summary[cb$variable == "dte"]))
})

test_that("an all-missing column is reported, not an error", {
  cb <- dr_codebook(data.frame(x = c(NA, NA)))
  expect_equal(cb$summary, "all missing")
})

test_that("a zero-column data frame gives a zero-row codebook", {
  expect_equal(nrow(dr_codebook(data.frame())), 0L)
})

test_that("save_data writes one codebook covering every dataset", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  a <- cars
  b <- head(iris)
  append_data(a, output_list = out, quiet = TRUE)
  append_data(b, output_list = out, quiet = TRUE)

  d <- withr::local_tempdir()
  save_data(out, d, quiet = TRUE)
  cb <- utils::read.csv(file.path(d, "00_codebook.csv"), stringsAsFactors = FALSE)
  expect_setequal(unique(cb$dataset), c("a", "b"))
  expect_equal(nrow(cb), ncol(cars) + ncol(iris))
})

test_that("codebook = FALSE on an entry excludes just that dataset", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  a <- cars
  b <- head(iris)
  append_data(a, output_list = out, codebook = FALSE, quiet = TRUE)
  append_data(b, output_list = out, quiet = TRUE)

  d <- withr::local_tempdir()
  save_data(out, d, quiet = TRUE)
  cb <- utils::read.csv(file.path(d, "00_codebook.csv"), stringsAsFactors = FALSE)
  expect_setequal(unique(cb$dataset), "b")
})

test_that("gpars$codebook = FALSE skips the codebook entirely", {
  out <- init_outputs(default = FALSE, quiet = TRUE,
                      gpars = dr_gpars(codebook = FALSE))
  a <- cars
  append_data(a, output_list = out, quiet = TRUE)
  d <- withr::local_tempdir()
  save_data(out, d, quiet = TRUE)
  expect_false(file.exists(file.path(d, "00_codebook.csv")))
})
