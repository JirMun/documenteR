test_that("CSV output has no phantom row-name column", {
  # Regression: utils::write.csv() writes row names by default as an unnamed
  # first column, which readr then reads back as `...1`. Every export made
  # with the original script carried that stray index column.
  d <- withr::local_tempdir()
  f <- file.path(d, "x.csv")
  write_data_file(head(cars, 3), f, ".csv")

  header <- readLines(f, n = 1L, warn = FALSE)
  expect_equal(header, '"speed","dist"')

  back <- utils::read.csv(f, stringsAsFactors = FALSE)
  expect_named(back, c("speed", "dist"))
  expect_equal(ncol(back), 2L)
})

test_that("the same holds for .tsv and .csv2", {
  d <- withr::local_tempdir()
  write_data_file(head(cars, 3), file.path(d, "x.tsv"), ".tsv")
  write_data_file(head(cars, 3), file.path(d, "x.csv2"), ".csv2")
  expect_equal(readLines(file.path(d, "x.tsv"), n = 1L), '"speed"\t"dist"')
  expect_equal(readLines(file.path(d, "x.csv2"), n = 1L), '"speed";"dist"')
})

test_that(".csv2 uses a comma decimal mark", {
  d <- withr::local_tempdir()
  f <- file.path(d, "x.csv2")
  write_data_file(data.frame(v = 1.5), f, ".csv2")
  expect_true(any(grepl("1,5", readLines(f), fixed = TRUE)))
})

test_that("CSV output is UTF-8 whatever the session locale", {
  d <- withr::local_tempdir()
  f <- file.path(d, "x.csv")
  df <- data.frame(city = c("Plzeň", "Ústí"), stringsAsFactors = FALSE)
  write_data_file(df, f, ".csv")
  back <- utils::read.csv(f, stringsAsFactors = FALSE, encoding = "UTF-8")
  expect_equal(enc2utf8(back$city), enc2utf8(df$city))
})

test_that("a BOM is written only when asked for", {
  d <- withr::local_tempdir()
  plain <- file.path(d, "plain.csv")
  bom <- file.path(d, "bom.csv")
  write_data_file(head(cars, 2), plain, ".csv", gpars = list(csv_bom = FALSE))
  write_data_file(head(cars, 2), bom, ".csv", gpars = list(csv_bom = TRUE))
  expect_false(identical(readBin(plain, "raw", 3L), as.raw(c(0xEF, 0xBB, 0xBF))))
  expect_identical(readBin(bom, "raw", 3L), as.raw(c(0xEF, 0xBB, 0xBF)))
})

test_that(".rds round-trips exactly", {
  d <- withr::local_tempdir()
  f <- file.path(d, "x.rds")
  write_data_file(cars, f, ".rds")
  expect_equal(readRDS(f), cars)
})

test_that("list columns are flattened for text formats, with a warning", {
  d <- withr::local_tempdir()
  df <- data.frame(id = 1:2)
  df$vals <- list(1:2, 3:4)
  f <- file.path(d, "x.csv")
  expect_warning(write_data_file(df, f, ".csv", name = "df"), "list column")
  back <- utils::read.csv(f, stringsAsFactors = FALSE)
  expect_equal(back$vals, c("1;2", "3;4"))
})

test_that("an unsupported format errors clearly", {
  d <- withr::local_tempdir()
  expect_error(
    write_data_file(cars, file.path(d, "x.dta"), ".dta"),
    "Cannot write"
  )
})

test_that("coerce_rectangular converts what it reasonably can", {
  expect_s3_class(coerce_rectangular(cars, "cars", "append_data"), "data.frame")
  m <- matrix(1:4, 2)
  expect_s3_class(coerce_rectangular(m, "m", "append_data"), "data.frame")
  expect_error(coerce_rectangular(NULL, "x", "append_data"), "nothing to save")
})

test_that("dr_formats lists something for every output type", {
  fmts <- dr_formats()
  expect_setequal(names(fmts), c("data", "tables", "stats", "plots"))
  expect_true(all(lengths(fmts) > 0L))
  expect_true(all(startsWith(unlist(fmts), ".")))
})
