# The statistics table is where the original script broke, so these tests are
# deliberately thorough about the edge cases around `formula` and `vars`.

test_that("formula_to_text substitutes variable labels", {
  expect_equal(
    formula_to_text("deaths/population", c(deaths = "Deaths", population = "Population")),
    "Deaths/Population"
  )
})

test_that("a formula with no vars is returned unchanged", {
  # Regression: the original built this with case_when() and raised
  # "`..2 (right)` must be a vector, not `NULL`" whenever a statistic had a
  # formula but no documented vars.
  expect_equal(formula_to_text("num/denom", NULL), "num/denom")
  expect_equal(formula_to_text("num/denom", character(0)), "num/denom")
  expect_equal(formula_to_text("num/denom", list()), "num/denom")
})

test_that("vars given as a list or without names are handled", {
  expect_equal(
    formula_to_text("a+b", list(a = "Alpha", b = "Beta")),
    "Alpha+Beta"
  )
  # Unnamed vars cannot be matched to anything, so the formula is unchanged.
  expect_equal(formula_to_text("a+b", c("Alpha", "Beta")), "a+b")
})

test_that("substitution respects whole-name boundaries", {
  # `n` must not be substituted inside `num`.
  expect_equal(
    formula_to_text("num/n", c(n = "Count", num = "Numerator")),
    "Numerator/Count"
  )
})

test_that("labels containing regex or replacement metacharacters are literal", {
  expect_equal(
    formula_to_text("x/y", c(x = "cost ($)", y = "count [n]")),
    "cost ($)/count [n]"
  )
})

test_that("an empty formula gives an empty string", {
  expect_equal(formula_to_text(NULL, c(a = "A")), "")
  expect_equal(formula_to_text("", c(a = "A")), "")
})

test_that("format_stat rounds to the requested number of decimals", {
  # Regression: the original passed `accuracy = 1 + 10^(-stat_round)` to
  # scales::comma(), which rounds to the nearest 1.001 - so a proportion of
  # 0.5 came out as "0".
  expect_equal(format_stat(0.5, "float", digits = 2), "0.50")
  expect_equal(format_stat(0.4567, "percent", digits = 1), "45.7")
  expect_equal(format_stat(0.4567, "float", digits = 3), "0.457")
  expect_equal(format_stat(1234.6, "integer"), "1 235")
  expect_equal(format_stat("hello", "char"), "hello")
})

test_that("format_stat honours the thousands separator", {
  expect_equal(format_stat(1234567, "integer", big_mark = ","), "1,234,567")
})

test_that("build_stats_table fills the sentence template", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  s <- 0.4567
  append_stats(s, output_list = out, type = "percent", stat_round = 1,
               text = "* per cent were affected.", quiet = TRUE)
  tbl <- build_stats_table(out$stats, out$gpars)
  expect_equal(tbl$stat, "45.7")
  expect_equal(tbl$text_fill, "45.7 per cent were affected.")
})

test_that("save_stats writes a table covering every appended statistic", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  a <- 10L
  b <- 0.25
  c_txt <- "note"
  append_stats(a, output_list = out, type = "integer", quiet = TRUE)
  append_stats(b, output_list = out, type = "percent", stat_round = 0,
               formula = "x/y", quiet = TRUE)   # formula, no vars
  append_stats(c_txt, output_list = out, quiet = TRUE)

  d <- withr::local_tempdir()
  man <- save_stats(out, d, quiet = TRUE)
  expect_true(file.exists(file.path(d, "01_stats.csv")))
  expect_true(all(man$status == "ok"))

  tbl <- utils::read.csv(file.path(d, "01_stats.csv"), stringsAsFactors = FALSE)
  expect_equal(nrow(tbl), 3L)
  expect_equal(tbl$stat[tbl$name == "b"], "25")
  expect_equal(tbl$formula_text[tbl$name == "b"], "x/y")
})

test_that("a text template without a placeholder warns", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  s <- 1
  expect_warning(
    append_stats(s, output_list = out, text = "no placeholder here", quiet = TRUE),
    "no .* placeholder"
  )
})
