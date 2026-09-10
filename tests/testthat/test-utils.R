test_that("path_join is vectorised and collapses separators", {
  expect_equal(path_join("a", "b", "c"), "a/b/c")
  expect_equal(path_join("a/", "/b"), "a/b")
  expect_equal(path_join("a", "", "c"), "a/c")
  expect_equal(path_join(character(0)), character(0))

  # Vectorised: one path per element, not one collapsed path. Getting this
  # wrong silently corrupted dr_versions() and dr_verify().
  expect_equal(path_join("root", c("0_1", "0_2")), c("root/0_1", "root/0_2"))
  expect_length(path_join("root", c("a", "b", "c")), 3L)
})

test_that("path_join keeps a UNC prefix", {
  expect_equal(path_join("//server/share", "dir"), "//server/share/dir")
})

test_that("is_blank treats NULL, NA and empty strings alike", {
  expect_true(is_blank(NULL))
  expect_true(is_blank(NA))
  expect_true(is_blank(""))
  expect_true(is_blank("   "))
  expect_true(is_blank(character(0)))
  expect_false(is_blank("x"))
  expect_false(is_blank(0))
})

test_that("sanitise_filename produces portable names", {
  expect_equal(sanitise_filename("plt_cars"), "plt_cars")
  expect_equal(sanitise_filename("Plzen kraj"), "Plzen_kraj")
  expect_equal(sanitise_filename("a/b:c*d?"), "a_b_c_d")
  expect_equal(sanitise_filename("__x__"), "x")
  expect_equal(sanitise_filename(""), "unnamed")
  # Diacritics are transliterated, not dropped.
  expect_equal(sanitise_filename("ústí"), "usti")
  expect_false(grepl("[^A-Za-z0-9._-]", sanitise_filename("české údaje")))
})

test_that("split_spec accepts both vectors and semicolon strings", {
  expect_equal(split_spec(".csv;.rds"), c(".csv", ".rds"))
  expect_equal(split_spec(c(".csv", ".rds")), c(".csv", ".rds"))
  expect_equal(split_spec(NULL), character(0))
  expect_equal(split_spec(""), character(0))
})

test_that("normalise_ext always yields a single leading dot", {
  expect_equal(normalise_ext("csv"), ".csv")
  expect_equal(normalise_ext(".CSV"), ".csv")
  expect_equal(normalise_ext("..csv"), ".csv")
})

test_that("to_inches converts correctly", {
  expect_equal(to_inches(2.54, "cm"), 1)
  expect_equal(to_inches(25.4, "mm"), 1)
  expect_equal(to_inches(1, "in"), 1)
  expect_equal(to_inches(300, "px", dpi = 300), 1)
})

test_that("resolve_dpi handles names, numbers and nonsense", {
  expect_equal(resolve_dpi("retina"), 320)
  expect_equal(resolve_dpi("print"), 300)
  expect_equal(resolve_dpi(150), 150)
  expect_equal(resolve_dpi("banana"), 300)
  expect_equal(resolve_dpi(NULL), 300)
})

test_that("as_named_chr normalises the shapes a user might pass", {
  expect_equal(as_named_chr(c(x = "A")), c(x = "A"))
  expect_equal(as_named_chr(list(x = "A")), c(x = "A"))
  expect_length(as_named_chr(NULL), 0L)
  expect_equal(unname(as_named_chr("A")), "A")
})

test_that("collapse_named tolerates missing names", {
  expect_equal(collapse_named(c(a = "1", b = "2")), "a = 1; b = 2")
  expect_equal(collapse_named(c("1", "2")), "1; 2")
  expect_equal(collapse_named(NULL), "")
})

test_that("merge_lists merges recursively rather than replacing", {
  base <- list(formats = list(data = ".csv", plots = ".png"), n = 1)
  out <- merge_lists(base, list(formats = list(plots = ".svg")))
  expect_equal(out$formats$plots, ".svg")
  expect_equal(out$formats$data, ".csv")
  expect_equal(out$n, 1)
})

test_that("write_utf8 round-trips non-ASCII text", {
  f <- tempfile()
  txt <- c("Plzeň", "Ústí nad Labem")
  write_utf8(txt, f)
  back <- readLines(f, encoding = "UTF-8", warn = FALSE)
  expect_equal(enc2utf8(back), enc2utf8(txt))
})
