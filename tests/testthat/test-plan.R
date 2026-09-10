make_entries <- function(...) {
  spec <- list(...)
  lapply(spec, function(sf) list(subfolder = sf))
}

test_that("root-level outputs are numbered in order", {
  entries <- make_entries(a = "", b = "", c = "")
  plan <- plan_outputs(entries, dr_gpars(), "data")
  expect_equal(plan$stem, c("01_a", "02_b", "03_c"))
  expect_equal(plan$dir, rep("", 3L))
})

test_that("subfolders are numbered after the root-level files", {
  entries <- make_entries(a = "", b = "", c = "maps", d = "maps")
  plan <- plan_outputs(entries, dr_gpars(), "plots")
  expect_equal(plan$stem[plan$name == "a"], "01_a")
  expect_equal(plan$stem[plan$name == "b"], "02_b")
  expect_equal(plan$dir[plan$name == "c"], "03_maps")
  expect_equal(plan$stem[plan$name == "c"], "03_01_c")
  expect_equal(plan$stem[plan$name == "d"], "03_02_d")
})

test_that("several subfolders get consecutive numbers", {
  entries <- make_entries(a = "", b = "one", c = "two")
  plan <- plan_outputs(entries, dr_gpars(), "plots")
  expect_equal(plan$dir[plan$name == "b"], "02_one")
  expect_equal(plan$dir[plan$name == "c"], "03_two")
})

test_that("rows come out in the order the numbering reads", {
  entries <- make_entries(a = "sub", b = "", c = "sub")
  plan <- plan_outputs(entries, dr_gpars(), "plots")
  # Root-level first, then the subfolder.
  expect_equal(plan$name, c("b", "a", "c"))
})

test_that("nested subfolders are kept as a path, each part sanitised", {
  entries <- make_entries(a = "maps/regionální")
  plan <- plan_outputs(entries, dr_gpars(), "plots")
  # There are no root-level files, so the first subfolder takes number 01.
  expect_equal(plan$dir, "01_maps/regionalni")
})

test_that("object names are sanitised in file stems", {
  entries <- list(`plt Plzeň` = list(subfolder = ""))
  plan <- plan_outputs(entries, dr_gpars(), "plots")
  expect_equal(plan$stem, "01_plt_Plzen")
})

test_that("per-entry formats override the type default", {
  entries <- list(
    a = list(subfolder = "", format = c(".csv", ".rds")),
    b = list(subfolder = "")
  )
  plan <- plan_outputs(entries, dr_gpars(), "data")
  expect_equal(plan$formats[plan$name == "a"], ".csv;.rds")
  expect_equal(plan$formats[plan$name == "b"], ".csv")
})

test_that("a plot entry's `device` is used as its format", {
  entries <- list(a = list(subfolder = "", device = c(".png", ".svg")))
  plan <- plan_outputs(entries, dr_gpars(), "plots")
  expect_equal(plan$formats, ".png;.svg")
})

test_that("an empty collection gives a zero-row plan", {
  plan <- plan_outputs(list(), dr_gpars(), "data")
  expect_equal(nrow(plan), 0L)
  expect_true(all(c("name", "dir", "stem", "formats") %in% names(plan)))
})

test_that("folder_setup only creates folders for types that have content", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  d <- cars
  append_data(d, output_list = out, quiet = TRUE)

  root <- withr::local_tempdir()
  dirs <- folder_setup(out, root)
  expect_setequal(names(dirs), c("output", "data"))
  expect_true(all(dir.exists(dirs)))
  expect_false(dir.exists(file.path(root, "03_plots")))
})

test_that("folder_setup insists on an output entry", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  root <- withr::local_tempdir()
  expect_error(
    folder_setup(out, root, folders = c(data = "02_data")),
    "must include an"
  )
})
