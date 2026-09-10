test_that("dr_can_draw accepts plots and rejects data", {
  expect_true(dr_can_draw(function() plot(1:10)))
  expect_true(dr_can_draw(grid::circleGrob()))
  expect_true(dr_can_draw(quote(plot(1:10))))
  expect_false(dr_can_draw(NULL))
  expect_false(dr_can_draw(cars))
  expect_false(dr_can_draw(1:10))
})

test_that("dr_save_plot writes a base-graphics plot at the requested size", {
  d <- withr::local_tempdir()
  f <- file.path(d, "p.png")
  dr_save_plot(function() plot(1:10), f, W = 10, H = 5, units = "cm", dpi = 100)
  expect_true(file.exists(f))
  expect_gt(file.size(f), 0)
})

test_that("physical size is honoured for raster output", {
  skip_if_not_installed("png")
  d <- withr::local_tempdir()
  f <- file.path(d, "p.png")
  dr_save_plot(function() plot(1:10), f, W = 10, H = 5, units = "cm", dpi = 100)
  dims <- dim(png::readPNG(f))
  # 10 cm at 100 dpi is 3.937 in -> ~394 px; allow a pixel of rounding.
  expect_equal(dims[2], round(10 / 2.54 * 100), tolerance = 2)
  expect_equal(dims[1], round(5 / 2.54 * 100), tolerance = 2)
})

test_that("every supported vector and raster format can be written", {
  d <- withr::local_tempdir()
  p <- function() plot(1:10)
  for (ext in c(".png", ".jpg", ".pdf", ".svg")) {
    f <- file.path(d, paste0("p", ext))
    expect_silent(dr_save_plot(p, f, W = 8, H = 6, units = "cm", dpi = 72))
    expect_gt(file.size(f), 0)
  }
})

test_that("a ggplot can be saved", {
  skip_if_not_installed("ggplot2")
  d <- withr::local_tempdir()
  f <- file.path(d, "gg.png")
  p <- ggplot2::ggplot(cars, ggplot2::aes(speed, dist)) + ggplot2::geom_point()
  dr_save_plot(p, f, W = 8, H = 6, units = "cm", dpi = 72)
  expect_true(file.exists(f))
})

test_that("a lattice plot can be saved", {
  skip_if_not_installed("lattice")
  d <- withr::local_tempdir()
  f <- file.path(d, "lat.png")
  p <- lattice::xyplot(dist ~ speed, data = cars)
  dr_save_plot(p, f, W = 8, H = 6, units = "cm", dpi = 72)
  expect_true(file.exists(f))
})

test_that("a grid grob can be saved", {
  d <- withr::local_tempdir()
  f <- file.path(d, "grob.png")
  dr_save_plot(grid::circleGrob(), f, W = 6, H = 6, units = "cm", dpi = 72)
  expect_true(file.exists(f))
})

test_that("a function requiring arguments is refused with guidance", {
  d <- withr::local_tempdir()
  expect_error(
    dr_save_plot(function(x) plot(x), file.path(d, "p.png"), W = 5, H = 5),
    "no required arguments"
  )
})

test_that("dr_save_plot validates path and dimensions", {
  d <- withr::local_tempdir()
  expect_error(dr_save_plot(function() plot(1), file.path(d, "p")), "extension")
  expect_error(
    dr_save_plot(function() plot(1), file.path(d, "p.png"), W = -1),
    "positive numbers"
  )
})

test_that("the graphics device is closed even when drawing fails", {
  d <- withr::local_tempdir()
  before <- grDevices::dev.list()
  expect_error(
    dr_save_plot(function() stop("boom"), file.path(d, "p.png"), W = 5, H = 5),
    "boom"
  )
  expect_equal(grDevices::dev.list(), before)
})

test_that("resolve_plot_pars layers entry over type over default", {
  gp <- dr_gpars()
  # Default only.
  p0 <- resolve_plot_pars(list(), gp)
  expect_equal(p0$W, 16)
  # A named preset.
  p1 <- resolve_plot_pars(list(type = "s"), gp)
  expect_equal(c(p1$W, p1$H), c(8, 8))
  # Explicit values win over the preset.
  p2 <- resolve_plot_pars(list(type = "s", W = 20), gp)
  expect_equal(p2$W, 20)
  expect_equal(p2$H, 8)
})

test_that("wrap_plot_labels only touches ggplot objects", {
  expect_identical(wrap_plot_labels(cars, list(title = 10)), cars)
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(cars, ggplot2::aes(speed, dist)) +
    ggplot2::geom_point() +
    ggplot2::labs(title = paste(rep("word", 20), collapse = " "))
  wrapped <- wrap_plot_labels(p, list(title = 20, labs = 10))
  expect_true(grepl("\n", wrapped$labels$title, fixed = TRUE))
})

test_that("preview_plot writes a file without opening a viewer", {
  out <- init_outputs(default = FALSE, quiet = TRUE)
  f <- preview_plot(function() plot(1:10), output_list = out,
                    type = "s", open = FALSE, quiet = TRUE)
  expect_true(file.exists(f))
})
