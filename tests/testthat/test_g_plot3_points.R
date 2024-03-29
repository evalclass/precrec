#' @importFrom precrec

context("PT 2: Plot points")
# Test plot(x, y, ...)

pt3_create_mspoints <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels)
  evalmod(mdat, mode = "basic")
}

pt3_create_smpoints <- function(raw_curves = FALSE) {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels, expd_first = "dsids")
  evalmod(mdat, mode = "basic", raw_curves = raw_curves)
}

pt3_create_mmpoints <- function(raw_curves = FALSE) {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  s4 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3, s4)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  l4 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3, l4)

  mdat <- mmdata(scores, labels,
    modnames = c("m1", "m2"), dsids = c(1, 2),
    expd_first = "modnames"
  )
  evalmod(mdat, mode = "basic", raw_curves = raw_curves)
}

ap3_test_basic_measures <- function(xpoints, ...) {
  testthat::expect_silent(plot(xpoints, ...))
  testthat::expect_silent(plot(xpoints, c(
    "sensitivity", "specificity", "error",
    "accuracy", "precision"
  ), ...))
  testthat::expect_silent(plot(xpoints, c(
    "sensitivity", "specificity", "error",
    "precision"
  ), ...))
  testthat::expect_silent(plot(xpoints, c(
    "sensitivity", "specificity",
    "precision"
  ), ...))
  testthat::expect_silent(plot(xpoints, c("sensitivity", "precision"), ...))
  testthat::expect_silent(plot(xpoints, "precision", ...))
}

test_that("plot sspoints", {
  pdf(NULL)
  on.exit(dev.off())

  data(P10N10)
  sspoints <- evalmod(
    scores = P10N10$scores, labels = P10N10$labels,
    mode = "basic"
  )

  ap3_test_basic_measures(sspoints)
  ap3_test_basic_measures(sspoints, type = "l")
  ap3_test_basic_measures(sspoints, type = "b")
})

test_that("plot mspoints", {
  pdf(NULL)
  on.exit(dev.off())

  mspoints <- pt3_create_mspoints()

  ap3_test_basic_measures(mspoints)
  ap3_test_basic_measures(mspoints, type = "l")
  ap3_test_basic_measures(mspoints, type = "b")
  ap3_test_basic_measures(mspoints, show_legend = TRUE)
})

test_that("plot smpoints", {
  pdf(NULL)
  on.exit(dev.off())

  smpoints <- pt3_create_smpoints()

  ap3_test_basic_measures(smpoints)
  ap3_test_basic_measures(smpoints, type = "l")
  ap3_test_basic_measures(smpoints, type = "b")
  ap3_test_basic_measures(smpoints, show_cb = FALSE)

  smpoints2 <- pt3_create_smpoints(raw_curves = TRUE)
  ap3_test_basic_measures(smpoints2, raw_curves = TRUE)
})

test_that("plot mmpoints", {
  pdf(NULL)
  on.exit(dev.off())

  mmpoints <- pt3_create_mmpoints()

  ap3_test_basic_measures(mmpoints)
  ap3_test_basic_measures(mmpoints, type = "l")
  ap3_test_basic_measures(mmpoints, type = "b")
  ap3_test_basic_measures(mmpoints, show_cb = TRUE)
  ap3_test_basic_measures(mmpoints, show_legend = FALSE)

  mmpoints2 <- pt3_create_mmpoints(raw_curves = TRUE)
  ap3_test_basic_measures(mmpoints2, raw_curves = TRUE)
})

test_that("plot raw_curve option sspoints", {
  get_args <- function(x, y = NULL, ...) {
    .get_plot_arglist(attr(x, "args"), y,
      def_curvetype = .get_metric_names("basic"),
      def_type = "p",
      def_show_cb = FALSE, def_raw_curves = TRUE,
      def_add_np_nn = TRUE, def_show_legend = TRUE,
      ...
    )
  }

  data(P10N10)
  points1 <- evalmod(
    mode = "basic", scores = P10N10$scores,
    labels = P10N10$labels
  )

  args1a <- get_args(points1, raw_curves = TRUE)
  expect_true(args1a[["raw_curves"]])

  args1b <- get_args(points1, raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- get_args(points1)
  expect_true(args1c[["raw_curves"]])
})

test_that("plot raw_curve option mspoints", {
  get_args <- function(x, y = NULL, ...) {
    .get_plot_arglist(attr(x, "args"), y,
      def_curvetype = .get_metric_names("basic"),
      def_type = "p",
      def_show_cb = FALSE, def_raw_curves = TRUE,
      def_add_np_nn = TRUE, def_show_legend = TRUE,
      ...
    )
  }

  points1 <- pt3_create_mspoints()

  args1a <- get_args(points1, raw_curves = TRUE)
  expect_true(args1a[["raw_curves"]])

  args1b <- get_args(points1, raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- get_args(points1)
  expect_true(args1c[["raw_curves"]])
})

test_that("plot raw_curve option smpoints", {
  get_args <- function(x, y = NULL, ...) {
    .get_plot_arglist(attr(x, "args"), y,
      def_curvetype = .get_metric_names("basic"),
      def_type = "p",
      def_show_cb = TRUE, def_raw_curves = NULL,
      def_add_np_nn = TRUE, def_show_legend = FALSE,
      ...
    )
  }

  points1 <- pt3_create_smpoints()

  expect_error(get_args(points1, raw_curves = TRUE), "Invalid raw_curves.")

  args1b <- get_args(points1, raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- get_args(points1)
  expect_false(args1c[["raw_curves"]])

  points2 <- pt3_create_smpoints(raw_curves = TRUE)

  args2a <- get_args(points2, raw_curves = TRUE)
  expect_true(args2a[["raw_curves"]])

  args2b <- get_args(points2, raw_curves = FALSE)
  expect_false(args2b[["raw_curves"]])

  args2c <- get_args(points2)
  expect_true(args2c[["raw_curves"]])
})

test_that("plot raw_curve option mmpoints", {
  get_args <- function(x, y = NULL, ...) {
    .get_plot_arglist(attr(x, "args"), y,
      def_curvetype = .get_metric_names("basic"),
      def_type = "p",
      def_show_cb = FALSE, def_raw_curves = NULL,
      def_add_np_nn = TRUE, def_show_legend = TRUE,
      ...
    )
  }

  points1 <- pt3_create_mmpoints()

  expect_error(get_args(points1, raw_curves = TRUE), "Invalid raw_curves.")

  args1b <- get_args(points1, raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- get_args(points1)
  expect_false(args1c[["raw_curves"]])

  points2 <- pt3_create_mmpoints(raw_curves = TRUE)

  args2a <- get_args(points2, raw_curves = TRUE)
  expect_true(args2a[["raw_curves"]])

  args2b <- get_args(points2, raw_curves = FALSE)
  expect_false(args2b[["raw_curves"]])

  args2c <- get_args(points2)
  expect_true(args2c[["raw_curves"]])
})
