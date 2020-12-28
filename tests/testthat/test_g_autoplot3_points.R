#' @importFrom precrec

context("AP 3: Autoplot for points")
# Test autoplot(object, ...)

ap3_check_libs <- function() {
  if (requireNamespace("ggplot2", quietly = TRUE)
      && requireNamespace("grid", quietly = TRUE)
      && requireNamespace("gridExtra", quietly = TRUE)) {
    TRUE
  } else {
    FALSE
  }
}

ap3_create_mspoints <- function() {
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

ap3_create_smpoints <- function(raw_curves = FALSE) {
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

ap3_create_mmpoints <- function(raw_curves = FALSE) {
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

  mdat <- mmdata(scores, labels, modnames = c("m1", "m2"), dsids = c(1, 2),
                 expd_first = "modnames")
  evalmod(mdat, mode = "basic", raw_curves = raw_curves)
}

ap3_test_basic_measures <- function(curves, raw_curves = FALSE, ...){
  pp <- ggplot2::autoplot(curves, raw_curves = raw_curves, ...)
  expect_error(pp, NA)

  pp <- ggplot2::autoplot(curves, c("sensitivity", "specificity", "error",
                                    "accuracy", "precision", "mcc", "score",
                                    "label"),
                          raw_curves = raw_curves, ...)
  expect_error(pp, NA)

  pp <- ggplot2::autoplot(curves, c("sensitivity", "specificity", "error",
                                    "accuracy", "precision", "mcc", "score"),
                          raw_curves = raw_curves, ...)
  expect_error(pp, NA)

  pp <- ggplot2::autoplot(curves, c("sensitivity", "specificity", "error",
                                    "accuracy", "precision", "mcc"),
                          raw_curves = raw_curves, ...)
  expect_error(pp, NA)

  pp <- ggplot2::autoplot(curves, c("sensitivity", "specificity", "error",
                                    "accuracy", "precision"),
                          raw_curves = raw_curves, ...)
  expect_error(pp, NA)

  pp <- ggplot2::autoplot(curves, c("sensitivity", "specificity", "error",
                                    "precision"),
                          raw_curves = raw_curves, ...)
  expect_error(pp, NA)

  pp <- ggplot2::autoplot(curves, c("sensitivity", "specificity", "precision"),
                          raw_curves = raw_curves, ...)
  expect_error(pp, NA)

  pp <- ggplot2::autoplot(curves, c("sensitivity", "precision"),
                          raw_curves = raw_curves, ...)
  expect_error(pp, NA)

  pp <- ggplot2::autoplot(curves, "precision", raw_curves = raw_curves, ...)
  expect_error(pp, NA)
}

test_that("autoplot sspoints", {
  if (!ap3_check_libs()) {
    skip("Libraries cannot be loaded")
  }
  pdf(NULL)
  on.exit(dev.off())

  data(P10N10)
  points <- evalmod(mode = "basic", scores = P10N10$scores,
                    labels = P10N10$labels)
  ap3_test_basic_measures(points)
  ap3_test_basic_measures(points, type = "l")
  ap3_test_basic_measures(points, type = "b")
})

test_that("autoplot for multiple sspoints returns grob", {
  if (!ap3_check_libs()) {
    skip("Libraries cannot be loaded")
  }
  pdf(NULL)
  on.exit(dev.off())

  data(P10N10)
  points <- evalmod(mode = "basic", scores = P10N10$scores,
                    labels = P10N10$labels)

  pp <- ggplot2::autoplot(points, ret_grob = TRUE)
  expect_true(is(pp, "grob"))
})

test_that("autoplot mspoints", {
  if (!ap3_check_libs()) {
    skip("Libraries cannot be loaded")
  }
  pdf(NULL)
  on.exit(dev.off())

  points <- ap3_create_mspoints()

  ap3_test_basic_measures(points)
  ap3_test_basic_measures(points, type = "l")
  ap3_test_basic_measures(points, type = "b")
  ap3_test_basic_measures(points, show_legend = TRUE)
})

test_that("autoplot for multiple mspoints returns grob", {
  if (!ap3_check_libs()) {
    skip("Libraries cannot be loaded")
  }
  pdf(NULL)
  on.exit(dev.off())

  points <- ap3_create_mspoints()

  pp <- ggplot2::autoplot(points, show_legend = FALSE, ret_grob = TRUE)
  expect_true(is(pp, "grob"))
})

test_that("autoplot smpoints", {
  if (!ap3_check_libs()) {
    skip("Libraries cannot be loaded")
  }
  pdf(NULL)
  on.exit(dev.off())

  points <- ap3_create_smpoints()

  ap3_test_basic_measures(points)
  ap3_test_basic_measures(points, type = "l")
  ap3_test_basic_measures(points, type = "b")
  ap3_test_basic_measures(points, show_cb = FALSE)

  points2 <- ap3_create_mmpoints(raw_curves = TRUE)
  ap3_test_basic_measures(points2, raw_curves = TRUE)
})

test_that("autoplot for multiple smpoints returns grob", {
  if (!ap3_check_libs()) {
    skip("Libraries cannot be loaded")
  }
  pdf(NULL)
  on.exit(dev.off())

  points <- ap3_create_smpoints()

  pp <- ggplot2::autoplot(points, show_legend = FALSE, ret_grob = TRUE)
  expect_true(is(pp, "grob"))
})

test_that("autoplot mmpoints", {
  if (!ap3_check_libs()) {
    skip("Libraries cannot be loaded")
  }
  pdf(NULL)
  on.exit(dev.off())

  points <- ap3_create_mmpoints()

  ap3_test_basic_measures(points)
  ap3_test_basic_measures(points, type = "l")
  ap3_test_basic_measures(points, type = "b")
  ap3_test_basic_measures(points, show_cb = TRUE)
  ap3_test_basic_measures(points, show_legend = FALSE)

  points2 <- ap3_create_mmpoints(raw_curves = TRUE)
  ap3_test_basic_measures(points2, raw_curves = TRUE)
})


test_that("autoplot for multiple mmpoints returns grob", {
  if (!ap3_check_libs()) {
    skip("Libraries cannot be loaded")
  }
  pdf(NULL)
  on.exit(dev.off())

  points <- ap3_create_mmpoints()

  pp <- ggplot2::autoplot(points, show_legend = FALSE, ret_grob = TRUE)
  expect_true(is(pp, "grob"))
})

test_that("autoplot raw_curve option sspoints", {
  get_args <- function(curves, ...) {
    .get_autoplot_arglist(attr(curves, "args"),
                          def_curvetype = .get_metric_names("basic"),
                          def_type = "p",
                          def_show_cb = FALSE, def_raw_curves = NULL,
                          def_add_np_nn = TRUE,
                          def_show_legend = FALSE,
                          def_ret_grob = FALSE,
                          def_reduce_points = FALSE, ...)
  }

  data(P10N10)
  points1 <- evalmod(mode = "basic", scores = P10N10$scores,
                     labels = P10N10$labels)

  args1a <- get_args(points1, raw_curves = TRUE)
  expect_true(args1a[["raw_curves"]])

  args1b <- get_args(points1, raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- get_args(points1)
  expect_true(args1c[["raw_curves"]])
})

test_that("autoplot raw_curve option mspoints", {
  get_args <- function(curves, ...) {
    .get_autoplot_arglist(attr(curves, "args"),
                          def_curvetype = .get_metric_names("basic"),
                          def_type = "p",
                          def_show_cb = FALSE, def_raw_curves = NULL,
                          def_add_np_nn = TRUE,
                          def_show_legend = TRUE,
                          def_ret_grob = FALSE,
                          def_reduce_points = FALSE, ...)
  }

  points1 <- ap3_create_mspoints()

  args1a <- get_args(points1, raw_curves = TRUE)
  expect_true(args1a[["raw_curves"]])

  args1b <- get_args(points1, raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- get_args(points1)
  expect_true(args1c[["raw_curves"]])
})

test_that("autoplot raw_curve option smpoints", {
  get_args <- function(curves, ...) {
    .get_autoplot_arglist(attr(curves, "args"),
                          def_curvetype = .get_metric_names("basic"),
                          def_type = "p",
                          def_show_cb = TRUE, def_raw_curves = NULL,
                          def_add_np_nn = TRUE,
                          def_show_legend = FALSE,
                          def_ret_grob = FALSE,
                          def_reduce_points = FALSE, ...)
  }

  points1 <- ap3_create_smpoints()

  expect_error(get_args(points1, raw_curves = TRUE), "Invalid raw_curves.")

  args1b <- get_args(points1, raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- get_args(points1)
  expect_false(args1c[["raw_curves"]])

  points2 <- ap3_create_smpoints(raw_curves = TRUE)

  args2a <- get_args(points2, raw_curves = TRUE)
  expect_true(args2a[["raw_curves"]])

  args2b <- get_args(points2, raw_curves = FALSE)
  expect_false(args2b[["raw_curves"]])

  args2c <- get_args(points2)
  expect_true(args2c[["raw_curves"]])
})

test_that("autoplot raw_curve option mmpoints", {
  get_args <- function(curves, ...) {
    .get_autoplot_arglist(attr(curves, "args"),
                          def_curvetype = .get_metric_names("basic"),
                          def_type = "p",
                          def_show_cb = FALSE, def_raw_curves = NULL,
                          def_add_np_nn = TRUE,
                          def_show_legend = TRUE,
                          def_ret_grob = FALSE,
                          def_reduce_points = FALSE, ...)
  }

  points1 <- ap3_create_mmpoints()

  expect_error(get_args(points1, raw_curves = TRUE), "Invalid raw_curves.")

  args1b <- get_args(points1, raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- get_args(points1)
  expect_false(args1c[["raw_curves"]])

  points2 <- ap3_create_mmpoints(raw_curves = TRUE)

  args2a <- get_args(points2, raw_curves = TRUE)
  expect_true(args2a[["raw_curves"]])

  args2b <- get_args(points2, raw_curves = FALSE)
  expect_false(args2b[["raw_curves"]])

  args2c <- get_args(points2)
  expect_true(args2c[["raw_curves"]])
})
