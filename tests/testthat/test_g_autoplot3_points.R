#' @importFrom precrec

context("AP 3: Autoplot for points")
# Test autoplot(object, ...)

skip_on_cran()

test_extra_ap3 <- FALSE

ap3_check_libs1 <- function() {
  if (requireNamespace("ggplot2", quietly = TRUE) &&
    requireNamespace("vdiffr", quietly = TRUE)) {
    TRUE
  } else {
    FALSE
  }
}

ap3_check_libs2 <- function() {
  if (requireNamespace("ggplot2", quietly = TRUE) &&
    requireNamespace("grid", quietly = TRUE) &&
    requireNamespace("gridExtra", quietly = TRUE)) {
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

  mdat <- mmdata(scores, labels,
    modnames = c("m1", "m2"), dsids = c(1, 2),
    expd_first = "modnames"
  )
  evalmod(mdat, mode = "basic", raw_curves = raw_curves)
}

ap3_test_basic_measures <- function(curves, ptitle, check_def_matrix = FALSE,
                                    raw_curves = FALSE, ...) {
  if (check_def_matrix) {
    p <- ggplot2::autoplot(curves,
      ret_grob = TRUE,
      raw_curves = raw_curves, ...
    )
    check_ggplot_fig(ptitle, p)
  }

  p_precision <- ggplot2::autoplot(curves, "precision",
    raw_curves = raw_curves, ...
  )
  check_ggplot_fig(paste0(ptitle, "_precision"), p_precision)

  if (!test_extra_ap3) {
    return(TRUE)
  }

  check_autoploat <- function(curves, metrics, ptitile2, raw_curves, ...) {
    p_poinsts <- ggplot2::autoplot(curves, metrics,
      ret_grob = TRUE,
      raw_curves = raw_curves, ...
    )
    check_ggplot_fig(paste0(ptitle, "_", ptitile2), p_poinsts)
  }

  metrics8 <- c(
    "sensitivity", "specificity", "error", "accuracy", "precision",
    "mcc", "score", "label"
  )
  check_autoploat(curves, metrics8, "metrics8", raw_curves, ...)

  metrics7 <- c(
    "sensitivity", "specificity", "error", "accuracy", "precision",
    "mcc", "score"
  )
  check_autoploat(curves, metrics7, "metrics7", raw_curves, ...)

  metrics6 <- c(
    "sensitivity", "specificity", "error", "accuracy", "precision",
    "mcc"
  )
  check_autoploat(curves, metrics6, "metrics6", raw_curves, ...)

  metrics5 <- c("sensitivity", "specificity", "error", "accuracy", "precision")
  check_autoploat(curves, metrics5, "metrics5", raw_curves, ...)

  metrics4 <- c("sensitivity", "specificity", "error", "precision")
  check_autoploat(curves, metrics4, "metrics4", raw_curves, ...)

  metrics3 <- c("sensitivity", "specificity", "precision")
  check_autoploat(curves, metrics3, "metrics3", raw_curves, ...)

  metrics2 <- c("sensitivity", "precision")
  check_autoploat(curves, metrics2, "metrics2", raw_curves, ...)
}

test_that("autoplot sspoints", {
  if (!ap3_check_libs1()) {
    skip("Libraries cannot be loaded")
  }

  data(P10N10)
  points <- evalmod(
    mode = "basic", scores = P10N10$scores,
    labels = P10N10$labels
  )
  ap3_test_basic_measures(points, "sspoints", check_def_matrix = TRUE)
  ap3_test_basic_measures(points, "sspoints_l", type = "l")
  ap3_test_basic_measures(points, "sspoints_b", type = "b")
})

test_that("autoplot for multiple sspoints returns grob", {
  if (!ap3_check_libs2()) {
    skip("Libraries cannot be loaded")
  }

  data(P10N10)
  points <- evalmod(
    mode = "basic", scores = P10N10$scores,
    labels = P10N10$labels
  )

  pp <- ggplot2::autoplot(points, multiplot_lib = "grid", ret_grob = TRUE)
  expect_true(is(pp, "grob"))
})

test_that("autoplot mspoints", {
  if (!ap3_check_libs1()) {
    skip("Libraries cannot be loaded")
  }

  points <- ap3_create_mspoints()

  ap3_test_basic_measures(points, "mspoints", check_def_matrix = TRUE)
  ap3_test_basic_measures(points, "mspoints_l", type = "l")
  ap3_test_basic_measures(points, "mspoints_b", type = "b")
  ap3_test_basic_measures(points, "mspoints_legend", show_legend = TRUE)
})

test_that("autoplot for multiple mspoints returns grob", {
  if (!ap3_check_libs2()) {
    skip("Libraries cannot be loaded")
  }

  points <- ap3_create_mspoints()

  pp <- ggplot2::autoplot(points,
    multiplot_lib = "grid",
    show_legend = FALSE, ret_grob = TRUE
  )
  expect_true(is(pp, "grob"))
})

test_that("autoplot smpoints", {
  if (!ap3_check_libs1()) {
    skip("Libraries cannot be loaded")
  }

  points <- ap3_create_smpoints()

  ap3_test_basic_measures(points, "smpoints", check_def_matrix = TRUE)
  ap3_test_basic_measures(points, "smpoints_l", type = "l")
  ap3_test_basic_measures(points, "smpoints_b", type = "b")
  ap3_test_basic_measures(points, "smpoints_cb", show_cb = FALSE)

  points2 <- ap3_create_mmpoints(raw_curves = TRUE)
  ap3_test_basic_measures(points2, "smpoints_raw_curves", raw_curves = TRUE)
})

test_that("autoplot for multiple smpoints returns grob", {
  if (!ap3_check_libs2()) {
    skip("Libraries cannot be loaded")
  }

  points <- ap3_create_smpoints()

  pp <- ggplot2::autoplot(points,
    multiplot_lib = "grid",
    show_legend = TRUE, ret_grob = TRUE
  )
  expect_true(is(pp, "grob"))
})

test_that("autoplot mmpoints", {
  if (!ap3_check_libs1()) {
    skip("Libraries cannot be loaded")
  }

  points <- ap3_create_mmpoints()

  ap3_test_basic_measures(points, "mmpoints", check_def_matrix = TRUE)
  ap3_test_basic_measures(points, "mmpoints_l", ctype = "l")
  ap3_test_basic_measures(points, "mmpoints_b", ctype = "b")
  ap3_test_basic_measures(points, "mmpoints_cb", cshow_cb = TRUE)
  ap3_test_basic_measures(points, "mmpoints_legend", cshow_legend = FALSE)

  points2 <- ap3_create_mmpoints(raw_curves = TRUE)
  ap3_test_basic_measures(points2, "mmpoints_raw_curves", raw_curves = TRUE)
})


test_that("autoplot for multiple mmpoints returns grob", {
  if (!ap3_check_libs2()) {
    skip("Libraries cannot be loaded")
  }

  points <- ap3_create_mmpoints()

  pp <- ggplot2::autoplot(points,
    multiplot_lib = "grid",
    show_legend = FALSE, ret_grob = TRUE
  )
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
      def_reduce_points = FALSE,
      def_multiplot_lib = "patchwork", ...
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

test_that("autoplot raw_curve option mspoints", {
  get_args <- function(curves, ...) {
    .get_autoplot_arglist(attr(curves, "args"),
      def_curvetype = .get_metric_names("basic"),
      def_type = "p",
      def_show_cb = FALSE, def_raw_curves = NULL,
      def_add_np_nn = TRUE,
      def_show_legend = TRUE,
      def_ret_grob = FALSE,
      def_reduce_points = FALSE,
      def_multiplot_lib = "patchwork", ...
    )
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
      def_reduce_points = FALSE,
      def_multiplot_lib = "patchwork", ...
    )
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
      def_reduce_points = FALSE,
      def_multiplot_lib = "patchwork", ...
    )
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
