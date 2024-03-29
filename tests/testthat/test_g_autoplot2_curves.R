#' @importFrom precrec

context("AP 2: Autoplot for curves")
# Test autoplot(object, ...)

skip_on_cran()

test_extra_ap2 <- FALSE

ap2_check_libs1 <- function() {
  if (requireNamespace("ggplot2", quietly = TRUE) &&
    requireNamespace("vdiffr", quietly = TRUE)) {
    TRUE
  } else {
    FALSE
  }
}

ap2_check_libs2 <- function() {
  if (requireNamespace("ggplot2", quietly = TRUE) &&
    requireNamespace("grid", quietly = TRUE) &&
    requireNamespace("gridExtra", quietly = TRUE)) {
    TRUE
  } else {
    FALSE
  }
}


ap2_create_mscurves <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels)
  evalmod(mdat)
}

ap2_create_smcurves <- function(raw_curves = FALSE) {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels, expd_first = "dsids")
  evalmod(mdat, raw_curves = raw_curves)
}

ap2_create_mmcurves <- function(raw_curves = FALSE) {
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
  evalmod(mdat, raw_curves = raw_curves)
}

ap2_test_roc_prc <- function(curves, ptitle, ...) {
  p <- ggplot2::autoplot(curves, ...)
  check_ggplot_fig(ptitle, p)

  if (!test_extra_ap2) {
    return(TRUE)
  }

  pp_roc_prc <- ggplot2::autoplot(curves, c("ROC", "PRC"), ...)
  check_ggplot_fig(paste0(ptitle, "_roc_prc"), pp_roc_prc)

  pp_roc <- ggplot2::autoplot(curves, "ROC", ...)
  check_ggplot_fig(paste0(ptitle, "_roc"), pp_roc)

  pp_prc <- ggplot2::autoplot(curves, "PRC", ...)
  check_ggplot_fig(paste0(ptitle, "_prc"), pp_prc)
}

test_that("autoplot sscurves", {
  if (!ap2_check_libs1()) {
    skip("Libraries cannot be loaded")
  }

  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)
  ap2_test_roc_prc(curves, "autoplot_sscurves")
})

test_that("autoplot for multiple sscurves returns grob", {
  if (!ap2_check_libs2()) {
    skip("Libraries cannot be loaded")
  }

  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)

  pp <- ggplot2::autoplot(curves, multiplot_lib = "grid", ret_grob = TRUE)
  expect_true(is(pp, "grob"))
})

test_that("autoplot mscurves", {
  if (!ap2_check_libs1()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ap2_create_mscurves()

  ap2_test_roc_prc(curves, "autoplot_mscurves")
  ap2_test_roc_prc(curves, "autoplot_mscurves_legend", show_legend = TRUE)
})

test_that("autoplot for multiple mscurves returns grob", {
  if (!ap2_check_libs2()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ap2_create_mscurves()

  pp <- ggplot2::autoplot(curves,
    multiplot_lib = "grid",
    show_legend = FALSE, ret_grob = TRUE
  )
  expect_true(is(pp, "grob"))
})

test_that("autoplot single smcurve", {
  if (!ap2_check_libs1()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ap2_create_smcurves()

  ap2_test_roc_prc(curves, "autoplot_single_smcurve")
  ap2_test_roc_prc(curves, "autoplot_single_smcurve_cb", show_cb = FALSE)

  curves2 <- ap2_create_smcurves(raw_curves = TRUE)
  ap2_test_roc_prc(curves2, "autoplot_single_smcurve_raw_curves",
    raw_curves = TRUE
  )
})

test_that("autoplot for multiple smcurves retruns grob", {
  if (!ap2_check_libs2()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ap2_create_smcurves()

  pp <- ggplot2::autoplot(curves,
    multiplot_lib = "grid",
    show_legend = FALSE, ret_grob = TRUE
  )
  expect_true(is(pp, "grob"))
})

test_that("autoplot mmcurves", {
  if (!ap2_check_libs1()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ap2_create_mmcurves()

  ap2_test_roc_prc(curves, "autoplot_mmcurves")
  ap2_test_roc_prc(curves, "autoplot_mmcurves_cb", show_cb = TRUE)
  ap2_test_roc_prc(curves, "autoplot_mmcurves_legend", show_legend = FALSE)

  curves2 <- ap2_create_smcurves(raw_curves = TRUE)
  ap2_test_roc_prc(curves2, "autoplot_mmcurves_raw_curves", raw_curves = TRUE)
})

test_that("autoplot multiple mmcurves returns grob", {
  if (!ap2_check_libs2()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ap2_create_mmcurves()

  pp <- ggplot2::autoplot(curves,
    multiplot_lib = "grid",
    show_legend = FALSE, ret_grob = TRUE
  )
  expect_true(is(pp, "grob"))
})

test_that("autoplot raw_curve option sscurves", {
  get_args <- function(curves, ...) {
    .get_autoplot_arglist(attr(curves, "args"),
      def_curvetype = c("ROC", "PRC"),
      def_type = "l",
      def_show_cb = FALSE, def_raw_curves = NULL,
      def_add_np_nn = TRUE,
      def_show_legend = FALSE,
      def_ret_grob = FALSE,
      def_reduce_points = TRUE,
      def_multiplot_lib = "patchwork", ...
    )
  }

  data(P10N10)
  curves1 <- evalmod(scores = P10N10$scores, labels = P10N10$labels)

  args1a <- get_args(curves1, raw_curves = TRUE)
  expect_true(args1a[["raw_curves"]])

  args1b <- get_args(curves1, raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- get_args(curves1)
  expect_true(args1c[["raw_curves"]])
})

test_that("autoplot raw_curve option mscurves", {
  get_args <- function(curves, ...) {
    .get_autoplot_arglist(attr(curves, "args"),
      def_curvetype = c("ROC", "PRC"),
      def_type = "l",
      def_show_cb = FALSE, def_raw_curves = NULL,
      def_add_np_nn = TRUE,
      def_show_legend = TRUE,
      def_ret_grob = FALSE,
      def_reduce_points = TRUE,
      def_multiplot_lib = "patchwork", ...
    )
  }

  curves1 <- ap2_create_mscurves()

  args1a <- get_args(curves1, raw_curves = TRUE)
  expect_true(args1a[["raw_curves"]])

  args1b <- get_args(curves1, raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- get_args(curves1)
  expect_true(args1c[["raw_curves"]])
})

test_that("autoplot raw_curve option smcurves", {
  get_args <- function(curves, ...) {
    .get_autoplot_arglist(attr(curves, "args"),
      def_curvetype = c("ROC", "PRC"),
      def_type = "l",
      def_show_cb = TRUE, def_raw_curves = NULL,
      def_add_np_nn = TRUE,
      def_show_legend = FALSE,
      def_ret_grob = FALSE,
      def_reduce_points = TRUE,
      def_multiplot_lib = "patchwork", ...
    )
  }

  curves1 <- ap2_create_smcurves()

  expect_error(get_args(curves1, raw_curves = TRUE), "Invalid raw_curves.")

  args1b <- get_args(curves1, raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- get_args(curves1)
  expect_false(args1c[["raw_curves"]])

  curves2 <- ap2_create_smcurves(raw_curves = TRUE)

  args2a <- get_args(curves2, raw_curves = TRUE)
  expect_true(args2a[["raw_curves"]])

  args2b <- get_args(curves2, raw_curves = FALSE)
  expect_false(args2b[["raw_curves"]])

  args2c <- get_args(curves2)
  expect_true(args2c[["raw_curves"]])
})

test_that("autoplot raw_curve option mmcurves", {
  get_args <- function(curves, ...) {
    .get_autoplot_arglist(attr(curves, "args"),
      def_curvetype = c("ROC", "PRC"),
      def_type = "l",
      def_show_cb = FALSE, def_raw_curves = NULL,
      def_add_np_nn = TRUE,
      def_show_legend = TRUE,
      def_ret_grob = FALSE,
      def_reduce_points = TRUE,
      def_multiplot_lib = "patchwork", ...
    )
  }

  curves1 <- ap2_create_mmcurves()

  expect_error(get_args(curves1, raw_curves = TRUE), "Invalid raw_curves.")

  args1b <- get_args(curves1, raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- get_args(curves1)
  expect_false(args1c[["raw_curves"]])

  curves2 <- ap2_create_mmcurves(raw_curves = TRUE)

  args2a <- get_args(curves2, raw_curves = TRUE)
  expect_true(args2a[["raw_curves"]])

  args2b <- get_args(curves2, raw_curves = FALSE)
  expect_false(args2b[["raw_curves"]])

  args2c <- get_args(curves2)
  expect_true(args2c[["raw_curves"]])
})
