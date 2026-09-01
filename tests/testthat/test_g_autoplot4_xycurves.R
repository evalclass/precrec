# AP 4: Autoplot for the objects of metric_curve()
# Test autoplot(object, ...)

skip_on_cran()

ap4_check_libs <- function() {
  requireNamespace("ggplot2", quietly = TRUE)
}

ap4_scores <- function() {
  c(0.9, 0.8, 0.75, 0.7, 0.6, 0.55, 0.4, 0.3, 0.2, 0.1)
}

ap4_labels <- function() {
  c(1, 1, 0, 1, 1, 0, 0, 1, 0, 0)
}

ap4_create_ssxycurves <- function(...) {
  metric_curve(scores = ap4_scores(), labels = ap4_labels(), ...)
}

ap4_create_mmxycurves <- function(...) {
  scores <- join_scores(ap4_scores(), rev(ap4_scores()), ap4_scores())
  labels <- join_labels(ap4_labels(), ap4_labels(), ap4_labels())
  mdat <- mmdata(scores, labels,
    modnames = c("m1", "m1", "m2"), dsids = c(1, 2, 1)
  )
  metric_curve(mdat, ...)
}

test_that("autoplot() draws the registered ROC pair as a curve", {
  skip_if_not(ap4_check_libs())

  check_ggplot_fig("xycurves_roc", autoplot(ap4_create_ssxycurves()))
})

test_that("autoplot() draws the registered PRC pair as a curve", {
  skip_if_not(ap4_check_libs())

  check_ggplot_fig("xycurves_prc", autoplot(ap4_create_ssxycurves(
    x_metric = "sensitivity", y_metric = "precision"
  )))
})

test_that("autoplot() draws an unregistered pair as points", {
  skip_if_not(ap4_check_libs())

  check_ggplot_fig("xycurves_points", autoplot(ap4_create_ssxycurves(
    x_metric = "predicted_positive_rate", y_metric = "lift"
  )))
})

test_that("autoplot() joins an unregistered pair when asked to", {
  skip_if_not(ap4_check_libs())

  check_ggplot_fig("xycurves_points_line", autoplot(
    ap4_create_ssxycurves(
      x_metric = "predicted_positive_rate", y_metric = "lift"
    ),
    type = "l"
  ))
})

test_that("autoplot() draws one curve per dataset with a legend", {
  skip_if_not(ap4_check_libs())

  check_ggplot_fig("xycurves_mm", autoplot(ap4_create_mmxycurves()))
})

test_that("an unbounded axis is left for ggplot2 to scale", {
  skip_if_not(ap4_check_libs())

  # The lift has no upper bound, so a fixed 0-to-1 y axis would cut the
  # curve off
  p <- autoplot(ap4_create_ssxycurves(
    x_metric = "predicted_positive_rate", y_metric = "lift"
  ))
  expect_null(p[["coordinates"]][["limits"]][["y"]])

  p2 <- autoplot(ap4_create_ssxycurves())
  expect_equal(p2[["coordinates"]][["limits"]][["y"]], c(0, 1))
})

test_that("the axis labels name the two measures", {
  skip_if_not(ap4_check_libs())

  p <- autoplot(ap4_create_ssxycurves(
    x_metric = "score", y_metric = "false_discovery_rate"
  ))
  expect_equal(gg_labs(p, "x"), "Score")
  expect_equal(gg_labs(p, "y"), "False discovery rate")
})
