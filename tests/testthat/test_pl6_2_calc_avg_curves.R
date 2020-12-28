#' @importFrom precrec

context("PL 6: Calculate average curves")
# Test calc_avg_rocprc(epoints, modnames, uniq_modnames, cb_alpha)

pl6_create_mdat_sm <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mmdata(scores, labels, expd_first = "dsids")
}

pl6_create_mdat_mm <- function() {
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

  mmdata(scores, labels, modnames = c("m1", "m2"), dsids = c(1, 2),
         expd_first = "modnames")
}

pl6_calc_avg_rocprc <- function(mdat, curvetype = "roc", cb_alpha = 0.05,
                                x_bins = 4) {
  plfunc <- function(s) {
    cdat <- create_confmats(mdat[[s]])
    pevals <- calc_measures(cdat)
    curves <- create_curves(pevals, x_bins = x_bins)
  }
  lcurves <- lapply(seq_along(mdat), plfunc)

  mc <- lapply(seq_along(lcurves), function(s) lcurves[[s]][[curvetype]])

  modnames <- attr(mdat, "data_info")[["modnames"]]
  uniq_modnames <- attr(mdat, "uniq_modnames")
  calc_avg_rocprc(mc, modnames, uniq_modnames, cb_alpha, x_bins)
}

test_that("calc_avg_rocprc() returns 'avgcurves'", {

  for (ct in c("roc", "prc")) {
    mdat1 <- pl6_create_mdat_sm()
    avg1 <- pl6_calc_avg_rocprc(mdat1, ct)
    expect_true(is(avg1, "avgcurves"))

    mdat2 <- pl6_create_mdat_mm()
    avg2 <- pl6_calc_avg_rocprc(mdat2, ct)
    expect_true(is(avg2, "avgcurves"))
  }

})

test_that("sm test data", {

  mdat <- pl6_create_mdat_sm()

  avg_roc <- pl6_calc_avg_rocprc(mdat, "roc")
  expect_equal(avg_roc[[1]][["x"]], c(0, 0, 0.25, 0.5, 0.75, 1, 1))
  expect_equal(avg_roc[[1]][["y_avg"]],
               c(0, 0.6666, 0.6666, 0.6666, 0.6666, 0.6666, 1),
               tolerance = 1e-3)
  expect_equal(avg_roc[[1]][["y_se"]],
               c(0, 0.1924, 0.1924, 0.1924, 0.1924, 0.1924, 0),
               tolerance = 1e-3)
  expect_equal(avg_roc[[1]][["y_ci_h"]],
               c(0, 1.043704, 1.043704, 1.043704, 1.043704, 1.043704, 1),
               tolerance = 1e-3)
  expect_equal(avg_roc[[1]][["y_ci_l"]],
               c(0, 0.2894, 0.2894, 0.2894, 0.2894, 0.2894, 1),
               tolerance = 1e-3)

  avg_prc <- pl6_calc_avg_rocprc(mdat, "prc")
  expect_equal(avg_prc[[1]][["x"]], c(0, 0, 0.25, 0.5, 0.75, 1, 1))
  expect_equal(avg_prc[[1]][["y_avg"]],
               c(1, 1, 1, 0.8666, 0.7948, 0.8333, 0.75),
               tolerance = 1e-3)
  expect_equal(avg_prc[[1]][["y_se"]],
               c(0, 0, 0, 0.1333, 0.1025, 0.0833, 0),
               tolerance = 1e-3)
  expect_equal(avg_prc[[1]][["y_ci_h"]],
               c(1, 1, 1, 1.127868, 0.9958, 0.9966, 0.75),
               tolerance = 1e-3)
  expect_equal(avg_prc[[1]][["y_ci_l"]],
               c(1, 1, 1, 0.6053, 0.5938, 0.67, 0.75),
               tolerance = 1e-3)

})

test_that("mm test data", {

  mdat <- pl6_create_mdat_mm()

  avg_roc <- pl6_calc_avg_rocprc(mdat, "roc")
  avg_prc <- pl6_calc_avg_rocprc(mdat, "prc")

  expect_equal(avg_roc[[1]][["x"]], c(0, 0, 0.25, 0.5, 0.75, 1, 1))
  expect_equal(avg_roc[[1]][["y_avg"]],
               c(0, 0.5, 0.5, 0.5, 0.5, 0.5, 1),
               tolerance = 1e-3)
  expect_equal(avg_roc[[1]][["y_se"]],
               c(0, 0.1666, 0.1666, 0.1666, 0.1666, 0.1666, 0),
               tolerance = 1e-3)
  expect_equal(avg_roc[[1]][["y_ci_h"]],
               c(0, 0.8266, 0.8266, 0.8266, 0.8266, 0.8266, 1),
               tolerance = 1e-3)
  expect_equal(avg_roc[[1]][["y_ci_l"]],
               c(0, 0.1733, 0.1733, 0.1733, 0.1733, 0.1733, 1),
               tolerance = 1e-3)

  expect_equal(avg_prc[[1]][["x"]], c(0, 0, 0.25, 0.5, 0.75, 1, 1))
  expect_equal(avg_prc[[1]][["y_avg"]], c(1, 1, 1, 0.8, 0.6923, 0.75, 0.75),
               tolerance = 1e-3)
  expect_equal(avg_prc[[1]][["y_se"]], c(0, 0, 0, 0.2, 0, 0, 0),
               tolerance = 1e-3)
  expect_equal(avg_prc[[1]][["y_ci_h"]], c(1, 1, 1, 1.192, 0.6923, 0.75, 0.75),
               tolerance = 1e-3)
  expect_equal(avg_prc[[1]][["y_ci_l"]], c(1, 1, 1, 0.408, 0.6923, 0.75, 0.75),
               tolerance = 1e-3)

  expect_equal(avg_roc[[2]][["x"]], c(0, 0, 0.25, 0.5, 0.75, 1, 1))
  expect_equal(avg_roc[[2]][["y_avg"]],
               c(0, 0.6666, 0.6666, 0.6666, 0.6666, 0.6666, 1),
               tolerance = 1e-3)
  expect_equal(avg_roc[[2]][["y_se"]],
               c(0, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0),
               tolerance = 1e-3)
  expect_equal(avg_roc[[2]][["y_ci_h"]], c(0, 1.319868, 1.319868, 1.319868,
                                           1.319868, 1.319868, 1),
               tolerance = 1e-3)
  expect_equal(avg_roc[[2]][["y_ci_l"]],
               c(0, 0.013333, 0.013333, 0.013333, 0.013333, 0.013333, 1),
               tolerance = 1e-3)

  expect_equal(avg_prc[[2]][["x"]], c(0, 0, 0.25, 0.5, 0.75, 1, 1))
  expect_equal(avg_prc[[2]][["y_avg"]], c(1, 1, 1, 0.8, 0.8461, 0.875, 0.75),
               tolerance = 1e-3)
  expect_equal(avg_prc[[2]][["y_se"]], c(0, 0, 0, 0.2, 0.1538, 0.125, 0),
               tolerance = 1e-3)
  expect_equal(avg_prc[[2]][["y_ci_h"]], c(1, 1, 1, 1.192, 1.147548, 1.12,
                                           0.75),
               tolerance = 1e-3)
  expect_equal(avg_prc[[2]][["y_ci_l"]], c(1, 1, 1, 0.408, 0.5446, 0.63, 0.75),
               tolerance = 1e-3)

})
