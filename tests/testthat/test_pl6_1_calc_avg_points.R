#' @importFrom precrec

context("PL 6: Calculate average points")
# Test calc_avg_basic(epoints, modnames, uniq_modnames, cb_alpha)

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

pl6_calc_avg_basic <- function(mdat, eval_type = "err", cb_alpha = 0.05) {
  eval_names <- list(err = "error",
                     acc = "accuracy",
                     sp = "specificity",
                     sn = "sensitivity",
                     prec = "precision",
                     mcc = "mcc",
                     fscore = "fscore")

  plfunc <- function(s) {
    cdat <- create_confmats(mdat[[s]])
    pevals <- calc_measures(cdat)
  }
  lpoints <- lapply(seq_along(mdat), plfunc)

  grp_func <- function(s) {
    list(x = lpoints[[s]][["basic"]][["rank"]],
         y = lpoints[[s]][["basic"]][[eval_names[[eval_type]]]])
  }
  pevals <- lapply(seq_along(lpoints), grp_func)

  modnames <- attr(mdat, "data_info")[["modnames"]]
  uniq_modnames <- attr(mdat, "uniq_modnames")

  calc_avg_basic(pevals, modnames, uniq_modnames, cb_alpha)
}

test_that("calc_avg_basic() returns 'avgpoints'", {

  for (et in c("err", "acc", "sp", "sn", "prec", "mcc", "fscore")) {
    mdat1 <- pl6_create_mdat_sm()
    avg1 <- pl6_calc_avg_basic(mdat1, et)
    expect_true(is(avg1, "avgpoints"))

    mdat2 <- pl6_create_mdat_mm()
    avg2 <- pl6_calc_avg_basic(mdat2, et)
    expect_true(is(avg2, "avgpoints"))
  }

})

test_that("sm test data", {

  mdat <- pl6_create_mdat_sm()

  avg_err <- pl6_calc_avg_basic(mdat, "err")
  expect_equal(avg_err[[1]][["x"]], c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(avg_err[[1]][["y_avg"]], c(0.75, 0.5, 0.4166, 0.3333, 0.25),
               tolerance = 1e-3)
  expect_equal(avg_err[[1]][["y_se"]], c(0, 0, 0.1666, 0.1666, 0),
               tolerance = 1e-3)
  expect_equal(avg_err[[1]][["y_ci_h"]], c(0.75, 0.5, 0.7433, 0.66, 0.25),
               tolerance = 1e-3)
  expect_equal(avg_err[[1]][["y_ci_l"]], c(0.75, 0.5, 0.09, 0.0066, 0.25),
               tolerance = 1e-3)

  avg_acc <- pl6_calc_avg_basic(mdat, "acc")
  expect_equal(avg_acc[[1]][["x"]], c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(avg_acc[[1]][["y_avg"]], c(0.25, 0.5, 0.5833, 0.6666, 0.75),
               tolerance = 1e-3)
  expect_equal(avg_acc[[1]][["y_se"]], c(0, 0, 0.1666, 0.1666, 0),
               tolerance = 1e-3)
  expect_equal(avg_acc[[1]][["y_ci_h"]], c(0.25, 0.5, 0.91, 0.9933, 0.75),
               tolerance = 1e-3)
  expect_equal(avg_acc[[1]][["y_ci_l"]], c(0.25, 0.5, 0.2566, 0.34, 0.75),
               tolerance = 1e-3)

  avg_sp <- pl6_calc_avg_basic(mdat, "sp")
  expect_equal(avg_sp[[1]][["x"]], c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(avg_sp[[1]][["y_avg"]], c(1, 1, 0.6666, 0.3333, 0),
               tolerance = 1e-3)
  expect_equal(avg_sp[[1]][["y_se"]], c(0, 0, 0.3333, 0.3333, 0),
               tolerance = 1e-3)
  expect_equal(avg_sp[[1]][["y_ci_h"]], c(1, 1, 1.32, 0.9866, 0),
               tolerance = 1e-3)
  expect_equal(avg_sp[[1]][["y_ci_l"]], c(1, 1, 0.013333, -0.32, 0),
               tolerance = 1e-3)

  avg_sn <- pl6_calc_avg_basic(mdat, "sn")
  expect_equal(avg_sn[[1]][["x"]], c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(avg_sn[[1]][["y_avg"]], c(0, 0.3333, 0.5555, 0.7777, 1),
               tolerance = 1e-3)
  expect_equal(avg_sn[[1]][["y_se"]], c(0, 0, 0.1111, 0.1111, 0),
               tolerance = 1e-3)
  expect_equal(avg_sn[[1]][["y_ci_h"]], c(0, 0.3333, 0.7733, 0.9955, 1),
               tolerance = 1e-3)
  expect_equal(avg_sn[[1]][["y_ci_l"]], c(0, 0.3333, 0.3377, 0.56, 1),
               tolerance = 1e-3)

  avg_prec <- pl6_calc_avg_basic(mdat, "prec")
  expect_equal(avg_prec[[1]][["x"]], c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(avg_prec[[1]][["y_avg"]], c(1, 1, 0.8333, 0.7777, 0.75),
               tolerance = 1e-3)
  expect_equal(avg_prec[[1]][["y_se"]], c(0, 0, 0.1666, 0.1111, 0),
               tolerance = 1e-3)
  expect_equal(avg_prec[[1]][["y_ci_h"]], c(1, 1, 1.15866, 0.9955, 0.75),
               tolerance = 1e-3)
  expect_equal(avg_prec[[1]][["y_ci_l"]], c(1, 1, 0.5066, 0.56, 0.75),
               tolerance = 1e-3)

  avg_prec <- pl6_calc_avg_basic(mdat, "mcc")
  expect_equal(avg_prec[[1]][["x"]], c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(avg_prec[[1]][["y_avg"]], c(NA, 1/3, 0.1924501, 0.1111111, NA),
               tolerance = 1e-3)
  expect_equal(avg_prec[[1]][["y_se"]], c(NA, 0, 0.3849002, 0.4444444, NA),
               tolerance = 1e-3)
  expect_equal(avg_prec[[1]][["y_ci_h"]], c(NA, 1/3, 0.9468406, 0.9822062, NA),
               tolerance = 1e-3)
  expect_equal(avg_prec[[1]][["y_ci_l"]], c(NA, 1/3, -0.5619404, -0.7599839,
                                            NA),
               tolerance = 1e-3)

  avg_fscore <- pl6_calc_avg_basic(mdat, "fscore")
  expect_equal(avg_fscore[[1]][["x"]], c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(avg_fscore[[1]][["y_avg"]], c(0, 0.5, 2/3, 0.7777778, 0.8571429),
               tolerance = 1e-3)
  expect_equal(avg_fscore[[1]][["y_se"]], c(0, 0, 0.1333333, 0.1111111, 0),
               tolerance = 1e-3)
  expect_equal(avg_fscore[[1]][["y_ci_h"]], c(0, 0.5, 0.9279952, 0.9955516,
                                              0.8571429),
               tolerance = 1e-3)
  expect_equal(avg_fscore[[1]][["y_ci_l"]], c(0, 0.5, 0.4053381, 0.560004,
                                              0.8571429),
               tolerance = 1e-3)

})

test_that("mm test data", {

  mdat <- pl6_create_mdat_mm()

  avg_err <- pl6_calc_avg_basic(mdat, "err")
  avg_acc <- pl6_calc_avg_basic(mdat, "acc")
  avg_sp <- pl6_calc_avg_basic(mdat, "sp")
  avg_sn <- pl6_calc_avg_basic(mdat, "sn")
  avg_prec <- pl6_calc_avg_basic(mdat, "prec")
  avg_mcc <- pl6_calc_avg_basic(mdat, "mcc")
  avg_fscore <- pl6_calc_avg_basic(mdat, "fscore")

  expect_equal(avg_err[[1]][["x"]], c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(avg_err[[1]][["y_avg"]], c(0.75, 0.5, 0.5, 0.5, 0.25),
               tolerance = 1e-3)
  expect_equal(avg_err[[1]][["y_se"]], c(0, 0, 0.25, 0, 0),
               tolerance = 1e-3)
  expect_equal(avg_err[[1]][["y_ci_h"]], c(0.75, 0.5, 0.99, 0.5, 0.25),
               tolerance = 1e-3)
  expect_equal(avg_err[[1]][["y_ci_l"]], c(0.75, 0.5, 0.01, 0.5, 0.25),
               tolerance = 1e-3)

  expect_equal(avg_acc[[1]][["x"]], c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(avg_acc[[1]][["y_avg"]], c(0.25, 0.5, 0.5, 0.5, 0.75),
               tolerance = 1e-3)
  expect_equal(avg_acc[[1]][["y_se"]], c(0, 0, 0.25, 0, 0),
               tolerance = 1e-3)
  expect_equal(avg_acc[[1]][["y_ci_h"]], c(0.25, 0.5, 0.99, 0.5, 0.75),
               tolerance = 1e-3)
  expect_equal(avg_acc[[1]][["y_ci_l"]], c(0.25, 0.5, 0.01, 0.5, 0.75),
               tolerance = 1e-3)

  expect_equal(avg_sp[[1]][["x"]], c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(avg_sp[[1]][["y_avg"]], c(1, 1, 0.5, 0, 0),
               tolerance = 1e-3)
  expect_equal(avg_sp[[1]][["y_se"]], c(0, 0, 0.5, 0, 0),
               tolerance = 1e-3)
  expect_equal(avg_sp[[1]][["y_ci_h"]], c(1, 1, 1.48, 0, 0),
               tolerance = 1e-3)
  expect_equal(avg_sp[[1]][["y_ci_l"]], c(1, 1, -0.48, 0, 0),
               tolerance = 1e-3)

  expect_equal(avg_sn[[1]][["x"]], c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(avg_sn[[1]][["y_avg"]], c(0, 0.3333, 0.5, 0.6666, 1),
               tolerance = 1e-3)
  expect_equal(avg_sn[[1]][["y_se"]], c(0, 0, 0.1666, 0, 0),
               tolerance = 1e-3)
  expect_equal(avg_sn[[1]][["y_ci_h"]], c(0, 0.3333, 0.8266, 0.6666, 1),
               tolerance = 1e-3)
  expect_equal(avg_sn[[1]][["y_ci_l"]], c(0, 0.3333, 0.1733, 0.6666, 1),
               tolerance = 1e-3)

  expect_equal(avg_prec[[1]][["x"]], c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(avg_prec[[1]][["y_avg"]], c(1, 1, 0.75, 0.6666, 0.75),
               tolerance = 1e-3)
  expect_equal(avg_prec[[1]][["y_se"]], c(0, 0, 0.25, 0, 0),
               tolerance = 1e-3)
  expect_equal(avg_prec[[1]][["y_ci_h"]], c(1, 1, 1.24, 0.6666, 0.75),
               tolerance = 1e-3)
  expect_equal(avg_prec[[1]][["y_ci_l"]], c(1, 1, 0.26, 0.6666, 0.75),
               tolerance = 1e-3)

  expect_equal(avg_mcc[[1]][["x"]], c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(avg_mcc[[1]][["y_avg"]], c(NA, 1/3, 0, -1/3, NA),
               tolerance = 1e-3)
  expect_equal(avg_mcc[[1]][["y_se"]], c(NA, 0, 0.5773503, 0, NA),
               tolerance = 1e-3)
  expect_equal(avg_mcc[[1]][["y_ci_h"]], c(NA, 1/3, 1.1316, -1/3, NA),
               tolerance = 1e-3)
  expect_equal(avg_mcc[[1]][["y_ci_l"]], c(NA, 1/3, -1.1316, -1/3, NA),
               tolerance = 1e-3)

  expect_equal(avg_fscore[[1]][["x"]], c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(avg_fscore[[1]][["y_avg"]], c(0, 0.5, 0.6, 2/3, 0.8571429),
               tolerance = 1e-3)
  expect_equal(avg_fscore[[1]][["y_se"]], c(0, 0, 0.2, 0, 0),
               tolerance = 1e-3)
  expect_equal(avg_fscore[[1]][["y_ci_h"]], c(0, 0.5, 0.9919928, 2/3,
                                              0.8571429),
               tolerance = 1e-3)
  expect_equal(avg_fscore[[1]][["y_ci_l"]], c(0, 0.5, 0.2080072, 2/3,
                                              0.8571429),
               tolerance = 1e-3)

  expect_equal(avg_err[[2]][["x"]], c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(avg_err[[2]][["y_avg"]], c(0.75, 0.5, 0.5, 0.25, 0.25),
               tolerance = 1e-3)
  expect_equal(avg_err[[2]][["y_se"]], c(0, 0, 0.25, 0.25, 0),
               tolerance = 1e-3)
  expect_equal(avg_err[[2]][["y_ci_h"]], c(0.75, 0.5, 0.99, 0.74, 0.25),
               tolerance = 1e-3)
  expect_equal(avg_err[[2]][["y_ci_l"]], c(0.75, 0.5, 0.01, -0.24, 0.25),
               tolerance = 1e-3)

  expect_equal(avg_acc[[2]][["x"]], c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(avg_acc[[2]][["y_avg"]], c(0.25, 0.5, 0.5, 0.75, 0.75),
               tolerance = 1e-3)
  expect_equal(avg_acc[[2]][["y_se"]], c(0, 0, 0.25, 0.25, 0),
               tolerance = 1e-3)
  expect_equal(avg_acc[[2]][["y_ci_h"]], c(0.25, 0.5, 0.99, 1.24, 0.75),
               tolerance = 1e-3)
  expect_equal(avg_acc[[2]][["y_ci_l"]], c(0.25, 0.5, 0.01, 0.26, 0.75),
               tolerance = 1e-3)

  expect_equal(avg_sp[[2]][["x"]], c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(avg_sp[[2]][["y_avg"]], c(1, 1, 0.5, 0.5, 0),
               tolerance = 1e-3)
  expect_equal(avg_sp[[2]][["y_se"]], c(0, 0, 0.5, 0.5, 0),
               tolerance = 1e-3)
  expect_equal(avg_sp[[2]][["y_ci_h"]], c(1, 1, 1.48, 1.48, 0),
               tolerance = 1e-3)
  expect_equal(avg_sp[[2]][["y_ci_l"]], c(1, 1, -0.48, -0.48, 00),
               tolerance = 1e-3)

  expect_equal(avg_sn[[2]][["x"]], c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(avg_sn[[2]][["y_avg"]], c(0, 0.3333, 0.5, 0.8333, 1),
               tolerance = 1e-3)
  expect_equal(avg_sn[[2]][["y_se"]], c(0, 0, 0.1666, 0.1666, 0),
               tolerance = 1e-3)
  expect_equal(avg_sn[[2]][["y_ci_h"]], c(0, 0.3333, 0.8266, 1.159836, 1),
               tolerance = 1e-3)
  expect_equal(avg_sn[[2]][["y_ci_l"]], c(0, 0.3333, 0.1733, 0.5066, 1),
               tolerance = 1e-3)

  expect_equal(avg_prec[[2]][["x"]], c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(avg_prec[[2]][["y_avg"]], c(1, 1, 0.75, 0.8333, 0.75),
               tolerance = 1e-3)
  expect_equal(avg_prec[[2]][["y_se"]], c(0, 0, 0.25, 0.1666, 0),
               tolerance = 1e-3)
  expect_equal(avg_prec[[2]][["y_ci_h"]], c(1, 1, 1.24, 1.159836, 0.75),
               tolerance = 1e-3)
  expect_equal(avg_prec[[2]][["y_ci_l"]], c(1, 1, 0.26, 0.5066, 0.75),
               tolerance = 1e-3)

  expect_equal(avg_mcc[[2]][["x"]], c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(avg_mcc[[2]][["y_avg"]], c(NA, 1/3, 0, 1/3, NA),
               tolerance = 1e-3)
  expect_equal(avg_mcc[[2]][["y_se"]], c(NA, 0, 0.5773503, 2/3, NA),
               tolerance = 1e-3)
  expect_equal(avg_mcc[[2]][["y_ci_h"]], c(NA, 1/3, 1.1316, 1.64, NA),
               tolerance = 1e-3)
  expect_equal(avg_mcc[[2]][["y_ci_l"]], c(NA, 1/3, -1.1316, -0.9733093, NA),
               tolerance = 1e-3)

  expect_equal(avg_fscore[[2]][["x"]], c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(avg_fscore[[2]][["y_avg"]], c(0, 0.5, 0.6, 0.8333333, 0.8571429),
               tolerance = 1e-3)
  expect_equal(avg_fscore[[2]][["y_se"]], c(0, 0, 0.2, 0.1666667, 0),
               tolerance = 1e-3)
  expect_equal(avg_fscore[[2]][["y_ci_h"]], c(0, 0.5, 0.9919928, 1.16,
                                              0.8571429),
               tolerance = 1e-3)
  expect_equal(avg_fscore[[2]][["y_ci_l"]], c(0, 0.5, 0.2080072, 0.5066727,
                                              0.8571429),
               tolerance = 1e-3)

})
