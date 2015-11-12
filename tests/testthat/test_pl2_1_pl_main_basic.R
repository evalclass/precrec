library(precrec)

context("PL 2: Pipeline main for basic evaluation values")
# Test .pl_main_basic(mdat, model_type, dataset_type, class_name_pf,
#                     cald_avg, cb_alpha, raw_curves)

pl2_create_mdat_ms <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels)
}

pl2_create_mdat_sm <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels, expd_first = "dsids")
}

pl2_create_mdat_mm <- function() {
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
}

test_that(".pl_main_basic() returns 'sspoints'", {
  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)

  mdat <- mmdata(s1, l1)
  pl <- .pl_main_basic(mdat, "single", "single", "ss")

  expect_true(is(pl, "sspoints"))
})

test_that(".pl_main_basic() returns 'mspoints'", {
  mdat <- pl2_create_mdat_ms()
  pl <- .pl_main_basic(mdat, "multiple", "single", "ms")

  expect_true(is(pl, "mspoints"))
})

test_that(".pl_main_basic() returns 'smpoints'", {
  mdat <- pl2_create_mdat_sm()
  pl <- .pl_main_basic(mdat, "single", "multiple", "sm")

  expect_true(is(pl, "smpoints"))
})

test_that(".pl_main_basic() returns 'mmpoints'", {
  mdat <- pl2_create_mdat_mm()
  pl <- .pl_main_basic(mdat, "multiple", "multiple", "mm")

  expect_true(is(pl, "mmpoints"))
})

test_that(".pl_main_basic() accepts 'calc_avg'", {

  f_check_calc_avg <- function(mdat, mt, dt, pf, val1 = "logical",
                               val2 = "logical") {

    for (et in c("err", "acc", "sp", "sn", "prec")) {
      pl1 <- .pl_main_basic(mdat, mt, dt, pf, calc_avg = TRUE,
                            raw_curves = TRUE)
      expect_equal(typeof(attr(pl1[[et]], "avgcurves")), val1)

      pl2 <- .pl_main_basic(mdat, mt, dt, pf, calc_avg = FALSE,
                            raw_curves = TRUE)
      expect_equal(typeof(attr(pl2[[et]], "avgcurves")), val2)
    }
  }

  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)
  mdat1 <- mmdata(s1, l1)
  f_check_calc_avg(mdat1, "single", "single", "ss")

  mdat2 <- pl2_create_mdat_ms()
  f_check_calc_avg(mdat2, "multiple", "single", "ms")

  mdat3 <- pl2_create_mdat_sm()
  f_check_calc_avg(mdat3, "single", "multiple", "sm", "list")

  mdat4 <- pl2_create_mdat_mm()
  f_check_calc_avg(mdat4, "multiple", "multiple", "mm", "list")
})

test_that(".pl_main_basic() accepts 'cb_alpha'", {

  f_check_cb_alpha <- function(mdat, mt, dt, pf) {
    for (et in c("err", "acc", "sp", "sn", "prec")) {
      pl1 <- .pl_main_basic(mdat, mt, dt, pf, cb_alpha = 0.05,
                            raw_curves = TRUE)
      expect_equal(attr(attr(pl1[[et]], "avgcurves"), "cb_zval"), 1.96,
                   tolerance = 1e-2)

      pl2 <- .pl_main_basic(mdat, mt, dt, pf, cb_alpha = 0.01,
                            raw_curves = TRUE)
      expect_equal(attr(attr(pl2[[et]], "avgcurves"), "cb_zval"), 2.575,
                   tolerance = 1e-3)
    }
  }

  mdat1 <- pl2_create_mdat_sm()
  f_check_cb_alpha(mdat1, "single", "multiple", "sm")

  mdat2 <- pl2_create_mdat_mm()
  f_check_cb_alpha(mdat2, "multiple", "multiple", "mm")
})

test_that(".pl_main_basic() accepts 'raw_curves'", {

  f_check_raw_curves <- function(mdat, mt, dt, pf, val1 = "list",
                                  val2 = "list") {

    for (et in c("err", "acc", "sp", "sn", "prec")) {
      pl1 <- .pl_main_basic(mdat, mt, dt, pf, raw_curves = FALSE)
      expect_equal(typeof(pl1[[et]]), val1)

      pl2 <- .pl_main_basic(mdat, mt, dt, pf, raw_curves = TRUE)
      expect_equal(typeof(pl2[[et]]), val2)
    }
  }

  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)
  mdat1 <- mmdata(s1, l1)
  f_check_raw_curves(mdat1, "single", "single", "ss")

  mdat2 <- pl2_create_mdat_ms()
  f_check_raw_curves(mdat2, "multiple", "single", "ms")

  mdat3 <- pl2_create_mdat_sm()
  f_check_raw_curves(mdat3, "single", "multiple", "sm", "logical")

  mdat4 <- pl2_create_mdat_mm()
  f_check_raw_curves(mdat4, "multiple", "multiple", "mm", "logical")
})

test_that("point object contains basic measure objects", {

  f_check_object <- function(mdat, mt, dt, pf, list_len) {
    pl <- .pl_main_basic(mdat, mt, dt, pf, raw_curves = TRUE)

    for (et in c("err", "acc", "sp", "sn", "prec")) {
      expect_equal(length(pl[[et]]), list_len)
      expect_true(is(pl[[et]], "pointgrp"))
    }
  }

  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)
  mdat1 <- mmdata(s1, l1)
  f_check_object(mdat1, "single", "single", "ss", 1)

  mdat2 <- pl2_create_mdat_ms()
  f_check_object(mdat2, "multiple", "single", "ms", 3)

  mdat3 <- pl2_create_mdat_sm()
  f_check_object(mdat3, "single", "multiple", "sm", 3)

  mdat4 <- pl2_create_mdat_mm()
  f_check_object(mdat4, "multiple", "multiple", "mm", 4)

})
