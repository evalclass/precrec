#' @importFrom precrec

context("PL 2: Pipeline main for ROC and Precision-Recall")
# Test .pl_main_rocprc(mdat, model_type, dataset_type, class_name_pf,
#                      cald_avg, cb_alpha, raw_curves, x_bins)

pl2_create_mdat_ms <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mmdata(scores, labels)
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

  mmdata(scores, labels, expd_first = "dsids")
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

  mmdata(scores, labels, modnames = c("m1", "m2"), dsids = c(1, 2),
         expd_first = "modnames")
}

test_that(".pl_main_rocprc() accepts 'x_bins'", {
  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)

  mdat <- mmdata(s1, l1)
  pl <- .pl_main_rocprc(mdat, "single", "single", "ss", x_bins = 10)

  expect_equal(attr(pl[["rocs"]][[1]], "args")[["x_bins"]], 10)
  expect_equal(attr(pl[["rocs"]][[1]], "args")[["x_bins"]], 10)

  expect_err_msg <- function(err_msg, mdat, x_bins) {
    eval(bquote(expect_error(.pl_main_rocprc(mdat, x_bins = x_bins), err_msg)))
  }

  err_msg <- "x_bins is not a number"
  expect_err_msg(err_msg, mdat, c(10, 20))

  err_msg <- "x_bins%%1 not equal to 0"
  expect_err_msg(err_msg, mdat, 1.5)
  expect_err_msg(err_msg, mdat, 0.001)

  err_msg <- "x_bins not greater than or equal to 1L"
  expect_err_msg(err_msg, mdat, 0)


})

test_that(".pl_main_rocprc() returns 'sscurves'", {
  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)

  mdat <- mmdata(s1, l1)
  pl <- .pl_main_rocprc(mdat, "single", "single", "ss")

  expect_true(is(pl, "sscurves"))
})

test_that(".pl_main_rocprc() returns 'mscurves'", {
  mdat <- pl2_create_mdat_ms()
  pl <- .pl_main_rocprc(mdat, "multiple", "single", "ms")

  expect_true(is(pl, "mscurves"))
})

test_that(".pl_main_rocprc() returns 'smcurves'", {
  mdat <- pl2_create_mdat_sm()
  pl <- .pl_main_rocprc(mdat, "single", "multiple", "sm")

  expect_true(is(pl, "smcurves"))
})

test_that(".pl_main_rocprc() returns 'mmcurves'", {
  mdat <- pl2_create_mdat_mm()
  pl <- .pl_main_rocprc(mdat, "multiple", "multiple", "mm")

  expect_true(is(pl, "mmcurves"))
})

test_that(".pl_main_rocprc() accepts 'calc_avg'", {

  f_check_calc_avg <- function(mdat, mt, dt, pf, val1 = "logical",
                               val2 = "logical") {

    for (ct in c("rocs", "prcs")) {
      pl1 <- .pl_main_rocprc(mdat, mt, dt, pf, calc_avg = TRUE,
                             raw_curves = TRUE)
      expect_equal(typeof(attr(pl1[[ct]], "avgcurves")), val1)

      pl2 <- .pl_main_rocprc(mdat, mt, dt, pf, calc_avg = FALSE,
                             raw_curves = TRUE)
      expect_equal(typeof(attr(pl2[[ct]], "avgcurves")), val2)
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

test_that(".pl_main_rocprc() accepts 'cb_alpha'", {

  f_check_cb_alpha <- function(mdat, mt, dt, pf) {
    for (ct in c("rocs", "prcs")) {
      pl1 <- .pl_main_rocprc(mdat, mt, dt, pf, cb_alpha = 0.05,
                             raw_curves = TRUE)
      expect_equal(attr(attr(pl1[[ct]], "avgcurves"), "cb_zval"), 1.96,
                   tolerance = 1e-2)

      pl2 <- .pl_main_rocprc(mdat, mt, dt, pf, cb_alpha = 0.01,
                             raw_curves = TRUE)
      expect_equal(attr(attr(pl2[[ct]], "avgcurves"), "cb_zval"), 2.575,
                   tolerance = 1e-3)
    }
  }

  mdat1 <- pl2_create_mdat_sm()
  f_check_cb_alpha(mdat1, "single", "multiple", "sm")

  mdat2 <- pl2_create_mdat_mm()
  f_check_cb_alpha(mdat2, "multiple", "multiple", "mm")
})

test_that(".pl_main_rocprc() accepts 'raw_curves'", {

  f_check_raw_curves <- function(mdat, mt, dt, pf, val1 = "list",
                                 val2 = "list") {

    for (ct in c("rocs", "prcs")) {
      pl1 <- .pl_main_rocprc(mdat, mt, dt, pf, raw_curves = FALSE)
      expect_equal(typeof(pl1[[ct]]), val1)

      pl2 <- .pl_main_rocprc(mdat, mt, dt, pf, raw_curves = TRUE)
      expect_equal(typeof(pl2[[ct]]), val2)
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

test_that("curve object contains 'crvgrp', 'roc_curve', 'prc_curve'", {

  f_check_object <- function(mdat, mt, dt, pf, list_len) {
    pl <- .pl_main_rocprc(mdat, mt, dt, pf, raw_curves = TRUE)

    for (ct in c("rocs", "prcs")) {
      expect_equal(length(pl[[ct]]), list_len)
      expect_true(is(pl[[ct]], "crvgrp"))
    }

    for (i in seq_len(length(list_len))) {
      expect_true(is(pl[["rocs"]][[i]], "roc_curve"))
      expect_true(is(pl[["prcs"]][[i]], "prc_curve"))
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
