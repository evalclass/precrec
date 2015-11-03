library(precrec)

context("PL 1: Pipeline main for ROC and Precision-Recall")
# Test .make_prefix(model_type, data_type), and
#      .pl_main_rocprc(mdat, x_bins)

pl1_create_mdat_ms <- function() {
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

pl1_create_mdat_sm <- function() {
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

pl1_create_mdat_mm <- function() {
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

test_that(".make_prefix() takes 'model_type' and 'data_type'", {
  expect_equal(.make_prefix("single", "single"), "ss")
  expect_equal(.make_prefix("multiple", "single"), "ms")
  expect_equal(.make_prefix("single", "multiple"), "sm")
  expect_equal(.make_prefix("multiple", "multiple"), "mm")

  expect_equal(.make_prefix("sing", "multi"), "")
  expect_equal(.make_prefix("s", "m"), "")
})

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

  err_msg <- "x_bins not greater than or equal to 1L"
  expect_err_msg(err_msg, mdat, 0)
  expect_err_msg(err_msg, mdat, 0.001)

})

test_that(".pl_main_rocprc() returns 'sscurves'", {
  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)

  mdat <- mmdata(s1, l1)
  pl <- .pl_main_rocprc(mdat, "single", "single", "ss")

  expect_true(is(pl, "sscurves"))
})

test_that(".pl_main_rocprc() returns 'mscurves'", {
  mdat <- pl1_create_mdat_ms()
  pl <- .pl_main_rocprc(mdat, "multiple", "single", "ms")

  expect_true(is(pl, "mscurves"))
})

test_that(".pl_main_rocprc() returns 'smcurves'", {
  mdat <- pl1_create_mdat_sm()
  pl <- .pl_main_rocprc(mdat, "single", "multiple", "sm")

  expect_true(is(pl, "smcurves"))
})

test_that(".pl_main_rocprc() returns 'mmcurves'", {
  mdat <- pl1_create_mdat_mm()
  pl <- .pl_main_rocprc(mdat, "multiple", "multiple", "mm")

  expect_true(is(pl, "mmcurves"))
})

test_that("'sscurves' contains 'ssrocs' and 'ssprcs'", {
  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)

  mdat <- mmdata(s1, l1)
  pl <- .pl_main_rocprc(mdat, "single", "single", "ss")

  expect_equal(length(pl[["rocs"]]), 1)
  expect_true(is(pl[["rocs"]], "crvgrp"))
  expect_true(is(pl[["rocs"]][[1]], "roc_curve"))

  expect_equal(length(pl[["prcs"]]), 1)
  expect_true(is(pl[["prcs"]], "crvgrp"))
  expect_true(is(pl[["prcs"]][[1]], "prc_curve"))
})

test_that("'mscurve' contains 'msrocs' and 'msprcs'", {
  mdat <- pl1_create_mdat_ms()
  pl <- .pl_main_rocprc(mdat, "multiple", "single", "ms")

  expect_equal(length(pl[["rocs"]]), 3)
  expect_true(is(pl[["rocs"]], "crvgrp"))
  expect_true(is(pl[["rocs"]][[1]], "roc_curve"))
  expect_true(is(pl[["rocs"]][[2]], "roc_curve"))
  expect_true(is(pl[["rocs"]][[3]], "roc_curve"))

  expect_equal(length(pl[["prcs"]]), 3)
  expect_true(is(pl[["prcs"]], "crvgrp"))
  expect_true(is(pl[["prcs"]][[1]], "prc_curve"))
  expect_true(is(pl[["prcs"]][[2]], "prc_curve"))
  expect_true(is(pl[["prcs"]][[3]], "prc_curve"))
})

test_that("'smcurve' contains 'msrocs' and 'msprcs'", {
  mdat <- pl1_create_mdat_sm()
  pl <- .pl_main_rocprc(mdat, "single", "multiple", "sm", raw_curves = TRUE)

  expect_equal(length(pl[["rocs"]]), 3)
  expect_true(is(pl[["rocs"]], "crvgrp"))
  expect_true(is(pl[["rocs"]][[1]], "roc_curve"))
  expect_true(is(pl[["rocs"]][[2]], "roc_curve"))
  expect_true(is(pl[["rocs"]][[3]], "roc_curve"))

  expect_equal(length(pl[["prcs"]]), 3)
  expect_true(is(pl[["prcs"]], "crvgrp"))
  expect_true(is(pl[["prcs"]][[1]], "prc_curve"))
  expect_true(is(pl[["prcs"]][[2]], "prc_curve"))
  expect_true(is(pl[["prcs"]][[3]], "prc_curve"))
})
