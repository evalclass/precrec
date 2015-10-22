library(precrec)

context("PL 0: Pipeline main")
# Test .make_prefix(model_type, data_type), and
#      pl_main_rocprc(mdat, x_bins)

test_that(".make_prefix() takes 'model_type' and 'data_type'", {
  expect_equal(.make_prefix("single", "single"), "ss")
  expect_equal(.make_prefix("multiple", "single"), "ms")
  expect_equal(.make_prefix("single", "multiple"), "sm")
  expect_equal(.make_prefix("multiple", "multiple"), "mm")

  expect_equal(.make_prefix("sing", "multi"), "")
  expect_equal(.make_prefix("s", "m"), "")
})

test_that("pl_main_rocprc() returns 'sscurves'", {
  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)

  mdat <- mmdata(s1, l1)
  pl <- pl_main_rocprc(mdat)

  expect_true(is(pl, "sscurves"))
})

test_that("pl_main_rocprc() returns 'mscurves'", {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  scores <- join_scores(s1, s2)

  l1 <- c(1, 0, 1, 0)
  l2 <- c(1, 1, 0, 0)
  labels <- join_labels(l1, l2)

  mdat <- mmdata(scores, labels)
  pl <- pl_main_rocprc(mdat)

  expect_true(is(pl, "mscurves"))
})

test_that("pl_main_rocprc() accepts 'x_bins'", {
  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)

  mdat <- mmdata(s1, l1)
  pl <- pl_main_rocprc(mdat, x_bins = 10)

  expect_equal(attr(pl[["rocs"]][[1]], "args")[["x_bins"]], 10)
  expect_equal(attr(pl[["rocs"]][[1]], "args")[["x_bins"]], 10)

  expect_err_msg <- function(err_msg, mdat, x_bins) {
    eval(bquote(expect_error(pl_main_rocprc(mdat, x_bins = x_bins), err_msg)))
  }

  err_msg <- "x_bins is not a number"
  expect_err_msg(err_msg, mdat, c(10, 20))

  err_msg <- "x_bins not greater than or equal to 1L"
  expect_err_msg(err_msg, mdat, 0)
  expect_err_msg(err_msg, mdat, 0.001)

})

test_that("'sscurves' contains 'ssrocs' and 'ssprcs'", {
  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)

  mdat <- mmdata(s1, l1)
  pl <- pl_main_rocprc(mdat)

  expect_equal(length(pl[["rocs"]]), 1)
  expect_true(is(pl[["rocs"]], "crvgrp"))
  expect_true(is(pl[["rocs"]][[1]], "roc_curve"))

  expect_equal(length(pl[["prcs"]]), 1)
  expect_true(is(pl[["prcs"]], "crvgrp"))
  expect_true(is(pl[["prcs"]][[1]], "prc_curve"))
})

test_that("'mscurves' contains 'msrocs' and 'msprcs'", {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  scores <- join_scores(s1, s2)

  l1 <- c(1, 0, 1, 0)
  l2 <- c(1, 1, 0, 0)
  labels <- join_labels(l1, l2)

  mdat <- mmdata(scores, labels)
  pl <- pl_main_rocprc(mdat)

  expect_equal(length(pl[["rocs"]]), 2)
  expect_true(is(pl[["rocs"]], "crvgrp"))
  expect_true(is(pl[["rocs"]][[1]], "roc_curve"))
  expect_true(is(pl[["rocs"]][[2]], "roc_curve"))

  expect_equal(length(pl[["prcs"]]), 2)
  expect_true(is(pl[["prcs"]], "crvgrp"))
  expect_true(is(pl[["prcs"]][[1]], "prc_curve"))
  expect_true(is(pl[["prcs"]][[2]], "prc_curve"))
})
