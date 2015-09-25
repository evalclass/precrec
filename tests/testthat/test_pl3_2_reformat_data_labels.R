context("M1 PL1: Reformat labels for evaluation")
# Test .factor_labels(args:olabs, args:olevs)

test_that("args:olabs takes an numeric vector", {
  expect_err_msg <- function(olabs) {
    err_msg <- "'olabs' must be either a numeric vector or a factor"
    eval(bquote(expect_error(.factor_labels(olabs), err_msg)))
  }

  expect_err_msg(c("1", "0"))
  expect_err_msg(list(1))
  expect_err_msg(data.frame(1))
  expect_err_msg(array(1))
  expect_err_msg(matrix(1))
  expect_err_msg(NULL)
})

test_that("args:olabs takes one or two unique labels", {
  expect_err_msg <- function(olabs) {
    err_msg <- "'olabs' cotains the invalid number of unique labels"
    eval(bquote(expect_error(.factor_labels(olabs), err_msg)))
  }

  expect_err_msg(c(0, 0, 1, 2, 3))
  expect_err_msg(c(-1, 0, -1, 2))
})

test_that("args:olevs takes a character vector or a factor", {
  expect_err_msg <- function(olevs) {
    olabs <- c(-1, 1)
    err_msg <- "'olevs' must be a charactor vector"
    eval(bquote(expect_error(.factor_labels(olabs, olevs = olevs), err_msg)))
  }

  expect_err_msg(c(0, 1))
  expect_err_msg(list("1"))
  expect_err_msg(data.frame("1"))
  expect_err_msg(array("1"))
  expect_err_msg(matrix("1"))
  expect_err_msg(c(TRUE, FALSE))
})

test_that("args:olevs takes one or two unique labels", {
  expect_err_msg <- function(olevs) {
    olabs <- c(0, 1)
    err_msg <- "'olevs' cotains the invalid number of unique labels"
    eval(bquote(expect_error(.factor_labels(olabs, olevs = olevs), err_msg)))
  }

  expect_err_msg(c())
  expect_err_msg(c("negative", "n", "p"))
})

test_that("factor_labels() reterns an ordered factor", {
  fmtlbs <- .factor_labels(c(1, 0, 1))

  expect_true(is.atomic(fmtlbs))
  expect_true(is.factor(fmtlbs))
  expect_true(is.ordered(fmtlbs))
})

test_that("factor_labels() returns a factor with two levels", {
  expect_equal_length <- function(olabs, len) {
    eval(bquote(expect_equal(length(levels(.factor_labels(olabs))), len)))
  }

  expect_equal_length(c(-1, 1), 2)
  expect_equal_length(c(-1, 0, -1), 2)
})

test_that("factor_labels() returns 'positive' and 'negative' levels", {
  npn_lbs <- .factor_labels(c(-1, 0, -1))
  pnp_lbs <- .factor_labels(c(1, 0, 1))

  pos_neg <- c("negative", "positive")
  expect_equal(levels(npn_lbs), pos_neg)
  expect_equal(levels(pnp_lbs), pos_neg)

  expect_equal(as.character(npn_lbs), c("negative", "positive", "negative"))
  expect_equal(as.character(pnp_lbs), c("positive", "negative", "positive"))

})
