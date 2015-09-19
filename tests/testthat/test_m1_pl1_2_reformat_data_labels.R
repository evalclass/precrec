context("M1 PL1: Reformat labels for evaluation")
# Test .factor_labels(args:obslabs, args:obslevels)

test_that("args:obslabs takes an numeric vector", {
  expect_err_msg <- function(obslbs) {
    err_msg <- "'obslabs' must be either a numeric vector or a factor"
    eval(bquote(expect_error(.factor_labels(obslbs), err_msg)))
  }

  expect_err_msg(c("1", "0"))
  expect_err_msg(list(1))
  expect_err_msg(data.frame(1))
  expect_err_msg(array(1))
  expect_err_msg(matrix(1))
  expect_err_msg(NULL)
})

test_that("args:obslabs takes one or two unique labels", {
  expect_err_msg <- function(obslbs) {
    err_msg <- "'obslabs' cotains the invalid number of unique labels"
    eval(bquote(expect_error(.factor_labels(obslbs), err_msg)))
  }

  expect_err_msg(c(0, 0, 1, 2, 3))
  expect_err_msg(c(-1, 0, -1, 2))
})

test_that("args:obslevels takes a character vector or a factor", {
  expect_err_msg <- function(obslevels) {
    obslbs <- c(-1, 1)
    err_msg <- "'obslevels' must be a charactor vector"
    eval(bquote(expect_error(.factor_labels(obslbs, obslevels = obslevels),
                             err_msg)))
  }

  expect_err_msg(c(0, 1))
  expect_err_msg(list("1"))
  expect_err_msg(data.frame("1"))
  expect_err_msg(array("1"))
  expect_err_msg(matrix("1"))
  expect_err_msg(c(TRUE, FALSE))
})

test_that("args:obslevels takes one or two unique labels", {
  expect_err_msg <- function(obslevels) {
    obslbs <- c(0, 1)
    err_msg <- "'obslevels' cotains the invalid number of unique labels"
    eval(bquote(expect_error(.factor_labels(obslbs, obslevels = obslevels),
                             err_msg)))
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
  expect_equal_length <- function(obslbs, len) {
    eval(bquote(expect_equal(length(levels(.factor_labels(obslbs))), len)))
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
