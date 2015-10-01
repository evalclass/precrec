library(precrec)

context("PL 3: Reformat labels for evaluation")
# Test .factor_labels(labels, levels)

test_that("factor_labels() reterns an ordered factor", {
  fmtlbs <- .factor_labels(c(1, 0, 1))

  expect_true(is.atomic(fmtlbs))
  expect_true(is.factor(fmtlbs))
  expect_true(is.ordered(fmtlbs))
})

test_that("factor_labels() returns a factor with two levels", {
  expect_equal_length <- function(labels, len) {
    eval(bquote(expect_equal(length(levels(.factor_labels(labels))), len)))
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

test_that("'labels' takes an numeric vector", {
  expect_err_msg <- function(err_msg, labels) {
    eval(bquote(expect_error(.factor_labels(labels), err_msg)))
  }

  err_msg <- "labels is not an atomic vector"
  expect_err_msg(err_msg, list(1))
  expect_err_msg(err_msg, data.frame(1))

  err_msg <- "is not TRUE"
  expect_err_msg(err_msg, c("1", "0"))
  expect_err_msg(err_msg, array(1))
  expect_err_msg(err_msg, matrix(1))
  expect_err_msg(err_msg, NULL)
})

test_that("'labels' takes two unique labels", {
  expect_err_msg <- function(labels) {
    err_msg <- "not equal to 2L"
    eval(bquote(expect_error(.factor_labels(labels), err_msg)))
  }

  expect_err_msg(c(0, 0, 1, 2, 3))
  expect_err_msg(c(-1, 0, -1, 2))
})

test_that("'levels' takes a character vector or a factor", {
  expect_err_msg <- function(err_msg, levels) {
    labels <- c(-1, 1)
    eval(bquote(expect_error(.factor_labels(labels, levels = levels), err_msg)))
  }

  err_msg <- "levels is not a character vector"
  expect_err_msg(err_msg, c(0, 1))
  expect_err_msg(err_msg, c(TRUE, FALSE))

  err_msg <- "levels is not an atomic vector"
  expect_err_msg(err_msg, list("1"))
  expect_err_msg(err_msg, data.frame("1"))

  err_msg <- "not equal to 2L"
  expect_err_msg(err_msg, array("1"))
  expect_err_msg(err_msg, matrix("1"))
})

test_that("'levels' takes two unique labels", {
  expect_err_msg <- function(err_msg, levels) {
    labels <- c(0, 1)
    eval(bquote(expect_error(.factor_labels(labels, levels = levels), err_msg)))
  }

  err_msg <- "levels must cotain two unique labels"
  expect_err_msg(err_msg, c())

  err_msg <- "not equal to 2L"
  expect_err_msg(err_msg, c("negative", "n", "p"))
})
