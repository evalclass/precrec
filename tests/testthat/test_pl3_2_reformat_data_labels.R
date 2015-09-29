context("PL3: Reformat labels for evaluation")
# Test .factor_labels(olabs, olevs)

test_that("'olabs' takes an numeric vector", {
  expect_err_msg <- function(err_msg, olabs) {
    eval(bquote(expect_error(.factor_labels(olabs), err_msg)))
  }

  err_msg <- "olabs is not an atomic vector"
  expect_err_msg(err_msg, list(1))
  expect_err_msg(err_msg, data.frame(1))

  err_msg <- "is not TRUE"
  expect_err_msg(err_msg, c("1", "0"))
  expect_err_msg(err_msg, array(1))
  expect_err_msg(err_msg, matrix(1))
  expect_err_msg(err_msg, NULL)
})

test_that("'olabs' takes two unique labels", {
  expect_err_msg <- function(olabs) {
    err_msg <- "not equal to 2L"
    eval(bquote(expect_error(.factor_labels(olabs), err_msg)))
  }

  expect_err_msg(c(0, 0, 1, 2, 3))
  expect_err_msg(c(-1, 0, -1, 2))
})

test_that("'olevs' takes a character vector or a factor", {
  expect_err_msg <- function(err_msg, olevs) {
    olabs <- c(-1, 1)
    eval(bquote(expect_error(.factor_labels(olabs, olevs = olevs), err_msg)))
  }

  err_msg <- "olevs is not a character vector"
  expect_err_msg(err_msg, c(0, 1))
  expect_err_msg(err_msg, c(TRUE, FALSE))

  err_msg <- "olevs is not an atomic vector"
  expect_err_msg(err_msg, list("1"))
  expect_err_msg(err_msg, data.frame("1"))

  err_msg <- "not equal to 2L"
  expect_err_msg(err_msg, array("1"))
  expect_err_msg(err_msg, matrix("1"))
})

test_that("'olevs' takes two unique labels", {
  expect_err_msg <- function(err_msg, olevs) {
    olabs <- c(0, 1)
    eval(bquote(expect_error(.factor_labels(olabs, olevs = olevs), err_msg)))
  }

  err_msg <- "olevs must cotain two unique labels"
  expect_err_msg(err_msg, c())

  err_msg <- "not equal to 2L"
  expect_err_msg(err_msg, c("negative", "n", "p"))
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
