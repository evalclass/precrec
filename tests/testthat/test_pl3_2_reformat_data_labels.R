library(precrec)

context("PL 3: Reformat labels for evaluation")
# Test .factor_labels(labels)

test_that("factor_labels() reterns a numeric vector", {
  fmtlbs <- .factor_labels(c(1, 0, 1))

  expect_true(is.atomic(fmtlbs[["labels"]]))
  expect_true(is.vector(fmtlbs[["labels"]]))
  expect_true(is.numeric(fmtlbs[["labels"]]))
})

test_that("factor_labels() returns a factor with two values", {
  expect_equal_length <- function(labels, len) {
    fmtlbs <- .factor_labels(c(1, 0, 1))
    labels <- fmtlbs[["labels"]]
    eval(bquote(expect_equal(length(table(labels)), len)))
  }

  expect_equal_length(c(-1, 1), 2)
  expect_equal_length(c(-1, 0, -1), 2)
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
