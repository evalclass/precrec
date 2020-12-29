#' @importFrom precrec

context("MM 3: Reformat labels for evaluation")
# Test .factor_labels(labels, posclass)

test_that(".factor_labels() reterns a numeric vector", {
  fmtlbs <- .factor_labels(c(1, 0, 1), NULL)

  expect_true(is.atomic(fmtlbs[["labels"]]))
  expect_true(is.vector(fmtlbs[["labels"]]))
  expect_true(is.numeric(fmtlbs[["labels"]]))
})

test_that(".factor_labels() returns a vector with two values", {
  expect_equal_length <- function(labels, len) {
    fmtlbs <- .factor_labels(c(1, 0, 1), NULL)
    labels <- fmtlbs[["labels"]]
    eval(bquote(expect_equal(length(table(labels)), len)))
  }

  expect_equal_length(c(-1, 1), 2)
  expect_equal_length(c(-1, 0, -1), 2)
})

test_that("'labels' takes a vector", {
  expect_err_msg <- function(err_msg, labels) {
    eval(bquote(expect_error(.factor_labels(labels, NULL), err_msg)))
  }

  err_msg <- "labels is not an atomic vector"
  expect_err_msg(err_msg, list(1))
  expect_err_msg(err_msg, data.frame(1))

  err_msg <- "is not TRUE"
  expect_err_msg(err_msg, array(1))
  expect_err_msg(err_msg, matrix(1))
  expect_err_msg(err_msg, NULL)
})

test_that("'labels' takes a numeric vector", {
  l1 <- c(1, 0, 0, 1)
  labs <- .factor_labels(l1, NULL)
  expect_equal(labs[["labels"]], c(2, 1, 1, 2))

  l2 <- c(0.1, -1, 0.1, 0.1)
  labs <- .factor_labels(l2, NULL)
  expect_equal(labs[["labels"]], c(2, 1, 2, 2))

  labs <- .factor_labels(l2, 0.1)
  expect_equal(labs[["labels"]], c(2, 1, 2, 2))

  labs <- .factor_labels(l2, -1)
  expect_equal(labs[["labels"]], c(1, 2, 1, 1))
})

test_that("'labels' takes an integer vector", {
  l1 <- c(1L, 0L, 0L, 1L)
  labs <- .factor_labels(l1, NULL)
  expect_equal(labs[["labels"]], c(2, 1, 1, 2))

  l2 <- c(0L, -1L, 0L, -1L)
  labs <- .factor_labels(l2, NULL)
  expect_equal(labs[["labels"]], c(2, 1, 2, 1))

  labs <- .factor_labels(l2, 0L)
  expect_equal(labs[["labels"]], c(2, 1, 2, 1))

  labs <- .factor_labels(l2, -1L)
  expect_equal(labs[["labels"]], c(1, 2, 1, 2))

})

test_that("'labels' takes a logical vector", {
  l1 <- c(TRUE, FALSE, FALSE, TRUE)
  labs <- .factor_labels(l1, NULL)
  expect_equal(labs[["labels"]], c(2, 1, 1, 2))

  labs <- .factor_labels(l1, TRUE)
  expect_equal(labs[["labels"]], c(2, 1, 1, 2))

  labs <- .factor_labels(l1, FALSE)
  expect_equal(labs[["labels"]], c(1, 2, 2, 1))

})

test_that("'labels' takes a character vector", {
  l1 <- c("Pos", "Neg", "Pos", "Neg")
  labs <- .factor_labels(l1, NULL)
  expect_equal(labs[["labels"]], c(2, 1, 2, 1))

  labs <- .factor_labels(l1, "Pos")
  expect_equal(labs[["labels"]], c(2, 1, 2, 1))

  labs <- .factor_labels(l1, "Neg")
  expect_equal(labs[["labels"]], c(1, 2, 1, 2))

})

test_that("'labels' takes a factor", {
  l1 <- factor(c(1L, 0L, 0L, 1L))
  labs <- .factor_labels(l1, NULL)
  expect_equal(labs[["labels"]], c(2, 1, 1, 2))

  l2 <- factor(c("P", "N", "P", "P"))
  labs <- .factor_labels(l2, NULL)
  expect_equal(labs[["labels"]], c(2, 1, 2, 2))

  labs <- .factor_labels(l2, "P")
  expect_equal(labs[["labels"]], c(2, 1, 2, 2))

  labs <- .factor_labels(l2, "N")
  expect_equal(labs[["labels"]], c(1, 2, 1, 1))

})

test_that("'labels' takes two unique labels", {
  expect_err_msg <- function(labels) {
    err_msg <- "invalid-labels"
    eval(bquote(expect_error(.factor_labels(labels, NULL), err_msg)))
  }

  expect_err_msg(c(0, 0, 1, 2, 3))
  expect_err_msg(c(-1, 0, -1, 2))
})

test_that(".factor_labels() accepts 'posclass'", {
  l1 <- c(1, 0, 1, 0)

  labs <- .factor_labels(l1, posclass = 0)
  expect_equal(labs[["labels"]], c(1, 2, 1, 2))

  labs <- .factor_labels(l1, posclass = 1)
  expect_equal(labs[["labels"]], c(2, 1, 2, 1))

  expect_err_msg <- function(l1, posclass, err_msg) {
    eval(bquote(expect_error(.factor_labels(l1, posclass = posclass),
                             err_msg)))
  }
  expect_err_msg(l1, -1, "invalid-posclass")

  err_msg <- "posclass must be the same data type as labels"
  expect_err_msg(l1, "0", err_msg)
  expect_err_msg(l1, "1", err_msg)

})
