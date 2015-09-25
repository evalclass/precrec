context("M2 PL1: Join label datasets")
# Test join_labels(arg:..., arg:byrow)

test_that("arg:... must be specified", {
  err_msg <- "No datasets specified"
  expect_error(join_labels(), err_msg)
})

test_that("arg:byrow should be TRUE or FALSE", {
  expect_err_msg <- function(byrow) {
    err_msg <- "'byrow' should be one of FALSE, TRUE"
    eval(bquote(expect_error(join_labels(c(0), byrow = byrow), err_msg)))
  }

  expect_err_msg("T")
  expect_err_msg(NA)
  expect_err_msg(list(c(TRUE, FALSE)))
  expect_err_msg(data.frame(c(TRUE, FALSE)))
  expect_err_msg(array(c(TRUE, FALSE)))
  expect_err_msg(matrix(c(TRUE, FALSE)))
})

test_that("join_labels() returns a list", {
  cdat <- join_labels(c(0, 1))

  expect_true(is.list(cdat))
})

test_that("join_labels() only accepts numeric vector or factors", {
  expect_err_msg <- function(vec1, vec2) {
    err_msg <- "Invalid type of label data"
    eval(bquote(expect_error(join_labels(vec1, vec2), err_msg)))
  }

  vec1 <- c(1, 2)

  vec2 <- c("0", "1")
  expect_err_msg(vec1, vec2)

  vec2 <- c(TRUE, FALSE)
  expect_err_msg(vec1, vec2)

})

test_that("join_labels() accepts two unique labels", {
  expect_err_msg <- function(vec1, vec2) {
    err_msg <- "Invalid number of labels"
    eval(bquote(expect_error(join_labels(vec1, vec2), err_msg)))
  }

  vec1 <- c(1, 2, 1)
  vec2 <- c(1, 2, 3)
  expect_err_msg(vec1, vec2)

  vec1 <- factor(c(1, 2, 1))
  vec2 <- factor(c(1, 2, 3))
  expect_err_msg(vec1, vec2)

})

