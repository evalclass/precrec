context("M2 PL1: Combine label datasets")
# Test combine_obslbs(arg:..., arg:byrow, arg:prefix)

test_that("arg:... must be specified", {
  err_msg <- "No datasets specified"
  expect_error(combine_obslbs(), err_msg)
})

test_that("arg:byrow should be TRUE or FALSE", {
  expect_err_msg <- function(byrow) {
    err_msg <- "'byrow' should be one of FALSE, TRUE"
    eval(bquote(expect_error(combine_obslbs(c(0), byrow = byrow), err_msg)))
  }

  expect_err_msg("T")
  expect_err_msg(NA)
  expect_err_msg(list(c(TRUE, FALSE)))
  expect_err_msg(data.frame(c(TRUE, FALSE)))
  expect_err_msg(array(c(TRUE, FALSE)))
  expect_err_msg(matrix(c(TRUE, FALSE)))
})

test_that("arg:prefix must be a charactor vector", {
  expect_err_msg <- function(prefix) {
    err_msg <- "'prefix' must be a charactor vector with length 1"
    eval(bquote(expect_error(combine_obslbs(c(0), prefix = prefix),
                             err_msg)))
  }

  expect_err_msg(c("1", "2"))
  expect_err_msg(as.character())
  expect_err_msg(list("1"))
  expect_err_msg(data.frame("1"))
  expect_err_msg(matrix("1"))
})

test_that("combine_obslbs() returns a list", {
  cdat <- combine_obslbs(c(0, 1))

  expect_true(is.list(cdat))
})

test_that("combine_obslbs() only accepts numeric vector or factors", {
  expect_err_msg <- function(vec1, vec2) {
    err_msg <- "Invalid type of label data"
    eval(bquote(expect_error(combine_obslbs(vec1, vec2), err_msg)))
  }

  vec1 <- c(1, 2)

  vec2 <- c("0", "1")
  expect_err_msg(vec1, vec2)

  vec2 <- c(TRUE, FALSE)
  expect_err_msg(vec1, vec2)

})

test_that("combine_obslbs() accepts two unique labels", {
  expect_err_msg <- function(vec1, vec2) {
    err_msg <- "Invalid number of labels"
    eval(bquote(expect_error(combine_obslbs(vec1, vec2), err_msg)))
  }

  vec1 <- c(1, 2, 1)
  vec2 <- c(1, 2, 3)
  expect_err_msg(vec1, vec2)

  vec1 <- factor(c(1, 2, 1))
  vec2 <- factor(c(1, 2, 3))
  expect_err_msg(vec1, vec2)

})

