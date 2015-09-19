context("M2 PL1: Combine score datasets")
# Test combine_scores(arg:..., arg:byrow, arg:prefix)

test_that("arg:... must be specified", {
  err_msg <- "No datasets specified"
  expect_error(combine_scores(), err_msg)
})

test_that("arg:byrow should be TRUE or FALSE", {
  expect_err_msg <- function(byrow) {
    err_msg <- "'byrow' should be one of FALSE, TRUE"
    eval(bquote(expect_error(combine_scores(c(0), byrow = byrow), err_msg)))
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
    eval(bquote(expect_error(combine_scores(c(0), prefix = prefix),
                             err_msg)))
  }

  expect_err_msg(c("1", "2"))
  expect_err_msg(as.character())
  expect_err_msg(list("1"))
  expect_err_msg(data.frame("1"))
  expect_err_msg(matrix("1"))
})

test_that("combine_scores() returns a list", {
  cdat <- combine_scores(c(0))

  expect_true(is.list(cdat))
})

test_that("combine_scores() only accepts numeric data", {
  expect_err_msg <- function(vec1, vec2) {
    err_msg <- "All vectors must be numeric"
    eval(bquote(expect_error(combine_scores(vec1, vec2), err_msg)))
  }

  vec1 <- c(1, 2)

  vec2 <- c("0", "1")
  expect_err_msg(vec1, vec2)

  vec2 <- c(TRUE, FALSE)
  expect_err_msg(vec1, vec2)

})
