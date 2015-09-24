context("M2 PL1: Join score datasets")
# Test join_pscores(arg:..., arg:byrow)

test_that("arg:... must be specified", {
  err_msg <- "No datasets specified"
  expect_error(join_pscores(), err_msg)
})

test_that("arg:byrow should be TRUE or FALSE", {
  expect_err_msg <- function(byrow) {
    err_msg <- "'byrow' should be one of FALSE, TRUE"
    eval(bquote(expect_error(join_pscores(c(0), byrow = byrow), err_msg)))
  }

  expect_err_msg("T")
  expect_err_msg(NA)
  expect_err_msg(list(c(TRUE, FALSE)))
  expect_err_msg(data.frame(c(TRUE, FALSE)))
  expect_err_msg(array(c(TRUE, FALSE)))
  expect_err_msg(matrix(c(TRUE, FALSE)))
})

test_that("join_pscores() returns a list", {
  cdat <- join_pscores(c(0))

  expect_true(is.list(cdat))
})

test_that("join_pscores() only accepts numeric data", {
  expect_err_msg <- function(vec1, vec2) {
    err_msg <- "All vectors must be numeric"
    eval(bquote(expect_error(join_pscores(vec1, vec2), err_msg)))
  }

  vec1 <- c(1, 2)

  vec2 <- c("0", "1")
  expect_err_msg(vec1, vec2)

  vec2 <- c(TRUE, FALSE)
  expect_err_msg(vec1, vec2)

})
