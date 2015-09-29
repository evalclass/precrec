context("PL1: Join score datasets")
# Test join_scores(..., byrow, chklen)

test_that("'...' must be specified", {
  err_msg <- "No datasets specified"
  expect_error(join_scores(), err_msg)
})

test_that("'byrow' should be TRUE or FALSE", {
  expect_err_msg <- function(err_msg, byrow) {
    eval(bquote(expect_error(join_scores(c(0), byrow = byrow), err_msg)))
  }

  err_msg <- "byrow contains 1 missing values"
  expect_err_msg(err_msg, NA)

  err_msg <- "byrow is not a flag"
  expect_err_msg(err_msg, "T")
  expect_err_msg(err_msg, list(c(TRUE, FALSE)))
  expect_err_msg(err_msg, data.frame(c(TRUE, FALSE)))
  expect_err_msg(err_msg, array(c(TRUE, FALSE)))
  expect_err_msg(err_msg, matrix(c(TRUE, FALSE)))
})

test_that("All vectors should have the same length", {
  vec1 <- c(1, 2)
  vec2 <- c(3, 4)
  vec3 <- c(5, 6, 7)

  s <- join_scores(vec1, vec2)
  expect_equal(length(s), 2)
  expect_equal(length(s[[1]]), 2)
  expect_equal(length(s[[2]]), 2)

  err_msg <- "All vectors must be of the same size"
  expect_error(join_scores(vec1, vec3), err_msg)
})

test_that("Checking vector lenght is ignore when 'chklen' is set", {
  vec1 <- c(1, 2)
  vec2 <- c(3, 4, 5)

  err_msg <- "All vectors must be of the same size"
  expect_error(join_scores(vec1, vec2), err_msg)

  s <- join_scores(vec1, vec2, chklen = FALSE)
  expect_equal(length(s), 2)
  expect_equal(length(s[[1]]), 2)
  expect_equal(length(s[[2]]), 3)
})

test_that("join_scores() returns a list", {
  cdat <- join_scores(c(0))

  expect_true(is.list(cdat))
})

test_that("join_scores() only accepts numeric data", {
  vec1 <- c(1, 2)
  vec2 <- c(2, 1)
  vec3 <- c("0", "1")
  vec4 <- c(TRUE, FALSE)

  s <- join_scores(vec1, vec2)
  expect_equal(length(s), 2)

  expect_err_msg <- function(vec1, vec2) {
    err_msg <- "All vectors must be numeric"
    eval(bquote(expect_error(join_scores(vec1, vec2), err_msg)))
  }

  expect_err_msg(vec1, vec3)
  expect_err_msg(vec1, vec4)

})
