context("PL1: Join label datasets")
# Test join_labels(..., byrow, chklen)

test_that("'...' must be specified", {
  err_msg <- "No datasets specified"
  expect_error(join_labels(), err_msg)
})

test_that("'byrow' should be TRUE or FALSE", {
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

test_that("All vectors should have the same length", {
  vec1 <- c(1, 0)
  vec2 <- c(0, 1)
  vec3 <- c(1, 1, 0)

  s <- join_labels(vec1, vec2)
  expect_equal(length(s), 2)
  expect_equal(length(s[[1]]), 2)
  expect_equal(length(s[[2]]), 2)

  err_msg <- "All vectors must be of the same size"
  expect_error(join_labels(vec1, vec3), err_msg)
})

test_that("Checking vector lenght is ignore when 'chklen' is set", {
  vec1 <- c(-1, 1)
  vec2 <- c(1, 1, -1)

  err_msg <- "All vectors must be of the same size"
  expect_error(join_labels(vec1, vec2), err_msg)

  s <- join_labels(vec1, vec2, chklen = FALSE)
  expect_equal(length(s), 2)
  expect_equal(length(s[[1]]), 2)
  expect_equal(length(s[[2]]), 3)
})

test_that("join_labels() returns a list", {
  cdat <- join_labels(c(0, 1))

  expect_true(is.list(cdat))
})

test_that("join_labels() only accepts numeric vector or factors", {
  vec1 <- c(1, 2)
  vec2 <- factor(c(1, 2))
  vec3 <- c("0", "1")
  vec4 <- c(TRUE, FALSE)

  l <- join_labels(vec1, vec2)
  expect_equal(is.factor(l[[1]]), FALSE)
  expect_equal(is.vector(l[[2]]), FALSE)

  expect_err_msg <- function(vec1, vec2) {
    err_msg <- "Invalid type of label data"
    eval(bquote(expect_error(join_labels(vec1, vec2), err_msg)))
  }

  expect_err_msg(vec1, vec3)
  expect_err_msg(vec1, vec4)

})

test_that("join_labels() accepts two unique labels", {
  expect_err_msg <- function(vec1, vec2) {
    err_msg <- "The number of unique labels must be 2"
    eval(bquote(expect_error(join_labels(vec1, vec2), err_msg)))
  }

  vec1 <- c(1, 0)
  vec2 <- c(0, 1)

  l <- join_labels(vec1, vec2)
  expect_equal(length(l), 2)
  expect_true(is.vector(l[[1]]))
  expect_true(is.vector(l[[2]]))

  vec1 <- c(1, 2, 1)
  vec2 <- c(1, 2, 3)
  expect_err_msg(vec1, vec2)

  vec1 <- factor(c(1, 2, 1))
  vec2 <- factor(c(1, 2, 3))
  expect_err_msg(vec1, vec2)

})

