#' @importFrom precrec

context("MM 2: Join label datasets")
# Test join_labels(..., byrow, chklen)

test_that("join_labels() combines muliple label datasets", {
  l1 <- c(1, 0, 1, 1)
  l2 <- c(1, 1, 0, 0)
  l3 <- c(0, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  expect_equal(labels[[1]], l1)
  expect_equal(labels[[2]], l2)
  expect_equal(labels[[3]], l3)
})

test_that("'...' must be specified", {
  err_msg <- "No datasets specified"
  expect_error(join_labels(), err_msg)
})

test_that("'byrow' should be TRUE or FALSE", {
  expect_error(join_labels(c(0, 1), byrow = TRUE), NA)
  expect_error(join_labels(c(0, 1), byrow = FALSE), NA)

  expect_err_msg <- function(err_msg, byrow) {
    eval(bquote(expect_error(join_labels(0, byrow = byrow), err_msg)))
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

test_that("'chklen' should be TRUE or FALSE", {
  expect_error(join_labels(c(0, 1), chklen = TRUE), NA)
  expect_error(join_labels(c(0, 1), chklen = FALSE), NA)

  expect_err_msg <- function(err_msg, chklen) {

    eval(bquote(expect_error(join_labels(0, chklen = chklen), err_msg)))
  }

  err_msg <- "chklen contains 1 missing values"
  expect_err_msg(err_msg, NA)

  err_msg <- "chklen is not a flag"
  expect_err_msg(err_msg, "T")
  expect_err_msg(err_msg, list(c(TRUE, FALSE)))
  expect_err_msg(err_msg, data.frame(c(TRUE, FALSE)))
  expect_err_msg(err_msg, array(c(TRUE, FALSE)))
  expect_err_msg(err_msg, matrix(c(TRUE, FALSE)))
})

test_that("All vectors should have the same length", {
  vec1 <- c(1, 0)
  vec2 <- c(0, 1)
  vec3 <- c(1, 1, 0)

  s <- join_labels(vec1, vec2)
  expect_equal(length(s), 2)
  expect_equal(length(s[[1]]), 2)
  expect_equal(length(s[[2]]), 2)

  err_msg <- "All vectors must be the same lengths"
  expect_error(join_labels(vec1, vec3), err_msg)
})

test_that("Checking vector lenghts is ignore by 'chklen'", {
  vec1 <- c(-1, 1)
  vec2 <- c(1, 1, -1)

  err_msg <- "All vectors must be the same lengths"
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

test_that("join_labels() only accepts vectors or factors", {
  vec1 <- c(1, 2)
  vec2 <- factor(c(1, 2))
  vec3 <- c("0", "1")
  vec4 <- c(TRUE, FALSE)

  l <- join_labels(vec1, vec2, vec3, vec4)
  expect_equal(is.numeric(l[[1]]), TRUE)
  expect_equal(is.vector(l[[1]]), TRUE)
  expect_equal(is.factor(l[[1]]), FALSE)

  expect_equal(is.factor(l[[2]]), TRUE)
  expect_equal(is.numeric(l[[2]]), FALSE)
  expect_equal(is.vector(l[[2]]), FALSE)

  expect_equal(is.character(l[[3]]), TRUE)
  expect_equal(is.vector(l[[3]]), TRUE)
  expect_equal(is.numeric(l[[3]]), FALSE)

  expect_equal(is.logical(l[[4]]), TRUE)
  expect_equal(is.vector(l[[4]]), TRUE)
  expect_equal(is.numeric(l[[4]]), FALSE)

  expect_err_msg <- function(vec1, vec2) {
    err_msg <- "Cannot join this type of data"
    eval(bquote(expect_error(join_labels(vec1, vec2), err_msg)))
  }

  vec5 <- c(NULL, NULL)
  expect_err_msg(vec1, vec5)
})

test_that("join_labels() accepts any number of unique labels", {
  expect_err_msg <- function(vec1, vec2) {
    eval(bquote(expect_error(join_labels(vec1, vec2), NA)))
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

