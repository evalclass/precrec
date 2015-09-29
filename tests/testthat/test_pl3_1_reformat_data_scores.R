context("PL3: Reformat scores for evaluation")
# Test .rank_scores(pscores, na.last, ties.method)

test_that("'pscores' is an numeric vector", {
  expect_err_msg <- function(err_msg, pscores) {
    eval(bquote(expect_error(.rank_scores(pscores), err_msg)))
  }

  err_msg <- "pscores is not a numeric or integer vector"
  expect_err_msg(err_msg, c("1", "0"))

  err_msg <- "pscores is not an atomic vector"
  expect_err_msg(err_msg, list(1))
  expect_err_msg(err_msg, data.frame(1))
  expect_err_msg(err_msg, array(1))
  expect_err_msg(err_msg, matrix(1))
  expect_err_msg(err_msg, factor(1))
  expect_err_msg(err_msg, NULL)
})

test_that("Length of 'pscores' must be >=1", {
  expect_err_msg <- function(pscores) {
    err_msg <- "not greater than 0L"
    eval(bquote(expect_error(.rank_scores(pscores), err_msg)))
  }

  expect_err_msg(as.numeric())
})

test_that("'na.last' should be TRUE or FALSE", {
  expect_err_msg <- function(err_msg, na.last) {
    pscores <- c(1.1, 2.2)
    eval(bquote(expect_error(.rank_scores(pscores, na.last = na.last),
                             err_msg)))
  }

  err_msg <- "na.last contains 1 missing values"
  expect_err_msg(err_msg, NA)

  err_msg <- "na.last is not an atomic vector"
  expect_err_msg(err_msg, list(c(TRUE, FALSE)))
  expect_err_msg(err_msg, data.frame(c(TRUE, FALSE)))

  err_msg <- "na.last is not a flag"
  expect_err_msg(err_msg, "T")
  expect_err_msg(err_msg, array(c(TRUE, FALSE)))
  expect_err_msg(err_msg, matrix(c(TRUE, FALSE)))
  expect_err_msg(err_msg, "keep")
})

test_that("'ties.method' should be one of the three options", {
  expect_err_msg <- function(err_msg, ties.method) {
    scores <- c(1, 2)
    choices <- c("average", "random", "first")
    eval(bquote(expect_error(.rank_scores(scores, ties.method = ties.method),
                             err_msg)))
  }

  err_msg <- "ties.method is not a string"
  expect_err_msg(err_msg, c("average", "first"))

  err_msg <- "ties.method should be one of"
  expect_err_msg(err_msg, c("avg"))
  expect_err_msg(err_msg, c("max"))
  })

test_that("rank_scores() reterns a numeric vector", {
  ranks <- .rank_scores(c(1.0, 0.1, 3.2))

  expect_true(is.atomic(ranks))
  expect_true(is.vector(ranks))
  expect_true(is.numeric(ranks))
})

test_that("rank_scores() reterns a vector with the same length as input", {
  expect_equal_length <- function(pscores) {
    eval(bquote(expect_equal(length(.rank_scores(pscores)), length(pscores))))
  }

  pscores1 <- c(-1.2, 1.0)
  pscores2 <- c(-1.2, 1.0, -1.2)

  expect_equal_length(pscores1)
  expect_equal_length(pscores2)
})

test_that("NAs in 'scores' should be controlled by 'na.last'", {
  expect_equal_ranks <- function(pscores, na.last, ranks) {
    eval(bquote(expect_equal(.rank_scores(pscores, na.last = na.last), ranks)))
  }

  na1_pscores <- c(NA, 0.2, 0.1)
  na2_pscores <- c(0.2, NA, 0.1)
  na3_pscores <- c(0.2, 0.1, NA)

  expect_equal_ranks(na1_pscores, TRUE, c(3, 2, 1))
  expect_equal_ranks(na1_pscores, FALSE, c(1, 3, 2))

  expect_equal_ranks(na2_pscores, TRUE, c(2, 3, 1))
  expect_equal_ranks(na2_pscores, FALSE, c(3, 1, 2))

  expect_equal_ranks(na3_pscores, TRUE, c(2, 1, 3))
  expect_equal_ranks(na3_pscores, FALSE, c(3, 2, 1))
})

test_that("Ties should be controlled by 'ties.method'", {
  expect_equal_ranks <- function(ties.method, ranks) {
    pscores <- c(0.1, 0.2, 0.2, 0.2, 0.3)
    eval(bquote(expect_equal(.rank_scores(pscores, ties.method = ties.method),
                             ranks)))
  }

  expect_equal_ranks("average", c(1, 3, 3, 3, 5))
  expect_equal_ranks("first", c(1, 2, 3, 4, 5))

  pscores2 <- c(0.1, 0.2, 0.2, 0.3)
  r0 <- .rank_scores(pscores2, ties.method = "random")

  r1 <- c(1, 2, 3, 4)
  r2 <- c(1, 3, 2, 4)

  expect_true(any(r0 == r1, r0 == r2))
})

