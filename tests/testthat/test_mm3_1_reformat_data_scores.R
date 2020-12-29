#' @importFrom precrec

context("MM 3: Reformat scores for evaluation")
# Test .rank_scores(scores, na_worst, ties_method)

test_that("rank_scores() reterns a numeric vector", {
  ranks <- .rank_scores(c(1.0, 0.1, 3.2))

  expect_true(is.atomic(ranks[["ranks"]]))
  expect_true(is.vector(ranks[["ranks"]]))
  expect_true(is.numeric(ranks[["ranks"]]))
})

test_that("rank_scores() reterns a vector with the same length as input", {
  expect_equal_length <- function(scores) {
    ranks <- .rank_scores(scores)
    eval(bquote(expect_equal(length(ranks[["ranks"]]), length(scores))))
  }

  scores1 <- c(-1.2, 1.0)
  scores2 <- c(-1.2, 1.0, -1.2)

  expect_equal_length(scores1)
  expect_equal_length(scores2)
})

test_that("'scores' is an numeric vector", {
  expect_err_msg <- function(err_msg, scores) {
    eval(bquote(expect_error(.rank_scores(scores), err_msg)))
  }

  err_msg <- "scores is not a numeric or integer vector"
  expect_err_msg(err_msg, c("1", "0"))

  err_msg <- "scores is not an atomic vector"
  expect_err_msg(err_msg, factor(1))
  expect_err_msg(err_msg, list(1))
  expect_err_msg(err_msg, data.frame(1))
  expect_err_msg(err_msg, array(1))
  expect_err_msg(err_msg, matrix(1))
  expect_err_msg(err_msg, NULL)
})

test_that("Length of 'scores' must be >=1", {
  expect_err_msg <- function(scores) {
    err_msg <- "not greater than 0L"
    eval(bquote(expect_error(.rank_scores(scores), err_msg)))
  }

  expect_err_msg(as.numeric())
})

test_that("'na_worst' should be TRUE or FALSE", {
  expect_err_msg <- function(err_msg, na_worst) {
    scores <- c(1.1, 2.2)
    eval(bquote(expect_error(.rank_scores(scores, na_worst = na_worst),
                             err_msg)))
  }

  err_msg <- "na_worst contains 1 missing values"
  expect_err_msg(err_msg, NA)

  err_msg <- "na_worst is not a flag"
  expect_err_msg(err_msg, list(c(TRUE, FALSE)))
  expect_err_msg(err_msg, data.frame(c(TRUE, FALSE)))
  expect_err_msg(err_msg, "T")
  expect_err_msg(err_msg, array(c(TRUE, FALSE)))
  expect_err_msg(err_msg, matrix(c(TRUE, FALSE)))
  expect_err_msg(err_msg, "keep")
})

test_that("'ties_method' should be one of the three options", {
  expect_err_msg <- function(err_msg, ties_method) {
    scores <- c(1, 2)
    eval(bquote(expect_error(.rank_scores(scores, ties_method = ties_method),
                             err_msg)))
  }

  err_msg <- "ties_method is not a string"
  expect_err_msg(err_msg, c("equiv", "first"))

  err_msg <- "ties_method must be one of"
  expect_err_msg(err_msg, "avg")
  expect_err_msg(err_msg, "max")
})

test_that("NAs in 'scores' should be controlled by 'na_worst'", {
  expect_equal_ranks <- function(scores, na_worst, ranks) {
    sranks <- .rank_scores(scores, na_worst = na_worst)
    eval(bquote(expect_equal(sranks[["ranks"]], ranks)))
  }

  na1_scores <- c(NA, 0.2, 0.1)
  na2_scores <- c(0.2, NA, 0.1)
  na3_scores <- c(0.2, 0.1, NA)

  expect_equal_ranks(na1_scores, TRUE, c(3, 1, 2))
  expect_equal_ranks(na1_scores, FALSE, c(1, 2, 3))

  expect_equal_ranks(na2_scores, TRUE, c(1, 3, 2))
  expect_equal_ranks(na2_scores, FALSE, c(2, 1, 3))

  expect_equal_ranks(na3_scores, TRUE, c(1, 2, 3))
  expect_equal_ranks(na3_scores, FALSE, c(2, 3, 1))
})

test_that("Ties should be controlled by 'ties_method'", {
  expect_equal_ranks <- function(ties_method, ranks) {
    scores <- c(0.1, 0.2, 0.2, 0.2, 0.3)
    sranks <- .rank_scores(scores, ties_method = ties_method)
    eval(bquote(expect_equal(sranks[["ranks"]], ranks)))
  }

  expect_equal_ranks("equiv", c(5, 2, 2, 2, 1))
  expect_equal_ranks("first", c(5, 2, 3, 4, 1))

  scores2 <- c(0.1, 0.2, 0.2, 0.3)
  r0 <- .rank_scores(scores2, ties_method = "random")

  r1 <- c(4, 2, 3, 1)
  r2 <- c(4, 3, 2, 1)

  expect_true(any(r0[["ranks"]] == r1, r0[["ranks"]] == r2))
})

