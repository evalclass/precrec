context("PL3: Reformat scores for evaluation")
# Test .rank_scores(pscores, na.last, ties.method)

test_that("'pscores' takes an numeric vector", {
  expect_err_msg <- function(pscores) {
    err_msg <- "'pscores' must be a numeric vector"
    eval(bquote(expect_error(.rank_scores(pscores), err_msg)))
  }

  expect_err_msg(c("1", "0"))
  expect_err_msg(list(1))
  expect_err_msg(data.frame(1))
  expect_err_msg(array(1))
  expect_err_msg(matrix(1))
  expect_err_msg(factor(1))
  expect_err_msg(NULL)
})

test_that("Length of 'pscores' must be >=1", {
  expect_err_msg <- function(pscores) {
    err_msg <- "'pscores' must be length >= 1"
    eval(bquote(expect_error(.rank_scores(pscores), err_msg)))
  }

  expect_err_msg(as.numeric())
})

test_that("'na.last' should be TRUE or FALSE", {
  expect_err_msg <- function(na.last) {
    pscores <- c(1.1, 2.2)
    err_msg <- "'na.last' must be either FALSE or TRUE"
    eval(bquote(expect_error(.rank_scores(pscores, na.last = na.last),
                             err_msg)))
  }

  expect_err_msg("T")
  expect_err_msg(NA)
  expect_err_msg(list(c(TRUE, FALSE)))
  expect_err_msg(data.frame(c(TRUE, FALSE)))
  expect_err_msg(array(c(TRUE, FALSE)))
  expect_err_msg(matrix(c(TRUE, FALSE)))
  expect_err_msg("keep")
})

test_that("'ties.method' should be one of the three options", {
  expect_err_msg <- function(ties.method) {
    scores <- c(1, 2)
    choices <- c("average", "random", "first")
    err_msg <- gettextf("'ties.method' should be one of %s",
                        paste(dQuote(choices), collapse = ", "))
    eval(bquote(expect_error(.rank_scores(scores, ties.method = ties.method),
                             err_msg)))
  }

  expect_err_msg(c("average", "first"))
  expect_err_msg(c("avg"))
  expect_err_msg(c("max"))
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

