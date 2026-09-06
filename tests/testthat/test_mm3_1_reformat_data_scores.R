# MM 3: Reformat scores for evaluation
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
    expect_equal(length(ranks[["ranks"]]), length(scores))
  }

  scores1 <- c(-1.2, 1.0)
  scores2 <- c(-1.2, 1.0, -1.2)

  expect_equal_length(scores1)
  expect_equal_length(scores2)
})

test_that("'scores' is an numeric vector", {
  expect_err_cls <- function(scores) {
    expect_error(
      .rank_scores(scores), "numeric vector",
      class = "precrec_error_invalid_scores"
    )
  }

  expect_err_cls(c("1", "0"))
  expect_err_cls(factor(1))
  expect_err_cls(list(1))
  expect_err_cls(data.frame(1))
  expect_err_cls(array(1))
  expect_err_cls(matrix(1))
  expect_err_cls(NULL)
})

test_that("Length of 'scores' must be >=1", {
  expect_err_msg <- function(scores) {
    expect_error(
      .rank_scores(scores), "must not be empty",
      class = "precrec_error_invalid_scores"
    )
  }

  expect_err_msg(as.numeric())
})

test_that("'na_worst' should be TRUE or FALSE", {
  expect_err_cls <- function(na_worst) {
    scores <- c(1.1, 2.2)
    expect_error(
      .rank_scores(scores, na_worst = na_worst),
      class = "precrec_error_invalid_na_worst"
    )
  }

  expect_err_cls(NA)
  expect_err_cls(list(c(TRUE, FALSE)))
  expect_err_cls(data.frame(c(TRUE, FALSE)))
  expect_err_cls("T")
  expect_err_cls(array(c(TRUE, FALSE)))
  expect_err_cls(matrix(c(TRUE, FALSE)))
  expect_err_cls("keep")
})

test_that("'ties_method' should be one of the three options", {
  expect_err_msg <- function(err_msg, ties_method) {
    scores <- c(1, 2)
    expect_error(
      .rank_scores(scores, ties_method = ties_method), err_msg,
      class = "precrec_error_invalid_ties_method"
    )
  }

  err_msg <- "single string"
  expect_err_msg(err_msg, c("equiv", "first"))

  err_msg <- "must be one of"
  expect_err_msg(err_msg, "avg")
  expect_err_msg(err_msg, "max")
})

test_that("NAs in 'scores' should be controlled by 'na_worst'", {
  expect_equal_ranks <- function(scores, na_worst, ranks) {
    sranks <- .rank_scores(scores, na_worst = na_worst)
    expect_equal(sranks[["ranks"]], ranks)
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

test_that("'na_worst' works when all scores are negative", {
  expect_equal_ranks <- function(scores, na_worst, ranks) {
    sranks <- .rank_scores(scores, na_worst = na_worst)
    expect_equal(sranks[["ranks"]], ranks)
  }

  na_scores <- c(-1, -2, NA, -3, -4)

  expect_equal_ranks(na_scores, TRUE, c(1, 2, 5, 3, 4))
  expect_equal_ranks(na_scores, FALSE, c(2, 3, 1, 4, 5))

  # Ranks must not depend on the sign of the scores
  expect_equal(
    .rank_scores(na_scores, na_worst = TRUE)[["ranks"]],
    .rank_scores(na_scores + 10, na_worst = TRUE)[["ranks"]]
  )
  expect_equal(
    .rank_scores(na_scores, na_worst = FALSE)[["ranks"]],
    .rank_scores(na_scores + 10, na_worst = FALSE)[["ranks"]]
  )
})

test_that("Ties should be controlled by 'ties_method'", {
  expect_equal_ranks <- function(ties_method, ranks) {
    scores <- c(0.1, 0.2, 0.2, 0.2, 0.3)
    sranks <- .rank_scores(scores, ties_method = ties_method)
    expect_equal(sranks[["ranks"]], ranks)
  }

  expect_equal_ranks("equiv", c(5, 2, 2, 2, 1))
  expect_equal_ranks("first", c(5, 2, 3, 4, 1))

  scores2 <- c(0.1, 0.2, 0.2, 0.3)
  r0 <- .rank_scores(scores2, ties_method = "random")

  r1 <- c(4, 2, 3, 1)
  r2 <- c(4, 3, 2, 1)

  expect_true(any(r0[["ranks"]] == r1, r0[["ranks"]] == r2))
})
