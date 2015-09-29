context("PL3: Reformat input data for evaluation")
# Test reformat_data(scores, labels,
#                    na.last, ties.method, levels, model_name)

test_that("'scores' and 'labels' must be specified", {
  expect_err_msg <- function(scores, labels, err_msg) {
    eval(bquote(expect_error(reformat_data(scores, labels), err_msg)))
  }

  expect_err_msg(NULL, c(0), "Invalid 'scores'")
  expect_err_msg(c(0), NULL, "Invalid 'labels'")
})

test_that("'scores' and 'labels' should be the same length", {
  expect_err_msg <- function(scores, labels) {
    err_msg <- "scores and labels must be of the same length"
    eval(bquote(expect_error(reformat_data(scores, labels), err_msg)))
  }

  expect_err_msg(c(0.1, 0.2), c(1, 0, 0))
  expect_err_msg(c(0.1), c(1, 0))
})

test_that("'model_name' must be a character vector", {
  expect_err_msg <- function(err_msg, model_name) {
    eval(bquote(expect_error(reformat_data(c(0, 1), c(0, 1),
                                           model_name = model_name),
                             err_msg)))
  }

  err_msg <- "model_name is not a string"
  expect_err_msg(err_msg, c(0.1, 0.2))
  expect_err_msg(err_msg, c("1", "2"))
  expect_err_msg(err_msg, as.character())

  err_msg <- "model_name is not an atomic vector"
  expect_err_msg(err_msg, factor(c(0.1, 0.2)))
  expect_err_msg(err_msg, list("1"))
  expect_err_msg(err_msg, data.frame("1"))
  expect_err_msg(err_msg, array("1"))
  expect_err_msg(err_msg, matrix("1"))
})

test_that("reformat_data() reterns a 'fmdat' object", {
  fmdat <- reformat_data(c(0.1, 0.2, 0), c(1, 0, 1))

  expect_equal(class(fmdat), "fmdat")
})

test_that("'fmdat' contains a list with 3 items", {
  fmdat <- reformat_data(c(0.1, 0.2, 0), c(1, 0, 1))

  expect_true(is.list(fmdat))
  expect_equal(length(fmdat), 3)
})

test_that("labels, ranks, and rank_idx must be the same length", {
  fmdat <- reformat_data(c(0.1, 0.2, 0), c(1, 0, 1))

  expect_true(length(fmdat[["labels"]]) != 0)
  expect_equal(length(fmdat[["labels"]]), length(fmdat[["ranks"]]))
  expect_equal(length(fmdat[["labels"]]), length(fmdat[["rank_idx"]]))
})

test_that("reformat_data() accepts 'na.last'", {
  expect_equal_ranks <- function(scores, labels, na.last, ranks) {
    fmdat <- reformat_data(scores, labels, na.last = na.last)
    eval(bquote(expect_equal(fmdat[["ranks"]], ranks)))
  }

  scores <- c(NA, 0.2, 0.1)
  labels <- c(1, 1, 0)

  expect_equal_ranks(scores, labels, TRUE, c(3, 2, 1))
  expect_equal_ranks(scores, labels, FALSE, c(1, 3, 2))

})

test_that("reformat_data() accepts 'ties.method'", {
  expect_equal_ranks <- function(ties.method, ranks) {
    scores <- c(0.1, 0.2, 0.2, 0.2, 0.3)
    labels <- c(1, 0, 1, 0, 1)
    fmdat <- reformat_data(scores, labels, ties.method = ties.method)
    eval(bquote(expect_equal(fmdat[["ranks"]], ranks)))
  }

  expect_equal_ranks("average", c(1, 3, 3, 3, 5))
  expect_equal_ranks("first", c(1, 2, 3, 4, 5))

})

test_that("reformat_data() accepts 'olevs'", {
  olevs <- c("N", "P")
  fmdat <- reformat_data(c(0.1, 0.2, 0), c(1, 0, 1), olevs = olevs)

  expect_equal(levels(fmdat[["labels"]]), olevs)

})
