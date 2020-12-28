#' @importFrom precrec

context("MM 3: Reformat input data for evaluation")
# Test reformat_data(scores, labels,
#                    na_worst, ties_method, modname)

test_that("reformat_data() reterns a 'fmdat' object", {
  fmdat1 <- reformat_data(c(0.1, 0.2, 0), c(1, 0, 1))
  fmdat2 <- reformat_data(c(0.1, 0.2, 0.3), c(0, 1, 1))
  fmdat3 <- reformat_data(c(0.3, 0.1, 0.2), c(-1, -1, 1))

  expect_true(is(fmdat1, "fmdat"))
  expect_true(is(fmdat2, "fmdat"))
  expect_true(is(fmdat3, "fmdat"))
})

test_that("reformat_data() accepts 'mode'", {
  fmdat1 <- reformat_data(c(0.1, 0.2, 0), c(1, 0, 1), mode = "aucroc")
  fmdat2 <- reformat_data(c(0.1, 0.2, 0.3), c(0, 1, 1), mode = "aucroc")
  fmdat3 <- reformat_data(c(0.3, 0.1, 0.2), c(-1, -1, 1), mode = "aucroc")

  expect_true(is(fmdat1, "sdat"))
  expect_true(is(fmdat2, "sdat"))
  expect_true(is(fmdat3, "sdat"))
})

test_that("'scores' and 'labels' must be specified", {
  expect_err_msg <- function(scores, labels, err_msg) {
    eval(bquote(expect_error(reformat_data(scores, labels), err_msg)))
  }

  expect_err_msg(NULL, 0, "Invalid scores")
  expect_err_msg(0, NULL, "Invalid labels")
})

test_that("'scores' and 'labels' should be the same length", {
  expect_err_msg <- function(scores, labels) {
    err_msg <- "scores and labels must be the same lengths"
    eval(bquote(expect_error(reformat_data(scores, labels), err_msg)))
  }

  expect_err_msg(c(0.1, 0.2), c(1, 0, 0))
  expect_err_msg(0.1, c(1, 0))
})

test_that("'modname' must be a character vector", {
  expect_err_msg <- function(err_msg, modname) {
    eval(bquote(expect_error(reformat_data(c(0, 1), c(0, 1),
                                           modname = modname),
                             err_msg)))
  }

  err_msg <- "modname is not a string"
  expect_err_msg(err_msg, c(0.1, 0.2))
  expect_err_msg(err_msg, c("1", "2"))
  expect_err_msg(err_msg, as.character())

  err_msg <- "modname is not an atomic vector"
  err_msg <- "modname is not a string"
  expect_err_msg(err_msg, factor(c(0.1, 0.2)))
  expect_err_msg(err_msg, list("1"))
  expect_err_msg(err_msg, data.frame("1"))
})

test_that("labels, ranks, and rank_idx must be the same length", {
  fmdat <- reformat_data(c(0.1, 0.2, 0), c(1, 0, 1))

  expect_true(length(fmdat[["labels"]]) != 0)
  expect_equal(length(fmdat[["labels"]]), length(fmdat[["ranks"]]))
  expect_equal(length(fmdat[["labels"]]), length(fmdat[["rank_idx"]]))
})

test_that("reformat_data() accepts 'na_worst'", {
  expect_equal_ranks <- function(scores, labels, na_worst, ranks) {
    fmdat <- reformat_data(scores, labels, na_worst = na_worst)
    eval(bquote(expect_equal(fmdat[["ranks"]], ranks)))
  }

  scores <- c(NA, 0.2, 0.1)
  labels <- c(1, 1, 0)

  expect_equal_ranks(scores, labels, TRUE, c(3, 1, 2))
  expect_equal_ranks(scores, labels, FALSE, c(1, 2, 3))

})

test_that("reformat_data() accepts 'ties_method'", {
  expect_equal_ranks <- function(ties_method, ranks) {
    scores <- c(0.1, 0.2, 0.2, 0.2, 0.3)
    labels <- c(1, 0, 1, 0, 1)
    fmdat <- reformat_data(scores, labels, ties_method = ties_method)
    eval(bquote(expect_equal(fmdat[["ranks"]], ranks)))
  }

  expect_equal_ranks("equiv", c(5, 2, 2, 2, 1))
  expect_equal_ranks("first", c(5, 2, 3, 4, 1))

})

test_that("'fmdat' contains a list with 4 items", {
  fmdat <- reformat_data(c(0.1, 0.2, 0), c(1, 0, 1))

  expect_true(is.list(fmdat))
  expect_equal(length(fmdat), 4)
})

