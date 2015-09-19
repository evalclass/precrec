context("Reformat input data for evaluation")
# Test reformat_data(arg:scores, arg:obslabs,
#                    arg:na.last, arg:ties.method, arg:levels, arg:model_name)

test_that("arg:scores and arg:obslabs must be specified", {
  expect_err_msg <- function(scores, obslabs) {
    err_msg <- "'scores' and 'obslabs' must be specified"
    eval(bquote(expect_error(reformat_data(scores, obslabs), err_msg)))
  }

  expect_err_msg(NULL, c(0))
})

test_that("arg:scores and arg:obslabs should be the same length", {
  expect_err_msg <- function(scores, obslabs) {
    err_msg <- "'scores' and 'obslabs' must be the same length"
    eval(bquote(expect_error(reformat_data(scores, obslabs), err_msg)))
  }

  expect_err_msg(c(0.1, 0.2), c(1, 0, 0))
})

test_that("arg:model_name must be a character vector", {
  expect_err_msg <- function(model_name) {
    err_msg <- "'model_name' must be a character vector"
    eval(bquote(expect_error(reformat_data(c(0), c(0),
                                           model_name = model_name),
                             err_msg)))
  }

  expect_err_msg(c(0.1, 0.2))
  expect_err_msg(NULL)
})

test_that("arg:model_name must be a single string", {
  expect_err_msg <- function(model_name) {
    err_msg <- "'model_name' must be a single string"
    eval(bquote(expect_error(reformat_data(c(0), c(0),
                                           model_name = model_name),
                             err_msg)))
  }

  expect_err_msg(c("1", "2"))
})

test_that("reformat_data() reterns a 'fmdat' object", {
  fmdat <- reformat_data(c(0.1, 0.2, 0), c(1, 0, 1))

  expect_equal(class(fmdat), "fmdat")
})

test_that("'fmdat' contains a list with 8 items", {
  fmdat <- reformat_data(c(0.1, 0.2, 0), c(1, 0, 1))

  expect_true(is.list(fmdat))
  expect_equal(length(fmdat), 8)
})

test_that("obslabs, ranks, and rank_idx must be the same length", {
  fmdat <- reformat_data(c(0.1, 0.2, 0), c(1, 0, 1))

  expect_true(length(fmdat[["obslabs"]]) != 0)
  expect_equal(length(fmdat[["obslabs"]]), length(fmdat[["ranks"]]))
  expect_equal(length(fmdat[["obslabs"]]), length(fmdat[["rank_idx"]]))
})
