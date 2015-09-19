context("Calculate evaluation measures")
# Test calc_measures(arg:cmats, arg:scores, arg:obslabs)

test_that("arg:cmats must be a 'cmats' object", {
  expect_err_msg <- function(cmats) {
    err_msg <- "An object of unknown class is specified"
    eval(bquote(expect_error(calc_measures(cmats), err_msg)))
  }

  expect_err_msg(list())
  expect_err_msg(data.frame())
})

test_that("calc_measures() can directly take scores and labels", {
  cmats <- create_confmats(scores = c(0.1, 0.2, 0.2, 0),
                           obslabs = c(1, 0, 1, 1))
  evals1 <- calc_measures(cmats)
  evals2 <- calc_measures(scores = c(0.1, 0.2, 0.2, 0),
                          obslabs = c(1, 0, 1, 1))

  expect_equal(evals1, evals2)
})

test_that("calc_measures() can take arguments for reformat_data()", {
  fmdat <- reformat_data(c(0.1, 0.2, 0.2, 0), c(1, 0, 1, 1),
                         ties.method = "first")
  cmats <- create_confmats(fmdat)
  evals1 <- calc_measures(cmats)
  evals2 <- calc_measures(scores = c(0.1, 0.2, 0.2, 0),
                          obslabs = c(1, 0, 1, 1), ties.method = "first")

  expect_equal(evals1, evals2)

  err_msg <- "Invalid arguments: na.rm"
  expect_error(create_confmats(cmats, na.rm = TRUE), err_msg)
})

test_that("calc_measures() reterns an 'evals' object", {
  evals <- calc_measures(scores = c(0.1, 0.2, 0), obslabs = c(1, 0, 1))

  expect_equal(class(evals), "evals")
})

test_that("'evals' contains a list with 9 items", {
  evals <- calc_measures(scores = c(0.1, 0.2, 0), obslabs = c(1, 0, 1))

  expect_true(is.list(evals))
  expect_equal(length(evals), 9)
})

test_that("calc_measures() reterns correct evaluation values", {
  evals <- calc_measures(scores = c(0.1, 0.2, 0, 0.3),
                         obslabs = c(1, 0, 0, 1))

  expect_equal(evals[["pos_num"]], 2)
  expect_equal(evals[["neg_num"]], 2)

#   "TPs" c(0, 1, 1, 2, 2)
#   "FNs" c(2, 1, 1, 0, 0)
#   "FPs" c(0, 0, 1, 1, 2)
#   "TNs" c(2, 2, 1, 1, 0)
  expect_equal(evals[["error"]], c(0.5, 0.25, 0.5, 0.25, 0.5))
  expect_equal(evals[["accuracy"]], c(0.5, 0.75, 0.5, 0.75, 0.5))
  expect_equal(evals[["specificity"]], c(1, 1, 0.5, 0.5, 0))
  expect_equal(evals[["sensitivity"]], c(0, 0.5, 0.5, 1, 1))
  expect_equal(evals[["precision"]], c(1, 1, 0.5, 2/3, 0.5))

})
