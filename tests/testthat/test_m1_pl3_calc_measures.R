context("M1 PL3: Calculate evaluation measures")
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
  err_msg <- "Invalid arguments: na.rm"
  expect_error(calc_measures(scores = c(0.1, 0.2, 0.2, 0),
                             obslabs = c(1, 0, 1, 1), na.rm = TRUE),
               err_msg)

  evals <- calc_measures(scores = c(0.1, 0.2, 0),
                         obslabs = c(1, 0, 1),
                         na.last = TRUE,
                         ties.method = "first")

  expect_equal(.get_obj_arg(evals, "fmdat", "na.last"), TRUE)
  expect_equal(.get_obj_arg(evals, "fmdat", "ties.method"), "first")
})


test_that("calc_measures() can take na.last argument", {
  expect_equal_ranks <- function(scores, na.last, ranks) {
    evals <- calc_measures(scores = scores,
                           obslabs = c(1, 0, 1),
                           na.last = na.last)

    fmdat <- .get_obj(evals, "fmdat")

    eval(bquote(expect_equal(.get_obj_arg(evals, NULL, "na.last"), na.last)))
    eval(bquote(expect_equal(.get_obj_arg(fmdat, NULL, "na.last"), na.last)))
    eval(bquote(expect_equal(fmdat[["ranks"]], ranks)))
    eval(bquote(expect_equal(.rank_scores(scores, na.last = na.last), ranks)))
  }

  na1_scores <- c(NA, 0.2, 0.1)
  na2_scores <- c(0.2, NA, 0.1)
  na3_scores <- c(0.2, 0.1, NA)

  expect_equal_ranks(na1_scores, TRUE, c(3, 2, 1))
  expect_equal_ranks(na1_scores, FALSE, c(1, 3, 2))

  expect_equal_ranks(na2_scores, TRUE, c(2, 3, 1))
  expect_equal_ranks(na2_scores, FALSE, c(3, 1, 2))

  expect_equal_ranks(na3_scores, TRUE, c(2, 1, 3))
  expect_equal_ranks(na3_scores, FALSE, c(3, 2, 1))
})

test_that("calc_measures() can take ties.method argument", {

  expect_equal_ranks <- function(ties.method, ranks) {
    evals <- calc_measures(scores = c(0.1, 0.2, 0.2, 0.2, 0.3),
                           obslabs = c(1, 0, 1, 1, 1),
                           ties.method = ties.method)

    fmdat <- .get_obj(evals, "fmdat")

    eval(bquote(expect_equal(.get_obj_arg(evals, NULL, "ties.method"),
                             ties.method)))
    eval(bquote(expect_equal(.get_obj_arg(fmdat, NULL, "ties.method"),
                             ties.method)))
    eval(bquote(expect_equal(fmdat[["ranks"]], ranks)))
  }

  expect_equal_ranks("average", c(1, 3, 3, 3, 5))
  expect_equal_ranks("first", c(1, 2, 3, 4, 5))

})

test_that("calc_measures() reterns an 'evals' object", {
  evals <- calc_measures(scores = c(0.1, 0.2, 0), obslabs = c(1, 0, 1))

  expect_equal(class(evals), "evals")
})

test_that("'evals' contains a list with 7 items", {
  evals <- calc_measures(scores = c(0.1, 0.2, 0), obslabs = c(1, 0, 1))

  expect_true(is.list(evals))
  expect_equal(length(evals), 7)
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
