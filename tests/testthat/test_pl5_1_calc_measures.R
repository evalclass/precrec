library(precrec)

context("PL 5: Calculate evaluation measures")
# Test calc_measures(cmats, scores, labels)

test_that("calc_measures() reterns an 'pevals' object", {
  pevals1 <- calc_measures(scores = c(0.1, 0.2, 0), labels = c(1, 0, 1))

  data(P10N10)
  fmdat <- reformat_data(P10N10$scores, P10N10$labels)
  cmats <- create_confmats(fmdat)
  pevals2 <- calc_measures(cmats)
  pevals3 <- calc_measures(scores = P10N10$scores, labels = P10N10$labels)

  expect_equal(class(pevals1), "pevals")
  expect_equal(class(pevals2), "pevals")
  expect_equal(class(pevals3), "pevals")
})

test_that("'cmats' must be a 'cmats' object", {
  expect_err_msg <- function(cmats) {
    err_msg <- "Unrecognized class for .validate()"
    eval(bquote(expect_error(calc_measures(cmats), err_msg)))
  }

  expect_err_msg(list())
  expect_err_msg(data.frame())
})

test_that("calc_measures() can directly take scores and labels", {
  cmats <- create_confmats(scores = c(0.1, 0.2, 0.2, 0),
                           labels = c(1, 0, 1, 1))
  pevals1 <- calc_measures(cmats)
  pevals2 <- calc_measures(scores = c(0.1, 0.2, 0.2, 0),
                          labels = c(1, 0, 1, 1))

  expect_equal(pevals1, pevals2)
})

test_that("calc_measures() can take arguments for reformat_data()", {
  err_msg <- "Invalid arguments: na.rm"
  expect_error(calc_measures(scores = c(0.1, 0.2, 0.2, 0),
                             labels = c(1, 0, 1, 1), na.rm = TRUE),
               err_msg)

  pevals <- calc_measures(scores = c(0.1, 0.2, 0),
                          labels = c(1, 0, 1),
                          na.last = TRUE,
                          ties.method = "first")

  expect_equal(.get_obj_arg(pevals, "fmdat", "na.last"), TRUE)
  expect_equal(.get_obj_arg(pevals, "fmdat", "ties.method"), "first")
})


test_that("calc_measures() can take na.last argument", {
  expect_equal_ranks <- function(scores, na.last, ranks) {
    pevals <- calc_measures(scores = scores,
                            labels = c(1, 0, 1),
                            na.last = na.last)

    fmdat <- .get_obj(pevals, "fmdat")

    eval(bquote(expect_equal(.get_obj_arg(pevals, NULL, "na.last"), na.last)))
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
    pevals <- calc_measures(scores = c(0.1, 0.2, 0.2, 0.2, 0.3),
                            labels = c(1, 0, 1, 1, 1),
                            ties.method = ties.method)

    fmdat <- .get_obj(pevals, "fmdat")

    eval(bquote(expect_equal(.get_obj_arg(pevals, NULL, "ties.method"),
                             ties.method)))
    eval(bquote(expect_equal(.get_obj_arg(fmdat, NULL, "ties.method"),
                             ties.method)))
    eval(bquote(expect_equal(fmdat[["ranks"]], ranks)))
  }

  expect_equal_ranks("average", c(1, 3, 3, 3, 5))
  expect_equal_ranks("first", c(1, 2, 3, 4, 5))

})

test_that("'pevals' contains a list with 7 items", {
  pevals <- calc_measures(scores = c(0.1, 0.2, 0), labels = c(1, 0, 1))

  expect_true(is.list(pevals))
  expect_equal(length(pevals), 7)
})

test_that("calc_measures() reterns correct evaluation values", {
  pevals <- calc_measures(scores = c(0.1, 0.2, 0, 0.3),
                          labels = c(1, 0, 0, 1))

  expect_equal(pevals[["pos_num"]], 2)
  expect_equal(pevals[["neg_num"]], 2)

#   "TPs" c(0, 1, 1, 2, 2)
#   "FNs" c(2, 1, 1, 0, 0)
#   "FPs" c(0, 0, 1, 1, 2)
#   "TNs" c(2, 2, 1, 1, 0)
  expect_equal(pevals[["error"]], c(0.5, 0.25, 0.5, 0.25, 0.5))
  expect_equal(pevals[["accuracy"]], c(0.5, 0.75, 0.5, 0.75, 0.5))
  expect_equal(pevals[["specificity"]], c(1, 1, 0.5, 0.5, 0))
  expect_equal(pevals[["sensitivity"]], c(0, 0.5, 0.5, 1, 1))
  expect_equal(pevals[["precision"]], c(1, 1, 0.5, 2/3, 0.5))

})
