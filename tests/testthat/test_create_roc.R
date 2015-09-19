context("Create a ROC curve")
# Test create_roc(arg:evals, arg:x.interval, arg:scores, arg:obslabs)

test_that("arg:evals must be an 'evals' object", {
  expect_err_msg <- function(evals) {
    err_msg <- "An object of unknown class is specified"
    eval(bquote(expect_error(create_roc(evals), err_msg)))
  }

  expect_err_msg(list())
  expect_err_msg(data.frame())
})

test_that("create_roc() directly takes scores and labels", {
  evals <- calc_measures(scores = c(0.1, 0.2, 0.2, 0),
                         obslabs = c(1, 0, 1, 1))
  roc_curve1 <- create_roc(evals)
  roc_curve2 <- create_roc(scores = c(0.1, 0.2, 0.2, 0),
                           obslabs = c(1, 0, 1, 1))

  expect_equal(roc_curve1[["auc"]], roc_curve2[["auc"]])
})

test_that("create_roc() takes arguments for reformat_data", {
  fmdat <- reformat_data(c(0.1, 0.2, 0.2, 0), c(1, 0, 1, 1),
                         ties.method = "first")
  cmats <- create_confmats(fmdat)
  evals <- calc_measures(cmats)
  roc_curve1 <- create_roc(evals)
  roc_curve2 <- create_roc(scores = c(0.1, 0.2, 0.2, 0),
                           obslabs = c(1, 0, 1, 1), ties.method = "first")

  expect_equal(roc_curve1[["auc"]], roc_curve2[["auc"]])

  err_msg <- "Invalid arguments: na.rm"
  expect_error(create_roc(evals, na.rm = TRUE), err_msg)
})

test_that("create_roc() reterns a 'roc_curve' object", {
  roc_curve <- create_roc(scores = c(0.1, 0.2, 0), obslabs = c(1, 0, 1))

  expect_equal(class(roc_curve), "roc_curve")
})

test_that("create_roc() reterns a correct ROC curve", {
  roc_curve <- create_roc(scores = c(0.6, 0.5, 0.4, 0.3, 0.2, 0.1),
                          obslabs = c(0, 1, 0, 1, 0, 1), x_interval = 0.1)

  expect_equal(roc_curve[["pos_num"]], 3)
  expect_equal(roc_curve[["neg_num"]], 3)

  expect_equal(roc_curve[["x"]], c(0, 0.1, 0.2, 0.3, 1/3, 1/3, 0.4, 0.5,
                                   0.6, 2/3, 2/3, 0.7, 0.8, 0.9, 1, 1))
  expect_equal(roc_curve[["y"]], c(0, 0, 0, 0, 0, 1/3, 1/3, 1/3, 1/3,
                                   1/3, 2/3, 2/3, 2/3, 2/3, 2/3, 1))
  expect_equal(roc_curve[["orig_points"]], c(TRUE, FALSE, FALSE, FALSE,
                                             TRUE, TRUE, FALSE, FALSE,
                                             FALSE, TRUE, TRUE, FALSE,
                                             FALSE, FALSE, TRUE, TRUE))
})

test_that("create_roc() reterns a correct ROC AUC", {
  roc_curve <- create_roc(scores = c(0.6, 0.5, 0.4, 0.3, 0.2, 0.1),
                          obslabs = c(0, 1, 0, 1, 0, 1), x_interval = 0.01)

  expect_equal(roc_curve[["auc"]], 1/3, tolerance = 1e-3)
})

