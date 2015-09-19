context("Create ROC and Precision-Recall curves")
# Test create_curves(arg:evals, arg:x.interval, arg:scores, arg:obslabs)

test_that("arg:evals must be an 'evals' object", {
  expect_err_msg <- function(evals) {
    err_msg <- "An object of unknown class is specified"
    eval(bquote(expect_error(create_curves(evals), err_msg)))
  }

  expect_err_msg(list())
  expect_err_msg(data.frame())
})

test_that("create_curves() directly takes scores and labels", {
  evals <- calc_measures(scores = c(0.1, 0.2, 0.2, 0),
                         obslabs = c(1, 0, 1, 1))
  curves1 <- create_curves(evals)
  curves2 <- create_curves(scores = c(0.1, 0.2, 0.2, 0),
                           obslabs = c(1, 0, 1, 1))

  expect_equal(curves1[["roc"]][["auc"]],
               curves2[["roc"]][["auc"]])
})

test_that("create_curves() takes arguments for reformat_data", {
  fmdat <- reformat_data(c(0.1, 0.2, 0.2, 0), c(1, 0, 1, 1),
                         ties.method = "first")
  cmats <- create_confmats(fmdat)
  evals <- calc_measures(cmats)
  curves1 <- create_curves(evals)
  curves2 <- create_curves(scores = c(0.1, 0.2, 0.2, 0),
                           obslabs = c(1, 0, 1, 1), ties.method = "first")

  expect_equal(curves1[["roc"]][["auc"]],
               curves2[["roc"]][["auc"]])

  err_msg <- "Invalid arguments: na.rm"
  expect_error(create_curves(evals, na.rm = TRUE), err_msg)
})

test_that("create_curves() reterns a 'curves' object", {
  curves <- create_curves(scores = c(0.1, 0.2, 0), obslabs = c(1, 0, 1))

  expect_equal(class(curves), "curves")
})

# test_that("'curves' contains a list with 17 items", {
#   curves <- create_curves(scores = c(0.1, 0.2, 0), obslabs = c(1, 0, 1))
#
#   expect_true(is.list(curves))
#   expect_equal(length(curves), 17)
# })

test_that("create_curves() reterns a correct ROC curve", {
  curves <- create_curves(scores = c(0.6, 0.5, 0.4, 0.3, 0.2, 0.1),
                          obslabs = c(0, 1, 0, 1, 0, 1), x_interval = 0.1)

  expect_equal(curves[["roc"]][["pos_num"]], 3)
  expect_equal(curves[["roc"]][["neg_num"]], 3)

  expect_equal(curves[["roc"]][["x"]], c(0, 0.1, 0.2, 0.3, 1/3, 1/3, 0.4, 0.5,
                                         0.6, 2/3, 2/3, 0.7, 0.8, 0.9, 1, 1))
  expect_equal(curves[["roc"]][["y"]], c(0, 0, 0, 0, 0, 1/3, 1/3, 1/3, 1/3,
                                         1/3, 2/3, 2/3, 2/3, 2/3, 2/3, 1))
  expect_equal(curves[["roc"]][["orig_points"]], c(TRUE, FALSE, FALSE, FALSE,
                                                   TRUE, TRUE, FALSE, FALSE,
                                                   FALSE, TRUE, TRUE, FALSE,
                                                   FALSE, FALSE, TRUE, TRUE))
})

test_that("create_curves() reterns correct a Precision-Recall curve", {
  curves <- create_curves(scores = c(0.6, 0.5, 0.4, 0.3, 0.2, 0.1),
                          obslabs = c(0, 1, 0, 1, 0, 1), x_interval = 0.1)

  expect_equal(curves[["prc"]][["pos_num"]], 3)
  expect_equal(curves[["prc"]][["neg_num"]], 3)

  expect_equal(curves[["prc"]][["x"]], c(0, 0.1, 0.2, 0.3, 1/3, 1/3, 0.4,
                                           0.5, 0.6, 2/3, 2/3, 0.7, 0.8, 0.9,
                                           1))
  expect_equal(curves[["prc"]][["y"]], c(0, 0.230769, 0.375, 0.473684, 0.5,
                                            1/3, 0.375, 0.428571, 0.473684,
                                            0.5, 0.4, 0.411765, 0.444444,
                                            0.473684, 0.5),
               tolerance = 1e-4)
  expect_equal(curves[["prc"]][["orig_points"]], c(TRUE, FALSE, FALSE, FALSE, TRUE,
                                            TRUE, FALSE, FALSE, FALSE, TRUE,
                                            TRUE, FALSE, FALSE, FALSE, TRUE))
})

test_that("create_curves() reterns a correct ROC AUC", {
  curves <- create_curves(scores = c(0.6, 0.5, 0.4, 0.3, 0.2, 0.1),
                          obslabs = c(0, 1, 0, 1, 0, 1), x_interval = 0.01)

  expect_equal(curves[["roc"]][["auc"]], 1/3, tolerance = 1e-3)
})

test_that("create_curves() reterns correct a PRC AUC with 1st point (0, 0)", {
  curves <- create_curves(scores = c(0.6, 0.5, 0.4, 0.3, 0.2, 0.1),
                          obslabs = c(0, 1, 0, 1, 0, 1), x_interval = 0.01)

  expect_equal(curves[["prc"]][["auc"]], 0.395, tolerance = 1e-3)
})

test_that("create_curves() reterns correct a PRC AUC with 1st point (0, 1)", {
  curves <- create_curves(scores = c(0.6, 0.5, 0.4, 0.3, 0.2, 0.1),
                          obslabs = c(1, 1, 0, 1, 0, 0), x_interval = 0.01)

  expect_equal(curves[["prc"]][["auc"]], 0.904, tolerance = 1e-3)
})
