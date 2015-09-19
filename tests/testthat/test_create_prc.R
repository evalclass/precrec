context("Create a Precision-Recall curve")
# Test create_prc(arg:evals, arg:x.interval, arg:scores, arg:obslabs)

test_that("arg:evals must be an 'evals' object", {
  expect_err_msg <- function(evals) {
    err_msg <- "An object of unknown class is specified"
    eval(bquote(expect_error(create_prc(evals), err_msg)))
  }

  expect_err_msg(list())
  expect_err_msg(data.frame())
})

test_that("create_prc() directly takes scores and labels", {
  evals <- calc_measures(scores = c(0.1, 0.2, 0.2, 0),
                         obslabs = c(1, 0, 1, 1))
  prc_curve1 <- create_prc(evals)
  prc_curve2 <- create_prc(scores = c(0.1, 0.2, 0.2, 0),
                        obslabs = c(1, 0, 1, 1))

  expect_equal(prc_curve1[["auc"]], prc_curve2[["auc"]])
})

test_that("create_prc() takes arguments for reformat_data", {
  fmdat <- reformat_data(c(0.1, 0.2, 0.2, 0), c(1, 0, 1, 1),
                         ties.method = "first")
  cmats <- create_confmats(fmdat)
  evals <- calc_measures(cmats)
  prc_curve1 <- create_prc(evals)
  prc_curve2 <- create_prc(scores = c(0.1, 0.2, 0.2, 0),
                           obslabs = c(1, 0, 1, 1), ties.method = "first")

  expect_equal(prc_curve1[["auc"]], prc_curve2[["auc"]])

  err_msg <- "Invalid arguments: na.rm"
  expect_error(create_curves(evals, na.rm = TRUE), err_msg)
})

test_that("create_prc() reterns a 'prc_curve' object", {
  prc_curve <- create_prc(scores = c(0.1, 0.2, 0), obslabs = c(1, 0, 1))

  expect_equal(class(prc_curve), "prc_curve")
})

test_that("create_prc() reterns a correct Precision-Recall curve", {
  prc_curve <- create_prc(scores = c(0.6, 0.5, 0.4, 0.3, 0.2, 0.1),
                          obslabs = c(0, 1, 0, 1, 0, 1), x_interval = 0.1)

  expect_equal(prc_curve[["pos_num"]], 3)
  expect_equal(prc_curve[["neg_num"]], 3)

  expect_equal(prc_curve[["x"]], c(0, 0.1, 0.2, 0.3, 1/3, 1/3, 0.4,
                                   0.5, 0.6, 2/3, 2/3, 0.7, 0.8, 0.9, 1))
  expect_equal(prc_curve[["y"]], c(0, 0.230769, 0.375, 0.473684, 0.5,
                                   1/3, 0.375, 0.428571, 0.473684,
                                   0.5, 0.4, 0.411765, 0.444444,
                                   0.473684, 0.5),
               tolerance = 1e-4)
  expect_equal(prc_curve[["orig_points"]], c(TRUE, FALSE, FALSE, FALSE, TRUE,
                                             TRUE, FALSE, FALSE, FALSE, TRUE,
                                             TRUE, FALSE, FALSE, FALSE, TRUE))
})

test_that("create_prc() reterns correct a PRC AUC with 1st point (0, 0)", {
  prc_curve <- create_prc(scores = c(0.6, 0.5, 0.4, 0.3, 0.2, 0.1),
                          obslabs = c(0, 1, 0, 1, 0, 1), x_interval = 0.01)

  expect_equal(prc_curve[["auc"]], 0.395, tolerance = 1e-3)
})

test_that("create_prc() reterns correct a PRC AUC with 1st point (0, 1)", {
  prc_curve <- create_prc(scores = c(0.6, 0.5, 0.4, 0.3, 0.2, 0.1),
                          obslabs = c(1, 1, 0, 1, 0, 0), x_interval = 0.01)

  expect_equal(prc_curve[["auc"]], 0.904, tolerance = 1e-3)
})
