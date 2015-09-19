context("M2 PL4: Create a list of ROC and Precision curves")
# Test .create_list_curves(arg:mdat, arg:mscores, arg:mobslabs,
#                          arg:x_interval, arg:model_names, arg:...)

test_that("arg:mdat must be an 'mdat' object", {
  expect_err_msg <- function(mdat) {
    err_msg <- "An object of unknown class is specified"
    eval(bquote(expect_error(.create_list_curves(mdat), err_msg)))
  }

  expect_err_msg(list())
  expect_err_msg(data.frame())
})

test_that("create_curves_for_multi() directly takes mscores and mobslabs", {
  vfunc <- function(s) {
    expect_equal(list_curves1[[s]][["roc"]][["auc"]],
                 list_curves2[[s]][["roc"]][["auc"]])
    expect_equal(list_curves1[[s]][["prc"]][["auc"]],
                 list_curves2[[s]][["prc"]][["auc"]])
  }

  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  mscores <- combine_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(1, 1, 0, 0)
  l3 <- c(0, 1, 0, 1)
  mobslabs <- combine_obslbs(l1, l2, l3)

  mdat <- create_mdat(mscores, mobslabs)

  list_curves1 <- .create_list_curves(mdat)
  list_curves2 <- .create_list_curves(mscores = mscores, mobslabs = mobslabs)

  lapply(1:3, vfunc)
})

test_that(".create_list_curves() takes arguments for reformat_data", {
  s = c(0.1, 0.2, 0.2, 0)
  l = c(1, 0, 1, 1)
  fmdat <- reformat_data(s, l, ties.method = "first")
  cmats <- create_confmats(fmdat)
  evals <- calc_measures(cmats)
  curves <- create_curves(evals)

#   list_curves <- .create_list_curves(mscores = mscores, mobslabs = mobslabs,
#                                      ties.method = "first")
#
#   expect_equal(curves[["roc"]][["auc"]], list_curves[[1]][["roc"]][["auc"]])
#   expect_equal(curves[["prc"]][["auc"]], list_curves[[1]][["prc"]][["auc"]])
})


