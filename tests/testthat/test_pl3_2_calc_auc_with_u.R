library(precrec)

context("PL 3: Calculate AUC (ROC) with the U statitics")
# Test calc_auc_with_u(sdat, scores, labels, na_worst, ties_method, keep_sdat)

test_that("calc_auc_with_u() reterns a 'uauc' object", {
  auc1 <- calc_auc_with_u(scores = c(0.1, 0.2, 0), labels = c(1, 0, 1))

  data(P10N10)
  sdat <- reformat_data(P10N10$scores, P10N10$labels, mode = "aucroc")
  auc2 <- calc_auc_with_u(sdat)
  auc3 <- calc_auc_with_u(scores = P10N10$scores, labels = P10N10$labels)

  expect_true(is(auc1, "uauc"))
  expect_true(is(auc2, "uauc"))
  expect_true(is(auc3, "uauc"))
})

test_that("'sdat' must be a 'sdat' object", {
  expect_err_msg <- function(sdat) {
    err_msg <- "Unrecognized class for .validate()"
    eval(bquote(expect_error(calc_auc_with_u(sdat), err_msg)))
  }

  expect_err_msg(list())
  expect_err_msg(data.frame())
})

test_that("calc_auc_with_u() can directly take scores and labels", {
  sdat <- reformat_data(c(0.1, 0.2, 0.2, 0), c(1, 0, 1, 1), mode = "aucroc")
  auc1 <- calc_auc_with_u(sdat)
  auc2 <- calc_auc_with_u(scores = c(0.1, 0.2, 0.2, 0),
                          labels = c(1, 0, 1, 1))

  expect_equal(auc1, auc2)
})

test_that("calc_auc_with_u() accepts arguments for reformat_data()", {
  err_msg <- "Invalid arguments: na.rm"
  expect_error(calc_auc_with_u(scores = c(0.1, 0.2, 0.2, 0),
                               labels = c(1, 0, 1, 1), na.rm = TRUE),
               err_msg)

  aucs <- calc_auc_with_u(scores = c(0.1, 0.2, 0),
                          labels = c(1, 0, 1),
                          na_worst = TRUE,
                          ties_method = "first",
                          keep_sdat = TRUE)

  expect_equal(.get_obj_arg(aucs, "sdat", "na_worst"), TRUE)
})
