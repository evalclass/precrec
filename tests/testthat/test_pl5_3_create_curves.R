#' @importFrom precrec

context("PL 5: Create ROC and Precision-Recall curves")
# Test create_curves(pevals, scores, labels, x_bins)

test_that("create_curves() reterns a 'curves' object", {
  curves1 <- create_curves(scores = c(0.1, 0.2, 0), labels = c(1, 0, 1))

  data(P10N10)
  fmdat <- reformat_data(P10N10$scores, P10N10$labels)
  cdat <- create_confmats(fmdat)
  pevals <- calc_measures(cdat)
  curves2 <- create_curves(pevals)
  curves3 <- create_curves(scores = P10N10$scores, labels = P10N10$labels)

  expect_true(is(curves1, "curves"))
  expect_true(is(curves2, "curves"))
  expect_true(is(curves3, "curves"))
})

test_that("'pevals' must be an 'pevals' object", {
  expect_err_msg <- function(pevals) {
    err_msg <- "Unrecognized class for .validate()"
    eval(bquote(expect_error(create_curves(pevals), err_msg)))
  }

  expect_err_msg(list())
  expect_err_msg(data.frame())
})

test_that("create_curves() directly takes scores and labels", {
  pevals <- calc_measures(scores = c(0.1, 0.2, 0.2, 0),
                         labels = c(1, 0, 1, 1))
  curves1 <- create_curves(pevals)
  curves2 <- create_curves(scores = c(0.1, 0.2, 0.2, 0),
                           labels = c(1, 0, 1, 1))

  expect_equal(attr(curves1[["roc"]], "auc"),
               attr(curves2[["roc"]], "auc"))
})

test_that("create_curves() accepts arguments for reformat_data()", {
  err_msg <- "Invalid arguments: na.rm"
  expect_error(create_curves(scores = c(0.1, 0.2, 0.2, 0),
                             labels = c(1, 0, 1, 1), na.rm = TRUE),
               err_msg)

  curves <- create_curves(scores = c(0.1, 0.2, 0),
                          labels = c(1, 0, 1),
                          na_worst = TRUE,
                          ties_method = "first",
                          keep_pevals = TRUE,
                          keep_fmdat = TRUE)

  expect_equal(.get_obj_arg(curves, "fmdat", "na_worst"), TRUE)
  expect_equal(.get_obj_arg(curves, "fmdat", "ties_method"), "first")
})

test_that("create_curves() accepts na_worst argument", {
  expect_equal_ranks <- function(scores, na_worst, ranks) {
    curves <- create_curves(scores = scores, labels = c(1, 0, 1),
                            na_worst = na_worst,
                            keep_pevals = TRUE,
                            keep_fmdat = TRUE)

    fmdat <- .get_obj(curves, "fmdat")

    eval(bquote(expect_equal(.get_obj_arg(curves, NULL, "na_worst"), na_worst)))
    eval(bquote(expect_equal(.get_obj_arg(fmdat, NULL, "na_worst"), na_worst)))
    eval(bquote(expect_equal(fmdat[["ranks"]], ranks)))

    sranks <- .rank_scores(scores, na_worst = na_worst)
    eval(bquote(expect_equal(sranks[["ranks"]], ranks)))
  }

  na1_scores <- c(NA, 0.2, 0.1)
  na2_scores <- c(0.2, NA, 0.1)
  na3_scores <- c(0.2, 0.1, NA)

  expect_equal_ranks(na1_scores, TRUE, c(3, 1, 2))
  expect_equal_ranks(na1_scores, FALSE, c(1, 2, 3))

  expect_equal_ranks(na2_scores, TRUE, c(1, 3, 2))
  expect_equal_ranks(na2_scores, FALSE, c(2, 1, 3))

  expect_equal_ranks(na3_scores, TRUE, c(1, 2, 3))
  expect_equal_ranks(na3_scores, FALSE, c(2, 3, 1))

})

test_that("create_curves() accepts ties_method argument", {

  expect_equal_ranks <- function(ties_method, ranks) {
    curves <- create_curves(scores = c(0.1, 0.2, 0.2, 0.2, 0.3),
                            labels = c(1, 0, 1, 1, 1),
                            ties_method = ties_method,
                            keep_pevals = TRUE,
                            keep_fmdat = TRUE)

    fmdat <- .get_obj(curves, "fmdat")

    eval(bquote(expect_equal(.get_obj_arg(curves, NULL, "ties_method"),
                             ties_method)))
    eval(bquote(expect_equal(.get_obj_arg(fmdat, NULL, "ties_method"),
                             ties_method)))
    eval(bquote(expect_equal(fmdat[["ranks"]], ranks)))
  }

  expect_equal_ranks("equiv", c(5, 2, 2, 2, 1))
  expect_equal_ranks("first", c(5, 2, 3, 4, 1))

})

test_that("'curves' contains a list with 2 items", {
  curves <- create_curves(scores = c(0.1, 0.2, 0), labels = c(1, 0, 1))

  expect_true(is.list(curves))
  expect_equal(length(curves), 2)
})

test_that("create_curves() reterns a correct ROC curve", {
  curves <- create_curves(scores = c(0.6, 0.5, 0.4, 0.3, 0.2, 0.1),
                          labels = c(0, 1, 0, 1, 0, 1), x_bins = 10)

  expect_equal(attr(curves[["roc"]], "np"), 3)
  expect_equal(attr(curves[["roc"]], "nn"), 3)

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
                          labels = c(0, 1, 0, 1, 0, 1), x_bins = 10)

  expect_equal(attr(curves[["prc"]], "np"), 3)
  expect_equal(attr(curves[["prc"]], "np"), 3)

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
                          labels = c(0, 1, 0, 1, 0, 1), x_bins = 100)

  expect_equal(attr(curves[["roc"]], "auc"), 1/3, tolerance = 1e-3)
})

test_that("create_curves() reterns correct a PRC AUC with 1st point (0, 0)", {
  curves <- create_curves(scores = c(0.6, 0.5, 0.4, 0.3, 0.2, 0.1),
                          labels = c(0, 1, 0, 1, 0, 1), x_bins = 100)

  expect_equal(attr(curves[["prc"]], "auc"), 0.395, tolerance = 1e-3)
})

test_that("create_curves() reterns correct a PRC AUC with 1st point (0, 1)", {
  curves <- create_curves(scores = c(0.6, 0.5, 0.4, 0.3, 0.2, 0.1),
                          labels = c(1, 1, 0, 1, 0, 0), x_bins = 100)

  expect_equal(attr(curves[["prc"]], "auc"), 0.904, tolerance = 1e-3)
})
