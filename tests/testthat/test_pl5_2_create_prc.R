#' @importFrom precrec

context("PL 5: Create a Precision-Recall curve")
# Test create_prc(pevals, scores, labels, x_bins)

test_that("create_prc() reterns a 'prc_curve' object", {
  prc_curve1 <- create_prc(scores = c(0.1, 0.2, 0), labels = c(1, 0, 1))

  data(P10N10)
  fmdat <- reformat_data(P10N10$scores, P10N10$labels)
  cdat <- create_confmats(fmdat)
  pevals <- calc_measures(cdat)
  prc_curve2 <- create_prc(pevals)
  prc_curve3 <- create_prc(scores = P10N10$scores, labels = P10N10$labels)

  expect_true(is(prc_curve1, "prc_curve"))
  expect_true(is(prc_curve2, "prc_curve"))
  expect_true(is(prc_curve3, "prc_curve"))
})

test_that("'pevals' must be an 'pevals' object", {
  expect_err_msg <- function(pevals) {
    err_msg <- "Unrecognized class for .validate()"
    eval(bquote(expect_error(create_prc(pevals), err_msg)))
  }

  expect_err_msg(list())
  expect_err_msg(data.frame())
})

test_that("create_prc() directly takes scores and labels", {
  pevals <- calc_measures(scores = c(0.1, 0.2, 0.2, 0),
                          labels = c(1, 0, 1, 1))
  prc_curve1 <- create_prc(pevals)
  prc_curve2 <- create_prc(scores = c(0.1, 0.2, 0.2, 0),
                           labels = c(1, 0, 1, 1))

  expect_equal(prc_curve1[["auc"]], prc_curve2[["auc"]])
})

test_that("create_prc() accepts arguments for reformat_data()", {
  err_msg <- "Invalid arguments: na.rm"
  expect_error(create_prc(scores = c(0.1, 0.2, 0.2, 0),
                          labels = c(1, 0, 1, 1), na.rm = TRUE),
               err_msg)

  prc_curve <- create_prc(scores = c(0.1, 0.2, 0),
                          labels = c(1, 0, 1),
                          na_worst = TRUE,
                          ties_method = "first",
                          keep_pevals = TRUE,
                          keep_fmdat = TRUE)

  expect_equal(.get_obj_arg(prc_curve, "fmdat", "na_worst"), TRUE)
  expect_equal(.get_obj_arg(prc_curve, "fmdat", "ties_method"), "first")
})

test_that("create_prc() accepts na_worst argument", {
  expect_equal_ranks <- function(scores, na_worst, ranks) {
    prc_curve <- create_prc(scores = scores, labels = c(1, 0, 1),
                            na_worst = na_worst,
                            keep_pevals = TRUE,
                            keep_fmdat = TRUE)

    fmdat <- .get_obj(prc_curve, "fmdat")

    eval(bquote(expect_equal(.get_obj_arg(prc_curve, NULL, "na_worst"),
                             na_worst)))
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

test_that("create_prc() accepts ties_method argument", {

  expect_equal_ranks <- function(ties_method, ranks) {
    prc_curve <- create_prc(scores = c(0.1, 0.2, 0.2, 0.2, 0.3),
                            labels = c(1, 0, 1, 1, 1),
                            ties_method = ties_method,
                            keep_pevals = TRUE,
                            keep_fmdat = TRUE)

    fmdat <- .get_obj(prc_curve, "fmdat")

    eval(bquote(expect_equal(.get_obj_arg(prc_curve, NULL, "ties_method"),
                             ties_method)))
    eval(bquote(expect_equal(.get_obj_arg(fmdat, NULL, "ties_method"),
                             ties_method)))
    eval(bquote(expect_equal(fmdat[["ranks"]], ranks)))
  }

  expect_equal_ranks("equiv", c(5, 2, 2, 2, 1))
  expect_equal_ranks("first", c(5, 2, 3, 4, 1))

})

test_that("create_prc() reterns a correct Precision-Recall curve", {
  prc_curve <- create_prc(scores = c(0.6, 0.5, 0.4, 0.3, 0.2, 0.1),
                          labels = c(0, 1, 0, 1, 0, 1), x_bins = 10)

  expect_equal(attr(prc_curve, "np"), 3)
  expect_equal(attr(prc_curve, "nn"), 3)

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
                          labels = c(0, 1, 0, 1, 0, 1), x_bins = 100)

  expect_equal(attr(prc_curve, "auc"), 0.395, tolerance = 1e-3)
})

test_that("create_prc() reterns correct a PRC AUC with 1st point (0, 1)", {
  prc_curve <- create_prc(scores = c(0.6, 0.5, 0.4, 0.3, 0.2, 0.1),
                          labels = c(1, 1, 0, 1, 0, 0), x_bins = 100)

  expect_equal(attr(prc_curve, "auc"), 0.904, tolerance = 1e-3)
})

pl5_create_ms_dat <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  list(scores = scores, labels = labels)
}

pl5_create_sm_dat <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  list(scores = scores, labels = labels)
}

pl5_create_mm_dat <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  s4 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3, s4)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  l4 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3, l4)

  list(scores = scores, labels = labels)
}

test_that("ss test data", {
  prc_curve <- create_prc(scores = c(1, 2, 3, 4),
                          labels = c(1, 0, 1, 0), x_bins = 4)

  expect_equal(prc_curve[["x"]], c(0, 0.25, 0.5, 0.5, 0.75, 1))
  expect_equal(prc_curve[["y"]], c(0, 0.3333, 0.5, 1/3, 0.4285, 0.5),
               tolerance = 1e-3)
  expect_equal(prc_curve[["orig_points"]], c(TRUE, FALSE, TRUE,
                                             TRUE, FALSE, TRUE))

})

test_that("ms test data", {
  msdat <- pl5_create_ms_dat()

  prc_curve1 <- create_prc(scores = msdat[["scores"]][[1]],
                           labels = msdat[["labels"]][[1]], x_bins = 4)
  expect_equal(prc_curve1[["x"]], c(0, 0.25, 1/3, 0.5, 2/3, 2/3, 0.75, 1))
  expect_equal(prc_curve1[["y"]], c(1, 1, 1, 1, 1, 2/3, 0.6923, 0.75),
               tolerance = 1e-3)
  expect_equal(prc_curve1[["orig_points"]], c(TRUE, FALSE, TRUE, FALSE,
                                              TRUE, TRUE, FALSE, TRUE))

  prc_curve2 <- create_prc(scores = msdat[["scores"]][[2]],
                           labels = msdat[["labels"]][[2]], x_bins = 4)
  expect_equal(prc_curve2[["x"]], c(0, 0.25, 1/3, 0.5, 2/3, 0.75, 1, 1))
  expect_equal(prc_curve2[["y"]], c(1, 1, 1, 1, 1, 1, 1, 0.75),
               tolerance = 1e-3)
  expect_equal(prc_curve2[["orig_points"]], c(TRUE, FALSE, TRUE, FALSE,
                                              TRUE, FALSE, TRUE, TRUE))

  prc_curve3 <- create_prc(scores = msdat[["scores"]][[3]],
                           labels = msdat[["labels"]][[3]], x_bins = 4)
  expect_equal(prc_curve3[["x"]], c(0, 0.25, 1/3, 1/3, 0.5, 2/3, 0.75, 1))
  expect_equal(prc_curve3[["y"]], c(1, 1, 1, 0.5, 0.6, 2/3, 0.6923, 0.75),
               tolerance = 1e-3)
  expect_equal(prc_curve3[["orig_points"]], c(TRUE, FALSE, TRUE, TRUE,
                                              FALSE, TRUE, FALSE, TRUE))

})

test_that("sm test data", {
  smdat <- pl5_create_sm_dat()

  prc_curve1 <- create_prc(scores = smdat[["scores"]][[1]],
                           labels = smdat[["labels"]][[1]], x_bins = 4)
  expect_equal(prc_curve1[["x"]], c(0, 0.25, 1/3, 0.5, 2/3, 2/3, 0.75, 1))
  expect_equal(prc_curve1[["y"]], c(1, 1, 1, 1, 1, 2/3, 0.6923, 0.75),
               tolerance = 1e-3)
  expect_equal(prc_curve1[["orig_points"]], c(TRUE, FALSE, TRUE, FALSE,
                                              TRUE, TRUE, FALSE, TRUE))

  prc_curve2 <- create_prc(scores = smdat[["scores"]][[2]],
                           labels = smdat[["labels"]][[2]], x_bins = 4)
  expect_equal(prc_curve2[["x"]], c(0, 0.25, 1/3, 0.5, 2/3, 0.75, 1, 1))
  expect_equal(prc_curve2[["y"]], c(1, 1, 1, 1, 1, 1, 1, 0.75),
               tolerance = 1e-3)
  expect_equal(prc_curve2[["orig_points"]], c(TRUE, FALSE, TRUE, FALSE,
                                              TRUE, FALSE, TRUE, TRUE))

  prc_curve3 <- create_prc(scores = smdat[["scores"]][[3]],
                           labels = smdat[["labels"]][[3]], x_bins = 4)
  expect_equal(prc_curve3[["x"]], c(0, 0.25, 1/3, 1/3, 0.5, 2/3, 0.75, 1))
  expect_equal(prc_curve3[["y"]], c(1, 1, 1, 0.5, 0.6, 2/3, 0.6923, 0.75),
               tolerance = 1e-3)
  expect_equal(prc_curve3[["orig_points"]], c(TRUE, FALSE, TRUE, TRUE,
                                              FALSE, TRUE, FALSE, TRUE))

})

test_that("mm test data", {
  mmdat <- pl5_create_mm_dat()

  prc_curve1 <- create_prc(scores = mmdat[["scores"]][[1]],
                           labels = mmdat[["labels"]][[1]], x_bins = 4)
  expect_equal(prc_curve1[["x"]], c(0, 0.25, 1/3, 0.5, 2/3, 2/3, 0.75, 1))
  expect_equal(prc_curve1[["y"]], c(1, 1, 1, 1, 1, 2/3, 0.6923, 0.75),
               tolerance = 1e-3)
  expect_equal(prc_curve1[["orig_points"]], c(TRUE, FALSE, TRUE, FALSE,
                                              TRUE, TRUE, FALSE, TRUE))

  prc_curve2 <- create_prc(scores = mmdat[["scores"]][[2]],
                           labels = mmdat[["labels"]][[2]], x_bins = 4)
  expect_equal(prc_curve2[["x"]], c(0, 0.25, 1/3, 0.5, 2/3, 0.75, 1, 1))
  expect_equal(prc_curve2[["y"]], c(1, 1, 1, 1, 1, 1, 1, 0.75),
               tolerance = 1e-3)
  expect_equal(prc_curve2[["orig_points"]], c(TRUE, FALSE, TRUE, FALSE,
                                              TRUE, FALSE, TRUE, TRUE))

  prc_curve3 <- create_prc(scores = mmdat[["scores"]][[3]],
                           labels = mmdat[["labels"]][[3]], x_bins = 4)
  expect_equal(prc_curve3[["x"]], c(0, 0.25, 1/3, 1/3, 0.5, 2/3, 0.75, 1))
  expect_equal(prc_curve3[["y"]], c(1, 1, 1, 0.5, 0.6, 2/3, 0.6923, 0.75),
               tolerance = 1e-3)
  expect_equal(prc_curve3[["orig_points"]], c(TRUE, FALSE, TRUE, TRUE,
                                              FALSE, TRUE, FALSE, TRUE))

  prc_curve4 <- create_prc(scores = mmdat[["scores"]][[4]],
                           labels = mmdat[["labels"]][[4]], x_bins = 4)
  expect_equal(prc_curve4[["x"]], c(0, 0.25, 1/3, 1/3, 0.5, 2/3, 0.75, 1))
  expect_equal(prc_curve4[["y"]], c(1, 1, 1, 0.5, 0.6, 2/3, 0.6923, 0.75),
               tolerance = 1e-3)
  expect_equal(prc_curve4[["orig_points"]], c(TRUE, FALSE, TRUE, TRUE,
                                              FALSE, TRUE, FALSE, TRUE))
})

