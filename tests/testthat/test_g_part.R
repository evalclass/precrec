#' @importFrom precrec

context("PA 1: Calculate partial AUC")
# Test part(x, xlim = c(0, 1), ylim = c(0, 1), curvetype, ...)

pa_create_msdat <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mmdata(scores, labels)
}

pa_create_smdat <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mmdata(scores, labels, expd_first = "dsids")
}

pa_create_mmdat <- function() {
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

  mmdata(scores, labels,
    modnames = c("m1", "m2"), dsids = c(1, 2),
    expd_first = "modnames"
  )
}

test_that("curvetype", {
  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)
  expect_silent(part(curves))
  expect_silent(part(curves, curvetype = c("ROC", "PRC")))
  expect_silent(part(curves, curvetype = "ROC"))
  expect_silent(part(curves, curvetype = "PRC"))
  expect_error(part(curves, curvetype = "PROC"), "Invalid curvetype")
})

test_that("xlim", {
  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)
  expect_silent(part(curves))
  expect_silent(part(curves, xlim = c(0, 1)))
  expect_silent(part(curves, xlim = c(0.25, 1)))
  expect_silent(part(curves, xlim = c(0, 0.75)))
  expect_silent(part(curves, xlim = c(0.25, 0.75)))
  expect_error(part(curves, xlim = 0.25), "not equal to 2L")
  expect_error(part(curves, xlim = c("0.25", "0.75")), "numeric or integer")
  expect_error(part(curves, xlim = c(0.75, 0.25)), "not less than")
  expect_error(part(curves, xlim = c(-0.75, 0.25)), "greater than or equal")
  expect_error(part(curves, xlim = c(0.75, 1.25)), "less than or equal")
})

test_that("ylim", {
  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)
  expect_silent(part(curves))
  expect_silent(part(curves, ylim = c(0, 1)))
  expect_silent(part(curves, ylim = c(0.25, 1)))
  expect_silent(part(curves, ylim = c(0, 0.75)))
  expect_silent(part(curves, ylim = c(0.25, 0.75)))
  expect_error(part(curves, ylim = 0.25), "not equal to 2L")
  expect_error(part(curves, ylim = c("0.25", "0.75")), "numeric or integer")
  expect_error(part(curves, ylim = c(0.75, 0.25)), "not less than")
  expect_error(part(curves, ylim = c(-0.75, 0.25)), "greater than or equal")
  expect_error(part(curves, ylim = c(0.75, 1.25)), "less than or equal")
})

pa_test_curves_basic <- function(curves) {
  testthat::expect_silent(part(curves, xlim = c(0.25, 0.75)))
  testthat::expect_silent(part(curves,
    xlim = c(0.25, 0.75),
    curvetype = "ROC"
  ))
  testthat::expect_silent(part(curves,
    xlim = c(0.25, 0.75),
    curvetype = "PRC"
  ))
  testthat::expect_silent(part(curves,
    xlim = c(0.25, 0.75),
    ylim = c(0.25, 0.75)
  ))
  testthat::expect_silent(part(curves,
    xlim = c(0.25, 0.75),
    ylim = c(0.25, 0.75),
    curvetype = "ROC"
  ))
  testthat::expect_silent(part(curves,
    xlim = c(0.25, 0.75),
    ylim = c(0.25, 0.75),
    curvetype = "PRC"
  ))
}

pa_test_attr <- function(curves, pauc_len, avg_only = FALSE) {
  cpart <- part(curves, xlim = c(0.25, 0.75), ylim = c(0.2, 0.7))

  testthat::expect_false(attr(curves, "partial"))
  testthat::expect_true(attr(cpart, "partial"))

  testthat::expect_equal(attr(cpart[["rocs"]], "xlim"), c(0.25, 0.75))
  testthat::expect_equal(attr(cpart[["rocs"]], "ylim"), c(0.2, 0.7))
  if (avg_only) {
    testthat::expect_equal(
      attr(attr(cpart, "grp_avg")[["rocs"]][[1]], "xlim"),
      c(0.25, 0.75)
    )
    testthat::expect_equal(
      attr(attr(cpart, "grp_avg")[["rocs"]][[1]], "ylim"),
      c(0.2, 0.7)
    )
  } else {
    testthat::expect_equal(attr(cpart[["rocs"]][[1]], "xlim"), c(0.25, 0.75))
    testthat::expect_equal(attr(cpart[["rocs"]][[1]], "ylim"), c(0.2, 0.7))
  }

  testthat::expect_equal(attr(cpart[["prcs"]], "xlim"), c(0.25, 0.75))
  testthat::expect_equal(attr(cpart[["prcs"]], "ylim"), c(0.2, 0.7))
  if (avg_only) {
    testthat::expect_equal(
      attr(attr(cpart, "grp_avg")[["prcs"]][[1]], "xlim"),
      c(0.25, 0.75)
    )
    testthat::expect_equal(
      attr(attr(cpart, "grp_avg")[["prcs"]][[1]], "ylim"),
      c(0.2, 0.7)
    )
  } else {
    testthat::expect_equal(attr(cpart[["prcs"]][[1]], "xlim"), c(0.25, 0.75))
    testthat::expect_equal(attr(cpart[["prcs"]][[1]], "ylim"), c(0.2, 0.7))
  }

  testthat::expect_equal(length(attr(cpart, "paucs")[["paucs"]]), pauc_len)
  testthat::expect_equal(length(attr(cpart, "paucs")[["spaucs"]]), pauc_len)
}

test_that("partial auc sscurves", {
  data(P10N10)

  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)
  pa_test_curves_basic(curves)
  pa_test_attr(curves, 2)
})

test_that("partial auc mscurves", {
  mdat <- pa_create_msdat()

  curves <- evalmod(mdat)
  pa_test_curves_basic(curves)
  pa_test_attr(curves, 6)
})

test_that("partial auc smcurves", {
  mdat <- pa_create_smdat()

  curves1 <- evalmod(mdat)
  pa_test_curves_basic(curves1)
  pa_test_attr(curves1, 2, TRUE)

  curves2 <- evalmod(mdat, raw_curves = TRUE)
  pa_test_curves_basic(curves2)
  pa_test_attr(curves2, 6)
})

test_that("partial auc mmcurves", {
  mdat <- pa_create_mmdat()

  curves1 <- evalmod(mdat)
  pa_test_curves_basic(curves1)
  pa_test_attr(curves1, 4, TRUE)

  curves2 <- evalmod(mdat, raw_curves = TRUE)
  pa_test_curves_basic(curves2)
  pa_test_attr(curves2, 8)
})

pa_test_paucs <- function(curves_part, idx, pauc, spauc) {
  testthat::expect_equal(attr(curves_part, "paucs")[["paucs"]][idx], pauc,
    tolerance = 1e-3
  )
  testthat::expect_equal(attr(curves_part, "paucs")[["spaucs"]][idx], spauc,
    tolerance = 1e-3
  )
}

pa_test_paucs_roc <- function(curves, idx) {
  curves_part1 <- part(curves, xlim = c(0, 0.1), ylim = c(0, 1))
  pa_test_paucs(curves_part1, idx, 0.02, 0.2)

  curves_part2 <- part(curves, xlim = c(0.1, 0.5), ylim = c(0, 0.5))
  pa_test_paucs(curves_part2, idx, 0.2, 1)

  curves_part3 <- part(curves, xlim = c(0.1, 0.5), ylim = c(0.5, 0.7))
  pa_test_paucs(curves_part3, idx, 0.04, 0.5)

  curves_part4 <- part(curves, xlim = c(0.1, 0.5), ylim = c(0.7, 1))
  pa_test_paucs(curves_part4, idx, 0, 0)

  curves_part5 <- part(curves, xlim = c(0, 0.5))
  pa_test_paucs(curves_part5, idx, 0.26, 0.52)

  curves_part6 <- part(curves, xlim = c(0.5, 0.75), ylim = c(0.25, 0.5))
  pa_test_paucs(curves_part6, idx, 0.0625, 1)

  curves_part7 <- part(curves, xlim = c(0.1, 0.2), ylim = c(0.6, 0.8))
  pa_test_paucs(curves_part7, idx, 0, 0)
}

test_that("partial auc ROC", {
  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)
  pa_test_paucs_roc(curves, 1)
})

test_that("partial auc ROC x_bins 1", {
  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels, x_bins = 1)
  pa_test_paucs_roc(curves, 1)
})

pa_test_paucs_prc <- function(curves, idx) {
  curves_part1 <- part(curves, xlim = c(0, 0.2), ylim = c(0.5, 1))
  pa_test_paucs(curves_part1, idx, 0.1, 1)

  curves_part2 <- part(curves, xlim = c(0.2, 0.3), ylim = c(0.6666667, 0.75))
  pa_test_paucs(curves_part2, idx, 0.004166665, 0.5)

  curves_part3 <- part(curves, xlim = c(0.3, 0.4), ylim = c(0.75, 0.8))
  pa_test_paucs(curves_part3, idx, 0.0025, 0.5)

  curves_part4 <- part(curves, xlim = c(0.4, 0.5), ylim = c(0.8, 0.8333333))
  pa_test_paucs(curves_part4, idx, 0.001666665, 0.5)

  curves_part5 <- part(curves, xlim = c(0, 0.5))
  pa_test_paucs(curves_part5, idx, 0.43, 0.86)

  curves_part6 <- part(curves, xlim = c(0.25, 0.5), ylim = c(0.25, 0.5))
  pa_test_paucs(curves_part6, idx, 0.0625, 1)

  curves_part7 <- part(curves, xlim = c(0.6, 0.8), ylim = c(0.7, 0.9))
  pa_test_paucs(curves_part7, idx, 0, 0)
}

test_that("partial auc PRC x_bins 1", {
  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels, x_bins = 1)
  pa_test_paucs_prc(curves, 2)
})
