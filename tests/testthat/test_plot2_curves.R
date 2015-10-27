library(precrec)

context("PT 2: Plot curves")
# Test plot(x, y, ...)

pt2_create_mscurves <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels)
  evalmod(mdat)
}

pt2_create_smcurves <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels, expd_first = "dsids")
  evalmod(mdat)
}

pt2_create_mmcurves <- function() {
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

  mdat <- mmdata(scores, labels, modnames = c("m1", "m2"), dsids = c(1, 2),
                 expd_first = "modnames")
  evalmod(mdat)
}

ap2_test_roc_prc <- function(curves, ...){

  expect_that(plot(curves, ...), not(throws_error()))
  expect_that(plot(curves, c("ROC", "PRC"), ...), not(throws_error()))
  expect_that(plot(curves, "ROC", ...), not(throws_error()))
  expect_that(plot(curves, "PRC", ...), not(throws_error()))
}

test_that("plot sscurves", {
  pdf(NULL)
  on.exit(dev.off())

  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)

  ap2_test_roc_prc(curves)
})

test_that("plot mscurves", {
  pdf(NULL)
  on.exit(dev.off())

  curves <- pt2_create_mscurves()

  ap2_test_roc_prc(curves)
  ap2_test_roc_prc(curves, show_legend = TRUE)
})

test_that("plot smcurves", {
  pdf(NULL)
  on.exit(dev.off())

  curves <- pt2_create_smcurves()

  ap2_test_roc_prc(curves)
  ap2_test_roc_prc(curves, show_ci = FALSE)
  ap2_test_roc_prc(curves, raw_curves = TRUE)
})

test_that("plot mmcurves", {
  pdf(NULL)
  on.exit(dev.off())

  curves <- pt2_create_mmcurves()

  ap2_test_roc_prc(curves)
  ap2_test_roc_prc(curves, show_ci = TRUE)
  ap2_test_roc_prc(curves, raw_curves = TRUE)
  ap2_test_roc_prc(curves, raw_curves = FALSE)
})
