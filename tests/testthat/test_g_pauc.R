#' @importFrom precrec

context("PC 1: Retrieve pAUCs")
# Test auc(curves)

pauc_create_mscurves <- function() {
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

pauc_create_smcurves <- function(raw_curves = TRUE) {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels, expd_first = "dsids")
  evalmod(mdat, raw_curves = raw_curves)
}

pauc_create_mmcurves <- function(raw_curves = TRUE) {
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
  evalmod(mdat, raw_curves = raw_curves)
}

test_that("paucs for sscurves", {
  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)
  expect_error(pauc(curves), "should be used first")

  curves.part <- part(curves, xlim = c(0, 0.5), ylim = c(0.5, 1))
  paucs <- pauc(curves.part)

  expect_equal(nrow(paucs), 2)
  expect_equal(nrow(subset(paucs, curvetypes == "PRC")), 1)
  expect_equal(nrow(subset(paucs, curvetypes == "ROC")), 1)
})

test_that("paucs for mscurves", {
  curves <- pauc_create_mscurves()
  expect_error(pauc(curves), "should be used first")

  curves.part <- part(curves, xlim = c(0, 0.5), ylim = c(0.5, 1))
  paucs <- pauc(curves.part)

  expect_equal(nrow(paucs), 6)
  expect_equal(nrow(subset(paucs, curvetypes == "PRC")), 3)
  expect_equal(nrow(subset(paucs, curvetypes == "ROC")), 3)
})

test_that("paucs for smcurves", {
  curves <- pauc_create_smcurves()
  expect_error(pauc(curves), "should be used first")

  curves.part <- part(curves, xlim = c(0, 0.5), ylim = c(0.5, 1))
  paucs <- pauc(curves.part)

  expect_equal(nrow(paucs), 6)
  expect_equal(nrow(subset(paucs, curvetypes == "PRC")), 3)
  expect_equal(nrow(subset(paucs, curvetypes == "ROC")), 3)
})

test_that("paucs for avg smcurves", {
  curves <- pauc_create_smcurves(FALSE)
  expect_error(pauc(curves), "should be used first")

  curves.part <- part(curves, xlim = c(0, 0.5), ylim = c(0.5, 1))
  paucs <- pauc(curves.part)

  expect_equal(nrow(paucs), 2)
  expect_equal(nrow(subset(paucs, curvetypes == "PRC")), 1)
  expect_equal(nrow(subset(paucs, curvetypes == "ROC")), 1)
})

test_that("paucs for mmcurves", {
  curves <- pauc_create_mmcurves()
  expect_error(pauc(curves), "should be used first")

  curves.part <- part(curves, xlim = c(0, 0.5), ylim = c(0.5, 1))
  paucs <- pauc(curves.part)

  expect_equal(nrow(paucs), 8)
  expect_equal(nrow(subset(paucs, curvetypes == "PRC")), 4)
  expect_equal(nrow(subset(paucs, curvetypes == "ROC")), 4)
})

test_that("paucs for avg mmcurves", {
  curves <- pauc_create_mmcurves(FALSE)
  expect_error(pauc(curves), "should be used first")

  curves.part <- part(curves, xlim = c(0, 0.5), ylim = c(0.5, 1))
  paucs <- pauc(curves.part)

  expect_equal(nrow(paucs), 4)
  expect_equal(nrow(subset(paucs, curvetypes == "PRC")), 2)
  expect_equal(nrow(subset(paucs, curvetypes == "ROC")), 2)
})

