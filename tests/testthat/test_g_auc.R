#' @importFrom precrec

context("AC 1: Retrieve AUCs")
# Test auc(curves)

auc_create_mscurves <- function() {
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

auc_create_smcurves <- function() {
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

auc_create_mmcurves <- function() {
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

test_that("aucs for sscurves", {
  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)
  aucs <- auc(curves)

  expect_equal(nrow(aucs), 2)
  expect_equal(nrow(subset(aucs, curvetypes == "PRC")), 1)
  expect_equal(nrow(subset(aucs, curvetypes == "ROC")), 1)
})

test_that("aucs for mscurves", {
  curves <- auc_create_mscurves()
  aucs <- auc(curves)

  expect_equal(nrow(aucs), 6)
  expect_equal(nrow(subset(aucs, curvetypes == "PRC")), 3)
  expect_equal(nrow(subset(aucs, curvetypes == "ROC")), 3)
})

test_that("aucs for smcurves", {
  curves <- auc_create_smcurves()
  aucs <- auc(curves)

  expect_equal(nrow(aucs), 6)
  expect_equal(nrow(subset(aucs, curvetypes == "PRC")), 3)
  expect_equal(nrow(subset(aucs, curvetypes == "ROC")), 3)
})

test_that("aucs for mmcurves", {
  curves <- auc_create_mmcurves()
  aucs <- auc(curves)

  expect_equal(nrow(aucs), 8)
  expect_equal(nrow(subset(aucs, curvetypes == "PRC")), 4)
  expect_equal(nrow(subset(aucs, curvetypes == "ROC")), 4)
})
