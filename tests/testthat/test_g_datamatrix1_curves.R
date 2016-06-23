library(precrec)

context("DF 1: as.data.frame curves")
# Test as.data.frame(x, ...)

df1_create_mscurves <- function() {
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

df1_create_smcurves <- function(raw_curves = FALSE) {
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

df1_create_mmcurves <- function(raw_curves = FALSE) {
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

test_that("as.data.frame sscurves", {

  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)

  curve_df <- as.data.frame(curves)
  expect_true(is.data.frame(curve_df))
})

test_that("as.data.frame mscurves", {

  curves <- df1_create_mscurves()

  curve_df <- as.data.frame(curves)
  expect_true(is.data.frame(curve_df))
})

test_that("as.data.frame smcurves", {

  curves <- df1_create_smcurves(raw_curves = TRUE)

  curve_df <- as.data.frame(curves)
  expect_true(is.data.frame(curve_df))
})

test_that("as.data.frame mmcurves", {

  curves <- df1_create_mmcurves(raw_curves = TRUE)

  curve_df <- as.data.frame(curves)
  expect_true(is.data.frame(curve_df))
})
