library(precrec)

context("PR 1: Print objects")
# Test print(x, ...)

test_that("print sscurves", {
  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)

  expect_output(print(curves), "=== AUCs ===")
  expect_output(print(curves), "=== Input data ===")
})

pr3_create_curves <- function() {
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

test_that("print mscurves", {
  curves <- pr3_create_curves()

  expect_output(print(curves), "=== AUCs ===")
  expect_output(print(curves), "=== Input data ===")
})

pr4_create_curves <- function() {
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

test_that("print smcurves", {
  curves <- pr4_create_curves()

  expect_output(print(curves), "=== AUCs ===")
  expect_output(print(curves), "=== Input data ===")
})

pr5_create_curves <- function() {
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

test_that("print mmcurves", {
  curves <- pr5_create_curves()

  expect_output(print(curves), "=== AUCs ===")
  expect_output(print(curves), "=== Input data ===")
})

