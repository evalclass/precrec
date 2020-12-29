#' @importFrom precrec

context("PR 1: Print objects")
# Test print(x, ...)

pr_create_msdat <- function() {
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

pr_create_smdat <- function() {
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

pr_create_mmdat <- function() {
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

  mmdata(scores, labels, modnames = c("m1", "m2"), dsids = c(1, 2),
         expd_first = "modnames")
}

test_that("print sscurves", {
  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)

  expect_output(print(curves), "=== AUCs ===")
  expect_output(print(curves), "=== Input data ===")
})

test_that("print mscurves", {
  mdat <- pr_create_msdat()
  curves <- evalmod(mdat)

  expect_output(print(curves), "=== AUCs ===")
  expect_output(print(curves), "=== Input data ===")

  curves.part <- part(curves, xlim = c(0.25, 0.75))

  expect_output(print(curves.part), "=== AUCs ===")
  expect_output(print(curves.part), "=== partial AUCs ===")
  expect_output(print(curves.part), "=== Input data ===")
})

test_that("print smcurves", {
  mdat <- pr_create_smdat()
  curves <- evalmod(mdat)

  expect_output(print(curves), "=== AUCs ===")
  expect_output(print(curves), "=== Input data ===")

  curves.part <- part(curves, xlim = c(0.25, 0.75))

  expect_output(print(curves.part), "=== AUCs ===")
  expect_output(print(curves.part), "average curves only")
  expect_output(print(curves.part), "=== Input data ===")

  curves <- evalmod(mdat, raw_curves = TRUE)
  curves.part <- part(curves, xlim = c(0.25, 0.75))

  expect_output(print(curves.part), "=== AUCs ===")
  expect_output(print(curves.part), "=== partial AUCs ===")
  expect_output(print(curves.part), "=== Input data ===")
})

test_that("print mmcurves", {
  mdat <- pr_create_mmdat()
  curves <- evalmod(mdat)

  expect_output(print(curves), "=== AUCs ===")
  expect_output(print(curves), "=== Input data ===")

  curves.part <- part(curves, xlim = c(0.25, 0.75))

  expect_output(print(curves.part), "=== AUCs ===")
  expect_output(print(curves.part), "average curves only")
  expect_output(print(curves.part), "=== Input data ===")

  curves <- evalmod(mdat, raw_curves = TRUE)
  curves.part <- part(curves, xlim = c(0.25, 0.75))

  expect_output(print(curves.part), "=== AUCs ===")
  expect_output(print(curves.part), "=== partial AUCs ===")
  expect_output(print(curves.part), "=== Input data ===")
})

test_that("print sspoints", {
  data(P10N10)
  points <- evalmod(mode = "basic", scores = P10N10$scores,
                    labels = P10N10$labels)

  expect_output(print(points), "=== Basic performance evaluation measures ===")
  expect_output(print(points), "=== Input data ===")
})

test_that("print mspoints", {
  mdat <- pr_create_msdat()
  points <- evalmod(mdat, mode = "basic")

  expect_output(print(points), "=== Basic performance evaluation measures ===")
  expect_output(print(points), "=== Input data ===")
})

test_that("print smpoints", {
  mdat <- pr_create_smdat()
  points <- evalmod(mdat, mode = "basic")

  expect_output(print(points), "=== Basic performance evaluation measures ===")
  expect_output(print(points), "=== Input data ===")
})

test_that("print mmpoints", {
  mdat <- pr_create_mmdat()
  points <- evalmod(mdat, mode = "basic")

  expect_output(print(points), "=== Basic performance evaluation measures ===")
  expect_output(print(points), "=== Input data ===")
})

test_that("print aucroc", {
  mdat <- pr_create_mmdat()
  aucroc <- evalmod(mdat, mode = "aucroc")

  expect_output(print(aucroc), "=== Input data ===")
  expect_output(print(aucroc), "=== AUCs ===")
})

