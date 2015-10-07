library(precrec)

context("PR 3: Print mscurves")
# Test print(x, ...)

pt3_create_curves <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels)
  evalmulti(mdat)
}

test_that("print mscurves", {
  pdf(NULL)
  on.exit(dev.off())

  curves <- pt3_create_curves()

  expect_output(print(curves), "ROC curves")
  expect_output(print(curves), "Precision-Recall curves")
})

test_that("print msroc", {
  pdf(NULL)
  on.exit(dev.off())

  curves <- pt3_create_curves()

  expect_output(print(curves[["rocs"]]), "ROC curves")
})


test_that("print msprc", {
  pdf(NULL)
  on.exit(dev.off())

  curves <- pt3_create_curves()

  expect_output(print(curves[["prcs"]]), "Precision-Recall curves")
})
