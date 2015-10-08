library(precrec)

context("PT 3: Plot mscurves")
# Test plot(x, y, ...)

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

test_that("plot mscurves", {
  pdf(NULL)
  on.exit(dev.off())

  curves <- pt3_create_curves()

  expect_that(plot(curves), not(throws_error()))
})

test_that("plot msroc", {
  pdf(NULL)
  on.exit(dev.off())

  curves <- pt3_create_curves()

  expect_that(plot(curves[["rocs"]]), not(throws_error()))
})


test_that("plot msprc", {
  pdf(NULL)
  on.exit(dev.off())

  curves <- pt3_create_curves()

  expect_that(plot(curves[["prcs"]]), not(throws_error()))
})
