library(precrec)

context("PT 4: Plot smcurves")
# Test plot(x, y, ...)

pt4_create_curves <- function() {
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

test_that("plot smcurves", {
  pdf(NULL)
  on.exit(dev.off())

  curves <- pt4_create_curves()

  expect_that(plot(curves), not(throws_error()))
})

test_that("plot smroc", {
  pdf(NULL)
  on.exit(dev.off())

  curves <- pt4_create_curves()

  expect_that(plot(curves[["rocs"]]), not(throws_error()))
})


test_that("plot smprc", {
  pdf(NULL)
  on.exit(dev.off())

  curves <- pt4_create_curves()

  expect_that(plot(curves[["prcs"]]), not(throws_error()))
})
