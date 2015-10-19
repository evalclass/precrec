library(precrec)

context("PT 5: Plot mmcurves")
# Test plot(x, y, ...)

pt5_create_curves <- function() {
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

test_that("plot mmcurves", {
  pdf(NULL)
  on.exit(dev.off())

  curves <- pt5_create_curves()

  expect_that(plot(curves), not(throws_error()))
})

test_that("plot mmroc", {
  pdf(NULL)
  on.exit(dev.off())

  curves <- pt5_create_curves()

  expect_that(plot(curves[["rocs"]]), not(throws_error()))
})


test_that("plot mmprc", {
  pdf(NULL)
  on.exit(dev.off())

  curves <- pt5_create_curves()

  expect_that(plot(curves[["prcs"]]), not(throws_error()))
})
