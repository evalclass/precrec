library(precrec)

context("PT 2: plot sscurves")
# Test plot(x, y, ...)

test_that("plot sscurves", {
  pdf(NULL)
  on.exit(dev.off())

  data(P10N10)
  curves <- evalmod_s(scores = P10N10$scores, labels = P10N10$labels)

  expect_that(plot(curves), not(throws_error()))
})

test_that("plot ssroc", {
  pdf(NULL)
  on.exit(dev.off())

  data(P10N10)
  curves <- evalmod_s(scores = P10N10$scores, labels = P10N10$labels)

  expect_that(plot(curves[["rocs"]]), not(throws_error()))
})


test_that("plot ssprc", {
  pdf(NULL)
  on.exit(dev.off())

  data(P10N10)
  curves <- evalmod_s(scores = P10N10$scores, labels = P10N10$labels)

  expect_that(plot(curves[["prcs"]]), not(throws_error()))
})
