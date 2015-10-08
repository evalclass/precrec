library(precrec)

context("PT 1: Plot pipeline objects")
# Test plot(x, y, ...)

test_that("plot roc_curve", {
  pdf(NULL)
  on.exit(dev.off())

  data(B500)
  roc_curve <- create_roc(scores = P10N10$scores,
                          labels = P10N10$labels)

  expect_that(plot(roc_curve), not(throws_error()))
})

test_that("plot prc_curve", {
  pdf(NULL)
  on.exit(dev.off())

  data(B500)
  prc_curve <- create_prc(scores = P10N10$scores,
                          labels = P10N10$labels)

  expect_that(plot(prc_curve), not(throws_error()))
})
