library(precrec)

context("PR 1: Print pipeline objects")
# Test print(x, ...)

test_that("plot curves", {
  data(B500)
  curves <- create_curves(scores = P10N10$scores,
                             labels = P10N10$labels)

  expect_output(print(curves), "ROC curve")
  expect_output(print(curves), "Precision-Recall curve")
})

test_that("plot roc_curve", {
  data(B500)
  roc_curve <- create_roc(scores = P10N10$scores,
                          labels = P10N10$labels)

  expect_output(print(roc_curve), "ROC curve")
})

test_that("plot prc_curve", {
  data(B500)
  prc_curve <- create_prc(scores = P10N10$scores,
                          labels = P10N10$labels)

  expect_output(print(prc_curve), "Precision-Recall curve")
})
