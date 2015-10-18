library(precrec)

context("PR 2: Print sscurves")
# Test print(x, ...)

test_that("print sscurves", {
  data(P10N10)
  curves <- evalmod_s(scores = P10N10$scores, labels = P10N10$labels)

  expect_output(print(curves), "ROC curve")
  expect_output(print(curves), "Precision-Recall curve")
})

test_that("print ssroc", {
  data(P10N10)
  curves <- evalmod_s(scores = P10N10$scores, labels = P10N10$labels)

  expect_output(print(curves[["rocs"]]), "ROC curve")
})


test_that("print ssprc", {
  data(P10N10)
  curves <- evalmod_s(scores = P10N10$scores, labels = P10N10$labels)

  expect_output(print(curves[["prcs"]]), "Precision-Recall curve")
})
