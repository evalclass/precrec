library(precrec)

context("AP 2: Autoplot for sscurves")
# Test autoplot(object, ...)

ap2_check_libs <- function() {
  if (requireNamespace("ggplot2", quietly = TRUE)
      && requireNamespace("grid", quietly = TRUE)
      && requireNamespace("gridExtra", quietly = TRUE)) {
    TRUE
  } else {
    FALSE
  }
}

test_that("autoplot sscurves", {
  if (!ap2_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)

  pp <- ggplot2::autoplot(curves, ret_grob = TRUE)
  expect_true(all(class(pp) == c("gtable", "grob", "gDesc")))
})

test_that("autoplot ssroc", {
  if (!ap2_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)

  pp <- ggplot2::autoplot(curves[["rocs"]])
  expect_true(all(class(pp) == c("gg", "ggplot")))
})


test_that("autoplot ssprc", {
  if (!ap2_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)

  pp <- ggplot2::autoplot(curves[["prcs"]])
  expect_true(all(class(pp) == c("gg", "ggplot")))
})
