library(precrec)

context("FT 2: Fortify sscurves")
# Test fortify(model, ...)

ft2_check_libs <- function() {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    TRUE
  } else {
    FALSE
  }
}

test_that("fortify sscurves", {
  if (!ft2_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)

  df <- ggplot2::fortify(curves)
  expect_true(is.list(df))
})

test_that("fortify ssroc", {
  if (!ft2_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)

  df <- ggplot2::fortify(curves[["rocs"]])
  expect_true(is.list(df))
})


test_that("fortify ssprc", {
  if (!ft2_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)

  df <- ggplot2::fortify(curves[["prcs"]])
  expect_true(is.list(df))
})
