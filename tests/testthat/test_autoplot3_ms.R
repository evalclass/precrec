library(precrec)

context("AP 3: Autoplot for mscurves")
# Test autoplot(object, ...)

ap3_check_libs <- function() {
  if (requireNamespace("ggplot2", quietly = TRUE)
      && requireNamespace("grid", quietly = TRUE)
      && requireNamespace("gridExtra", quietly = TRUE)) {
    TRUE
  } else {
    FALSE
  }
}

ap3_create_curves <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels)
  evalmod(mdat)
}

test_that("autoplot mscurves", {
  if (!ap3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ap3_create_curves()

  pp <- ggplot2::autoplot(curves, show_legend = FALSE, ret_grob = TRUE)
  expect_true(all(class(pp) == c("gtable", "grob", "gDesc")))
})

test_that("autoplot msroc", {
  if (!ap3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ap3_create_curves()

  pp <- ggplot2::autoplot(curves[["rocs"]])
  expect_true(all(class(pp) == c("gg", "ggplot")))
})


test_that("autoplot msprc", {
  if (!ap3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ap3_create_curves()

  pp <- ggplot2::autoplot(curves[["prcs"]])
  expect_true(all(class(pp) == c("gg", "ggplot")))
})
