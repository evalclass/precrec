library(precrec)

context("AP 4: Autoplot for smcurves")
# Test autoplot(object, ...)

ap4_check_libs <- function() {
  if (requireNamespace("ggplot2", quietly = TRUE)
      && requireNamespace("grid", quietly = TRUE)
      && requireNamespace("gridExtra", quietly = TRUE)) {
    TRUE
  } else {
    FALSE
  }
}

ap4_create_curves <- function() {
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

test_that("autoplot smcurves", {
  if (!ap4_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ap4_create_curves()

  pp <- ggplot2::autoplot(curves, show_legend = FALSE, ret_grob = TRUE)
  expect_true(all(class(pp) == c("gtable", "grob", "gDesc")))
})

test_that("autoplot smroc", {
  if (!ap4_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ap4_create_curves()

  df <- ggplot2::fortify(curves, all_curves = FALSE)
  pp <- ggplot2::autoplot(curves[["rocs"]], df = df)
  expect_true(all(class(pp) == c("gg", "ggplot")))
})


test_that("autoplot smprc", {
  if (!ap4_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ap4_create_curves()

  df <- ggplot2::fortify(curves, all_curves = FALSE)
  pp <- ggplot2::autoplot(curves[["prcs"]], df = df)
  expect_true(all(class(pp) == c("gg", "ggplot")))
})
