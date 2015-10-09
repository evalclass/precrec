library(precrec)

context("FT 4: Fortify smcurves")
# Test fortify(model, ...)

ft4_check_libs <- function() {
  if (requireNamespace("ggplot2", quietly = TRUE)
      && requireNamespace("grid", quietly = TRUE)
      && requireNamespace("gridExtra", quietly = TRUE)) {
    TRUE
  } else {
    FALSE
  }
}

ft4_create_curves <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels, expd_first = "setids")
  evalmod_m(mdat)
}

test_that("fortify smcurves", {
  if (!ft4_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ft4_create_curves()

  df <- ggplot2::fortify(curves)
  expect_true(is.list(df))
})

test_that("fortify smroc", {
  if (!ft4_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ft4_create_curves()

  df <- ggplot2::fortify(curves[["rocs"]])
  expect_true(is.list(df))
})


test_that("fortify smprc", {
  if (!ft4_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ft4_create_curves()

  df <- ggplot2::fortify(curves[["prcs"]])
  expect_true(is.list(df))
})
