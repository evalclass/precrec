#' @importFrom precrec

context("FT 1: Fortify pipeline objects")
# Test fortify(model, ...)

ft1_check_libs <- function() {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    TRUE
  } else {
    FALSE
  }
}

test_that("fortify fmdat", {
  if (!ft1_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  data(B500)
  fmdat <- reformat_data(B500$good_er_scores, B500$labels)

  curve_df <- ggplot2::fortify(fmdat)
  expect_true(is.list(curve_df))
})

test_that("fortify cmat", {
  if (!ft1_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  data(B500)
  cmat <- create_confmats(scores = B500$good_er_scores,
                          labels = B500$labels)

  curve_df <- ggplot2::fortify(cmat)
  expect_true(is.list(curve_df))
})


test_that("fortify pevals", {
  if (!ft1_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  data(B500)
  pevals <- calc_measures(scores = B500$good_er_scores,
                          labels = B500$labels)

  curve_df <- ggplot2::fortify(pevals)
  expect_true(is.list(curve_df))
})
