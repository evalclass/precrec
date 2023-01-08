#' @importFrom precrec

context("AP 1: Autoplot for pipeline functions")
# Test autoplot(object, ...)

skip_on_cran()

ap1_check_libs <- function() {
  if (requireNamespace("ggplot2", quietly = TRUE) &&
    requireNamespace("vdiffr", quietly = TRUE)) {
    TRUE
  } else {
    FALSE
  }
}

test_that("autoplot fmdat", {
  if (!ap1_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  orig_seed <- globalenv()[[".Random.seed"]]
  on.exit(assign(".Random.seed", orig_seed, envir = .GlobalEnv))
  set.seed(1234)

  data(B500)
  fmdat <- reformat_data(B500$good_er_scores, B500$labels)
  p <- ggplot2::autoplot(fmdat)
  check_ggplot_fig("autoplot_fmdat", p)
})

test_that("autoplot cmats", {
  if (!ap1_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  data(B500)
  cmats <- create_confmats(
    scores = B500$good_er_scores,
    labels = B500$labels
  )
  p <- ggplot2::autoplot(cmats)
  check_ggplot_fig("autoplot_cmats", p)
})


test_that("autoplot pevals", {
  if (!ap1_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  data(B500)
  pevals <- calc_measures(
    scores = B500$good_er_scores,
    labels = B500$labels
  )
  suppressWarnings(vdiffr::expect_doppelganger(
    "autoplot_pevals",
    ggplot2::autoplot(pevals)
  ))
})
