library(precrec)

context("AP 1: Autoplot for pipeline functions")
# Test autoplot(object, ...)

ap1_check_libs <- function() {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    TRUE
  } else {
    FALSE
  }
}

test_that("autoplot fmdat", {
  if (!ap1_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  data(B500)
  fmdat <- reformat_data(B500$good_er_scores, B500$labels)

  pp <- ggplot2::autoplot(fmdat)
  expect_true(all(class(pp) == c("gg", "ggplot")))
})

test_that("autoplot cmat", {
  if (!ap1_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  data(B500)
  cmat <- create_confmats(scores = B500$good_er_scores,
                          labels = B500$labels)

  pp <- ggplot2::autoplot(cmat)
  expect_true(all(class(pp) == c("gg", "ggplot")))
})


test_that("autoplot pevals", {
  if (!ap1_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  data(B500)
  pevals <- calc_measures(scores = B500$good_er_scores,
                          labels = B500$labels)

  pp <- ggplot2::autoplot(pevals)
  expect_true(all(class(pp) == c("gg", "ggplot")))
})

# test_that("autoplot roc_curve", {
#   if (!ap1_check_libs()) {
#     skip("Libraries cannot be loaded")
#   }
#
#   data(B500)
#   roc_curve <- create_roc(scores = P10N10$scores,
#                           labels = P10N10$labels)
#
#   pp <- ggplot2::autoplot(roc_curve)
#   expect_true(all(class(pp) == c("gg", "ggplot")))
# })
#
# test_that("autoplot prc_curve", {
#   if (!ap1_check_libs()) {
#     skip("Libraries cannot be loaded")
#   }
#
#   data(B500)
#   prc_curve <- create_prc(scores = P10N10$scores,
#                           labels = P10N10$labels)
#
#   pp <- ggplot2::autoplot(prc_curve)
#   expect_true(all(class(pp) == c("gg", "ggplot")))
# })

