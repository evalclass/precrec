#' @importFrom precrec

context("FT 2: Fortify curves")
# Test fortify(model, ...)

ft2_check_libs <- function() {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    TRUE
  } else {
    FALSE
  }
}

ft2_create_mscurves <- function() {
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

ft2_create_smcurves <- function(raw_curves = FALSE) {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels, expd_first = "dsids")
  evalmod(mdat, raw_curves = raw_curves)
}

ft2_create_mmcurves <- function(raw_curves = FALSE) {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  s4 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3, s4)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  l4 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3, l4)

  mdat <- mmdata(scores, labels, modnames = c("m1", "m2"), dsids = c(1, 2),
                 expd_first = "modnames")
  evalmod(mdat, raw_curves = raw_curves)
}

test_that("fortify sscurves", {
  if (!ft2_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)

  curve_df <- ggplot2::fortify(curves)
  expect_true(is.list(curve_df))
})

test_that("sscurves - reduce points", {
  if (!ft2_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  samp1 <- create_sim_samples(1, 5000, 5000)
  curves1 <- evalmod(scores = samp1$scores, labels = samp1$labels)
  curve_df1a <- ggplot2::fortify(curves1)
  expect_gte(nrow(curve_df1a), 10000)

  curve_df1b <- ggplot2::fortify(curves1, reduce_points = TRUE)
  expect_lte(nrow(curve_df1b), 10000)


  curves2 <- evalmod(scores = samp1$scores, labels = samp1$labels,
                     x_bins = 100)
  curve_df2a <- ggplot2::fortify(curves2)
  expect_gte(nrow(curve_df2a), 1000)

  curve_df2b <- ggplot2::fortify(curves2, reduce_points = TRUE)
  expect_lte(nrow(curve_df2b), 1000)
})

test_that("fortify mscurves", {
  if (!ft2_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ft2_create_mscurves()

  curve_df <- ggplot2::fortify(curves)
  expect_true(is.list(curve_df))
})

test_that("mscurves - reduce points", {
  if (!ft2_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  samp2 <- create_sim_samples(4, 5000, 5000)
  curves1 <- evalmod(scores = samp2$scores, labels = samp2$labels)
  curve_df1a <- ggplot2::fortify(curves1)
  expect_gte(nrow(curve_df1a), 40000)

  curve_df1b <- ggplot2::fortify(curves1, reduce_points = TRUE)
  expect_lte(nrow(curve_df1b), 40000)

  curves2 <- evalmod(scores = samp2$scores, labels = samp2$labels,
                     x_bins = 100)
  curve_df2a <- ggplot2::fortify(curves2)
  expect_gte(nrow(curve_df2a), 4000)

  curve_df2b <- ggplot2::fortify(curves2, reduce_points = TRUE)
  expect_lte(nrow(curve_df2b), 4000)
})

test_that("fortify smcurves", {
  if (!ft2_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ft2_create_smcurves(raw_curves = TRUE)

  curve_df <- ggplot2::fortify(curves)
  expect_true(is.list(curve_df))
})

test_that("smcurves - reduce points", {
  if (!ft2_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  samp3 <- create_sim_samples(4, 5000, 5000)
  mdat3 <- mmdata(samp3$scores, samp3$labels, expd_first = "dsids")

  curves1 <- evalmod(mdat3, raw_curves = TRUE)
  curve_df1a <- ggplot2::fortify(curves1, raw_curves = TRUE)
  expect_gte(nrow(curve_df1a), 40000)

  curve_df1b <- ggplot2::fortify(curves1, raw_curves = TRUE,
                                 reduce_points = TRUE)
  expect_lte(nrow(curve_df1b), 40000)

  curves2 <- evalmod(mdat3, x_bins = 100, raw_curves = TRUE)
  curve_df2a <- ggplot2::fortify(curves2, raw_curves = TRUE)
  expect_gte(nrow(curve_df2a), 4000)

  curve_df2b <- ggplot2::fortify(curves2, raw_curves = TRUE,
                                 reduce_points = TRUE)
  expect_lte(nrow(curve_df2b), 4000)
})

test_that("fortify mmcurves", {
  if (!ft2_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ft2_create_mmcurves(raw_curves = TRUE)

  curve_df <- ggplot2::fortify(curves)
  expect_true(is.list(curve_df))
})

test_that("mmcurves - reduce points", {
  if (!ft2_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  samp4 <- create_sim_samples(4, 5000, 5000)
  mdat4 <- mmdata(samp4$scores, samp4$labels, modnames = c("m1", "m2"),
                  dsids = c(1, 2), expd_first = "modnames")

  curves1 <- evalmod(mdat4, raw_curves = TRUE)
  curve_df1a <- ggplot2::fortify(curves1, raw_curves = TRUE)
  expect_gte(nrow(curve_df1a), 40000)

  curve_df1b <- ggplot2::fortify(curves1, raw_curves = TRUE,
                                 reduce_points = TRUE)
  expect_lte(nrow(curve_df1b), 40000)

  curves2 <- evalmod(mdat4, x_bins = 100, raw_curves = TRUE)
  curve_df2a <- ggplot2::fortify(curves2, raw_curves = TRUE)
  expect_gte(nrow(curve_df2a), 4000)

  curve_df2b <- ggplot2::fortify(curves2, raw_curves = TRUE,
                                 reduce_points = TRUE)
  expect_lte(nrow(curve_df2b), 4000)
})

test_that("fortify raw_curve option smcurves", {
  curves1 <- ft2_create_smcurves()

  args1a <- .get_fortify_arglist(attr(curves1, "args"), def_raw_curves = TRUE)
  expect_true(args1a[["raw_curves"]])

  args1b <- .get_fortify_arglist(attr(curves1, "args"), def_raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- .get_fortify_arglist(attr(curves1, "args"), def_raw_curves = NULL)
  expect_false(args1c[["raw_curves"]])


  curves2 <- ft2_create_smcurves(raw_curves = TRUE)

  args2a <- .get_fortify_arglist(attr(curves2, "args"), def_raw_curves = TRUE)
  expect_true(args2a[["raw_curves"]])

  args2b <- .get_fortify_arglist(attr(curves2, "args"), def_raw_curves = FALSE)
  expect_false(args2b[["raw_curves"]])

  args2c <- .get_fortify_arglist(attr(curves2, "args"), def_raw_curves = NULL)
  expect_true(args2c[["raw_curves"]])

})

test_that("fortify raw_curve option mmcurves", {
  curves1 <- ft2_create_mmcurves()

  args1a <- .get_fortify_arglist(attr(curves1, "args"), def_raw_curves = TRUE)
  expect_true(args1a[["raw_curves"]])

  args1b <- .get_fortify_arglist(attr(curves1, "args"), def_raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- .get_fortify_arglist(attr(curves1, "args"), def_raw_curves = NULL)
  expect_false(args1c[["raw_curves"]])

  curves2 <- ft2_create_mmcurves(raw_curves = TRUE)

  args2a <- .get_fortify_arglist(attr(curves2, "args"), def_raw_curves = TRUE)
  expect_true(args2a[["raw_curves"]])

  args2b <- .get_fortify_arglist(attr(curves2, "args"), def_raw_curves = FALSE)
  expect_false(args2b[["raw_curves"]])

  args2c <- .get_fortify_arglist(attr(curves2, "args"), def_raw_curves = NULL)
  expect_true(args2c[["raw_curves"]])

})

