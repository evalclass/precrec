#' @importFrom precrec

context("DF 1: as.data.frame curves")
# Test as.data.frame(x, ...)

df1_create_mscurves <- function() {
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

df1_create_smcurves <- function(raw_curves = FALSE) {
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

df1_create_mmcurves <- function(raw_curves = FALSE) {
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

  mdat <- mmdata(scores, labels,
    modnames = c("m1", "m2"), dsids = c(1, 2),
    expd_first = "modnames"
  )
  evalmod(mdat, raw_curves = raw_curves)
}

df1_comp_dfs <- function(df1, df2) {
  testthat::expect_equal(names(df1), names(df2))

  for (vname in names(df1)) {
    testthat::expect_equal(df1[[vname]], df2[[vname]])
  }
}

test_that("as.data.frame sscurves", {
  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)

  curve_df <- as.data.frame(curves)
  expect_true(is.data.frame(curve_df))

  curve_df2 <- suppressWarnings(as.data.frame(curves, use_rcpp = FALSE))
  df1_comp_dfs(curve_df, curve_df2)
})


test_that("as.data.frame sscurves - dsid_modname", {
  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)

  curve_df <- as.data.frame(curves, raw_curves = TRUE, check_ggplot = TRUE)
  expect_equal(
    as.character(curve_df$dsid_modname),
    paste(curve_df$modname, curve_df$dsid, sep = ":")
  )
})

test_that("as.data.frame mscurves", {
  curves <- df1_create_mscurves()

  curve_df <- as.data.frame(curves)
  expect_true(is.data.frame(curve_df))

  curve_df2 <- suppressWarnings(as.data.frame(curves, use_rcpp = FALSE))
  df1_comp_dfs(curve_df, curve_df2)

  curve_df3 <- suppressWarnings(as.data.frame(curves, use_rcpp = TRUE))
  df1_comp_dfs(curve_df, curve_df3)
})

test_that("as.data.frame mscurves - dsid_modname", {
  curves <- df1_create_mscurves()

  curve_df <- as.data.frame(curves, raw_curves = TRUE, check_ggplot = TRUE)
  expect_equal(
    as.character(curve_df$dsid_modname),
    paste(curve_df$modname, curve_df$dsid, sep = ":")
  )
})

test_that("as.data.frame smcurves", {
  curves <- df1_create_smcurves(raw_curves = TRUE)

  curve_df <- as.data.frame(curves)
  expect_true(is.data.frame(curve_df))

  curve_df2 <- suppressWarnings(as.data.frame(curves, use_rcpp = FALSE))
  df1_comp_dfs(curve_df, curve_df2)
})

test_that("as.data.frame smcurves without rawcurves", {
  curves <- df1_create_smcurves(raw_curves = FALSE)

  curve_df <- as.data.frame(curves)
  expect_true(is.data.frame(curve_df))

  curve_df2 <- suppressWarnings(as.data.frame(curves,
    raw_curves = FALSE,
    use_rcpp = FALSE
  ))
  df1_comp_dfs(curve_df, curve_df2)
})

test_that("as.data.frame smcurves - dsid_modname", {
  curves <- df1_create_smcurves(raw_curves = TRUE)

  curve_df <- as.data.frame(curves, raw_curves = TRUE, check_ggplot = TRUE)
  expect_equal(
    as.character(curve_df$dsid_modname),
    paste(curve_df$modname, curve_df$dsid, sep = ":")
  )
})

test_that("as.data.frame mmcurves", {
  curves <- df1_create_mmcurves(raw_curves = TRUE)

  curve_df <- as.data.frame(curves)
  expect_true(is.data.frame(curve_df))

  curve_df2 <- suppressWarnings(as.data.frame(curves, use_rcpp = FALSE))
  df1_comp_dfs(curve_df, curve_df2)
})

test_that("as.data.frame mmcurves without rawcurves", {
  curves <- df1_create_mmcurves(raw_curves = FALSE)

  curve_df <- as.data.frame(curves)
  expect_true(is.data.frame(curve_df))

  curve_df2 <- suppressWarnings(as.data.frame(
    raw_curves = FALSE,
    curves, use_rcpp = FALSE
  ))
  df1_comp_dfs(curve_df, curve_df2)
})

test_that("as.data.frame mmcurves - dsid_modname", {
  curves <- df1_create_mmcurves(raw_curves = TRUE)

  curve_df <- as.data.frame(curves, raw_curves = TRUE, check_ggplot = TRUE)
  expect_equal(
    as.character(curve_df$dsid_modname),
    paste(curve_df$modname, curve_df$dsid, sep = ":")
  )
})

test_that("as.data mode", {
  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)
  attr(curves, "args")[["mode"]] <- "x"
  expect_error(as.data.frame(curves), "Invalid mode")
})

test_that("as.data raw_curve option sscurves", {
  get_args <- function(x, ...) {
    .get_dataframe_arglist(attr(x, "args"),
      def_raw_curves = TRUE,
      ...
    )
  }

  data(P10N10)
  curves1 <- evalmod(scores = P10N10$scores, labels = P10N10$labels)

  args1a <- get_args(curves1, raw_curves = TRUE)
  expect_true(args1a[["raw_curves"]])

  args1b <- get_args(curves1, raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- get_args(curves1)
  expect_true(args1c[["raw_curves"]])
})

test_that("as.data raw_curve option mscurves", {
  get_args <- function(x, ...) {
    .get_dataframe_arglist(attr(x, "args"),
      def_raw_curves = TRUE,
      ...
    )
  }

  curves1 <- df1_create_mscurves()

  args1a <- get_args(curves1, raw_curves = TRUE)
  expect_true(args1a[["raw_curves"]])

  args1b <- get_args(curves1, raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- get_args(curves1)
  expect_true(args1c[["raw_curves"]])
})

test_that("as.data raw_curve option smcurves", {
  get_args <- function(x, ...) {
    .get_dataframe_arglist(attr(x, "args"),
      def_raw_curves = NULL,
      ...
    )
  }

  curves1 <- df1_create_smcurves()

  args1a <- get_args(curves1, raw_curves = TRUE)
  expect_true(args1a[["raw_curves"]])

  args1b <- get_args(curves1, raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- get_args(curves1)
  expect_false(args1c[["raw_curves"]])

  curves2 <- df1_create_smcurves(raw_curves = TRUE)

  args2a <- get_args(curves2, raw_curves = TRUE)
  expect_true(args2a[["raw_curves"]])

  args2b <- get_args(curves2, raw_curves = FALSE)
  expect_false(args2b[["raw_curves"]])

  args2c <- get_args(curves2)
  expect_true(args2c[["raw_curves"]])
})

test_that("as.data raw_curve option mmcurves", {
  get_args <- function(x, ...) {
    .get_dataframe_arglist(attr(x, "args"),
      def_raw_curves = NULL,
      ...
    )
  }

  curves1 <- df1_create_mmcurves()

  args1a <- get_args(curves1, raw_curves = TRUE)
  expect_true(args1a[["raw_curves"]])

  args1b <- get_args(curves1, raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- get_args(curves1)
  expect_false(args1c[["raw_curves"]])

  curves2 <- df1_create_mmcurves(raw_curves = TRUE)

  args2a <- get_args(curves2, raw_curves = TRUE)
  expect_true(args2a[["raw_curves"]])

  args2b <- get_args(curves2, raw_curves = FALSE)
  expect_false(args2b[["raw_curves"]])

  args2c <- get_args(curves2)
  expect_true(args2c[["raw_curves"]])
})
