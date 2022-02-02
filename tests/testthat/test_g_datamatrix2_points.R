#' @importFrom precrec

context("DF 2: as.data.frame points")
# Test as.data.frame(x, ...)

df2_create_mspoints <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels)
  evalmod(mdat, mode = "basic")
}

df2_create_smpoints <- function(raw_curves = FALSE) {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels, expd_first = "dsids")
  evalmod(mdat, mode = "basic", raw_curves = raw_curves)
}

df2_create_mmpoints <- function(raw_curves = FALSE) {
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
  evalmod(mdat, mode = "basic", raw_curves = raw_curves)
}

df2_comp_dfs <- function(df1, df2) {
  expect_equal(names(df1), names(df2))

  for (vname in names(df1)) {
    expect_equal(df1[[vname]], df2[[vname]])
  }
}


test_that("as.data.frame sspoints", {

  data(P10N10)
  sspoints <- evalmod(scores = P10N10$scores, labels = P10N10$labels,
                      mode = "basic")

  point_df <- as.data.frame(sspoints)
  expect_true(is.data.frame(point_df))

  point_df2 <- suppressWarnings(as.data.frame(sspoints, use_rcpp = FALSE))
  df2_comp_dfs(point_df, point_df2)
})

test_that("as.data.frame mspoints", {

  mspoints <- df2_create_mspoints()

  point_df <- as.data.frame(mspoints)
  expect_true(is.data.frame(point_df))

  point_df2 <- suppressWarnings(as.data.frame(mspoints, use_rcpp = FALSE))
  df2_comp_dfs(point_df, point_df2)
})

test_that("as.data.frame smpoints", {

  smpoints <- df2_create_smpoints(raw_curves = TRUE)

  point_df <- as.data.frame(smpoints)
  expect_true(is.data.frame(point_df))

  point_df2 <- suppressWarnings(as.data.frame(smpoints, use_rcpp = FALSE))
  df2_comp_dfs(point_df, point_df2)
})

test_that("as.data.frame mmpoints", {

  mmpoints <- df2_create_mmpoints(raw_curves = TRUE)

  point_df <- as.data.frame(mmpoints)
  expect_true(is.data.frame(point_df))

  point_df2 <- suppressWarnings(as.data.frame(mmpoints, use_rcpp = FALSE))
  df2_comp_dfs(point_df, point_df2)
})

test_that("as.data.frame raw_point option sspoints", {
  get_args <- function(x, y = NULL, ...) {
    .get_dataframe_arglist(attr(x, "args"),
                           def_raw_curves = TRUE, ...)
  }

  data(P10N10)
  points1 <- evalmod(mode = "basic", scores = P10N10$scores,
                     labels = P10N10$labels)

  args1a <- get_args(points1, raw_curves = TRUE)
  expect_true(args1a[["raw_curves"]])

  args1b <- get_args(points1, raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- get_args(points1)
  expect_true(args1c[["raw_curves"]])
})

test_that("as.data.frame raw_point option mspoints", {
  get_args <- function(x, y = NULL, ...) {
    .get_dataframe_arglist(attr(x, "args"),
                           def_raw_curves = TRUE, ...)
  }

  points1 <- df2_create_mspoints()

  args1a <- get_args(points1, raw_curves = TRUE)
  expect_true(args1a[["raw_curves"]])

  args1b <- get_args(points1, raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- get_args(points1)
  expect_true(args1c[["raw_curves"]])
})

test_that("as.data.frame raw_point option smpoints", {
  get_args <- function(x, y = NULL, ...) {
    .get_dataframe_arglist(attr(x, "args"),
                           def_raw_curves = NULL, ...)
  }

  points1 <- df2_create_smpoints()

  args1a <- get_args(points1, raw_curves = TRUE)
  expect_true(args1a[["raw_curves"]])

  args1b <- get_args(points1, raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- get_args(points1)
  expect_false(args1c[["raw_curves"]])

  points2 <- df2_create_smpoints(raw_curves = TRUE)

  args2a <- get_args(points2, raw_curves = TRUE)
  expect_true(args2a[["raw_curves"]])

  args2b <- get_args(points2, raw_curves = FALSE)
  expect_false(args2b[["raw_curves"]])

  args2c <- get_args(points2)
  expect_true(args2c[["raw_curves"]])
})

test_that("as.data.frame raw_point option mmpoints", {
  get_args <- function(x, y = NULL, ...) {
    .get_dataframe_arglist(attr(x, "args"),
                           def_raw_curves = NULL, ...)
  }

  points1 <- df2_create_mmpoints()

  args1a <- get_args(points1, raw_curves = TRUE)
  expect_true(args1a[["raw_curves"]])

  args1b <- get_args(points1, raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- get_args(points1)
  expect_false(args1c[["raw_curves"]])

  points2 <- df2_create_mmpoints(raw_curves = TRUE)

  args2a <- get_args(points2, raw_curves = TRUE)
  expect_true(args2a[["raw_curves"]])

  args2b <- get_args(points2, raw_curves = FALSE)
  expect_false(args2b[["raw_curves"]])

  args2c <- get_args(points2)
  expect_true(args2c[["raw_curves"]])
})
