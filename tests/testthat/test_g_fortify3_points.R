#' @importFrom precrec

context("FT 3: Fortify points")
# Test fortify(model, ...)

ft3_check_libs <- function() {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    TRUE
  } else {
    FALSE
  }
}

ft3_create_mspoints <- function() {
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

ft3_create_smpoints <- function(raw_curves = FALSE) {
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

ft3_create_mmpoints <- function(raw_curves = FALSE) {
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

test_that("fortify sspoints", {
  if (!ft3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  data(P10N10)
  sspoints <- evalmod(scores = P10N10$scores, labels = P10N10$labels,
                      mode = "basic")

  point_df <- ggplot2::fortify(sspoints)
  expect_true(is.list(point_df))
})

test_that("fortify sspoints - dsid_modname", {
  if (!ft3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  data(P10N10)
  sspoints <- evalmod(scores = P10N10$scores, labels = P10N10$labels,
                      mode = "basic")

  point_df <- ggplot2::fortify(sspoints)
  expect_equal(as.character(point_df$dsid_modname),
               paste(point_df$modname, point_df$dsid, sep = ":"))
})

test_that("fortify mspoints", {
  if (!ft3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  mspoints <- ft3_create_mspoints()

  point_df <- ggplot2::fortify(mspoints)
  expect_true(is.list(point_df))
})

test_that("fortify mspoints - dsid_modname", {
  if (!ft3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  mspoints <- ft3_create_mspoints()

  point_df <- ggplot2::fortify(mspoints)
  expect_equal(as.character(point_df$dsid_modname),
               paste(point_df$modname, point_df$dsid, sep = ":"))
})

test_that("fortify smpoints", {
  if (!ft3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  smpoints <- ft3_create_smpoints(raw_curves = TRUE)

  point_df <- ggplot2::fortify(smpoints)
  expect_true(is.list(point_df))
})

test_that("fortify smpoints - dsid_modname", {
  if (!ft3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  smpoints <- ft3_create_smpoints(raw_curves = TRUE)

  point_df <- ggplot2::fortify(smpoints)
  expect_equal(as.character(point_df$dsid_modname),
               paste(point_df$modname, point_df$dsid, sep = ":"))
})

test_that("fortify mmpoints", {
  if (!ft3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  mmpoints <- ft3_create_mmpoints(raw_curves = TRUE)

  point_df <- ggplot2::fortify(mmpoints)
  expect_true(is.list(point_df))
})

test_that("fortify mmpoints - dsid_modname", {
  if (!ft3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  mmpoints <- ft3_create_mmpoints(raw_curves = TRUE)

  point_df <- ggplot2::fortify(mmpoints)
  expect_equal(as.character(point_df$dsid_modname),
               paste(point_df$modname, point_df$dsid, sep = ":"))

  samp1 <- create_sim_samples(5, 10, 10, c("random", "poor_er", "good_er"))
  samp1$modnames <- rep(c("random", "poor_er", "good_er"), each = 5)
  samp1$dsids <- rep(1:5, 3)

  mmpoints <- evalmod(mode = "basic", scores = samp1$scores, labels = samp1$labels,
                      modnames = samp1$modnames, dsids = samp1$dsids,
                      raw_curves = TRUE)
  mmpoints_df <- fortify(mmpoints, raw_curves = TRUE)

  expect_equal(unique(paste0(mmpoints_df$modname, ":", mmpoints_df$dsid)),
               as.character(unique(mmpoints_df$dsid_modname)))
})

test_that("fortify raw_curve option smpoints", {
  points1 <- ft3_create_smpoints()

  args1a <- .get_fortify_arglist(attr(points1, "args"), def_raw_curves = TRUE)
  expect_true(args1a[["raw_curves"]])

  args1b <- .get_fortify_arglist(attr(points1, "args"), def_raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- .get_fortify_arglist(attr(points1, "args"), def_raw_curves = NULL)
  expect_false(args1c[["raw_curves"]])


  points2 <- ft3_create_smpoints(raw_curves = TRUE)

  args2a <- .get_fortify_arglist(attr(points2, "args"), def_raw_curves = TRUE)
  expect_true(args2a[["raw_curves"]])

  args2b <- .get_fortify_arglist(attr(points2, "args"), def_raw_curves = FALSE)
  expect_false(args2b[["raw_curves"]])

  args2c <- .get_fortify_arglist(attr(points2, "args"), def_raw_curves = NULL)
  expect_true(args2c[["raw_curves"]])

})

test_that("fortify raw_curve option mmpoints", {
  points1 <- ft3_create_mmpoints()

  args1a <- .get_fortify_arglist(attr(points1, "args"), def_raw_curves = TRUE)
  expect_true(args1a[["raw_curves"]])

  args1b <- .get_fortify_arglist(attr(points1, "args"), def_raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- .get_fortify_arglist(attr(points1, "args"), def_raw_curves = NULL)
  expect_false(args1c[["raw_curves"]])

  points2 <- ft3_create_mmpoints(raw_curves = TRUE)

  args2a <- .get_fortify_arglist(attr(points2, "args"), def_raw_curves = TRUE)
  expect_true(args2a[["raw_curves"]])

  args2b <- .get_fortify_arglist(attr(points2, "args"), def_raw_curves = FALSE)
  expect_false(args2b[["raw_curves"]])

  args2c <- .get_fortify_arglist(attr(points2, "args"), def_raw_curves = NULL)
  expect_true(args2c[["raw_curves"]])

})
