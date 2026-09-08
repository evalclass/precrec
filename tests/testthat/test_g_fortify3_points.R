# FT 3: Fortify points
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

  mdat <- mmdata(scores, labels,
    modnames = c("m1", "m2"), dsids = c(1, 2),
    expd_first = "modnames"
  )
  evalmod(mdat, mode = "basic", raw_curves = raw_curves)
}

test_that("fortify sspoints", {
  if (!ft3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  data(P10N10)
  sspoints <- evalmod(
    scores = P10N10$scores, labels = P10N10$labels,
    mode = "basic"
  )

  point_df <- ggplot2::fortify(sspoints)
  expect_true(is.list(point_df))
})

test_that("fortify sspoints - dsid_modname", {
  if (!ft3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  data(P10N10)
  sspoints <- evalmod(
    scores = P10N10$scores, labels = P10N10$labels,
    mode = "basic"
  )

  point_df <- ggplot2::fortify(sspoints)
  expect_equal(
    as.character(point_df$dsid_modname),
    paste(point_df$modname, point_df$dsid, sep = ":")
  )
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
  expect_equal(
    as.character(point_df$dsid_modname),
    paste(point_df$modname, point_df$dsid, sep = ":")
  )
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
  expect_equal(
    as.character(point_df$dsid_modname),
    paste(point_df$modname, point_df$dsid, sep = ":")
  )
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
  expect_equal(
    as.character(point_df$dsid_modname),
    paste(point_df$modname, point_df$dsid, sep = ":")
  )

  samp1 <- create_sim_samples(5, 10, 10, c("random", "poor_er", "good_er"))
  samp1$modnames <- rep(c("random", "poor_er", "good_er"), each = 5)
  samp1$dsids <- rep(1:5, 3)

  mmpoints <- evalmod(
    mode = "basic", scores = samp1$scores,
    labels = samp1$labels,
    modnames = samp1$modnames, dsids = samp1$dsids,
    raw_curves = TRUE
  )
  mmpoints_df <- fortify(mmpoints, raw_curves = TRUE)

  expect_equal(
    unique(paste0(mmpoints_df$modname, ":", mmpoints_df$dsid)),
    as.character(unique(mmpoints_df$dsid_modname))
  )
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

ft3_create_big_sspoints <- function(n = 4000, ...) {
  set.seed(1)
  evalmod(
    scores = runif(n), labels = rbinom(n, 1, 0.5), mode = "basic", ...
  )
}

test_that("fortify reduce_points thins the basic metrics", {
  if (!ft3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  points1 <- ft3_create_big_sspoints()
  nmetrics <- length(attr(points1, "metrics"))

  full <- ggplot2::fortify(points1, reduce_points = FALSE)
  reduced <- ggplot2::fortify(points1, reduce_points = TRUE)

  # One point per cutoff without reduction, x_bins per metric with it
  expect_equal(nrow(full), nmetrics * 4001)
  expect_equal(nrow(reduced), nmetrics * 1000)

  # FALSE is the default, so an unasked-for call is unchanged
  expect_equal(ggplot2::fortify(points1), full)
})

test_that("Reduced basic metrics keep both ends of the x axis", {
  if (!ft3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  points1 <- ft3_create_big_sspoints()
  reduced <- ggplot2::fortify(points1, reduce_points = TRUE)

  for (metric in c("sensitivity", "specificity", "precision")) {
    xs <- reduced[reduced[["curvetype"]] == metric, "x"]
    expect_equal(xs[1], 0)
    expect_equal(xs[length(xs)], 1)
    # A thinned axis is still an axis: strictly increasing, no repeats
    expect_true(all(diff(xs) > 0))
  }
})

test_that("x_bins sets how many basic points are kept", {
  if (!ft3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  points1 <- ft3_create_big_sspoints(x_bins = 100)
  expect_equal(attr(points1, "args")[["x_bins"]], 100)

  reduced <- ggplot2::fortify(points1, reduce_points = TRUE)
  nmetrics <- length(attr(points1, "metrics"))
  expect_equal(nrow(reduced), nmetrics * 100)
})

test_that("Reduction drops nothing when there is little to drop", {
  if (!ft3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  # 51 cutoffs against x_bins of 1000, so every point is kept and asking for
  # the reduction has to be a no-op rather than a resampling
  points1 <- ft3_create_big_sspoints(n = 50)

  expect_equal(
    ggplot2::fortify(points1, reduce_points = TRUE),
    ggplot2::fortify(points1, reduce_points = FALSE)
  )
})

test_that("Reduced values are the calculated ones, not new ones", {
  if (!ft3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  points1 <- ft3_create_big_sspoints()
  full <- ggplot2::fortify(points1, reduce_points = FALSE)
  reduced <- ggplot2::fortify(points1, reduce_points = TRUE)

  # Every kept row must appear untouched in the unreduced frame: thinning
  # selects points, it does not interpolate between them
  keys <- function(d) paste(d[["curvetype"]], d[["x"]], d[["y"]])
  expect_true(all(keys(reduced) %in% keys(full)))
})
