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

ft3_create_mscurves <- function() {
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

ft3_create_smcurves <- function(raw_curves = FALSE) {
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

ft3_create_mmcurves <- function(raw_curves = FALSE) {
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
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels,
                    mode = "basic")

  curve_df <- ggplot2::fortify(curves)
  expect_true(is.list(curve_df))
})

test_that("fortify mspoints", {
  if (!ft3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ft3_create_mscurves()

  curve_df <- ggplot2::fortify(curves)
  expect_true(is.list(curve_df))
})

test_that("fortify smpoints", {
  if (!ft3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ft3_create_smcurves(raw_curves = TRUE)

  curve_df <- ggplot2::fortify(curves)
  expect_true(is.list(curve_df))
})

test_that("fortify mmpoints", {
  if (!ft3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ft3_create_mmcurves(raw_curves = TRUE)

  curve_df <- ggplot2::fortify(curves)
  expect_true(is.list(curve_df))
})

test_that("fortify raw_curve option smpoints", {
  curves1 <- ft3_create_smcurves()

  args1a <- .get_fortify_arglist(attr(curves1, "args"), def_raw_curves = TRUE)
  expect_true(args1a[["raw_curves"]])

  args1b <- .get_fortify_arglist(attr(curves1, "args"), def_raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- .get_fortify_arglist(attr(curves1, "args"), def_raw_curves = NULL)
  expect_false(args1c[["raw_curves"]])


  curves2 <- ft3_create_smcurves(raw_curves = TRUE)

  args2a <- .get_fortify_arglist(attr(curves2, "args"), def_raw_curves = TRUE)
  expect_true(args2a[["raw_curves"]])

  args2b <- .get_fortify_arglist(attr(curves2, "args"), def_raw_curves = FALSE)
  expect_false(args2b[["raw_curves"]])

  args2c <- .get_fortify_arglist(attr(curves2, "args"), def_raw_curves = NULL)
  expect_true(args2c[["raw_curves"]])

})

test_that("fortify raw_curve option mmpoints", {
  curves1 <- ft3_create_mmcurves()

  args1a <- .get_fortify_arglist(attr(curves1, "args"), def_raw_curves = TRUE)
  expect_true(args1a[["raw_curves"]])

  args1b <- .get_fortify_arglist(attr(curves1, "args"), def_raw_curves = FALSE)
  expect_false(args1b[["raw_curves"]])

  args1c <- .get_fortify_arglist(attr(curves1, "args"), def_raw_curves = NULL)
  expect_false(args1c[["raw_curves"]])

  curves2 <- ft3_create_mmcurves(raw_curves = TRUE)

  args2a <- .get_fortify_arglist(attr(curves2, "args"), def_raw_curves = TRUE)
  expect_true(args2a[["raw_curves"]])

  args2b <- .get_fortify_arglist(attr(curves2, "args"), def_raw_curves = FALSE)
  expect_false(args2b[["raw_curves"]])

  args2c <- .get_fortify_arglist(attr(curves2, "args"), def_raw_curves = NULL)
  expect_true(args2c[["raw_curves"]])

})
