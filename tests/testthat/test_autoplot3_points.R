library(precrec)

context("AP 3: Autoplot for points")
# Test autoplot(object, ...)

ap3_check_libs <- function() {
  if (requireNamespace("ggplot2", quietly = TRUE)
      && requireNamespace("grid", quietly = TRUE)
      && requireNamespace("gridExtra", quietly = TRUE)) {
    TRUE
  } else {
    FALSE
  }
}

ap3_create_mspoints <- function() {
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

ap3_create_smpoints <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels, expd_first = "dsids")
  evalmod(mdat, mode = "basic")
}

ap3_create_mmpoints <- function() {
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
  evalmod(mdat, mode = "basic")
}

test_that("autoplot sspoints", {
  if (!ap3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  data(P10N10)
  curves <- evalmod(mode = "basic", scores = P10N10$scores,
                    labels = P10N10$labels)

  pp <- ggplot2::autoplot(curves, ret_grob = TRUE)
  expect_true(all(class(pp) == c("gtable", "grob", "gDesc")))
})

test_that("autoplot mspoints", {
  if (!ap3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ap3_create_mspoints()

  pp <- ggplot2::autoplot(curves, show_legend = FALSE, ret_grob = TRUE)
  expect_true(all(class(pp) == c("gtable", "grob", "gDesc")))
})

test_that("autoplot smpoints", {
  if (!ap3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ap3_create_smpoints()

  pp <- ggplot2::autoplot(curves, show_legend = FALSE, ret_grob = TRUE)
  expect_true(all(class(pp) == c("gtable", "grob", "gDesc")))
})

test_that("autoplot mmpoints", {
  if (!ap3_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ap3_create_mmpoints()

  pp <- ggplot2::autoplot(curves, show_legend = FALSE, ret_grob = TRUE)
  expect_true(all(class(pp) == c("gtable", "grob", "gDesc")))
})

