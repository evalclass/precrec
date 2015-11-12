library(precrec)

context("PT 3: Plot points")
# Test plot(x, y, ...)

pt3_create_mspoints <- function() {
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

pt3_create_smpoints <- function(raw_curves = FALSE) {
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

pt3_create_mmpoints <- function(raw_curves = FALSE) {
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

ap3_test_basic_measures <- function(points, ...){

  expect_that(plot(points, ...), not(throws_error()))
  expect_that(plot(points, c("sensitivity", "specificity", "error",
                             "accuracy", "precision"), ...),
              not(throws_error()))
  expect_that(plot(points, c("sensitivity", "specificity", "error",
                             "precision"), ...), not(throws_error()))
  expect_that(plot(points, c("sensitivity", "specificity", "precision"),
                   ...), not(throws_error()))
  expect_that(plot(points, c("sensitivity", "precision"),
                   ...), not(throws_error()))

  expect_that(plot(points, "precision", ...), not(throws_error()))
}

test_that("plot sspoints", {
  pdf(NULL)
  on.exit(dev.off())

  data(P10N10)
  points <- evalmod(scores = P10N10$scores, labels = P10N10$labels,
                    mode = "basic")

  ap3_test_basic_measures(points)
  ap3_test_basic_measures(points, type = "l")
  ap3_test_basic_measures(points, type = "b")
})

test_that("plot mspoints", {
  pdf(NULL)
  on.exit(dev.off())

  points <- pt3_create_mspoints()

  ap3_test_basic_measures(points)
  ap3_test_basic_measures(points, type = "l")
  ap3_test_basic_measures(points, type = "b")
  ap3_test_basic_measures(points, show_legend = TRUE)
})

test_that("plot smpoints", {
  pdf(NULL)
  on.exit(dev.off())

  points <- pt3_create_smpoints()

  ap3_test_basic_measures(points)
  ap3_test_basic_measures(points, type = "l")
  ap3_test_basic_measures(points, type = "b")
  ap3_test_basic_measures(points, show_cb = FALSE)

  points2 <- pt3_create_smpoints(raw_curves = TRUE)
  ap3_test_basic_measures(points2, raw_curves = TRUE)
})

test_that("plot mmpoints", {
  pdf(NULL)
  on.exit(dev.off())

  points <- pt3_create_mmpoints()

  ap3_test_basic_measures(points)
  ap3_test_basic_measures(points, type = "l")
  ap3_test_basic_measures(points, type = "b")
  ap3_test_basic_measures(points, show_cb = TRUE)
  ap3_test_basic_measures(points, show_legend = FALSE)

  points2 <- pt3_create_mmpoints(raw_curves = TRUE)
  ap3_test_basic_measures(points2, raw_curves = TRUE)
})
