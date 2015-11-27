library(precrec)

context("MA 1: evalmod")
# Test evalmod()
#

test_that("m1 scores", {
  s1 <- c(3, 2, 2, 1)
  l1 <- c(1, 0, 1, 0)

  mdat1 <- mmdata(s1, l1)
  cv1 <- evalmod(mdat1, x_bins = 4)

  expect_equal(cv1[["rocs"]][[1]][["x"]], c(0, 0, 0.25, 0.5, 0.75, 1))
  expect_equal(cv1[["rocs"]][[1]][["y"]], c(0, 0.5, 0.75, 1, 1, 1))

  expect_equal(cv1[["prcs"]][[1]][["x"]], c(0, 0.25, 0.5, 0.75, 1, 1))
  expect_equal(cv1[["prcs"]][[1]][["y"]], c(1, 1, 1, 0.75, 0.6666666667, 0.5),
               tolerance = 1e-2)
})

test_that("m2 scores", {
  s2 <- c(4, 3, 2, 1)
  l2 <- c(0, 0, 1, 1)

  mdat2 <- mmdata(s2, l2)
  cv2 <- evalmod(mdat2, x_bins = 4)

  expect_equal(cv2[["rocs"]][[1]][["x"]], c(0, 0.25, 0.5, 0.75, 1, 1, 1))
  expect_equal(cv2[["rocs"]][[1]][["y"]], c(0, 0, 0, 0, 0, 0.5, 1))

  expect_equal(cv2[["prcs"]][[1]][["x"]], c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(cv2[["prcs"]][[1]][["y"]], c(0, 0.2, 0.3333333333, 0.4285714286,
                                           0.5),
               tolerance = 1e-2)
})

test_that("m3 scores", {
  s3 <- c(3, 3, 2, 1)
  l3 <- c(1, 0, 0, 1)

  mdat3 <- mmdata(s3, l3)
  cv3 <- evalmod(mdat3, x_bins = 4)

  expect_equal(cv3[["rocs"]][[1]][["x"]], c(0, 0.25, 0.5, 0.75, 1, 1))
  expect_equal(cv3[["rocs"]][[1]][["y"]], c(0, 0.25, 0.5, 0.5, 0.5, 1))

  expect_equal(cv3[["prcs"]][[1]][["x"]], c(0, 0.25, 0.5, 0.5, 0.75, 1))
  expect_equal(cv3[["prcs"]][[1]][["y"]], c(0.5, 0.5, 0.5, 0.3333333333,
                                           0.4285714286, 0.5),
               tolerance = 1e-2)
})
