#' @importFrom precrec

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

test_that("'mode' must be consistent between 'mmdata' and 'evalmode'", {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 0)
  l2 <- c(1, 1, 0, 0)
  l3 <- c(0, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  md1 <- mmdata(scores, labels)
  expect_equal(attr(md1, "args")[["mode"]], "rocprc")

  expect_error(evalmod(md1), NA)
  em1_1 <- evalmod(md1)
  expect_equal(attr(evalmod(md1), "args")[["mode"]], "rocprc")

  expect_error(evalmod(md1, mode = 'rocprc'), NA)
  em1_2 <- evalmod(md1, mode = 'rocprc')
  expect_equal(attr(em1_2, "args")[["mode"]], "rocprc")

  expect_error(evalmod(md1, mode = 'basic'), NA)
  em1_3 <- evalmod(md1, mode = 'basic')
  expect_equal(attr(em1_3, "args")[["mode"]], "basic")

  expect_error(evalmod(md1, mode = 'aucroc'), NA)
  em1_4 <- evalmod(md1, mode = 'aucroc')
  expect_equal(attr(em1_4, "args")[["mode"]], "aucroc")


  md2 <- mmdata(scores, labels, mode = 'basic')
  expect_equal(attr(md2, "args")[["mode"]], "basic")

  expect_error(evalmod(md2), NA)
  em2_1 <- evalmod(md2)
  expect_equal(attr(em2_1, "args")[["mode"]], "basic")

  expect_error(evalmod(md2, mode = 'rocprc'), NA)
  em2_2 <- evalmod(md2, mode = 'rocprc')
  expect_equal(attr(em2_2, "args")[["mode"]], "rocprc")

  expect_error(evalmod(md2, mode = 'basic'), NA)
  em2_3 <- evalmod(md2, mode = 'basic')
  expect_equal(attr(em2_3, "args")[["mode"]], "basic")

  expect_error(evalmod(md2, mode = 'aucroc'), NA)
  em2_4 <- evalmod(md2, mode = 'aucroc')
  expect_equal(attr(em2_4, "args")[["mode"]], "aucroc")


  md3 <- mmdata(scores, labels, mode = 'aucroc')
  expect_equal(attr(md3, "args")[["mode"]], "aucroc")

  expect_error(evalmod(md3), NA)
  em3_1 <- evalmod(md3)
  expect_equal(attr(em3_1, "args")[["mode"]], "aucroc")

  expect_error(evalmod(md3, mode = 'rocprc'), "Invalid 'mode':")

  expect_error(evalmod(md3, mode = 'basic'), "Invalid 'mode':")

  expect_error(evalmod(md3, mode = 'aucroc'), NA)
  em3_4 <- evalmod(md3, mode = 'aucroc')
  expect_equal(attr(em3_4, "args")[["mode"]], "aucroc")

})
