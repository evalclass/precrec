context("M2 PL1: Create mdat for evalmulti")
# Test create_mdat(arg:mscores, arg:mobslabs)

test_that("arg:mscores and arg:mobslabs must be specified", {
  expect_err_msg <- function(mscores, mobslabs) {
    err_msg <- "Incorrect type of data"
    eval(bquote(expect_error(create_mdat(mscores, mobslabs), err_msg)))
  }

  mscores <- NULL
  mobslabs <- c(0)

  expect_err_msg(mscores, mobslabs)
})

test_that("arg:mscores and arg:obslabs should be the same length", {
  expect_err_msg <- function(mscores, mobslabs) {
    err_msg <- paste0("'mscores' and 'mobslabs' should be ",
                      "of the same size, or ",
                      "the length of 'mobslabs' should be 1")
    eval(bquote(expect_error(create_mdat(mscores, mobslabs), err_msg)))
  }

  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  mscores <- combine_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(1, 1, 0, 0)
  mobslabs <- combine_obslbs(l1, l2)

  expect_err_msg(mscores, mobslabs)
})

test_that("'mdat' contains a list with 2 items", {
  mdat <- create_mdat(c(0.1, 0.2, 0), c(1, 0, 1))

  expect_true(is.list(mdat))
  expect_equal(length(mdat), 2)
})


test_that("create_mdat() reterns a 'mdat' object", {
  mdat <- create_mdat(c(0.1, 0.2, 0), c(1, 0, 1))

  expect_equal(class(mdat), "mdat")
})

test_that("'mdat' contains a list with 2 items", {
  mdat <- create_mdat(c(0.1, 0.2, 0), c(1, 0, 1))

  expect_true(is.list(mdat))
  expect_equal(length(mdat), 2)
})

test_that("'mdat' contains sdat objects", {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  mscores <- combine_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(1, 1, 0, 0)
  l3 <- c(0, 1, 0, 1)
  mobslabs <- combine_obslbs(l1, l2, l3)

  mdat <- create_mdat(mscores, mobslabs)

  expect_equal(length(mdat[["mdat"]]), 3)
  expect_equal(names(mdat[["mdat"]]), c("m1", "m2", "m3"))
  expect_equal(class(mdat[["mdat"]][["m1"]]), "sdat")
})

test_that("create_mdat() can take only one 'mobslabs' dataset", {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  mscores <- combine_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  mobslabs <- combine_obslbs(l1)

  mdat <- create_mdat(mscores, mobslabs)

  m1l1 <- mdat[["mdat"]][["m1"]][["sdat"]][["obslabs"]]
  m2l1 <- mdat[["mdat"]][["m2"]][["sdat"]][["obslabs"]]
  m3l1 <- mdat[["mdat"]][["m3"]][["sdat"]][["obslabs"]]

  expect_equal(l1, m1l1)
  expect_equal(l1, m2l1)
  expect_equal(l1, m3l1)
})

test_that("All items in 'mscores' and 'mobslabs' must be of the same length", {
  expect_err_msg <- function(mscores, mobslabs) {
    err_msg <- "'scores' and 'obslabs' must be of the same length"
    eval(bquote(expect_error(create_mdat(mscores, mobslabs), err_msg)))
  }

  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  mscores <- combine_scores(s1, s2, s3)

  l1 <- c(1, 0, 1)
  l2 <- c(1, 1, 0)
  l3 <- c(0, 1, 0)
  mobslabs <- combine_obslbs(l1, l2, l3)

  expect_err_msg(mscores, mobslabs)
})

