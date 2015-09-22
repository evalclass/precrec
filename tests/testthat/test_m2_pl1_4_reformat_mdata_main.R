context("M2 PL1: Create mfmdat for evalmulti")
# Test reformat_mdata(arg:mscores, arg:mobslabs)

test_that("arg:mscores and arg:mobslabs must be specified", {
  expect_err_msg <- function(mscores, mobslabs) {
    err_msg <- "Incorrect type of data"
    eval(bquote(expect_error(reformat_mdata(mscores, mobslabs), err_msg)))
  }

  mscores <- NULL
  mobslabs <- c(0)

  expect_err_msg(mscores, mobslabs)
})

test_that("arg:mscores and arg:mobslabs should be the same length", {
  expect_err_msg <- function(mscores, mobslabs) {
    err_msg <- paste0("'mscores' and 'mobslabs' should be ",
                      "of the same size, or ",
                      "the length of 'mobslabs' should be 1")
    eval(bquote(expect_error(reformat_mdata(mscores, mobslabs), err_msg)))
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

test_that("create_mdat() reterns a 'mfmdat' object", {
  mfmdat <- reformat_mdata(c(0.1, 0.2, 0), c(1, 0, 1))

  expect_equal(class(mfmdat), "mfmdat")
})

test_that("'mfmdat' contains a list", {
  mfmdat <- reformat_mdata(c(0.1, 0.2, 0), c(1, 0, 1))

  expect_true(is.list(mfmdat))
  expect_equal(length(mfmdat), 1)
})

test_that("'mfmdat' contains a list with 3 items", {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  mscores <- combine_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(1, 1, 0, 0)
  l3 <- c(0, 1, 0, 1)
  mobslabs <- combine_obslbs(l1, l2, l3)

  mfmdat <- reformat_mdata(mscores, mobslabs)

  expect_true(is.list(mfmdat))
  expect_equal(length(mfmdat), 3)
})

test_that("reformat_mdata() can take only one 'mobslabs' dataset", {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  mscores <- combine_scores(s1, s2, s3)

  l1 <- c(2, 1, 2, 2)
  mobslabs <- combine_obslbs(l1)

  mfmdat <- reformat_mdata(mscores, mobslabs)

  m1l1 <- mfmdat[[1]][["olabs"]]
  m2l1 <- mfmdat[[2]][["olabs"]]
  m3l1 <- mfmdat[[3]][["olabs"]]

  expect_equal(l1, as.numeric(m1l1))
  expect_equal(l1, as.numeric(m2l1))
  expect_equal(l1, as.numeric(m3l1))
})

test_that("All items in 'mscores' and 'mobslabs' must be of the same length", {
  expect_err_msg <- function(mscores, mobslabs) {
    err_msg <- "'pscores' and 'olabs' must be of the same length"
    eval(bquote(expect_error(reformat_mdata(mscores, mobslabs), err_msg)))
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

