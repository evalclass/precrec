context("M2 PL1: Create mmdata")
# Test mmdata(arg:pscores, arg:olabs, arg:model_names, arg:data_nos,
#             arg:na.last, arg:ties.method, arg:olevs, arg:...)

test_that("arg:pscores and arg:olabs must be specified", {
  expect_err_msg <- function(pscores, olabs) {
    err_msg <- "Incorrect type of data"
    eval(bquote(expect_error(mmdata(pscores, olabs), err_msg)))
  }

  pscores <- NULL
  olabs <- c(0)

  expect_err_msg(pscores, olabs)
})

test_that("arg:pscores and arg:olabs should be the same length", {
  expect_err_msg <- function(pscores, olabs) {
    err_msg <- paste0("'pscores' and 'olabs' should be ",
                      "of the same size, or ",
                      "the size of 'olabs' should be 1")
    eval(bquote(expect_error(mmdata(pscores, olabs), err_msg)))
  }

  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  pscores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(1, 1, 0, 0)
  olabs <- join_labels(l1, l2)

  expect_err_msg(pscores, olabs)
})

test_that("mmdata() reterns a 'mmdata' object", {
  mdat <- mmdata(c(0.1, 0.2, 0), c(1, 0, 1))

  expect_equal(class(mdat), "mdat")
})

test_that("'mdat' contains a list", {
  mdat <- mmdata(c(0.1, 0.2, 0), c(1, 0, 1))

  expect_true(is.list(mdat))
  expect_equal(length(mdat), 1)
})

test_that("'mdat' contains a list with 3 items", {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  pscores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(1, 1, 0, 0)
  l3 <- c(0, 1, 0, 1)
  olabs <- join_labels(l1, l2, l3)

  mdat <- mmdata(pscores, olabs)

  expect_true(is.list(mdat))
  expect_equal(length(mdat), 3)
})

test_that("mmdata() can take only one 'olabs' dataset", {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  pscores <- join_scores(s1, s2, s3)

  l1 <- c(2, 1, 2, 2)
  olabs <- join_labels(l1)

  mdat <- mmdata(pscores, olabs)

  m1l1 <- mdat[[1]][["olabs"]]
  m2l1 <- mdat[[2]][["olabs"]]
  m3l1 <- mdat[[3]][["olabs"]]

  expect_equal(l1, as.numeric(m1l1))
  expect_equal(l1, as.numeric(m2l1))
  expect_equal(l1, as.numeric(m3l1))
})

test_that("All items in 'pscores' and 'olabs' must be of the same length", {
  expect_err_msg <- function(pscores, olabs) {
    err_msg <- "'pscores' and 'olabs' must be of the same length"
    eval(bquote(expect_error(mmdata(pscores, olabs), err_msg)))
  }

  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  pscores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1)
  l2 <- c(1, 1, 0)
  l3 <- c(0, 1, 0)
  olabs <- join_labels(l1, l2, l3)

  expect_err_msg(pscores, olabs)
})
