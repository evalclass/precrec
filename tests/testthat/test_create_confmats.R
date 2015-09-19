context("Create confusion matrices")
# Test create_confmats(arg:fmdat, arg:scores, arg:obslabs)

test_that("arg:fmdat must be a 'fmdat' object", {
  expect_err_msg <- function(fmdat) {
    err_msg <- "An object of unknown class is specified"
    eval(bquote(expect_error(create_confmats(fmdat), err_msg)))
  }

  expect_err_msg(list())
  expect_err_msg(data.frame())
})

test_that("create_confmats() can directly take scores and labels", {
  fmdat <- reformat_data(c(0.1, 0.2, 0.2, 0), c(1, 0, 1, 1))
  cmats1 <- create_confmats(fmdat)
  cmats2 <- create_confmats(scores = c(0.1, 0.2, 0.2, 0),
                            obslabs = c(1, 0, 1, 1))

  expect_equal(cmats1, cmats2)
})

test_that("create_confmats() can take arguments for reformat_data()", {
  fmdat <- reformat_data(c(0.1, 0.2, 0.2, 0), c(1, 0, 1, 1),
                         ties.method = "first")
  cmats1 <- create_confmats(fmdat)
  cmats2 <- create_confmats(scores = c(0.1, 0.2, 0.2, 0),
                            obslabs = c(1, 0, 1, 1), ties.method = "first")

  expect_equal(cmats1, cmats2)

  err_msg <- "Invalid arguments: na.rm"
  expect_error(create_confmats(fmdat, na.rm = TRUE), err_msg)
})

test_that("create_confmats() reterns a 'cmats' object", {
  cmats <- create_confmats(scores = c(0.1, 0.2, 0), obslabs = c(1, 0, 1))

  expect_equal(class(cmats), "cmats")
})

test_that("'cmats' contains a list with 9 items", {
  cmats <- create_confmats(scores = c(0.1, 0.2, 0), obslabs = c(1, 0, 1))

  expect_true(is.list(cmats))
  expect_equal(length(cmats), 9)
})

test_that("TPs, FNs, FPs, and TNs must be the same length", {
  cmats <- create_confmats(scores = c(0.1, 0.2, 0), obslabs = c(1, 0, 1))
  vec_size <- length(cmats[["ranks"]])

  expect_true(vec_size != 0)
  expect_equal(vec_size, length(cmats[["tp"]]))
  expect_equal(vec_size, length(cmats[["fn"]]))
  expect_equal(vec_size, length(cmats[["fp"]]))
  expect_equal(vec_size, length(cmats[["tn"]]))
})

test_that("'cmats' contains correct items", {
  cmats <- create_confmats(scores = c(0.1, 0.2, 0), obslabs = c(1, 0, 1))
  np <- cmats[["pos_num"]]
  nn <- cmats[["neg_num"]]
  vec_size <- length(cmats[["ranks"]])

  expect_equal(cmats[["tp"]][1], 0)
  expect_equal(cmats[["fn"]][1], np)
  expect_equal(cmats[["fp"]][1], 0)
  expect_equal(cmats[["tn"]][1], nn)

  expect_equal(cmats[["tp"]][vec_size], np)
  expect_equal(cmats[["fn"]][vec_size], 0)
  expect_equal(cmats[["fp"]][vec_size], nn)
  expect_equal(cmats[["tn"]][vec_size], 0)
})

test_that("create_confmats() reterns correct matrices", {
  cmats <- create_confmats(scores = c(0.1, 0.2, 0, 0.3),
                           obslabs = c(1, 0, 0, 1))

  expect_equal(cmats[["pos_num"]], 2)
  expect_equal(cmats[["neg_num"]], 2)
  expect_equal(cmats[["tp"]], c(0, 1, 1, 2, 2))
  expect_equal(cmats[["fn"]], c(2, 1, 1, 0, 0))
  expect_equal(cmats[["fp"]], c(0, 0, 1, 1, 2))
  expect_equal(cmats[["tn"]], c(2, 2, 1, 1, 0))
})

test_that("create_confmats() handles tied scores 1", {
  cmats <- create_confmats(scores = c(0.3, 0.2, 0.2, 0.2, 0.2, 0.1),
                           obslabs = c(0, 1, 0, 1, 0, 1))
  expect_equal(cmats[["tp"]], c(0, 0, 0.5, 1, 1.5, 2, 3))
  expect_equal(cmats[["fp"]], c(0, 1, 1.5, 2, 2.5, 3, 3))
})

test_that("create_confmats() handles tied scores 2", {
  cmats <- create_confmats(scores = c(0.3, 0.2, 0.2, 0.2, 0.2),
                           obslabs = c(0, 1, 0, 1, 0))
  expect_equal(cmats[["tp"]], c(0, 0, 0.5, 1, 1.5, 2))
  expect_equal(cmats[["fp"]], c(0, 1, 1.5, 2, 2.5, 3))
})
