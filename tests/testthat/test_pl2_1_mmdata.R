library(precrec)

context("PL 2: Create mmdata")
# Test .pmatch_tiesmethod(val),
#      mmdata(scores, labels, model_names, setids,
#             na.last, ties.method, levels, ...)

test_that(".pmatch_tiesmethod() returns 'average', 'random', 'first'", {
  expect_equal(.pmatch_tiesmethod("a"), "average")
  expect_equal(.pmatch_tiesmethod("r"), "random")
  expect_equal(.pmatch_tiesmethod("f"), "first")

  expect_equal(.pmatch_tiesmethod("A"), "A")
  expect_equal(.pmatch_tiesmethod(1), 1)
  expect_equal(.pmatch_tiesmethod(NULL), NULL)
})

test_that("mmdata() returns an 'mdat' object", {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(1, 1, 0, 0)
  l3 <- c(0, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels)

  expect_equal(class(mdat), "mdat")
  expect_equal(length(mdat), 3)

})

test_that("'scores' and 'labels' must be specified", {
  expect_err_msg <- function(scores, labels) {
    err_msg <- "Cannot join this type of data"
    eval(bquote(expect_error(mmdata(scores, labels), err_msg)))
  }

  scores <- NULL
  labels <- c(0)

  expect_err_msg(scores, labels)
})

test_that("'scores' and 'labels' should be the same length", {
  expect_err_msg <- function(scores, labels) {
    err_msg <- paste0("'scores' and 'labels' should be ",
                      "of the same size, or ",
                      "the size of 'labels' should be 1")
    eval(bquote(expect_error(mmdata(scores, labels), err_msg)))
  }

  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(1, 1, 0, 0)
  labels <- join_labels(l1, l2)

  expect_err_msg(scores, labels)
})

test_that("mmdata() accepts 'model_names'", {
  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)

  mdat <- mmdata(s1, l1, model_names = "model1")
  expect_equal(attr(mdat[[1]], "args")[["model_name"]], "model1")

  expect_err_msg <- function(err_msg, s1, l1, model_names) {
    eval(bquote(expect_error(mmdata(s1, l1, model_names = model_names),
                             err_msg)))
  }

  err_msg <- "not equal to datalen"
  expect_err_msg(err_msg, s1, l1, c("A", "B"))

  err_msg <- "model_names is not a character vector"
  expect_err_msg(err_msg, s1, l1, NA)

})

test_that("mmdata() accepts 'setids'", {
  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)

  mdat <- mmdata(s1, l1, setids = 10)
  expect_equal(attr(mdat[[1]], "args")[["setid"]], 10)

  expect_err_msg <- function(err_msg, s1, l1, setids) {
    eval(bquote(expect_error(mmdata(s1, l1, setids = setids), err_msg)))
  }

  err_msg <- "not equal to datalen"
  expect_err_msg(err_msg, s1, l1, c("A", "B"))

  err_msg <- "is not TRUE"
  expect_err_msg(err_msg, s1, l1, NA)

})

test_that("mmdata() accepts 'na.last'", {
  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)

  mdat <- mmdata(s1, l1, na.last = FALSE)
  expect_equal(attr(mdat[[1]], "args")[["na.last"]], FALSE)

  mdat <- mmdata(s1, l1, na.last = TRUE)
  expect_equal(attr(mdat[[1]], "args")[["na.last"]], TRUE)

  expect_err_msg <- function(s1, l1, na.last) {
    err_msg <- "na.last contains 1 missing values"
    eval(bquote(expect_error(mmdata(s1, l1, na.last = na.last), err_msg)))
  }
  expect_err_msg(s1, l1, as.logical(NA))
  expect_err_msg(s1, l1, NA)

})

test_that("mmdata() accepts 'ties.method'", {
  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)

  mdat <- mmdata(s1, l1, ties.method = "average")
  expect_equal(attr(mdat[[1]], "args")[["ties.method"]], "average")

  mdat <- mmdata(s1, l1, ties.method = "random")
  expect_equal(attr(mdat[[1]], "args")[["ties.method"]], "random")

  mdat <- mmdata(s1, l1, ties.method = "first")
  expect_equal(attr(mdat[[1]], "args")[["ties.method"]], "first")

  expect_err_msg <- function(s1, l1, ties.method) {
    err_msg <- "ties.method should be one of "
    eval(bquote(expect_error(mmdata(s1, l1, ties.method = ties.method),
                             err_msg)))
  }
  expect_err_msg(s1, l1, "min")
  expect_err_msg(s1, l1, "max")

})

test_that("mmdata() accepts 'levels'", {
  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)

  mdat <- mmdata(s1, l1, levels = c("N", "P"))
  expect_equal(attr(mdat[[1]], "args")[["levels"]], c("N", "P"))

  expect_err_msg <- function(err_msg, s1, l1, levels) {
    eval(bquote(expect_error(mmdata(s1, l1, levels = levels), err_msg)))
  }

  err_msg <- "levels is not a character vector"
  expect_err_msg(err_msg, s1, l1, c(0, 1))

  err_msg <- "not equal to 2L"
  expect_err_msg(err_msg, s1, l1, c("N", "P", "P2"))

})

test_that("mmdata() accepts 'exp_priority", {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(1, 1, 0, 1)
  l3 <- c(0, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  dlen <- 3

  mdat1 <- mmdata(scores, labels, exp_priority = "model_names")
  expect_equal(attr(mdat1, "model_names"), c("m1", "m2", "m3"))
  expect_equal(attr(mdat1, "setids"), rep(1, 3))

  mdat2 <- mmdata(scores, labels, exp_priority = "setids")
  expect_equal(attr(mdat2, "model_names"), rep("m1", 3))
  expect_equal(attr(mdat2, "setids"), seq(3))
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
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(1, 1, 0, 0)
  l3 <- c(0, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels)

  expect_true(is.list(mdat))
  expect_equal(length(mdat), 3)
})

test_that("mmdata() can take only one 'labels' dataset", {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(2, 1, 2, 2)
  labels <- join_labels(l1)

  mdat <- mmdata(scores, labels)

  m1l1 <- mdat[[1]][["labels"]]
  m2l1 <- mdat[[2]][["labels"]]
  m3l1 <- mdat[[3]][["labels"]]

  expect_equal(l1, as.numeric(m1l1))
  expect_equal(l1, as.numeric(m2l1))
  expect_equal(l1, as.numeric(m3l1))
})

test_that("All items in 'scores' and 'labels' must be of the same length", {
  expect_err_msg <- function(scores, labels) {
    err_msg <- "scores and labels must be of the same length"
    eval(bquote(expect_error(mmdata(scores, labels), err_msg)))
  }

  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1)
  l2 <- c(1, 1, 0)
  l3 <- c(0, 1, 0)
  labels <- join_labels(l1, l2, l3)

  expect_err_msg(scores, labels)
})



