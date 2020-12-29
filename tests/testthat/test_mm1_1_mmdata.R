#' @importFrom precrec

context("MM 1: Create mmdata")
# Test .pmatch_tiesmethod(val),
#      .pmatch_expd_first(val),
#      mmdata(scores, labels, modnames, dsids,
#             posclass, na_worst, ties_method, expd_first, ...)

test_that(".pmatch_tiesmethod() returns 'equiv', 'random', 'first'", {
  expect_equal(.pmatch_tiesmethod("equiv"), "equiv")
  expect_equal(.pmatch_tiesmethod("random"), "random")
  expect_equal(.pmatch_tiesmethod("first"), "first")

  expect_equal(.pmatch_tiesmethod("e"), "equiv")
  expect_equal(.pmatch_tiesmethod("r"), "random")
  expect_equal(.pmatch_tiesmethod("f"), "first")

  expect_equal(.pmatch_tiesmethod("A"), "A")
  expect_equal(.pmatch_tiesmethod(1), 1)
  expect_equal(.pmatch_tiesmethod(NULL), NULL)
})

test_that(".pmatch_expd_first() returns 'dsids' or 'modnames'", {
  expect_equal(.pmatch_expd_first("dsids"), "dsids")
  expect_equal(.pmatch_expd_first("modnames"), "modnames")

  expect_equal(.pmatch_expd_first("d"), "dsids")
  expect_equal(.pmatch_expd_first("m"), "modnames")

  expect_equal(.pmatch_expd_first("A"), "A")
  expect_equal(.pmatch_expd_first(1), 1)
  expect_equal(.pmatch_expd_first(NULL), NULL)
})

test_that("mmdata() returns an 'mdat' object", {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 0)
  l2 <- c(1, 1, 0, 0)
  l3 <- c(0, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels)

  expect_true(is(mdat, "mdat"))
  expect_equal(length(mdat), 3)

})

test_that("'scores' and 'labels' must be specified", {
  expect_err_msg <- function(scores, labels) {
    err_msg <- "Cannot join this type of data"
    eval(bquote(expect_error(mmdata(scores, labels), err_msg)))
  }

  scores <- NULL
  labels <- 0

  expect_err_msg(scores, labels)
})

test_that("'scores' and 'labels' should be the same lengths", {
  expect_err_msg <- function(scores, labels) {
    err_msg <- paste0("scores and labels must be the same lengths")
    eval(bquote(expect_error(mmdata(scores, labels), err_msg)))
  }

  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2)

  expect_err_msg(scores, labels)
})

test_that("mmdata() accepts 'mode'", {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 0)
  l2 <- c(1, 1, 0, 0)
  l3 <- c(0, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels, mode = "aucroc")

  expect_true(is(mdat, "mdat"))
  expect_equal(length(mdat), 3)

  expect_true(is(mdat[[1]], "sdat"))
  expect_true(is(mdat[[2]], "sdat"))
  expect_true(is(mdat[[3]], "sdat"))
})

test_that("mmdata() accepts 'modnames'", {
  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)

  mdat <- mmdata(s1, l1, modnames = "model1")
  expect_equal(attr(mdat[[1]], "args")[["modname"]], "model1")

  expect_err_msg <- function(err_msg, s1, l1, modnames) {
    eval(bquote(expect_error(mmdata(s1, l1, modnames = modnames),
                             err_msg)))
  }

  err_msg <- "Invalid"
  expect_err_msg(err_msg, s1, l1, c("A", "B"))

  err_msg <- "modnames is not a character vector"
  expect_err_msg(err_msg, s1, l1, NA)

})

test_that("mmdata() accepts 'dsids'", {
  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)

  mdat <- mmdata(s1, l1, dsids = 10)
  expect_equal(attr(mdat[[1]], "args")[["dsid"]], 10)

  expect_err_msg <- function(err_msg, s1, l1, dsids) {
    eval(bquote(expect_error(mmdata(s1, l1, dsids = dsids), err_msg)))
  }

  err_msg <- "Invalid"
  expect_err_msg(err_msg, s1, l1, c("A", "B"))

  err_msg <- "dsids is not a numeric or integer vector"
  expect_err_msg(err_msg, s1, l1, NA)

})

test_that("mmdata() accepts 'posclass'", {
  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)

  mdat <- mmdata(s1, l1, posclass = 0)
  expect_equal(attr(mdat[[1]], "args")[["posclass"]], 0)

  mdat <- mmdata(s1, l1, posclass = 1)
  expect_equal(attr(mdat[[1]], "args")[["posclass"]], 1)

  expect_err_msg <- function(s1, l1, posclass, err_msg) {
    eval(bquote(expect_error(mmdata(s1, l1, posclass = posclass), err_msg)))
  }
  expect_err_msg(s1, l1, -1, "invalid-posclass")

  err_msg <- "posclass must be the same data type as labels"
  expect_err_msg(s1, l1, "0", err_msg)
  expect_err_msg(s1, l1, "1", err_msg)

})

test_that("mmdata() accepts 'na_worst'", {
  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)

  mdat <- mmdata(s1, l1, na_worst = FALSE)
  expect_equal(attr(mdat[[1]], "args")[["na_worst"]], FALSE)

  mdat <- mmdata(s1, l1, na_worst = TRUE)
  expect_equal(attr(mdat[[1]], "args")[["na_worst"]], TRUE)

  expect_err_msg <- function(s1, l1, na_worst) {
    err_msg <- "na_worst contains 1 missing values"
    eval(bquote(expect_error(mmdata(s1, l1, na_worst = na_worst), err_msg)))
  }
  expect_err_msg(s1, l1, as.logical(NA))
  expect_err_msg(s1, l1, NA)

})

test_that("mmdata() accepts 'ties_method'", {
  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)

  mdat <- mmdata(s1, l1, ties_method = "equiv")
  expect_equal(attr(mdat[[1]], "args")[["ties_method"]], "equiv")

  mdat <- mmdata(s1, l1, ties_method = "random")
  expect_equal(attr(mdat[[1]], "args")[["ties_method"]], "random")

  mdat <- mmdata(s1, l1, ties_method = "first")
  expect_equal(attr(mdat[[1]], "args")[["ties_method"]], "first")

  expect_err_msg <- function(s1, l1, ties_method) {
    err_msg <- "ties_method must be one of "
    eval(bquote(expect_error(mmdata(s1, l1, ties_method = ties_method),
                             err_msg)))
  }
  expect_err_msg(s1, l1, "min")
  expect_err_msg(s1, l1, "max")

})

test_that("mmdata() accepts 'expd_first", {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(1, 1, 0, 1)
  l3 <- c(0, 1, 1, 1)
  labels <- join_labels(l1, l2, l3)

  dlen <- 3

  mdat1 <- mmdata(scores, labels, expd_first = "modnames")
  expect_equal(attr(mdat1, "data_info")[["modnames"]], c("m1", "m2", "m3"))
  expect_equal(attr(mdat1, "data_info")[["dsids"]], rep(1, 3))

  mdat2 <- mmdata(scores, labels, expd_first = "dsids")
  expect_equal(attr(mdat2, "data_info")[["modnames"]], rep("m1", 3))
  expect_equal(attr(mdat2, "data_info")[["dsids"]], seq(3))
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

  l1 <- c(1, 0, 1, 0)
  l2 <- c(1, 1, 0, 0)
  l3 <- c(0, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels)

  expect_true(is.list(mdat))
  expect_equal(length(mdat), 3)
})

test_that("mmdata() accepts only one 'labels' dataset", {
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

test_that("All items in 'scores' and 'labels' must be the same lengths", {
  expect_err_msg <- function(scores, labels) {
    err_msg <- "scores and labels must be the same lengths"
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
