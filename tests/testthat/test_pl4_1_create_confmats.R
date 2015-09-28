context("PL4: Create confusion matrices")
# Test create_confmats(fmdat, pscores, olabs)

test_that("'fmdat' must be a 'fmdat' object", {
  expect_err_msg <- function(fmdat) {
    err_msg <- "Unrecognized class for .validate()"
    eval(bquote(expect_error(create_confmats(fmdat), err_msg)))
  }

  expect_err_msg(list())
  expect_err_msg(data.frame())
})

test_that("create_confmats() can directly take scores and labels", {
  fmdat <- reformat_data(c(0.1, 0.2, 0.2, 0), c(1, 0, 1, 1))
  cmats1 <- create_confmats(fmdat)
  cmats2 <- create_confmats(pscores = c(0.1, 0.2, 0.2, 0),
                            olabs = c(1, 0, 1, 1))

  expect_equal(cmats1, cmats2)
})

test_that("create_confmats() can take arguments for reformat_data()", {
  err_msg <- "Invalid arguments: na.rm"
  expect_error(create_confmats(pscores = c(0.1, 0.2, 0.2, 0),
                               olabs = c(1, 0, 1, 1), na.rm = TRUE),
               err_msg)

  cmats <- create_confmats(pscores = c(0.1, 0.2, 0),
                           olabs = c(1, 0, 1),
                           na.last = TRUE,
                           ties.method = "first")

  expect_equal(.get_obj_arg(cmats, "fmdat", "na.last"), TRUE)
  expect_equal(.get_obj_arg(cmats, "fmdat", "ties.method"), "first")
})

test_that("create_confmats() can take na.last argument", {
  expect_equal_ranks <- function(pscores, na.last, ranks) {
    cmats <- create_confmats(pscores = pscores,
                             olabs = c(1, 0, 1),
                             na.last = na.last)

    fmdat <- .get_obj(cmats, "fmdat")

    eval(bquote(expect_equal(.get_obj_arg(cmats, NULL, "na.last"), na.last)))
    eval(bquote(expect_equal(.get_obj_arg(fmdat, NULL, "na.last"), na.last)))
    eval(bquote(expect_equal(fmdat[["ranks"]], ranks)))
    eval(bquote(expect_equal(.rank_scores(pscores, na.last = na.last), ranks)))
  }

  na1_pscores <- c(NA, 0.2, 0.1)
  na2_pscores <- c(0.2, NA, 0.1)
  na3_pscores <- c(0.2, 0.1, NA)

  expect_equal_ranks(na1_pscores, TRUE, c(3, 2, 1))
  expect_equal_ranks(na1_pscores, FALSE, c(1, 3, 2))

  expect_equal_ranks(na2_pscores, TRUE, c(2, 3, 1))
  expect_equal_ranks(na2_pscores, FALSE, c(3, 1, 2))

  expect_equal_ranks(na3_pscores, TRUE, c(2, 1, 3))
  expect_equal_ranks(na3_pscores, FALSE, c(3, 2, 1))
})

test_that("create_confmats() can take ties.method argument", {

  expect_equal_ranks <- function(ties.method, ranks) {
    cmats <- create_confmats(pscores = c(0.1, 0.2, 0.2, 0.2, 0.3),
                             olabs = c(1, 0, 1, 1, 1),
                             ties.method = ties.method)

    fmdat <- .get_obj(cmats, "fmdat")

    eval(bquote(expect_equal(.get_obj_arg(cmats, NULL, "ties.method"),
                             ties.method)))
    eval(bquote(expect_equal(.get_obj_arg(fmdat, NULL, "ties.method"),
                             ties.method)))
    eval(bquote(expect_equal(fmdat[["ranks"]], ranks)))
  }

  expect_equal_ranks("average", c(1, 3, 3, 3, 5))
  expect_equal_ranks("first", c(1, 2, 3, 4, 5))

})

test_that("create_confmats() reterns a 'cmats' object", {
  cmats <- create_confmats(pscores = c(0.1, 0.2, 0), olabs = c(1, 0, 1))

  expect_equal(class(cmats), "cmats")
})

test_that("'cmats' contains a list with 7 items", {
  cmats <- create_confmats(pscores = c(0.1, 0.2, 0), olabs = c(1, 0, 1))

  expect_true(is.list(cmats))
  expect_equal(length(cmats), 7)
})

test_that("TPs, FNs, FPs, and TNs must be the same length", {
  cmats <- create_confmats(pscores = c(0.1, 0.2, 0), olabs = c(1, 0, 1))
  vec_size <- length(cmats[["ranks"]])

  expect_true(vec_size != 0)
  expect_equal(vec_size, length(cmats[["tp"]]))
  expect_equal(vec_size, length(cmats[["fn"]]))
  expect_equal(vec_size, length(cmats[["fp"]]))
  expect_equal(vec_size, length(cmats[["tn"]]))
})

test_that("'cmats' contains correct items", {
  cmats <- create_confmats(pscores = c(0.1, 0.2, 0), olabs = c(1, 0, 1))
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
  cmats <- create_confmats(pscores = c(0.1, 0.2, 0, 0.3),
                           olabs = c(1, 0, 0, 1))

  expect_equal(cmats[["pos_num"]], 2)
  expect_equal(cmats[["neg_num"]], 2)
  expect_equal(cmats[["tp"]], c(0, 1, 1, 2, 2))
  expect_equal(cmats[["fn"]], c(2, 1, 1, 0, 0))
  expect_equal(cmats[["fp"]], c(0, 0, 1, 1, 2))
  expect_equal(cmats[["tn"]], c(2, 2, 1, 1, 0))
})

test_that("create_confmats() handles tied scores 1", {
  cmats <- create_confmats(pscores = c(0.3, 0.2, 0.2, 0.2, 0.2, 0.1),
                           olabs = c(0, 1, 0, 1, 0, 1))
  expect_equal(cmats[["tp"]], c(0, 0, 0.5, 1, 1.5, 2, 3))
  expect_equal(cmats[["fp"]], c(0, 1, 1.5, 2, 2.5, 3, 3))
})

test_that("create_confmats() handles tied scores 2", {
  cmats <- create_confmats(pscores = c(0.3, 0.2, 0.2, 0.2, 0.2),
                           olabs = c(0, 1, 0, 1, 0))
  expect_equal(cmats[["tp"]], c(0, 0, 0.5, 1, 1.5, 2))
  expect_equal(cmats[["fp"]], c(0, 1, 1.5, 2, 2.5, 3))
})
