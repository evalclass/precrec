context("PL3: Reformat input data for evaluation")
# Test reformat_data(pscores, olabs,
#                    na.last, ties.method, levels, model_name)

test_that("'pscores' and 'olabs' must be specified", {
  expect_err_msg <- function(pscores, olabs, err_msg) {
    eval(bquote(expect_error(reformat_data(pscores, olabs), err_msg)))
  }

  expect_err_msg(NULL, c(0), "Invalid 'pscores'")
  expect_err_msg(c(0), NULL, "Invalid 'olabs'")
})

test_that("'pscores' and 'olabs' should be the same length", {
  expect_err_msg <- function(pscores, olabs) {
    err_msg <- "'pscores' and 'olabs' must be of the same length"
    eval(bquote(expect_error(reformat_data(pscores, olabs), err_msg)))
  }

  expect_err_msg(c(0.1, 0.2), c(1, 0, 0))
  expect_err_msg(c(0.1), c(1, 0))
})

test_that("'model_name' must be a character vector", {
  expect_err_msg <- function(model_name) {
    err_msg <- "'model_name' must be a single string"
    eval(bquote(expect_error(reformat_data(c(0, 1), c(0, 1),
                                           model_name = model_name),
                             err_msg)))
  }

  expect_err_msg(c(0.1, 0.2))
  expect_err_msg(list("1"))
  expect_err_msg(data.frame("1"))
  expect_err_msg(array("1"))
  expect_err_msg(matrix("1"))
  expect_err_msg(factor(c(0.1, 0.2)))
})

test_that("'model_name' must be a single string", {
  expect_err_msg <- function(model_name) {
    err_msg <- "'model_name' must be a single string"
    eval(bquote(expect_error(reformat_data(c(0), c(0),
                                           model_name = model_name),
                             err_msg)))
  }

  expect_err_msg(c("1", "2"))
  expect_err_msg(as.character())
})

test_that("reformat_data() reterns a 'fmdat' object", {
  fmdat <- reformat_data(c(0.1, 0.2, 0), c(1, 0, 1))

  expect_equal(class(fmdat), "fmdat")
})

test_that("'fmdat' contains a list with 3 items", {
  fmdat <- reformat_data(c(0.1, 0.2, 0), c(1, 0, 1))

  expect_true(is.list(fmdat))
  expect_equal(length(fmdat), 3)
})

test_that("olabs, ranks, and rank_idx must be the same length", {
  fmdat <- reformat_data(c(0.1, 0.2, 0), c(1, 0, 1))

  expect_true(length(fmdat[["olabs"]]) != 0)
  expect_equal(length(fmdat[["olabs"]]), length(fmdat[["ranks"]]))
  expect_equal(length(fmdat[["olabs"]]), length(fmdat[["rank_idx"]]))
})

test_that("reformat_data() accepts 'na.last'", {
  expect_equal_ranks <- function(pscores, olabs, na.last, ranks) {
    fmdat <- reformat_data(pscores, olabs, na.last = na.last)
    eval(bquote(expect_equal(fmdat[["ranks"]], ranks)))
  }

  pscores <- c(NA, 0.2, 0.1)
  olabs <- c(1, 1, 0)

  expect_equal_ranks(pscores, olabs, TRUE, c(3, 2, 1))
  expect_equal_ranks(pscores, olabs, FALSE, c(1, 3, 2))

})

test_that("reformat_data() accepts 'ties.method'", {
  expect_equal_ranks <- function(ties.method, ranks) {
    pscores <- c(0.1, 0.2, 0.2, 0.2, 0.3)
    olabs <- c(1, 0, 1, 0, 1)
    fmdat <- reformat_data(pscores, olabs, ties.method = ties.method)
    eval(bquote(expect_equal(fmdat[["ranks"]], ranks)))
  }

  expect_equal_ranks("average", c(1, 3, 3, 3, 5))
  expect_equal_ranks("first", c(1, 2, 3, 4, 5))

})

test_that("reformat_data() accepts 'olevs'", {
  olevs <- c("N", "P")
  fmdat <- reformat_data(c(0.1, 0.2, 0), c(1, 0, 1), olevs = olevs)

  expect_equal(levels(fmdat[["olabs"]]), olevs)

})
