context("PL0: Pipeline main")
# Test .pmatch_model_data_types(val),
#      .make_prefix(model_type, data_type), and
#      pl_main(mdat, model_type, data_type, x_interval)

test_that(".pmatch_model_data_types() returns 'single' or 'multiple'", {
  expect_equal(.pmatch_model_data_types("s"), "single")
  expect_equal(.pmatch_model_data_types("m"), "multiple")

  expect_equal(.pmatch_model_data_types("L"), "L")
  expect_equal(.pmatch_model_data_types(1), 1)
  expect_equal(.pmatch_model_data_types(NULL), NULL)
})

test_that(".make_prefix() takes 'model_type' and 'data_type'", {
  expect_equal(.make_prefix("single", "single"), "ss")
  expect_equal(.make_prefix("multiple", "single"), "ms")
  expect_equal(.make_prefix("single", "multiple"), "sm")
  expect_equal(.make_prefix("multiple", "multiple"), "mm")

  expect_equal(.make_prefix("sing", "multi"), "")
  expect_equal(.make_prefix("s", "m"), "")
})

test_that("pl_main() returns 'sscurves'", {
  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)

  mdat <- mmdata(s1, l1)
  pl <- pl_main(mdat)

  expect_equal(class(pl), "sscurves")
})

test_that("'sscurves' contains 'ssrocs' and 'ssprcs'", {
  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)

  mdat <- mmdata(s1, l1)
  pl <- pl_main(mdat)

  expect_equal(length(pl[["rocs"]]), 1)
  expect_equal(class(pl[["rocs"]]), "ssroc")
  expect_equal(class(pl[["rocs"]][[1]]), "roc_curve")

  expect_equal(length(pl[["prcs"]]), 1)
  expect_equal(class(pl[["prcs"]]), "ssprc")
  expect_equal(class(pl[["prcs"]][[1]]), "prc_curve")
})

test_that("pl_main() accepts 'x_interval'", {
  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)

  mdat <- mmdata(s1, l1)
  pl <- pl_main(mdat, x_interval = 0.1)

  expect_equal(attr(pl[["rocs"]][[1]], "args")[["x_interval"]], 0.1)
  expect_equal(attr(pl[["rocs"]][[1]], "args")[["x_interval"]], 0.1)

  expect_err_msg <- function(err_msg, mdat, x_interval) {
    eval(bquote(expect_error(pl_main(mdat, x_interval = x_interval), err_msg)))
  }

  err_msg <- "x_interval is not a number"
  expect_err_msg(err_msg, mdat, c(0.1, 0.2))

  err_msg <- "is not TRUE"
  expect_err_msg(err_msg, mdat, 0)
  expect_err_msg(err_msg, mdat, 1.1)

})

test_that("pl_main() returns 'mscurves'", {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  pscores <- join_scores(s1, s2)

  l1 <- c(1, 0, 1, 0)
  l2 <- c(1, 1, 0, 0)
  olabs <- join_labels(l1, l2)

  mdat <- mmdata(pscores, olabs)
  pl <- pl_main(mdat, model_type = "multiple")

  expect_equal(class(pl), "mscurves")
})

test_that("'mscurves' contains 'msrocs' and 'msprcs'", {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  pscores <- join_scores(s1, s2)

  l1 <- c(1, 0, 1, 0)
  l2 <- c(1, 1, 0, 0)
  olabs <- join_labels(l1, l2)

  mdat <- mmdata(pscores, olabs)
  pl <- pl_main(mdat, model_type = "multiple")

  expect_equal(length(pl[["rocs"]]), 2)
  expect_equal(class(pl[["rocs"]]), "msroc")
  expect_equal(class(pl[["rocs"]][[1]]), "roc_curve")
  expect_equal(class(pl[["rocs"]][[2]]), "roc_curve")

  expect_equal(length(pl[["prcs"]]), 2)
  expect_equal(class(pl[["prcs"]]), "msprc")
  expect_equal(class(pl[["prcs"]][[1]]), "prc_curve")
  expect_equal(class(pl[["prcs"]][[2]]), "prc_curve")
})
