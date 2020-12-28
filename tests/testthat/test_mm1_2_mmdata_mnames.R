#' @importFrom precrec

context("MM 1: Create model names")
# Test .pmatch_expd_first(val),
#      .create_modnames(dlen, modnames, dsids, expd_first)

test_that(".pmatch_expd_first() returns 'dsids' or 'modnames'", {
  expect_equal(.pmatch_expd_first("d"), "dsids")
  expect_equal(.pmatch_expd_first("m"), "modnames")

  expect_equal(.pmatch_expd_first("S"), "S")
  expect_equal(.pmatch_expd_first(1), 1)
  expect_equal(.pmatch_expd_first(NULL), NULL)
})

test_that("model names and dataset IDs are unchanged", {
  dlen <- 3
  modnames <- c("1", "2", "3")
  dsids <- c(1, 2, 3)

  mnames <- .create_modnames(dlen, modnames, dsids)

  expect_equal(mnames[["mn"]], modnames)
  expect_equal(mnames[["ds"]], dsids)

})

test_that("a single dataset ID is assigned", {
  dlen <- 3
  modnames <- c("1", "2", "3")
  dsids <-  rep(1, 3)

  mnames <- .create_modnames(dlen, modnames, NULL)

  expect_equal(mnames[["mn"]], modnames)
  expect_equal(mnames[["ds"]], dsids)
})

test_that("a single model name is assigned", {
  dlen <- 3
  modnames <- rep("m1", 3)
  dsids <-  c(1, 2, 3)

  mnames <- .create_modnames(dlen, NULL, dsids)

  expect_equal(mnames[["mn"]], modnames)
  expect_equal(mnames[["ds"]], dsids)
})

test_that("model names and dataset IDs are expanded 1", {
  dlen <- 6
  modnames <- c("m1", "m2")
  dsids <-  c(1, 2, 3)

  mnames <- .create_modnames(dlen, modnames, dsids,
                             expd_first = "dsids")

  expect_equal(mnames[["mn"]], c(rep("m1", 3), rep("m2", 3)))
  expect_equal(mnames[["ds"]], rep(c(1, 2, 3), 2))
})

test_that("model names and dataset IDs are expanded 2", {
  dlen <- 6
  modnames <- c("m1", "m2")
  dsids <-  c(1, 2, 3)

  mnames <- .create_modnames(dlen, modnames, dsids,
                             expd_first = "modnames")

  expect_equal(mnames[["mn"]], rep(c("m1", "m2"), 3))
  expect_equal(mnames[["ds"]], c(1, 1, 2, 2, 3, 3))
})

test_that("model names are expanded with a single dataset ID", {
  dlen <- 3
  modnames <- c("m1", "m2", "m3")
  dsids <-  rep(1, 3)

  mnames <- .create_modnames(dlen, NULL, NULL,
                             expd_first = "modnames")

  expect_equal(mnames[["mn"]], modnames)
  expect_equal(mnames[["ds"]], dsids)
})

test_that("dataset IDs are expanded with a single model name", {
  dlen <- 3
  modnames <- rep("m1", 3)
  dsids <-  seq(3)

  mnames <- .create_modnames(dlen, NULL, NULL,
                             expd_first = "dsids")

  expect_equal(mnames[["mn"]], modnames)
  expect_equal(mnames[["ds"]], dsids)
})
