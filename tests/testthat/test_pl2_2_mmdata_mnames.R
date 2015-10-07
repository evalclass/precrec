library(precrec)

context("PL 2: Create model names")
# Test .pmatch_expd_first(val),
#      .create_modnames(dlen, model_names, setids, expd_first)

test_that(".pmatch_expd_first() returns 'setids' or 'model_names'", {
  expect_equal(.pmatch_expd_first("s"), "setids")
  expect_equal(.pmatch_expd_first("m"), "model_names")

  expect_equal(.pmatch_expd_first("S"), "S")
  expect_equal(.pmatch_expd_first(1), 1)
  expect_equal(.pmatch_expd_first(NULL), NULL)
})

test_that("model names and dataset IDs are unchanged", {
  dlen <- 3
  model_names <- c("1", "2", "3")
  setids <- c(1, 2, 3)

  mnames <- .create_modnames(dlen, model_names, setids)

  expect_equal(mnames[["mn"]], model_names)
  expect_equal(mnames[["ds"]], setids)

})

test_that("a single dataset ID is assigned", {
  dlen <- 3
  model_names <- c("1", "2", "3")
  setids <-  rep(1, 3)

  mnames <- .create_modnames(dlen, model_names, NULL)

  expect_equal(mnames[["mn"]], model_names)
  expect_equal(mnames[["ds"]], setids)
})

test_that("a single model name is assigned", {
  dlen <- 3
  model_names <- rep("m1", 3)
  setids <-  c(1, 2, 3)

  mnames <- .create_modnames(dlen, NULL, setids)

  expect_equal(mnames[["mn"]], model_names)
  expect_equal(mnames[["ds"]], setids)
})

test_that("model names and dataset IDs are expanded 1", {
  dlen <- 6
  model_names <- c("m1", "m2")
  setids <-  c(1, 2, 3)

  mnames <- .create_modnames(dlen, model_names, setids,
                             expd_first = "setids")

  expect_equal(mnames[["mn"]], c(rep("m1", 3), rep("m2", 3)))
  expect_equal(mnames[["ds"]], rep(c(1, 2, 3), 2))
})

test_that("model names and dataset IDs are expanded 2", {
  dlen <- 6
  model_names <- c("m1", "m2")
  setids <-  c(1, 2, 3)

  mnames <- .create_modnames(dlen, model_names, setids,
                             expd_first = "model_names")

  expect_equal(mnames[["mn"]], rep(c("m1", "m2"), 3))
  expect_equal(mnames[["ds"]], c(1, 1, 2, 2, 3, 3))
})

test_that("model names are expanded with a single dataset ID", {
  dlen <- 3
  model_names <- c("m1", "m2", "m3")
  setids <-  rep(1, 3)

  mnames <- .create_modnames(dlen, NULL, NULL,
                             expd_first = "model_names")

  expect_equal(mnames[["mn"]], model_names)
  expect_equal(mnames[["ds"]], setids)
})

test_that("dataset IDs are expanded with a single model name", {
  dlen <- 3
  model_names <- rep("m1", 3)
  setids <-  seq(3)

  mnames <- .create_modnames(dlen, NULL, NULL,
                             expd_first = "setids")

  expect_equal(mnames[["mn"]], model_names)
  expect_equal(mnames[["ds"]], setids)
})
