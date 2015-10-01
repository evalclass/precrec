library(precrec)

context("PL 2: Create model names")
# Test .pmatch_exp_priority(val),
#      .create_modnames(dlen, model_names, data_nos, exp_priority)

test_that(".pmatch_exp_priority() returns 'data_nos' or 'model_names'", {
  expect_equal(.pmatch_exp_priority("d"), "data_nos")
  expect_equal(.pmatch_exp_priority("m"), "model_names")

  expect_equal(.pmatch_exp_priority("D"), "D")
  expect_equal(.pmatch_exp_priority(1), 1)
  expect_equal(.pmatch_exp_priority(NULL), NULL)
})

test_that("model names and data numbers are unchanged", {
  dlen <- 3
  model_names <- c("1", "2", "3")
  data_nos <- c(1, 2, 3)

  mnames <- .create_modnames(dlen, model_names, data_nos)

  expect_equal(mnames[["mn"]], model_names)
  expect_equal(mnames[["dn"]], data_nos)

})

test_that("a single data number is assigned", {
  dlen <- 3
  model_names <- c("1", "2", "3")
  data_nos <-  rep(1, 3)

  mnames <- .create_modnames(dlen, model_names, NULL)

  expect_equal(mnames[["mn"]], model_names)
  expect_equal(mnames[["dn"]], data_nos)
})

test_that("a single model name is assigned", {
  dlen <- 3
  model_names <- rep("m1", 3)
  data_nos <-  c(1, 2, 3)

  mnames <- .create_modnames(dlen, NULL, data_nos)

  expect_equal(mnames[["mn"]], model_names)
  expect_equal(mnames[["dn"]], data_nos)
})

test_that("model names and data numbers are expanded 1", {
  dlen <- 6
  model_names <- c("m1", "m2")
  data_nos <-  c(1, 2, 3)

  mnames <- .create_modnames(dlen, model_names, data_nos,
                             exp_priority = "data_nos")

  expect_equal(mnames[["mn"]], c(rep("m1", 3), rep("m2", 3)))
  expect_equal(mnames[["dn"]], rep(c(1, 2, 3), 2))
})

test_that("model names and data numbers are expanded 2", {
  dlen <- 6
  model_names <- c("m1", "m2")
  data_nos <-  c(1, 2, 3)

  mnames <- .create_modnames(dlen, model_names, data_nos,
                             exp_priority = "model_names")

  expect_equal(mnames[["mn"]], rep(c("m1", "m2"), 3))
  expect_equal(mnames[["dn"]], c(1, 1, 2, 2, 3, 3))
})

test_that("model names are expanded with a single data number", {
  dlen <- 3
  model_names <- c("m1", "m2", "m3")
  data_nos <-  rep(1, 3)

  mnames <- .create_modnames(dlen, NULL, NULL,
                             exp_priority = "model_names")

  expect_equal(mnames[["mn"]], model_names)
  expect_equal(mnames[["dn"]], data_nos)
})

test_that("data numbers are expanded with a single model name", {
  dlen <- 3
  model_names <- rep("m1", 3)
  data_nos <-  seq(3)

  mnames <- .create_modnames(dlen, NULL, NULL,
                             exp_priority = "data_nos")

  expect_equal(mnames[["mn"]], model_names)
  expect_equal(mnames[["dn"]], data_nos)
})
