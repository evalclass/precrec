#' @importFrom precrec

context("UC 2: Use cases 2")

test_that("single class - positive", {

  # Data preparation
  sssamps1 <- create_sim_samples(1, 10, 0, "random")
  ssmdat1 <- mmdata(sssamps1$scores, sssamps1$labels)

  smsamps1 <- create_sim_samples(5, 10, 0, "random")
  smmdat1 <- mmdata(smsamps1$scores, smsamps1$labels)

  sssamps2 <- create_sim_samples(1, 5, 5, "random")
  ssmdat2 <- mmdata(sssamps2$scores, sssamps2$labels)

  smsamps2 <- create_sim_samples(5, 5, 5, "random")
  smmdat2 <- mmdata(smsamps2$scores, smsamps2$labels)

  # Test
  expect_error(evalmod(ssmdat1), "Curves cannot be calculated. Only a single class")
  expect_error(evalmod(smmdat1), "Curves cannot be calculated. Only a single class")

  expect_silent(evalmod(ssmdat2))
  expect_silent(evalmod(smmdat2))
})

test_that("single class - negative", {

  # Data preparation
  sssamps1 <- create_sim_samples(1, 0, 10, "random")
  ssmdat1 <- mmdata(sssamps1$scores, sssamps1$labels)

  smsamps1 <- create_sim_samples(5, 0, 10, "random")
  smmdat1 <- mmdata(smsamps1$scores, smsamps1$labels)

  sssamps2 <- create_sim_samples(1, 5, 5, "random")
  ssmdat2 <- mmdata(sssamps2$scores, sssamps2$labels)

  smsamps2 <- create_sim_samples(5, 5, 5, "random")
  smmdat2 <- mmdata(smsamps2$scores, smsamps2$labels)

  # Test
  expect_error(evalmod(ssmdat1), "Curves cannot be calculated. Only a single class")
  expect_error(evalmod(smmdat1), "Curves cannot be calculated. Only a single class")

  expect_silent(evalmod(ssmdat2))
  expect_silent(evalmod(smmdat2))
})

test_that("single class - positive (mode='basic')", {

  # Data preparation
  sssamps1 <- create_sim_samples(1, 10, 0, "random")
  ssmdat1 <- mmdata(sssamps1$scores, sssamps1$labels)

  smsamps1 <- create_sim_samples(5, 10, 0, "random")
  smmdat1 <- mmdata(smsamps1$scores, smsamps1$labels)

  sssamps2 <- create_sim_samples(1, 5, 5, "random")
  ssmdat2 <- mmdata(sssamps2$scores, sssamps2$labels)

  smsamps2 <- create_sim_samples(5, 5, 5, "random")
  smmdat2 <- mmdata(smsamps2$scores, smsamps2$labels)

  # Test
  expect_error(evalmod(ssmdat1, mode = "basic"),
               "Basic measures cannot be calculated. Only a single class")
  expect_error(evalmod(smmdat1, mode = "basic"),
               "Basic measures cannot be calculated. Only a single class")

  expect_silent(evalmod(ssmdat2, mode = "basic"))
  expect_silent(evalmod(smmdat2, mode = "basic"))
})

test_that("single class - negative (mode='basic')", {

  # Data preparation
  sssamps1 <- create_sim_samples(1, 0, 10, "random")
  ssmdat1 <- mmdata(sssamps1$scores, sssamps1$labels)

  smsamps1 <- create_sim_samples(5, 0, 10, "random")
  smmdat1 <- mmdata(smsamps1$scores, smsamps1$labels)

  sssamps2 <- create_sim_samples(1, 5, 5, "random")
  ssmdat2 <- mmdata(sssamps2$scores, sssamps2$labels)

  smsamps2 <- create_sim_samples(5, 5, 5, "random")
  smmdat2 <- mmdata(smsamps2$scores, smsamps2$labels)

  # Test
  expect_error(evalmod(ssmdat1, mode = "basic"),
               "Basic measures cannot be calculated. Only a single class")
  expect_error(evalmod(smmdat1, mode = "basic"),
               "Basic measures cannot be calculated. Only a single class")

  expect_silent(evalmod(ssmdat2, mode = "basic"))
  expect_silent(evalmod(smmdat2, mode = "basic"))
})

test_that("single class - positive (mode='aucroc')", {

  # Data preparation
  sssamps1 <- create_sim_samples(1, 10, 0, "random")
  ssmdat1 <- mmdata(sssamps1$scores, sssamps1$labels)

  smsamps1 <- create_sim_samples(5, 10, 0, "random")
  smmdat1 <- mmdata(smsamps1$scores, smsamps1$labels)

  sssamps2 <- create_sim_samples(1, 5, 5, "random")
  ssmdat2 <- mmdata(sssamps2$scores, sssamps2$labels)

  smsamps2 <- create_sim_samples(5, 5, 5, "random")
  smmdat2 <- mmdata(smsamps2$scores, smsamps2$labels)

  # Test
  expect_error(evalmod(ssmdat1, mode = "aucroc"),
               "AUCs with the U statistic cannot be calculated. Only a single class")
  expect_error(evalmod(smmdat1, mode = "aucroc"),
               "AUCs with the U statistic cannot be calculated. Only a single class")

  expect_silent(evalmod(ssmdat2, mode = "aucroc"))
  expect_silent(evalmod(smmdat2, mode = "aucroc"))
})

test_that("single class - negative (mode='aucroc')", {

  # Data preparation
  sssamps1 <- create_sim_samples(1, 0, 10, "random")
  ssmdat1 <- mmdata(sssamps1$scores, sssamps1$labels)

  smsamps1 <- create_sim_samples(5, 0, 10, "random")
  smmdat1 <- mmdata(smsamps1$scores, smsamps1$labels)

  sssamps2 <- create_sim_samples(1, 5, 5, "random")
  ssmdat2 <- mmdata(sssamps2$scores, sssamps2$labels)

  smsamps2 <- create_sim_samples(5, 5, 5, "random")
  smmdat2 <- mmdata(smsamps2$scores, smsamps2$labels)

  # Test
  expect_error(evalmod(ssmdat1, mode = "aucroc"),
               "AUCs with the U statistic cannot be calculated. Only a single class")
  expect_error(evalmod(smmdat1, mode = "aucroc"),
               "AUCs with the U statistic cannot be calculated. Only a single class")

  expect_silent(evalmod(ssmdat2, mode = "aucroc"))
  expect_silent(evalmod(smmdat2, mode = "aucroc"))
})
