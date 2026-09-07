# UC 2: Use cases 2

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
  expect_error(
    evalmod(ssmdat1),
    "Curves cannot be calculated. Only a single class"
  )
  expect_error(
    evalmod(smmdat1),
    "Curves cannot be calculated. Only a single class"
  )

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
  expect_error(
    evalmod(ssmdat1),
    "Curves cannot be calculated. Only a single class"
  )
  expect_error(
    evalmod(smmdat1),
    "Curves cannot be calculated. Only a single class"
  )

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

  # Test. Accuracy and error rate are defined for a single-class dataset, so
  # mode = "basic" warns and calculates rather than stopping
  expect_warning(
    points1 <- evalmod(ssmdat1, mode = "basic"),
    "Some basic metrics cannot be calculated. Only a single class"
  )
  # Every one of the five datasets is single-class, so all five warn.
  # expect_warning() would take the first and let the rest escape the test.
  warns <- capture_warnings(evalmod(smmdat1, mode = "basic"))
  expect_length(warns, 5)
  expect_true(all(grepl(
    "Some basic metrics cannot be calculated. Only a single class",
    warns,
    fixed = TRUE
  )))

  df1 <- as.data.frame(points1)
  expect_false(any(is.na(df1[df1$type == "accuracy", "y"])))
  expect_true(all(is.na(df1[df1$type == "specificity", "y"])))

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

  # Test. Accuracy and error rate are defined for a single-class dataset, so
  # mode = "basic" warns and calculates rather than stopping
  expect_warning(
    points1 <- evalmod(ssmdat1, mode = "basic"),
    "Some basic metrics cannot be calculated. Only a single class"
  )
  # Every one of the five datasets is single-class, so all five warn.
  # expect_warning() would take the first and let the rest escape the test.
  warns <- capture_warnings(evalmod(smmdat1, mode = "basic"))
  expect_length(warns, 5)
  expect_true(all(grepl(
    "Some basic metrics cannot be calculated. Only a single class",
    warns,
    fixed = TRUE
  )))

  df1 <- as.data.frame(points1)
  expect_false(any(is.na(df1[df1$type == "accuracy", "y"])))
  expect_true(all(is.na(df1[df1$type == "sensitivity", "y"])))

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
  expect_error(
    evalmod(ssmdat1, mode = "aucroc"),
    "AUCs with the U statistic cannot be calculated. Only a single class"
  )
  expect_error(
    evalmod(smmdat1, mode = "aucroc"),
    "AUCs with the U statistic cannot be calculated. Only a single class"
  )

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
  expect_error(
    evalmod(ssmdat1, mode = "aucroc"),
    "AUCs with the U statistic cannot be calculated. Only a single class"
  )
  expect_error(
    evalmod(smmdat1, mode = "aucroc"),
    "AUCs with the U statistic cannot be calculated. Only a single class"
  )

  expect_silent(evalmod(ssmdat2, mode = "aucroc"))
  expect_silent(evalmod(smmdat2, mode = "aucroc"))
})

test_that("on_single_class = 'na' evaluates what it can", {
  # Data preparation. The second fold holds positives only.
  scores <- list(runif(20), runif(10), runif(20))
  labels <- list(
    c(rep(1, 10), rep(0, 10)), rep(1, 10), c(rep(1, 10), rep(0, 10))
  )
  mdat <- mmdata(scores, labels, modnames = rep("m1", 3), dsids = 1:3)

  # Test
  expect_error(
    evalmod(mdat),
    "Curves cannot be calculated. Only a single class"
  )
  expect_warning(
    curves <- evalmod(mdat, on_single_class = "na"),
    "Curves cannot be calculated. Only a single class"
  )

  # The degenerate fold keeps its rows, with NA where a number would be
  aucs <- auc(curves)
  expect_equal(nrow(aucs), 6)
  expect_true(all(is.na(aucs[aucs$dsids == 2, "aucs"])))
  expect_false(any(is.na(aucs[aucs$dsids != 2, "aucs"])))
})

test_that("on_single_class = 'na' leaves NA out of the AUC CIs", {
  scores <- list(runif(20), runif(10), runif(20))
  labels <- list(
    c(rep(1, 10), rep(0, 10)), rep(1, 10), c(rep(1, 10), rep(0, 10))
  )
  mdat <- mmdata(scores, labels, modnames = rep("m1", 3), dsids = 1:3)
  curves <- suppressWarnings(evalmod(mdat, on_single_class = "na"))

  # n counts the datasets the interval was built from, not the ones supplied
  cis <- auc_ci(curves)
  expect_equal(cis[["n"]], c(2, 2))
  expect_false(any(is.na(cis[["mean"]])))
})

test_that("on_single_class = 'na' works for mode = 'aucroc'", {
  samps <- create_sim_samples(1, 10, 0, "random")
  mdat <- mmdata(samps$scores, samps$labels, mode = "aucroc")

  expect_error(
    evalmod(mdat, mode = "aucroc"),
    "AUCs with the U statistic cannot be calculated. Only a single class"
  )
  expect_warning(
    uaucs <- evalmod(mdat, mode = "aucroc", on_single_class = "na"),
    "AUCs with the U statistic cannot be calculated. Only a single class"
  )

  df <- as.data.frame(uaucs)
  expect_equal(nrow(df), 1)
  expect_true(is.na(df[["aucs"]]))
  expect_true(is.na(df[["ustats"]]))
})

test_that("'on_single_class' must be 'error' or 'na'", {
  samps <- create_sim_samples(1, 5, 5, "random")
  mdat <- mmdata(samps$scores, samps$labels)

  expect_error(evalmod(mdat, on_single_class = "skip"),
    class = "precrec_error_invalid_on_single_class"
  )
  expect_error(evalmod(mdat, on_single_class = 1),
    class = "precrec_error_invalid_on_single_class"
  )
})
