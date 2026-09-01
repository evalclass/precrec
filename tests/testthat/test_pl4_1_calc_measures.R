# PL 4: Calculate evaluation measures
# Test calc_measures(cmats, scores, labels)

test_that("calc_measures() reterns an 'pevals' object", {
  pevals1 <- calc_measures(scores = c(0.1, 0.2, 0), labels = c(1, 0, 1))

  data(P10N10)
  fmdat <- reformat_data(P10N10$scores, P10N10$labels)
  cmats <- create_confmats(fmdat)
  pevals2 <- calc_measures(cmats)
  pevals3 <- calc_measures(scores = P10N10$scores, labels = P10N10$labels)

  expect_true(is(pevals1, "pevals"))
  expect_true(is(pevals2, "pevals"))
  expect_true(is(pevals3, "pevals"))
})

test_that("'cmats' must be a 'cmats' object", {
  expect_err_msg <- function(cmats) {
    err_msg <- "Unrecognized class for .validate()"
    expect_error(calc_measures(cmats), err_msg)
  }

  expect_err_msg(list())
  expect_err_msg(data.frame())
})

test_that("calc_measures() can directly take scores and labels", {
  cmats <- create_confmats(
    scores = c(0.1, 0.2, 0.2, 0),
    labels = c(1, 0, 1, 1)
  )
  pevals1 <- calc_measures(cmats)
  pevals2 <- calc_measures(
    scores = c(0.1, 0.2, 0.2, 0),
    labels = c(1, 0, 1, 1)
  )

  expect_equal(pevals1, pevals2)
})

test_that("calc_measures() accepts arguments for reformat_data()", {
  err_msg <- "Invalid arguments: na.rm"
  expect_error(
    calc_measures(
      scores = c(0.1, 0.2, 0.2, 0),
      labels = c(1, 0, 1, 1), na.rm = TRUE
    ),
    err_msg
  )

  pevals <- calc_measures(
    scores = c(0.1, 0.2, 0),
    labels = c(1, 0, 1),
    na_worst = TRUE,
    ties_method = "first",
    keep_fmdat = TRUE
  )

  expect_equal(.get_obj_arg(pevals, "fmdat", "na_worst"), TRUE)
  expect_equal(.get_obj_arg(pevals, "fmdat", "ties_method"), "first")
})


test_that("calc_measures() accepts na_worst argument", {
  expect_equal_ranks <- function(scores, na_worst, ranks) {
    pevals <- calc_measures(
      scores = scores,
      labels = c(1, 0, 1),
      na_worst = na_worst,
      keep_fmdat = TRUE
    )

    fmdat <- .get_obj(pevals, "fmdat")

    expect_equal(.get_obj_arg(pevals, NULL, "na_worst"), na_worst)
    expect_equal(.get_obj_arg(fmdat, NULL, "na_worst"), na_worst)
    expect_equal(fmdat[["ranks"]], ranks)

    sranks <- .rank_scores(scores, na_worst = na_worst)
    expect_equal(sranks[["ranks"]], ranks)
  }

  na1_scores <- c(NA, 0.2, 0.1)
  na2_scores <- c(0.2, NA, 0.1)
  na3_scores <- c(0.2, 0.1, NA)

  expect_equal_ranks(na1_scores, TRUE, c(3, 1, 2))
  expect_equal_ranks(na1_scores, FALSE, c(1, 2, 3))

  expect_equal_ranks(na2_scores, TRUE, c(1, 3, 2))
  expect_equal_ranks(na2_scores, FALSE, c(2, 1, 3))

  expect_equal_ranks(na3_scores, TRUE, c(1, 2, 3))
  expect_equal_ranks(na3_scores, FALSE, c(2, 3, 1))
})

test_that("calc_measures() accepts ties_method argument", {
  expect_equal_ranks <- function(ties_method, ranks) {
    pevals <- calc_measures(
      scores = c(0.1, 0.2, 0.2, 0.2, 0.3),
      labels = c(1, 0, 1, 1, 1),
      ties_method = ties_method,
      keep_fmdat = TRUE
    )

    fmdat <- .get_obj(pevals, "fmdat")

    expect_equal(.get_obj_arg(pevals, NULL, "ties_method"), ties_method)
    expect_equal(.get_obj_arg(fmdat, NULL, "ties_method"), ties_method)
    expect_equal(fmdat[["ranks"]], ranks)
  }

  expect_equal_ranks("equiv", c(5, 2, 2, 2, 1))
  expect_equal_ranks("first", c(5, 2, 3, 4, 1))
})

test_that("'pevals' contains a list with 1 item", {
  pevals <- calc_measures(scores = c(0.1, 0.2, 0), labels = c(1, 0, 1))

  expect_true(is.list(pevals))
  expect_equal(length(pevals), 1)
})

test_that("calc_measures() reterns correct evaluation values", {
  pevals <- calc_measures(
    scores = c(0.1, 0.2, 0, 0.3),
    labels = c(1, 0, 0, 1)
  )
  pb <- pevals[["basic"]]

  #   "TPs" c(0, 1, 1, 2, 2)
  #   "FNs" c(2, 1, 1, 0, 0)
  #   "FPs" c(0, 0, 1, 1, 2)
  #   "TNs" c(2, 2, 1, 1, 0)
  expect_equal(pb[["error"]], c(0.5, 0.25, 0.5, 0.25, 0.5))
  expect_equal(pb[["accuracy"]], c(0.5, 0.75, 0.5, 0.75, 0.5))
  expect_equal(pb[["specificity"]], c(1, 1, 0.5, 0.5, 0))
  expect_equal(pb[["sensitivity"]], c(0, 0.5, 0.5, 1, 1))
  expect_equal(pb[["precision"]], c(1, 1, 0.5, 2 / 3, 0.5))
  expect_equal(pb[["mcc"]], c(NA, 0.5773503, 0, 0.5773503, NA),
    tolerance = 1e-4
  )
  expect_equal(pb[["fscore"]], c(0, 2 / 3, 0.5, 0.8, 2 / 3), tolerance = 1e-4)
})

test_that("calc_measures() returns correct confusion-matrix measures", {
  pevals <- calc_measures(
    scores = c(0.1, 0.2, 0, 0.3),
    labels = c(1, 0, 0, 1)
  )
  pb <- pevals[["basic"]]

  #   "TPs" c(0, 1, 1, 2, 2)
  #   "FNs" c(2, 1, 1, 0, 0)
  #   "FPs" c(0, 0, 1, 1, 2)
  #   "TNs" c(2, 2, 1, 1, 0)
  expect_equal(pb[["balanced_accuracy"]], c(0.5, 0.75, 0.5, 0.75, 0.5))

  # The last rank predicts nothing negative, so its NPV comes from the rank
  # before it, the way the first rank's precision does
  expect_equal(pb[["npv"]], c(0.5, 2 / 3, 0.5, 1, 1), tolerance = 1e-4)
  expect_equal(pb[["informedness"]], c(0, 0.5, 0, 0.5, 0))
  expect_equal(pb[["markedness"]], c(0.5, 2 / 3, 0, 2 / 3, 0.5),
    tolerance = 1e-4
  )
  expect_equal(pb[["kappa"]], c(0, 0.5, 0, 0.5, 0))
})

test_that("calc_measures() handles imbalanced labels", {
  pevals <- calc_measures(
    scores = c(0.1, 0.2, 0, 0.3, 0.5),
    labels = c(1, 0, 0, 0, 0)
  )
  pb <- pevals[["basic"]]

  # 1 positive and 4 negatives, so chance agreement is high and kappa parts
  # company with informedness
  expect_equal(
    pb[["balanced_accuracy"]],
    c(0.5, 0.375, 0.25, 0.125, 0.625, 0.5)
  )
  expect_equal(pb[["npv"]], c(0.8, 0.75, 2 / 3, 0.5, 1, 1), tolerance = 1e-4)
  expect_equal(pb[["informedness"]], c(0, -0.25, -0.5, -0.75, 0.25, 0))
  expect_equal(pb[["markedness"]],
    c(-0.2, -0.25, -1 / 3, -0.5, 0.25, 0.2),
    tolerance = 1e-4
  )
  expect_equal(pb[["kappa"]],
    c(0, -0.25, -0.3636364, -0.4285714, 0.1176471, 0),
    tolerance = 1e-4
  )
})

test_that("calc_measures() takes the beta of the F-beta score", {
  sc <- c(0.1, 0.2, 0, 0.3)
  lb <- c(1, 0, 0, 1)
  pb1 <- calc_measures(scores = sc, labels = lb)[["basic"]]
  pb2 <- calc_measures(scores = sc, labels = lb, beta = 1)[["basic"]]
  pb3 <- calc_measures(scores = sc, labels = lb, beta = 2)[["basic"]]

  # beta = 1 is the F1 score the function has always returned
  expect_equal(pb1[["fscore"]], pb2[["fscore"]])
  expect_equal(pb3[["fscore"]],
    c(0, 5 / 9, 0.5, 10 / 11, 5 / 6),
    tolerance = 1e-4
  )

  # Only the F-score depends on beta
  expect_equal(pb1[["precision"]], pb3[["precision"]])
  expect_equal(pb1[["sensitivity"]], pb3[["sensitivity"]])
})

test_that("'beta' must be a single non-negative finite number", {
  sc <- c(0.1, 0.2, 0, 0.3)
  lb <- c(1, 0, 0, 1)
  expect_error(calc_measures(scores = sc, labels = lb, beta = -1),
    class = "precrec_error_invalid_beta"
  )
  expect_error(calc_measures(scores = sc, labels = lb, beta = "1"),
    class = "precrec_error_invalid_beta"
  )
  expect_error(calc_measures(scores = sc, labels = lb, beta = c(1, 2)),
    class = "precrec_error_invalid_beta"
  )
  expect_error(calc_measures(scores = sc, labels = lb, beta = Inf),
    class = "precrec_error_invalid_beta"
  )
})

pl4_create_ms_dat <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  list(scores = scores, labels = labels)
}

pl4_create_sm_dat <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  list(scores = scores, labels = labels)
}

pl4_create_mm_dat <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  s4 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3, s4)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  l4 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3, l4)

  list(scores = scores, labels = labels)
}

test_that("ss test data", {
  pevals <- calc_measures(
    scores = c(1, 2, 3, 4),
    labels = c(1, 0, 1, 0)
  )
  pb <- pevals[["basic"]]

  expect_equal(pb[["error"]], c(0.5, 0.75, 0.5, 0.75, 0.5))
  expect_equal(pb[["accuracy"]], c(0.5, 0.25, 0.5, 0.25, 0.5))
  expect_equal(pb[["specificity"]], c(1, 0.5, 0.5, 0, 0))
  expect_equal(pb[["sensitivity"]], c(0, 0, 0.5, 0.5, 1))
  expect_equal(pb[["precision"]], c(0, 0, 0.5, 1 / 3, 0.5))
  expect_equal(pb[["mcc"]], c(NA, -0.5773503, 0, -0.5773503, NA),
    tolerance = 1e-4
  )
  expect_equal(pb[["fscore"]], c(0, 0, 0.5, 0.4, 2 / 3), tolerance = 1e-4)
})

test_that("ms test data", {
  msdat <- pl4_create_ms_dat()

  pevals1 <- calc_measures(
    scores = msdat[["scores"]][[1]],
    labels = msdat[["labels"]][[1]]
  )
  pb1 <- pevals1[["basic"]]
  expect_equal(pb1[["error"]], c(0.75, 0.5, 0.25, 0.5, 0.25))
  expect_equal(pb1[["accuracy"]], c(0.25, 0.5, 0.75, 0.5, 0.75))
  expect_equal(pb1[["specificity"]], c(1, 1, 1, 0, 0))
  expect_equal(pb1[["sensitivity"]], c(0, 1 / 3, 2 / 3, 2 / 3, 1))
  expect_equal(pb1[["precision"]], c(1, 1, 1, 2 / 3, 0.75))
  expect_equal(pb1[["mcc"]], c(NA, 1 / 3, 0.5773503, -1 / 3, NA),
    tolerance = 1e-4
  )
  expect_equal(pb1[["fscore"]], c(0, 0.5, 0.8, 2 / 3, 0.8571429),
    tolerance = 1e-4
  )

  pevals2 <- calc_measures(
    scores = msdat[["scores"]][[2]],
    labels = msdat[["labels"]][[2]]
  )
  pb2 <- pevals2[["basic"]]
  expect_equal(pb2[["error"]], c(0.75, 0.5, 0.25, 0, 0.25))
  expect_equal(pb2[["accuracy"]], c(0.25, 0.5, 0.75, 1, 0.75))
  expect_equal(pb2[["specificity"]], c(1, 1, 1, 1, 0))
  expect_equal(pb2[["sensitivity"]], c(0, 1 / 3, 2 / 3, 1, 1))
  expect_equal(pb2[["precision"]], c(1, 1, 1, 1, 0.75))
  expect_equal(pb2[["mcc"]], c(NA, 1 / 3, 0.5773503, 1, NA),
    tolerance = 1e-4
  )
  expect_equal(pb2[["fscore"]], c(0, 0.5, 0.8, 1, 0.8571429), tolerance = 1e-4)

  pevals3 <- calc_measures(
    scores = msdat[["scores"]][[3]],
    labels = msdat[["labels"]][[3]]
  )
  pb3 <- pevals3[["basic"]]
  expect_equal(pb3[["error"]], c(0.75, 0.5, 0.75, 0.5, 0.25))
  expect_equal(pb3[["accuracy"]], c(0.25, 0.5, 0.25, 0.5, 0.75))
  expect_equal(pb3[["specificity"]], c(1, 1, 0, 0, 0))
  expect_equal(pb3[["sensitivity"]], c(0, 1 / 3, 1 / 3, 2 / 3, 1))
  expect_equal(pb3[["precision"]], c(1, 1, 0.5, 2 / 3, 0.75))
  expect_equal(pb3[["mcc"]], c(NA, 1 / 3, -0.5773503, -1 / 3, NA),
    tolerance = 1e-4
  )
  expect_equal(pb3[["fscore"]], c(0, 0.5, 0.4, 2 / 3, 0.8571429),
    tolerance = 1e-4
  )
})

test_that("sm test data", {
  smdat <- pl4_create_sm_dat()

  pevals1 <- calc_measures(
    scores = smdat[["scores"]][[1]],
    labels = smdat[["labels"]][[1]]
  )
  pb1 <- pevals1[["basic"]]
  expect_equal(pb1[["error"]], c(0.75, 0.5, 0.25, 0.5, 0.25))
  expect_equal(pb1[["accuracy"]], c(0.25, 0.5, 0.75, 0.5, 0.75))
  expect_equal(pb1[["specificity"]], c(1, 1, 1, 0, 0))
  expect_equal(pb1[["sensitivity"]], c(0, 1 / 3, 2 / 3, 2 / 3, 1))
  expect_equal(pb1[["precision"]], c(1, 1, 1, 2 / 3, 0.75))
  expect_equal(pb1[["mcc"]], c(NA, 1 / 3, 0.5773503, -1 / 3, NA),
    tolerance = 1e-4
  )
  expect_equal(pb1[["fscore"]], c(0, 0.5, 0.8, 2 / 3, 0.8571429),
    tolerance = 1e-4
  )

  pevals2 <- calc_measures(
    scores = smdat[["scores"]][[2]],
    labels = smdat[["labels"]][[2]]
  )
  pb2 <- pevals2[["basic"]]
  expect_equal(pb2[["error"]], c(0.75, 0.5, 0.25, 0, 0.25))
  expect_equal(pb2[["accuracy"]], c(0.25, 0.5, 0.75, 1, 0.75))
  expect_equal(pb2[["specificity"]], c(1, 1, 1, 1, 0))
  expect_equal(pb2[["sensitivity"]], c(0, 1 / 3, 2 / 3, 1, 1))
  expect_equal(pb2[["precision"]], c(1, 1, 1, 1, 0.75))
  expect_equal(pb2[["mcc"]], c(NA, 1 / 3, 0.5773503, 1, NA),
    tolerance = 1e-4
  )
  expect_equal(pb2[["fscore"]], c(0, 0.5, 0.8, 1, 0.8571429), tolerance = 1e-4)

  pevals3 <- calc_measures(
    scores = smdat[["scores"]][[3]],
    labels = smdat[["labels"]][[3]]
  )
  pb3 <- pevals3[["basic"]]
  expect_equal(pb3[["error"]], c(0.75, 0.5, 0.75, 0.5, 0.25))
  expect_equal(pb3[["accuracy"]], c(0.25, 0.5, 0.25, 0.5, 0.75))
  expect_equal(pb3[["specificity"]], c(1, 1, 0, 0, 0))
  expect_equal(pb3[["sensitivity"]], c(0, 1 / 3, 1 / 3, 2 / 3, 1))
  expect_equal(pb3[["precision"]], c(1, 1, 0.5, 2 / 3, 0.75))
  expect_equal(pb3[["mcc"]], c(NA, 1 / 3, -0.5773503, -1 / 3, NA),
    tolerance = 1e-4
  )
  expect_equal(pb3[["fscore"]], c(0, 0.5, 0.4, 2 / 3, 0.8571429),
    tolerance = 1e-4
  )
})

test_that("mm test data", {
  mmdat <- pl4_create_mm_dat()

  pevals1 <- calc_measures(
    scores = mmdat[["scores"]][[1]],
    labels = mmdat[["labels"]][[1]]
  )
  pb1 <- pevals1[["basic"]]
  expect_equal(pb1[["error"]], c(0.75, 0.5, 0.25, 0.5, 0.25))
  expect_equal(pb1[["accuracy"]], c(0.25, 0.5, 0.75, 0.5, 0.75))
  expect_equal(pb1[["specificity"]], c(1, 1, 1, 0, 0))
  expect_equal(pb1[["sensitivity"]], c(0, 1 / 3, 2 / 3, 2 / 3, 1))
  expect_equal(pb1[["precision"]], c(1, 1, 1, 2 / 3, 0.75))
  expect_equal(pb1[["mcc"]], c(NA, 1 / 3, 0.5773503, -1 / 3, NA),
    tolerance = 1e-4
  )
  expect_equal(pb1[["fscore"]], c(0, 0.5, 0.8, 2 / 3, 0.8571429),
    tolerance = 1e-4
  )

  pevals2 <- calc_measures(
    scores = mmdat[["scores"]][[2]],
    labels = mmdat[["labels"]][[2]]
  )
  pb2 <- pevals2[["basic"]]
  expect_equal(pb2[["error"]], c(0.75, 0.5, 0.25, 0, 0.25))
  expect_equal(pb2[["accuracy"]], c(0.25, 0.5, 0.75, 1, 0.75))
  expect_equal(pb2[["specificity"]], c(1, 1, 1, 1, 0))
  expect_equal(pb2[["sensitivity"]], c(0, 1 / 3, 2 / 3, 1, 1))
  expect_equal(pb2[["precision"]], c(1, 1, 1, 1, 0.75))
  expect_equal(pb2[["mcc"]], c(NA, 1 / 3, 0.5773503, 1, NA),
    tolerance = 1e-4
  )
  expect_equal(pb2[["fscore"]], c(0, 0.5, 0.8, 1, 0.8571429), tolerance = 1e-4)

  pevals3 <- calc_measures(
    scores = mmdat[["scores"]][[3]],
    labels = mmdat[["labels"]][[3]]
  )
  pb3 <- pevals3[["basic"]]
  expect_equal(pb3[["error"]], c(0.75, 0.5, 0.75, 0.5, 0.25))
  expect_equal(pb3[["accuracy"]], c(0.25, 0.5, 0.25, 0.5, 0.75))
  expect_equal(pb3[["specificity"]], c(1, 1, 0, 0, 0))
  expect_equal(pb3[["sensitivity"]], c(0, 1 / 3, 1 / 3, 2 / 3, 1))
  expect_equal(pb3[["precision"]], c(1, 1, 0.5, 2 / 3, 0.75))
  expect_equal(pb3[["mcc"]], c(NA, 1 / 3, -0.5773503, -1 / 3, NA),
    tolerance = 1e-4
  )
  expect_equal(pb3[["fscore"]], c(0, 0.5, 0.4, 2 / 3, 0.8571429),
    tolerance = 1e-4
  )

  pevals4 <- calc_measures(
    scores = mmdat[["scores"]][[3]],
    labels = mmdat[["labels"]][[3]]
  )
  pb4 <- pevals4[["basic"]]
  expect_equal(pb4[["error"]], c(0.75, 0.5, 0.75, 0.5, 0.25))
  expect_equal(pb4[["accuracy"]], c(0.25, 0.5, 0.25, 0.5, 0.75))
  expect_equal(pb4[["specificity"]], c(1, 1, 0, 0, 0))
  expect_equal(pb4[["sensitivity"]], c(0, 1 / 3, 1 / 3, 2 / 3, 1))
  expect_equal(pb4[["precision"]], c(1, 1, 0.5, 2 / 3, 0.75))
  expect_equal(pb4[["mcc"]], c(NA, 1 / 3, -0.5773503, -1 / 3, NA),
    tolerance = 1e-4
  )
  expect_equal(pb4[["fscore"]], c(0, 0.5, 0.4, 2 / 3, 0.8571429),
    tolerance = 1e-4
  )
})

# Test calc_measures(cmats, ..., metrics)

test_that("calc_measures() calculates nothing extra by default", {
  pevals <- calc_measures(scores = c(0.1, 0.2, 0), labels = c(1, 0, 1))

  expect_setequal(
    intersect(names(pevals[["basic"]]), .get_metric_names("basic_all")),
    .get_metric_names("basic")
  )
})

test_that("calc_measures() adds only the measures it is asked for", {
  pevals <- calc_measures(
    scores = c(0.1, 0.2, 0), labels = c(1, 0, 1),
    metrics = .resolve_metrics(c("fpr", "odds"))
  )
  pb <- pevals[["basic"]]

  expect_true(all(c("fpr", "odds") %in% names(pb)))
  expect_false("lift" %in% names(pb))
})

test_that("the derived measures match their definitions", {
  scores <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4)
  labels <- c(1, 1, 0, 1, 0, 0)
  cmats <- create_confmats(
    scores = scores, labels = labels, keep_fmdat = TRUE
  )
  pb <- calc_measures(cmats, metrics = .resolve_metrics("all"))[["basic"]]

  tp <- cmats[["tp"]]
  fp <- cmats[["fp"]]
  tn <- cmats[["tn"]]
  fn <- cmats[["fn"]]
  n_all <- cmats[["pos_num"]] + cmats[["neg_num"]]

  expect_equal(pb[["fpr"]], fp / cmats[["neg_num"]])
  expect_equal(pb[["fnr"]], fn / cmats[["pos_num"]])
  expect_equal(pb[["predicted_positive_rate"]], (tp + fp) / n_all)
  expect_equal(pb[["predicted_negative_rate"]], (tn + fn) / n_all)
  expect_equal(pb[["false_discovery_rate"]], 1 - pb[["precision"]])
  expect_equal(pb[["false_omission_rate"]], 1 - pb[["npv"]])
  expect_equal(
    pb[["lift"]],
    pb[["sensitivity"]] / pb[["predicted_positive_rate"]]
  )
})

test_that("the odds ratio is NA wherever it is undefined, never infinite", {
  # FP is zero at the top of every dataset and FN at the bottom, so the ratio
  # has an empty cell under it there by construction
  scores <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4)
  labels <- c(1, 1, 0, 1, 0, 0)
  pb <- calc_measures(
    scores = scores, labels = labels,
    metrics = .resolve_metrics("all")
  )[["basic"]]

  n <- length(pb[["odds"]])
  expect_true(is.na(pb[["odds"]][1]))
  expect_true(is.na(pb[["odds"]][n]))
  expect_false(any(is.infinite(pb[["odds"]])))
  expect_false(any(is.nan(pb[["odds"]])))
  expect_false(any(is.infinite(pb[["lift"]])))
})

test_that("the derived measures line up with the ones they come from", {
  pb <- calc_measures(
    scores = c(0.1, 0.2, 0, 0.4), labels = c(1, 0, 1, 0),
    metrics = .resolve_metrics("all")
  )[["basic"]]

  all_names <- .get_metric_names("basic_all")
  lens <- vapply(pb[all_names], length, integer(1))
  expect_equal(
    unname(lens), rep(length(pb[["error"]]), length(all_names))
  )
})

test_that("a dataset with one class leaves the derived measures NA", {
  # Specificity is NA without negatives, and the FPR that comes off it too
  pb <- suppressWarnings(calc_measures(
    scores = c(0.1, 0.2, 0.3), labels = c(1, 1, 1),
    metrics = .resolve_metrics("all")
  ))[["basic"]]

  expect_true(all(is.na(pb[["fpr"]])))
  expect_false(any(is.na(pb[["fnr"]])))
})

test_that("nothing is derived when no measure was asked for", {
  # The curve pipelines and every default `evalmod()` call take this path, so
  # it has to cost nothing: the table is not even read
  cmats <- create_confmats(scores = c(0.1, 0.2, 0), labels = c(1, 0, 1))
  pb <- calc_measures(cmats)[["basic"]]

  expect_identical(.add_derived_measures(pb, cmats, NULL), pb)
  expect_identical(.add_derived_measures(pb, cmats, character(0)), pb)
  expect_identical(
    .add_derived_measures(pb, cmats, .get_metric_names("basic")), pb
  )
})

test_that("mutual information matches its definition", {
  scores <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4)
  labels <- c(1, 1, 0, 1, 0, 0)
  cmats <- create_confmats(scores = scores, labels = labels)
  pb <- calc_measures(cmats, metrics = .resolve_metrics("mi"))[["basic"]]

  # I(Y-hat; Y) over the four cells, in bits, calculated the long way
  n <- cmats[["pos_num"]] + cmats[["neg_num"]]
  expected <- vapply(seq_along(pb[["mi"]]), function(i) {
    tab <- matrix(
      c(
        cmats[["tp"]][i], cmats[["fp"]][i],
        cmats[["fn"]][i], cmats[["tn"]][i]
      ),
      nrow = 2
    )
    p <- tab / n
    rows <- rowSums(p)
    cols <- colSums(p)
    out <- 0
    for (r in 1:2) {
      for (cc in 1:2) {
        if (p[r, cc] > 0) {
          out <- out + p[r, cc] * log2(p[r, cc] / (rows[r] * cols[cc]))
        }
      }
    }
    out
  }, numeric(1))

  expect_equal(pb[["mi"]], expected)
})

test_that("mutual information is zero, not NA, where it is defined to be", {
  # A cutoff that predicts one class for everything carries no information
  # about the labels. ROCR reports NaN at those two points
  pb <- calc_measures(
    scores = c(0.9, 0.8, 0.7, 0.6), labels = c(1, 0, 1, 0),
    metrics = .resolve_metrics("mi")
  )[["basic"]]

  n <- length(pb[["mi"]])
  expect_equal(pb[["mi"]][1], 0)
  expect_equal(pb[["mi"]][n], 0)
  expect_false(any(is.na(pb[["mi"]])))
})

test_that("chi-square is n times the squared MCC", {
  scores <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4)
  labels <- c(1, 1, 0, 1, 0, 0)
  cmats <- create_confmats(scores = scores, labels = labels)
  pb <- calc_measures(cmats, metrics = .resolve_metrics("chisq"))[["basic"]]
  n <- cmats[["pos_num"]] + cmats[["neg_num"]]

  expect_equal(pb[["chisq"]], n * pb[["mcc"]]^2)

  # And therefore NA wherever a margin of the table is empty, which is where
  # the MCC is already NA
  expect_equal(is.na(pb[["chisq"]]), is.na(pb[["mcc"]]))
})

test_that("chi-square matches Pearson's statistic", {
  scores <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4)
  labels <- c(1, 1, 0, 1, 0, 0)
  cmats <- create_confmats(scores = scores, labels = labels)
  pb <- calc_measures(cmats, metrics = .resolve_metrics("chisq"))[["basic"]]

  i <- 3
  tab <- matrix(
    c(
      cmats[["tp"]][i], cmats[["fp"]][i],
      cmats[["fn"]][i], cmats[["tn"]][i]
    ),
    nrow = 2
  )
  expected <- suppressWarnings(
    unname(stats::chisq.test(tab, correct = FALSE)$statistic)
  )

  expect_equal(pb[["chisq"]][i], expected)
})

test_that("cost with the default weights is the error rate", {
  pb <- calc_measures(
    scores = c(0.9, 0.8, 0.7, 0.6), labels = c(1, 0, 1, 0),
    metrics = .resolve_metrics("cost")
  )[["basic"]]

  expect_equal(pb[["cost"]], pb[["error"]])
})

test_that("cost weights the two error counts", {
  scores <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4)
  labels <- c(1, 1, 0, 1, 0, 0)
  cmats <- create_confmats(scores = scores, labels = labels)
  pb <- calc_measures(cmats,
    metrics = .resolve_metrics("cost"),
    cost_fp = 3, cost_fn = 0.5
  )[["basic"]]
  n <- cmats[["pos_num"]] + cmats[["neg_num"]]

  expect_equal(
    pb[["cost"]],
    (cmats[["fn"]] * 0.5 + cmats[["fp"]] * 3) / n
  )
})

test_that("sar is the mean of accuracy, AUC(ROC) and 1 - RMSE", {
  scores <- c(0.95, 0.8, 0.7, 0.55, 0.4, 0.2)
  labels <- c(1, 1, 0, 1, 0, 0)
  cmats <- create_confmats(
    scores = scores, labels = labels, keep_fmdat = TRUE
  )
  pb <- calc_measures(cmats, metrics = .resolve_metrics("sar"))[["basic"]]

  roc_auc <- auc(evalmod(scores = scores, labels = labels))
  roc_auc <- roc_auc[roc_auc$curvetypes == "ROC", "aucs"]
  rmse <- sqrt(mean((scores - labels)^2))

  expect_equal(pb[["sar"]], (pb[["accuracy"]] + roc_auc + (1 - rmse)) / 3)
})

test_that("the AUC inside sar is the one auc() reports", {
  scores <- c(0.95, 0.8, 0.7, 0.55, 0.4, 0.2)
  labels <- c(1, 1, 0, 1, 0, 0)
  cmats <- create_confmats(
    scores = scores, labels = labels, keep_fmdat = TRUE
  )

  from_counts <- .roc_auc_from_counts(
    cmats[["tp"]], cmats[["fp"]], cmats
  )
  reported <- auc(evalmod(scores = scores, labels = labels))
  expect_equal(
    from_counts, reported[reported$curvetypes == "ROC", "aucs"]
  )
})

test_that("sar warns and returns NA when the scores are not probabilities", {
  cmats <- create_confmats(
    scores = c(9, 8, 7, 6), labels = c(1, 0, 1, 0), keep_fmdat = TRUE
  )

  expect_warning(
    pb <- calc_measures(cmats, metrics = .resolve_metrics("sar"))[["basic"]],
    "probabilities"
  )
  expect_true(all(is.na(pb[["sar"]])))

  # The measures that do not need the score values are still returned
  expect_false(any(is.na(pb[["accuracy"]])))
})

test_that("sar says so when it is handed matrices without the scores", {
  # calc_measures() asks for them when it builds the matrices itself, so this
  # is only reachable by handing it a cmats object built elsewhere
  cmats <- create_confmats(scores = c(0.9, 0.8), labels = c(1, 0))

  expect_error(
    calc_measures(cmats, metrics = .resolve_metrics("sar")),
    "keep_fmdat"
  )
})

test_that("calc_measures() keeps the scores when sar is asked for", {
  pb <- calc_measures(
    scores = c(0.9, 0.8, 0.7, 0.6), labels = c(1, 0, 1, 0),
    metrics = .resolve_metrics("sar")
  )[["basic"]]

  expect_false(any(is.na(pb[["sar"]])))
})

test_that("the AUC of a dataset with one class is NA inside sar", {
  cmats <- suppressWarnings(create_confmats(
    scores = c(0.9, 0.8, 0.7), labels = c(1, 1, 1), keep_fmdat = TRUE
  ))

  expect_true(is.na(
    .roc_auc_from_counts(cmats[["tp"]], cmats[["fp"]], cmats)
  ))
})
