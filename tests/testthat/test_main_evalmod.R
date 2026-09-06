# MA 1: evalmod
# Test evalmod()
#

test_that("m1 scores", {
  s1 <- c(3, 2, 2, 1)
  l1 <- c(1, 0, 1, 0)

  mdat1 <- mmdata(s1, l1)
  cv1 <- evalmod(mdat1, x_bins = 4)

  expect_equal(cv1[["rocs"]][[1]][["x"]], c(0, 0, 0.25, 0.5, 0.75, 1))
  expect_equal(cv1[["rocs"]][[1]][["y"]], c(0, 0.5, 0.75, 1, 1, 1))

  expect_equal(cv1[["prcs"]][[1]][["x"]], c(0, 0.25, 0.5, 0.75, 1, 1))
  expect_equal(cv1[["prcs"]][[1]][["y"]], c(1, 1, 1, 0.75, 0.6666666667, 0.5),
    tolerance = 1e-2
  )
})

test_that("m2 scores", {
  s2 <- c(4, 3, 2, 1)
  l2 <- c(0, 0, 1, 1)

  mdat2 <- mmdata(s2, l2)
  cv2 <- evalmod(mdat2, x_bins = 4)

  expect_equal(cv2[["rocs"]][[1]][["x"]], c(0, 0.25, 0.5, 0.75, 1, 1, 1))
  expect_equal(cv2[["rocs"]][[1]][["y"]], c(0, 0, 0, 0, 0, 0.5, 1))

  expect_equal(cv2[["prcs"]][[1]][["x"]], c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(cv2[["prcs"]][[1]][["y"]], c(
    0, 0.2, 0.3333333333, 0.4285714286,
    0.5
  ),
  tolerance = 1e-2
  )
})

test_that("m3 scores", {
  s3 <- c(3, 3, 2, 1)
  l3 <- c(1, 0, 0, 1)

  mdat3 <- mmdata(s3, l3)
  cv3 <- evalmod(mdat3, x_bins = 4)

  expect_equal(cv3[["rocs"]][[1]][["x"]], c(0, 0.25, 0.5, 0.75, 1, 1))
  expect_equal(cv3[["rocs"]][[1]][["y"]], c(0, 0.25, 0.5, 0.5, 0.5, 1))

  expect_equal(cv3[["prcs"]][[1]][["x"]], c(0, 0.25, 0.5, 0.5, 0.75, 1))
  expect_equal(cv3[["prcs"]][[1]][["y"]], c(
    0.5, 0.5, 0.5, 0.3333333333,
    0.4285714286, 0.5
  ),
  tolerance = 1e-2
  )
})

test_that("'mode' must be consistent between 'mmdata' and 'evalmode'", {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 0)
  l2 <- c(1, 1, 0, 0)
  l3 <- c(0, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  md1 <- mmdata(scores, labels)
  expect_equal(attr(md1, "args")[["mode"]], "rocprc")

  expect_silent(evalmod(md1))
  em1_1 <- evalmod(md1)
  expect_equal(attr(evalmod(md1), "args")[["mode"]], "rocprc")

  expect_silent(evalmod(md1, mode = "rocprc"))
  em1_2 <- evalmod(md1, mode = "rocprc")
  expect_equal(attr(em1_2, "args")[["mode"]], "rocprc")

  expect_silent(evalmod(md1, mode = "basic"))
  em1_3 <- evalmod(md1, mode = "basic")
  expect_equal(attr(em1_3, "args")[["mode"]], "basic")

  expect_silent(evalmod(md1, mode = "aucroc"))
  em1_4 <- evalmod(md1, mode = "aucroc")
  expect_equal(attr(em1_4, "args")[["mode"]], "aucroc")


  md2 <- mmdata(scores, labels, mode = "basic")
  expect_equal(attr(md2, "args")[["mode"]], "basic")

  expect_silent(evalmod(md2))
  em2_1 <- evalmod(md2)
  expect_equal(attr(em2_1, "args")[["mode"]], "basic")

  expect_silent(evalmod(md2, mode = "rocprc"))
  em2_2 <- evalmod(md2, mode = "rocprc")
  expect_equal(attr(em2_2, "args")[["mode"]], "rocprc")

  expect_silent(evalmod(md2, mode = "basic"))
  em2_3 <- evalmod(md2, mode = "basic")
  expect_equal(attr(em2_3, "args")[["mode"]], "basic")

  expect_silent(evalmod(md2, mode = "aucroc"))
  em2_4 <- evalmod(md2, mode = "aucroc")
  expect_equal(attr(em2_4, "args")[["mode"]], "aucroc")


  md3 <- mmdata(scores, labels, mode = "aucroc")
  expect_equal(attr(md3, "args")[["mode"]], "aucroc")

  expect_silent(evalmod(md3))
  em3_1 <- evalmod(md3)
  expect_equal(attr(em3_1, "args")[["mode"]], "aucroc")

  expect_error(evalmod(md3, mode = "rocprc"), "Invalid 'mode':")

  expect_error(evalmod(md3, mode = "basic"), "Invalid 'mode':")

  expect_silent(evalmod(md3, mode = "aucroc"))
  em3_4 <- evalmod(md3, mode = "aucroc")
  expect_equal(attr(em3_4, "args")[["mode"]], "aucroc")
})

test_that("evalmod() ranks NAs as 'na_worst' requests for negative scores", {
  scores <- c(-1, -2, NA, -3, -4)
  labels <- c(1, 1, 1, 0, 0)

  # Adding a constant to every score must leave the curves unchanged
  expect_equal(
    auc(evalmod(scores = scores, labels = labels))[["aucs"]],
    auc(evalmod(scores = scores + 10, labels = labels))[["aucs"]]
  )

  expect_equal(
    auc(evalmod(scores = scores, labels = labels, na_worst = FALSE))[["aucs"]],
    auc(evalmod(
      scores = scores + 10, labels = labels,
      na_worst = FALSE
    ))[["aucs"]]
  )

  # The NA belongs to a positive label, so ranking it worst lowers the AUCs
  aucs_worst <- auc(evalmod(scores = scores, labels = labels))[["aucs"]]
  aucs_best <- auc(evalmod(
    scores = scores, labels = labels,
    na_worst = FALSE
  ))[["aucs"]]
  expect_true(all(aucs_worst < aucs_best))
  expect_equal(subset(
    auc(evalmod(scores = scores, labels = labels)),
    curvetypes == "ROC"
  )[["aucs"]], 2 / 3, tolerance = 1e-4)
})

test_that("evalmod() passes 'beta' through to the F-beta score", {
  data(P10N10)
  pfunc <- function(beta) {
    points <- evalmod(
      mode = "basic", scores = P10N10$scores,
      labels = P10N10$labels, beta = beta
    )
    as.data.frame(points)
  }

  df1 <- pfunc(1)
  df2 <- pfunc(2)

  # Only the F-score moves
  expect_equal(
    df1[df1$type != "fscore", "y"],
    df2[df2$type != "fscore", "y"]
  )
  expect_false(isTRUE(all.equal(
    df1[df1$type == "fscore", "y"],
    df2[df2$type == "fscore", "y"]
  )))
})

test_that("evalmod() returns the new confusion-matrix measures", {
  data(P10N10)
  points <- evalmod(
    mode = "basic", scores = P10N10$scores,
    labels = P10N10$labels
  )
  df <- as.data.frame(points)

  expect_true(all(
    c(
      "balanced_accuracy", "npv", "informedness", "markedness", "kappa"
    ) %in% levels(df[["type"]])
  ))
})

test_that("'beta' must be a single non-negative finite number", {
  data(P10N10)
  expect_err <- function(beta) {
    expect_error(
      evalmod(
        mode = "basic", scores = P10N10$scores,
        labels = P10N10$labels, beta = beta
      ),
      class = "precrec_error_invalid_beta"
    )
  }

  expect_err(-1)
  expect_err("1")
  expect_err(c(1, 2))
  expect_err(Inf)
})

# Test evalmod(mode = "basic", metrics = )

em_metrics_mdat <- function() {
  set.seed(1)
  samps <- create_sim_samples(2, 20, 20, "good_er")
  mmdata(samps[["scores"]], samps[["labels"]],
    modnames = samps[["modnames"]], dsids = samps[["dsids"]]
  )
}

test_that("evalmod() holds fourteen measures when 'metrics' is not given", {
  # An existing caller's default plot has one panel per measure, so this is
  # what keeps the new measures from changing it
  mp <- evalmod(em_metrics_mdat(), mode = "basic")

  expect_length(mp, 14)
  expect_equal(attr(mp, "metrics"), .get_metric_names("basic"))
})

test_that("evalmod(metrics = ) adds the measures it is given", {
  mp <- evalmod(em_metrics_mdat(), mode = "basic", metrics = c("lift", "fpr"))

  expect_length(mp, 16)
  expect_equal(
    attr(mp, "metrics"),
    c(.get_metric_names("basic"), "fpr", "lift")
  )
})

test_that("evalmod(metrics = 'all') holds every measure", {
  mp <- evalmod(em_metrics_mdat(), mode = "basic", metrics = "all")

  expect_length(mp, length(.get_metric_names("basic_all")))
  expect_equal(attr(mp, "metrics"), .get_metric_names("basic_all"))
})

test_that("evalmod() accepts the ROCR identifiers for the new measures", {
  mp <- evalmod(em_metrics_mdat(), mode = "basic", metrics = c("fall", "rpp"))

  expect_true(
    all(c("fpr", "predicted_positive_rate") %in% attr(mp, "metrics"))
  )
})

test_that("evalmod() rejects a measure it does not know", {
  expect_error(
    evalmod(em_metrics_mdat(), mode = "basic", metrics = "nonesuch"),
    class = "precrec_error_invalid_metrics"
  )
})

test_that("the added measures survive averaging over datasets", {
  mp <- evalmod(em_metrics_mdat(), mode = "basic", metrics = "all")
  avg <- attr(mp, "grp_avg")

  expect_true("lift" %in% names(avg))
  expect_false(all(is.na(avg[["lift"]])))
})

test_that("as.data.frame() carries only the measures the object holds", {
  df1 <- as.data.frame(evalmod(em_metrics_mdat(), mode = "basic"))
  df2 <- as.data.frame(
    evalmod(em_metrics_mdat(), mode = "basic", metrics = "fpr")
  )

  expect_false("fpr" %in% levels(factor(df1[["type"]])))
  expect_true("fpr" %in% levels(factor(df2[["type"]])))
})

test_that("a measure that was not calculated cannot be plotted", {
  mp <- evalmod(em_metrics_mdat(), mode = "basic")

  expect_error(autoplot(mp, curvetype = "lift"),
    class = "precrec_error_invalid_curvetype"
  )
  expect_error(plot(mp, curvetype = "lift"),
    class = "precrec_error_invalid_curvetype"
  )
})

# Test evalmod(cost_fp = , cost_fn = )

test_that("the cost weights reach the measure", {
  mdat <- em_metrics_mdat()
  mp1 <- evalmod(mdat, mode = "basic", metrics = "cost")
  mp2 <- evalmod(mdat,
    mode = "basic", metrics = "cost",
    cost_fp = 3, cost_fn = 0.5
  )

  cost1 <- as.data.frame(mp1)
  cost2 <- as.data.frame(mp2)
  expect_false(isTRUE(all.equal(
    cost1[cost1$type == "cost", "y"],
    cost2[cost2$type == "cost", "y"]
  )))
})

test_that("a cost must be a single number and cannot be negative", {
  mdat <- em_metrics_mdat()

  expect_error(evalmod(mdat, mode = "basic", cost_fp = -1),
    class = "precrec_error_invalid_cost_fp"
  )
  expect_error(evalmod(mdat, mode = "basic", cost_fn = "1"),
    class = "precrec_error_invalid_cost_fn"
  )
  expect_error(evalmod(mdat, mode = "basic", cost_fp = c(1, 2)),
    class = "precrec_error_invalid_cost_fp"
  )
})
