# G: average precision
# Test average_precision(curves), .calc_average_precision(pb, x_name, y_name)

ap_samples <- function(n_ds = 1) {
  set.seed(1)
  create_sim_samples(n_ds, 50, 50, c("poor_er", "good_er"))
}

test_that("average_precision() rejects an object of unknown class", {
  expect_error(average_precision(list()), "unknown class")
  expect_error(average_precision(1), "unknown class")
})

test_that("average_precision() returns one row per model and dataset", {
  samps <- ap_samples(3)
  mdat <- mmdata(samps[["scores"]], samps[["labels"]],
    modnames = samps[["modnames"]], dsids = samps[["dsids"]]
  )
  aps <- average_precision(evalmod(mdat))

  expect_s3_class(aps, "data.frame")
  expect_equal(names(aps), c("modnames", "dsids", "aps", "baselines"))
  expect_equal(nrow(aps), 6)
  expect_equal(
    sort(unique(as.character(aps[["modnames"]]))),
    c("good_er", "poor_er")
  )
})

test_that("average_precision() sums the recall gains times the precision", {
  data(P10N10)
  curves <- evalmod(scores = P10N10[["scores"]], labels = P10N10[["labels"]])
  pb <- calc_metrics(
    scores = P10N10[["scores"]], labels = P10N10[["labels"]]
  )[["basic"]]

  rec <- pb[["sensitivity"]]
  prec <- pb[["precision"]]
  n <- length(rec)
  expected <- sum((rec[2:n] - rec[1:(n - 1)]) * prec[2:n])

  expect_equal(average_precision(curves)[["aps"]], expected)
})

test_that("average_precision() reads the raw points, not the curve", {
  # x_bins reduces the interpolated curve the AUC is taken from, and leaves
  # the per-cutoff points average precision is built on alone
  samps <- ap_samples()
  mdat <- mmdata(samps[["scores"]], samps[["labels"]],
    modnames = samps[["modnames"]], dsids = samps[["dsids"]]
  )

  coarse <- average_precision(evalmod(mdat, x_bins = 4))
  fine <- average_precision(evalmod(mdat, x_bins = 1000))

  expect_equal(coarse[["aps"]], fine[["aps"]])
})

test_that("average_precision() survives raw_curves = FALSE", {
  # Unlike prbe(), which reads the per-dataset curves, this is gathered
  # before they are dropped
  samps <- ap_samples(3)
  mdat <- mmdata(samps[["scores"]], samps[["labels"]],
    modnames = samps[["modnames"]], dsids = samps[["dsids"]]
  )

  expect_equal(
    average_precision(evalmod(mdat, raw_curves = FALSE)),
    average_precision(evalmod(mdat, raw_curves = TRUE))
  )
})

test_that("average precision is 1 for a perfectly separable dataset", {
  curves <- evalmod(
    scores = c(0.9, 0.8, 0.7, 0.2, 0.1), labels = c(1, 1, 1, 0, 0)
  )

  expect_equal(average_precision(curves)[["aps"]], 1)
})

test_that("average precision differs from the interpolated PRC AUC", {
  # The two are different estimators of the same thing, and the step
  # estimator reads high - that gap is the reason both are reported
  data(P10N10)
  curves <- evalmod(scores = P10N10[["scores"]], labels = P10N10[["labels"]])

  aucs <- auc(curves)
  prc_auc <- aucs[aucs[["curvetypes"]] == "PRC", "aucs"]
  ap <- average_precision(curves)[["aps"]]

  expect_false(isTRUE(all.equal(ap, prc_auc)))
  expect_gt(ap, prc_auc)
})

test_that(".calc_average_precision() is NA for a ROC curve", {
  pb <- calc_metrics(
    scores = c(0.9, 0.8, 0.7, 0.6), labels = c(1, 1, 0, 0)
  )[["basic"]]

  expect_true(is.na(
    .calc_average_precision(pb, "specificity", "sensitivity")
  ))
  expect_false(is.na(
    .calc_average_precision(pb, "sensitivity", "precision")
  ))
})

test_that("the average precision attribute is on the PRC curve only", {
  curves <- create_curves(
    scores = c(0.9, 0.8, 0.7, 0.6), labels = c(1, 1, 0, 0)
  )

  expect_true(is.na(attr(curves[["roc"]], "ap")))
  expect_false(is.na(attr(curves[["prc"]], "ap")))
})

test_that("average_precision() reports the chance level too", {
  set.seed(1)
  scores <- c(rnorm(20, 1.2), rnorm(980, 0))
  labels <- rep(c(1, 0), c(20, 980))
  curves <- evalmod(scores = scores, labels = labels)

  aps <- average_precision(curves)
  expect_equal(aps[["baselines"]], 0.02)

  # The same baseline the interpolated area is read against
  prc <- subset(auc(curves), curvetypes == "PRC")
  expect_equal(aps[["baselines"]], prc[["baselines"]])
})
