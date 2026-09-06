# G: prob_metrics and prob_metrics_ci
# Test prob_metrics(mdat, scores, labels, eps, ...)

pm_test_scores <- function() {
  c(0.9, 0.8, 0.6, 0.55, 0.4, 0.45, 0.3, 0.2, 0.1, 0.05)
}

pm_test_labels <- function() {
  c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
}

test_that("prob_metrics() returns a data frame of all three metrics", {
  pm <- prob_metrics(scores = pm_test_scores(), labels = pm_test_labels())

  expect_true(is.data.frame(pm))
  expect_false(is(pm, "data.table"))
  expect_equal(names(pm), c("modnames", "dsids", "metrics", "values"))
  expect_equal(pm[["metrics"]], c("brier", "rmse", "logloss"))
})

test_that("prob_metrics() matches the definitions of the three metrics", {
  scores <- pm_test_scores()
  labels <- pm_test_labels()
  pm <- prob_metrics(scores = scores, labels = labels)

  brier <- mean((scores - labels)^2)
  logloss <- -mean(labels * log(scores) + (1 - labels) * log(1 - scores))

  expect_equal(pm[["values"]][1], brier)
  expect_equal(pm[["values"]][2], sqrt(brier))
  expect_equal(pm[["values"]][3], logloss)
})

test_that("prob_metrics() accepts an 'mdat' object", {
  scores <- pm_test_scores()
  labels <- pm_test_labels()
  mdat <- mmdata(scores, labels)

  expect_equal(
    prob_metrics(mdat),
    prob_metrics(scores = scores, labels = labels)
  )
})

test_that("prob_metrics() keeps model names and dataset IDs", {
  scores <- join_scores(pm_test_scores(), rev(pm_test_scores()))
  labels <- join_labels(pm_test_labels(), pm_test_labels())
  mdat <- mmdata(scores, labels, modnames = c("m1", "m1"), dsids = c(1, 2))
  pm <- prob_metrics(mdat)

  expect_equal(nrow(pm), 6)
  expect_equal(pm[["modnames"]], rep("m1", 6))
  expect_equal(pm[["dsids"]], c(1, 1, 1, 2, 2, 2))

  # The reversed scores are the worse model on all three counts
  expect_true(all(pm[["values"]][4:6] > pm[["values"]][1:3]))
})

test_that("prob_metrics() clamps the log loss of certain predictions", {
  # A confident and wrong prediction would otherwise make the log loss
  # infinite
  pm1 <- prob_metrics(scores = c(1, 0, 0), labels = c(1, 0, 1))
  expect_true(is.finite(pm1[["values"]][3]))

  # A larger eps is a smaller penalty
  pm2 <- prob_metrics(
    scores = c(1, 0, 0), labels = c(1, 0, 1),
    eps = 1e-5
  )
  expect_true(pm2[["values"]][3] < pm1[["values"]][3])

  # The Brier score is not clamped
  expect_equal(pm1[["values"]][1], pm2[["values"]][1])
})

test_that("'scores' must be probabilities", {
  labels <- pm_test_labels()

  expect_error(
    prob_metrics(scores = pm_test_scores() * 10, labels = labels),
    class = "precrec_error_invalid_scores"
  )
  expect_error(
    prob_metrics(scores = pm_test_scores() - 1, labels = labels),
    class = "precrec_error_invalid_scores"
  )

  na_scores <- pm_test_scores()
  na_scores[1] <- NA
  expect_error(
    prob_metrics(scores = na_scores, labels = labels),
    class = "precrec_error_invalid_scores"
  )
})

test_that("'eps' must be a single number between 0 and 0.5", {
  scores <- pm_test_scores()
  labels <- pm_test_labels()

  expect_error(prob_metrics(scores = scores, labels = labels, eps = -1),
    class = "precrec_error_invalid_eps"
  )
  expect_error(prob_metrics(scores = scores, labels = labels, eps = 1),
    class = "precrec_error_invalid_eps"
  )
  expect_error(prob_metrics(scores = scores, labels = labels, eps = "0"),
    class = "precrec_error_invalid_eps"
  )
})

# Test prob_metrics_ci(mdat, scores, labels, eps, alpha, dtype, ...)

pm_test_mdat <- function() {
  set.seed(1)
  samps <- create_sim_samples(4, 20, 20, c("poor_er", "good_er"))
  mmdata(samps[["scores"]], samps[["labels"]],
    modnames = samps[["modnames"]], dsids = samps[["dsids"]]
  )
}

test_that("prob_metrics_ci() returns one row per model and metric", {
  ci <- prob_metrics_ci(pm_test_mdat())

  expect_true(is.data.frame(ci))
  expect_equal(names(ci), c(
    "modnames", "metrics", "mean", "error",
    "lower_bound", "upper_bound", "n"
  ))
  expect_equal(nrow(ci), 6)
  expect_equal(ci[["n"]], rep(4, 6))
})

test_that("prob_metrics_ci() matches the per-dataset values", {
  mdat <- pm_test_mdat()
  pm <- prob_metrics(mdat)
  ci <- prob_metrics_ci(mdat)

  brier <- pm[pm$modnames == "poor_er" & pm$metrics == "brier", "values"]
  ci_row <- ci[ci$modnames == "poor_er" & ci$metrics == "brier", ]

  expect_equal(ci_row[["mean"]], mean(brier))
  expect_equal(
    ci_row[["error"]],
    qnorm(0.975) * sd(brier) / sqrt(length(brier))
  )
})

test_that("prob_metrics_ci() widens the interval for the t distribution", {
  mdat <- pm_test_mdat()
  ci_z <- prob_metrics_ci(mdat, dtype = "normal")
  ci_t <- prob_metrics_ci(mdat, dtype = "t")

  expect_true(all(ci_t[["error"]] > ci_z[["error"]]))
  expect_equal(ci_z[["mean"]], ci_t[["mean"]])
})

test_that("prob_metrics_ci() keeps the metrics inside their own ranges", {
  # alpha = 0 asks for the whole distribution, so the raw interval is
  # unbounded and only the clipping is left to look at
  ci <- prob_metrics_ci(pm_test_mdat(), alpha = 0)

  brier <- ci[ci$metrics == "brier", ]
  logloss <- ci[ci$metrics == "logloss", ]

  # The Brier score cannot leave [0, 1] and is clipped to it
  expect_true(all(brier[["lower_bound"]] == 0))
  expect_true(all(brier[["upper_bound"]] == 1))

  # The log loss is bounded below by 0 but has no upper bound to clip against
  expect_true(all(logloss[["lower_bound"]] == 0))
  expect_equal(
    logloss[["upper_bound"]],
    logloss[["mean"]] + logloss[["error"]]
  )
})

test_that("prob_metrics_ci() requires multiple datasets", {
  expect_error(
    prob_metrics_ci(scores = pm_test_scores(), labels = pm_test_labels()),
    class = "precrec_error_invalid_mdat"
  )
})

test_that("'dtype' must be a known distribution", {
  expect_error(prob_metrics_ci(pm_test_mdat(), dtype = "cauchy"),
    class = "precrec_error_invalid_dtype"
  )
})

test_that("'alpha' must be a single number between 0 and 1", {
  expect_error(prob_metrics_ci(pm_test_mdat(), alpha = 2),
    class = "precrec_error_invalid_alpha"
  )
})
