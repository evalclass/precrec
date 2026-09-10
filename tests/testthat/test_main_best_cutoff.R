# Main: best_cutoff()

p10n10 <- function() {
  data(P10N10, package = "precrec", envir = environment())
  P10N10
}

## Three positives and three negatives arranged so that Youden's J reaches
## the same maximum at two different cutoffs
tied_j <- list(
  scores = c(5, 4, 3, 2, 1, 0),
  labels = c(1, 1, 0, 1, 0, 0)
)

imbalanced <- function(seed = 3) {
  set.seed(seed)
  list(
    scores = c(rnorm(20, 1.2), rnorm(180, 0)),
    labels = rep(c(1, 0), c(20, 180))
  )
}

test_that("best_cutoff() returns the documented columns", {
  d <- p10n10()
  best <- best_cutoff(scores = d$scores, labels = d$labels)

  expect_s3_class(best, "data.frame")
  expect_equal(nrow(best), 1)
  expect_equal(
    names(best)[1:8],
    c(
      "modname", "dsid", "metric", "value", "rank", "normalized_rank",
      "score", "label"
    )
  )
  expect_equal(best$metric, "informedness")
})

test_that("the result is a row of metric_table()", {
  # The point of returning the whole row: every other metric is there to be
  # read at the chosen cutoff, and it is the same number the table gives
  d <- p10n10()
  tab <- metric_table(scores = d$scores, labels = d$labels)
  best <- best_cutoff(scores = d$scores, labels = d$labels)

  row <- tab[tab$rank == best$rank, ]
  shared <- intersect(names(tab), names(best))
  expect_equal(nrow(row), 1)
  for (column in shared) {
    expect_equal(best[[column]], row[[column]], info = column)
  }

  # `value` is the criterion column, not a separately computed number
  expect_equal(best$value, best$informedness)
  expect_equal(best$value, max(tab$informedness, na.rm = TRUE))
})

test_that("Youden's J is the default, and its names all reach it", {
  d <- p10n10()
  by_default <- best_cutoff(scores = d$scores, labels = d$labels)

  for (name in c("youden", "youdens_j", "informedness")) {
    named <- best_cutoff(scores = d$scores, labels = d$labels, metric = name)
    expect_equal(named, by_default, info = name)
  }
})

test_that("topleft is the closest point to the perfect corner", {
  d <- p10n10()
  best <- best_cutoff(scores = d$scores, labels = d$labels, metric = "topleft")
  tab <- metric_table(
    scores = d$scores, labels = d$labels, metrics = "roc_dist"
  )

  expect_equal(best$metric, "roc_dist")
  expect_equal(best$value, min(tab$roc_dist, na.rm = TRUE))

  # It really is the distance to (0, 1) in ROC space, recomputed here
  dist <- sqrt((1 - tab$specificity)^2 + (1 - tab$sensitivity)^2)
  expect_equal(best$value, min(dist, na.rm = TRUE))
})

test_that("each metric is optimized in the direction that is better", {
  d <- p10n10()
  maximized <- c(
    "accuracy", "sensitivity", "precision", "mcc", "fscore",
    "balanced_accuracy", "npv", "informedness", "markedness", "kappa",
    "jaccard", "sedi"
  )
  minimized <- c(
    "error", "cost", "roc_dist", "fpr", "fnr", "false_discovery_rate",
    "false_omission_rate", "negative_likelihood_ratio"
  )

  for (metric in c(maximized, minimized)) {
    best <- best_cutoff(
      scores = d$scores, labels = d$labels, metric = metric
    )
    tab <- metric_table(
      scores = d$scores, labels = d$labels, metrics = metric
    )
    wanted <- if (metric %in% maximized) {
      max(tab[[metric]], na.rm = TRUE)
    } else {
      min(tab[[metric]], na.rm = TRUE)
    }

    expect_equal(best$value, wanted, info = metric)
    expect_equal(best$value, best[[metric]], info = metric)
  }
})

test_that("a tie is broken toward the smallest rank", {
  # Two cutoffs share the maximum, and the one that calls fewer instances
  # positive is the one returned
  tab <- metric_table(scores = tied_j$scores, labels = tied_j$labels)
  optimal <- tab$rank[tab$informedness == max(tab$informedness)]
  expect_equal(optimal, c(2, 4)) # the tie the fixture is built for

  best <- best_cutoff(scores = tied_j$scores, labels = tied_j$labels)
  expect_equal(best$rank, 2L)
  expect_equal(best$rank, min(optimal))
})

test_that("a metric with no interior optimum lands on the end of the range", {
  # Documented rather than warned about: these are the correct answers to
  # the question asked
  d <- p10n10()
  n <- length(d$scores)

  sn <- best_cutoff(
    scores = d$scores, labels = d$labels, metric = "sensitivity"
  )
  sp <- best_cutoff(
    scores = d$scores, labels = d$labels, metric = "specificity"
  )

  expect_equal(sn$rank, n) # everything called positive
  expect_equal(sn$value, 1)
  expect_equal(sp$rank, 0L) # nothing called positive
  expect_equal(sp$value, 1)
})

test_that("the prevalence-blind and prevalence-aware criteria disagree", {
  # The reason the argument exists. On twenty positives against a hundred
  # and eighty negatives, Youden's J is free to accept a cutoff whose
  # precision no one would deploy, because neither sensitivity nor
  # specificity knows how rare the positives are.
  d <- imbalanced()
  youden <- best_cutoff(scores = d$scores, labels = d$labels)
  mcc <- best_cutoff(scores = d$scores, labels = d$labels, metric = "mcc")

  expect_true(youden$rank > mcc$rank)
  expect_true(youden$precision < mcc$precision)

  # Each is the best of its own criterion, and worse on the other's
  expect_true(youden$informedness >= mcc$informedness)
  expect_true(mcc$mcc >= youden$mcc)
})

test_that("a heavier cost of a miss moves the cutoff toward more positives", {
  d <- imbalanced()
  args <- list(scores = d$scores, labels = d$labels, metric = "cost")

  cheap <- do.call(best_cutoff, c(args, cost_fp = 1, cost_fn = 1))
  dear <- do.call(best_cutoff, c(args, cost_fp = 1, cost_fn = 20))

  expect_true(dear$rank > cheap$rank)
  expect_true(dear$sensitivity >= cheap$sensitivity)

  # With equal costs the criterion is the error rate
  by_error <- best_cutoff(
    scores = d$scores, labels = d$labels, metric = "error"
  )
  expect_equal(cheap$value, by_error$value)
})

test_that("beta reaches the F-score through the dots", {
  d <- imbalanced()
  args <- list(scores = d$scores, labels = d$labels, metric = "fscore")

  balanced <- do.call(best_cutoff, args)
  recall_heavy <- do.call(best_cutoff, c(args, beta = 3))

  expect_true(recall_heavy$rank > balanced$rank)
  expect_true(recall_heavy$sensitivity >= balanced$sensitivity)
})

test_that("an object that is already calculated gives the same answer", {
  d <- p10n10()
  points <- evalmod(
    scores = d$scores, labels = d$labels, mode = "basic",
    metrics = "roc_dist"
  )

  expect_equal(
    best_cutoff(points, metric = "topleft"),
    best_cutoff(scores = d$scores, labels = d$labels, metric = "topleft")
  )
  # The object carries `roc_dist` as well, so it has one column more; the
  # cutoff it picks is the same one
  reused <- best_cutoff(points)
  built <- best_cutoff(scores = d$scores, labels = d$labels)
  expect_equal(reused[, names(built)], built)
})

test_that("several models and test datasets get one row each", {
  samps <- create_sim_samples(3, 50, 50, c("poor_er", "good_er"))
  mdat <- mmdata(samps[["scores"]], samps[["labels"]],
    modnames = samps[["modnames"]], dsids = samps[["dsids"]]
  )
  best <- best_cutoff(mdat, metric = "mcc")
  tab <- metric_table(mdat, metrics = "mcc")

  expect_equal(nrow(best), 6)
  expect_equal(as.character(best$modname), rep(c("poor_er", "good_er"), each = 3))
  expect_equal(best$dsid, tab$dsid[!duplicated(paste(tab$modname, tab$dsid))])

  # Each row is the optimum of its own pairing, and of nothing else
  for (i in seq_len(nrow(best))) {
    rows <- tab$modname == best$modname[i] & tab$dsid == best$dsid[i]
    expect_equal(best$value[i], max(tab$mcc[rows], na.rm = TRUE))
  }
})

test_that("the ranks are per dataset even when the datasets differ in size", {
  mdat <- mmdata(
    list(c(5, 4, 3, 2, 1, 0), c(3, 2, 1, 0)),
    list(c(1, 1, 0, 1, 0, 0), c(1, 1, 0, 0)),
    modnames = c("m1", "m1"), dsids = c(1, 2)
  )
  best <- best_cutoff(mdat)

  expect_equal(nrow(best), 2)
  expect_equal(best$rank, c(2L, 2L))
  expect_equal(best$normalized_rank, c(2 / 6, 2 / 4))
})

test_that("a pairing with no usable value keeps its row", {
  # A single-class dataset has no informedness anywhere, and dropping the
  # row would leave the caller to notice its absence
  best <- suppressWarnings(
    best_cutoff(scores = c(3, 2, 1), labels = c(1, 1, 1))
  )

  expect_equal(nrow(best), 1)
  expect_equal(as.character(best$modname), "m1")
  expect_equal(best$metric, "informedness")
  expect_true(is.na(best$value))
  expect_true(is.na(best$rank))
})

test_that("best_cutoff() refuses a metric it cannot optimize", {
  d <- p10n10()

  # These four describe a cutoff rather than scoring it
  for (metric in c(
    "score", "label", "predicted_positive_rate", "predicted_negative_rate"
  )) {
    expect_error(
      best_cutoff(scores = d$scores, labels = d$labels, metric = metric),
      class = "precrec_error_invalid_metric"
    )
  }

  expect_error(
    best_cutoff(scores = d$scores, labels = d$labels, metric = "nonesuch"),
    class = "precrec_error_invalid_metric"
  )
  expect_error(
    best_cutoff(scores = d$scores, labels = d$labels, metric = c("mcc", "f")),
    class = "precrec_error_invalid_metric"
  )
  expect_error(
    best_cutoff(scores = d$scores, labels = d$labels, metric = 1),
    class = "precrec_error_invalid_metric"
  )
})

test_that("best_cutoff() refuses input it cannot read cutoffs from", {
  d <- p10n10()

  # Curves do not keep the scores
  expect_error(
    best_cutoff(evalmod(scores = d$scores, labels = d$labels)),
    class = "precrec_error_invalid_x"
  )

  # An object that does not hold the criterion
  points <- evalmod(scores = d$scores, labels = d$labels, mode = "basic")
  expect_error(
    best_cutoff(points, metric = "topleft"),
    class = "precrec_error_invalid_metric"
  )

  # Arguments meant for evalmod() reach an object that is already built
  expect_error(
    best_cutoff(points, metric = "mcc", beta = 2),
    class = "precrec_error_invalid_x"
  )

  # Several test datasets averaged together
  samps <- create_sim_samples(2, 50, 50, "good_er")
  averaged <- evalmod(
    mmdata(samps[["scores"]], samps[["labels"]], dsids = samps[["dsids"]]),
    mode = "basic"
  )
  expect_error(best_cutoff(averaged), class = "precrec_error_invalid_x")
})
