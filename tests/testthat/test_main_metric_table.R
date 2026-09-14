# Main: metric_table()

test_that("metric_table() returns one row per cutoff", {
  data(P10N10)
  tab <- metric_table(scores = P10N10$scores, labels = P10N10$labels)

  # Twenty instances, so twenty-one cutoffs: call none, call one, ... all
  expect_s3_class(tab, "data.frame")
  expect_false(data.table::is.data.table(tab))
  expect_equal(nrow(tab), 21)
  expect_equal(tab$rank, 0:20)
  expect_equal(tab$normalized_rank, (0:20) / 20)
})

test_that("metric_table() puts the keys first and the metrics in order", {
  data(P10N10)
  tab <- metric_table(scores = P10N10$scores, labels = P10N10$labels)

  expect_equal(
    names(tab),
    c(
      "modname", "dsid", "rank", "normalized_rank", "score", "label",
      "error", "accuracy", "specificity", "sensitivity", "precision",
      "mcc", "fscore", "balanced_accuracy", "npv", "informedness",
      "markedness", "kappa"
    )
  )
})

test_that("a row is the cutoff that calls the top k positive", {
  scores <- c(9, 8, 7, 6, 3, 2)
  labels <- c(1, 1, 0, 1, 0, 0)
  tab <- metric_table(scores = scores, labels = labels)

  # Rank 0 classifies nothing, so there is no instance to report
  expect_true(is.na(tab$score[1]))
  expect_true(is.na(tab$label[1]))
  expect_equal(tab$sensitivity[1], 0)
  expect_equal(tab$specificity[1], 1)

  # Every other row carries the score of the instance at that rank,
  # descending, and its class
  expect_equal(tab$score[-1], sort(scores, decreasing = TRUE))
  expect_equal(tab$label[-1], c(1, 1, -1, 1, -1, -1))

  # Three positives: cutting at rank 3 catches two of them
  expect_equal(tab$sensitivity[tab$rank == 3], 2 / 3)
  expect_equal(tab$specificity[tab$rank == 3], 2 / 3)
  expect_equal(tab$precision[tab$rank == 3], 2 / 3)
})

test_that("the table is the long data frame on its side", {
  # The two must not drift: one C++ converter feeds both. The two end rows
  # are the deliberate exception - the table blanks the precision and NPV
  # cells that have no denominator, and the long form keeps the inherited
  # values because the curve is anchored on them. Everything else is equal
  # row for row.
  samps <- create_sim_samples(1, 30, 30, c("good_er", "poor_er"))
  mdat <- mmdata(samps[["scores"]], samps[["labels"]],
    modnames = samps[["modnames"]]
  )
  points <- evalmod(mdat, mode = "basic")

  tab <- metric_table(points)
  long <- as.data.frame(points)

  for (metric in c("sensitivity", "precision", "mcc", "kappa")) {
    part <- long[long$type == metric, ]
    part <- part[order(part$modname, part$x), ]
    ordered <- tab[order(tab$modname, tab$normalized_rank), ]

    ends <- ordered$rank == 0L |
      ordered$rank == ave(ordered$rank, ordered$modname, FUN = max)
    blanked <- metric == "precision" & ends

    expect_equal(ordered[[metric]][!blanked], part$y[!blanked])
    expect_equal(ordered$normalized_rank, part$x)

    # The exception is exactly the cells the table blanks, and only there
    if (metric == "precision") {
      expect_true(all(is.na(ordered[[metric]][ordered$rank == 0L])))
      expect_false(anyNA(part$y))
    }
  }
})

test_that("metric_table() adds the metrics it is asked for", {
  data(P10N10)
  # Scaled into [0, 1] so that `sar`, which wants probabilities, has
  # something to work with under `metrics = "all"`
  probs <- (P10N10$scores - min(P10N10$scores)) / diff(range(P10N10$scores))
  args <- list(scores = probs, labels = P10N10$labels)

  plain <- do.call(metric_table, args)
  more <- do.call(metric_table, c(args, list(metrics = c("lift", "jaccard"))))
  every <- do.call(metric_table, c(args, list(metrics = "all")))

  # Added to the default set rather than replacing it, the way evalmod
  # takes the same argument
  expect_equal(setdiff(names(more), names(plain)), c("lift", "jaccard"))
  expect_true(all(names(plain) %in% names(every)))
  expect_true(ncol(every) > ncol(more))

  # The shared columns are untouched by the extra ones
  expect_equal(more[names(plain)], plain)
})

test_that("an object already calculated gives the same table", {
  data(P10N10)
  points <- evalmod(
    scores = P10N10$scores, labels = P10N10$labels,
    mode = "basic"
  )

  expect_equal(
    metric_table(points),
    metric_table(scores = P10N10$scores, labels = P10N10$labels)
  )
})

test_that("metric_table() keeps every model and test dataset apart", {
  samps <- create_sim_samples(3, 20, 20, c("good_er", "poor_er"))
  mdat <- mmdata(samps[["scores"]], samps[["labels"]],
    modnames = samps[["modnames"]], dsids = samps[["dsids"]]
  )
  tab <- metric_table(mdat)

  # Two models over three test sets, forty-one cutoffs each. Nothing is
  # averaged across datasets: a cutoff belongs to the one it came from.
  expect_equal(nrow(tab), 2 * 3 * 41)
  expect_equal(
    as.vector(table(tab$modname, tab$dsid)), rep(41L, 6)
  )
  expect_equal(sort(unique(as.character(tab$modname))), c("good_er", "poor_er"))
  expect_equal(sort(unique(as.character(tab$dsid))), c("1", "2", "3"))

  # The scores differ per test set, so the tables must too
  first <- tab$score[tab$modname == "good_er" & tab$dsid == "1"]
  second <- tab$score[tab$modname == "good_er" & tab$dsid == "2"]
  expect_false(isTRUE(all.equal(first, second)))
})

test_that("the rank scale follows the size of its own test dataset", {
  # Unequal test sets: the count and the normalized rank must agree with
  # the dataset the row belongs to, not with the first one seen
  scores <- list(c(3, 2, 1, 0), c(5, 4, 3, 2, 1, 0))
  labels <- list(c(1, 1, 0, 0), c(1, 1, 1, 0, 0, 0))
  mdat <- mmdata(scores, labels, modnames = c("m1", "m1"), dsids = c(1, 2))
  tab <- metric_table(mdat)

  expect_equal(tab$rank[tab$dsid == "1"], 0:4)
  expect_equal(tab$rank[tab$dsid == "2"], 0:6)
  expect_equal(tab$normalized_rank[tab$dsid == "1"], (0:4) / 4)
  expect_equal(tab$normalized_rank[tab$dsid == "2"], (0:6) / 6)
})

test_that("tied scores are reported as the ties they are", {
  scores <- c(5, 4, 4, 4, 2, 1)
  labels <- c(1, 1, 0, 1, 0, 0)
  tab <- metric_table(scores = scores, labels = labels)

  # The cutoffs are still per instance, and three of them share a score
  expect_equal(tab$score[-1], c(5, 4, 4, 4, 2, 1))

  # Splitting the tie makes the metrics march evenly across it rather than
  # stepping in the order the tied instances happened to arrive
  tied <- tab$sensitivity[tab$rank %in% 2:4]
  expect_equal(diff(tied), rep(diff(tied)[1], 2))
})

test_that("metric_table() refuses input it cannot read cutoffs from", {
  data(P10N10)
  args <- list(scores = P10N10$scores, labels = P10N10$labels)

  # Curves and the fast AUC do not keep the scores
  expect_error(metric_table(do.call(evalmod, args)),
    class = "precrec_error_invalid_x"
  )
  expect_error(metric_table(do.call(evalmod, c(args, mode = "aucroc"))),
    class = "precrec_error_invalid_x"
  )

  # An object carries the metrics it was built with
  points <- do.call(evalmod, c(args, mode = "basic"))
  expect_error(metric_table(points, metrics = "lift"),
    class = "precrec_error_invalid_metrics"
  )
})

test_that("metric_table() refuses an object that kept only the average", {
  samps <- create_sim_samples(3, 20, 20, "good_er")
  mdat <- mmdata(samps[["scores"]], samps[["labels"]],
    dsids = samps[["dsids"]]
  )

  expect_error(metric_table(evalmod(mdat, mode = "basic")),
    class = "precrec_error_invalid_x"
  )

  # Keeping the raw points is all it takes
  expect_s3_class(
    metric_table(evalmod(mdat, mode = "basic", raw_curves = TRUE)),
    "data.frame"
  )
})

test_that("metric_table() takes the arguments evalmod takes", {
  scores <- c(9, 8, 7, 6, 3, 2)
  labels <- c("p", "p", "n", "p", "n", "n")

  named <- metric_table(
    scores = scores, labels = labels, posclass = "p", modnames = "mod"
  )
  expect_equal(unique(as.character(named$modname)), "mod")
  expect_equal(named$label[-1], c(1, 1, -1, 1, -1, -1))

  # beta reaches the F-score
  half <- metric_table(
    scores = scores, labels = labels, posclass = "p",
    beta = 0.5
  )
  expect_false(isTRUE(all.equal(half$fscore, named$fscore)))
})

test_that("at = picks the row the threshold actually produces", {
  data(P10N10)
  scores <- P10N10$scores
  labels <- P10N10$labels
  full <- metric_table(scores = scores, labels = labels)

  at <- c(21, 17, 14, 6, 5, 0)
  tab <- metric_table(scores = scores, labels = labels, at = at)

  expect_equal(nrow(tab), length(at))
  expect_equal(tab$at, at)
  # The rank is the number of instances the rule `score >= at` selects
  expect_equal(tab$rank, vapply(at, function(t) sum(scores >= t), integer(1)))
  # And the row is the row of that rank, untouched
  expect_equal(
    tab[, setdiff(names(tab), "at")],
    `rownames<-`(full[match(tab$rank, full$rank), ], NULL)
  )
})

test_that("at = reports the threshold asked for and the cutoff realizing it", {
  data(P10N10)
  tab <- metric_table(
    scores = P10N10$scores, labels = P10N10$labels,
    at = c(16.5, 14)
  )

  # `at` is what was asked for, `score` the smallest score still positive
  expect_equal(tab$at, c(16.5, 14))
  expect_equal(tab$score, c(17, 14))
  expect_true(all(tab$score >= tab$at))
  # 14 is a run of six tied scores, and the rank is the end of the run
  expect_equal(tab$rank, c(4L, 12L))
})

test_that("at = puts its column after the identifiers", {
  data(P10N10)
  tab <- metric_table(scores = P10N10$scores, labels = P10N10$labels, at = 14)

  expect_equal(names(tab)[1:6], c(
    "modname", "dsid", "at", "rank", "normalized_rank", "score"
  ))
})

test_that("a threshold above every score is the rank 0 row", {
  data(P10N10)
  tab <- metric_table(scores = P10N10$scores, labels = P10N10$labels, at = 999)

  expect_equal(tab$rank, 0L)
  expect_true(is.na(tab$score))
  expect_equal(tab$sensitivity, 0)
  expect_equal(tab$specificity, 1)
})

test_that("an NA score is never called positive by at =", {
  scores <- c(9, 8, NA, 6, 3, 2)
  labels <- c(1, 1, 1, 0, 0, 0)

  tab <- metric_table(scores = scores, labels = labels, at = c(1, 8))

  # Five scorable instances at or above 1, not six
  expect_equal(tab$rank, c(5L, 2L))
  expect_equal(tab$sensitivity, c(2 / 3, 2 / 3))
})

test_that("at = resolves per test dataset, not globally", {
  scores <- list(c(9, 8, 7, 6), c(4, 3, 2, 1))
  labels <- list(c(1, 1, 0, 0), c(1, 1, 0, 0))
  mdat <- mmdata(scores, labels, modnames = c("m", "m"), dsids = c(1, 2))

  tab <- metric_table(mdat, at = 5)

  expect_equal(nrow(tab), 2)
  expect_equal(as.character(tab$dsid), c("1", "2"))
  # The same threshold is above all of the second dataset and none of the first
  expect_equal(tab$rank, c(4L, 0L))
  expect_equal(tab$at, c(5, 5))
})

test_that("at = works on an object that is already calculated", {
  data(P10N10)
  points <- evalmod(
    scores = P10N10$scores, labels = P10N10$labels, mode = "basic"
  )

  expect_equal(
    metric_table(points, at = 14),
    metric_table(scores = P10N10$scores, labels = P10N10$labels, at = 14)
  )
})

test_that("at = rejects what is not a threshold", {
  data(P10N10)
  args <- list(scores = P10N10$scores, labels = P10N10$labels)

  expect_error(
    do.call(metric_table, c(args, list(at = "14"))),
    class = "precrec_error_invalid_at"
  )
  expect_error(
    do.call(metric_table, c(args, list(at = numeric(0)))),
    class = "precrec_error_invalid_at"
  )
  expect_error(
    do.call(metric_table, c(args, list(at = c(14, NA)))),
    class = "precrec_error_invalid_at"
  )
})

test_that("best_cutoff() refuses at = rather than narrowing its search", {
  data(P10N10)

  expect_error(
    best_cutoff(scores = P10N10$scores, labels = P10N10$labels, at = 14),
    class = "precrec_error_invalid_at"
  )
})

test_that("the end rows report no precision or NPV", {
  # Precision is TP / (TP + FP) and NPV is TN / (TN + FN), so the row that
  # calls nothing positive has no denominator for the first and the row
  # that calls everything positive has none for the second.
  data(P10N10)
  tab <- metric_table(
    scores = P10N10$scores, labels = P10N10$labels,
    metrics = c(
      "precision", "npv", "false_discovery_rate",
      "false_omission_rate", "markedness"
    )
  )
  n <- max(tab$rank)

  for (metric in c("precision", "false_discovery_rate", "markedness")) {
    expect_true(is.na(tab[[metric]][tab$rank == 0L]))
  }
  for (metric in c("npv", "false_omission_rate", "markedness")) {
    expect_true(is.na(tab[[metric]][tab$rank == n]))
  }

  # Everywhere else they are measured
  middle <- tab$rank > 0L & tab$rank < n
  expect_false(anyNA(tab$precision[middle]))
  expect_false(anyNA(tab$npv[middle]))
})

test_that("the blanked cells do not depend on the instance at the end", {
  # They were filled in from the neighboring row, so precision at rank 0
  # followed the top-ranked instance - 1 if it was a positive and 0 if it
  # was a negative, for a rule that predicts nothing either way.
  base <- c(rnorm(10, 1), rnorm(90))
  labels <- rep(c(1, 0), c(10, 90))

  picked <- vapply(c(1, 0), function(top) {
    scores <- base
    i <- if (top == 1) which(labels == 1)[1] else which(labels == 0)[1]
    scores[i] <- max(base) + 1
    tab <- metric_table(scores = scores, labels = labels)
    tab$precision[tab$rank == 0L]
  }, numeric(1))

  expect_true(all(is.na(picked)))
})

test_that("a threshold outside the score range reports no precision", {
  # The transfer case `at` exists for: a cutoff chosen elsewhere can sit
  # above every score here, and then it predicts nothing
  data(P10N10)
  tab <- metric_table(
    scores = P10N10$scores, labels = P10N10$labels,
    at = c(max(P10N10$scores) + 1, min(P10N10$scores) - 1),
    metrics = c("precision", "npv")
  )

  expect_equal(tab$rank, c(0L, length(P10N10$scores)))
  expect_true(is.na(tab$precision[1]))
  expect_equal(tab$sensitivity[1], 0)
  expect_true(is.na(tab$npv[2]))
  expect_equal(tab$sensitivity[2], 1)
})

test_that("blanking the table leaves the curves alone", {
  # `create_prc_curve()` reads the same array and is anchored on the
  # inherited precision, which is what makes the interpolation correct
  set.seed(7)
  scores <- c(rnorm(20, 1), rnorm(980))
  labels <- rep(c(1, 0), c(20, 980))
  curves <- evalmod(scores = scores, labels = labels, raw_curves = TRUE)

  areas <- auc(curves)
  prc <- as.data.frame(curves)
  prc <- prc[prc$type == "PRC", ]

  expect_equal(areas$aucs[areas$curvetypes == "ROC"], 0.82)
  expect_equal(areas$aucs[areas$curvetypes == "PRC"], 0.2938277359,
    tolerance = 1e-8
  )
  expect_equal(average_precision(curves)$aps, 0.2979068387, tolerance = 1e-8)
  expect_equal(prbe(curves)$prbe, 0.25)
  expect_equal(prc$y[1], 1) # the anchor, still the limit from above
})
