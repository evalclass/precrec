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
  # The two must not drift: one C++ converter feeds both
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

    expect_equal(ordered[[metric]], part$y)
    expect_equal(ordered$normalized_rank, part$x)
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
