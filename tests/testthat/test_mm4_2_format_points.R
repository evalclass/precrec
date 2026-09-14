# MM 4: Reconstruct per-instance data from a table of curve points
# Test format_points(points_df, threshold_col, ...)

#
# A complete table - one row per distinct score - carries everything the
# per-instance data did, so the round trip must be exact.
#
.complete_points_table <- function(scores, labels, np, nn) {
  th <- sort(unique(scores), decreasing = TRUE)
  tp <- vapply(th, function(t) sum(scores >= t & labels == 1), double(1))
  fp <- vapply(th, function(t) sum(scores >= t & labels == 0), double(1))

  data.frame(
    threshold = th,
    tpr = tp / np, fpr = fp / nn,
    recall = tp / np, precision = tp / (tp + fp)
  )
}

.test_scores_labels <- function() {
  set.seed(42)
  np <- 60
  nn <- 140
  list(
    np = np, nn = nn,
    labels = c(rep(1, np), rep(0, nn)),
    scores = round(c(stats::rnorm(np, 2, 1), stats::rnorm(nn, 0, 1)), 1)
  )
}

test_that("format_points() returns scores, labels, modnames and dsids", {
  df <- data.frame(
    threshold = c(0.9, 0.7, 0.5),
    tpr = c(0.2, 0.6, 1), fpr = c(0.1, 0.4, 1)
  )

  expect_message(
    format_points(df,
      threshold_col = "threshold", tpr_col = "tpr", fpr_col = "fpr",
      np = 10, nn = 20
    ),
    "Reconstructed"
  )

  pts <- suppressMessages(format_points(df,
    threshold_col = "threshold", tpr_col = "tpr", fpr_col = "fpr",
    np = 10, nn = 20
  ))

  expect_true(is.list(pts))
  expect_equal(names(pts), c("scores", "labels", "modnames", "dsids"))
  expect_equal(length(pts$scores), 1)
  expect_equal(length(pts$scores[[1]]), 30)
  expect_equal(sum(pts$labels[[1]] == 1), 10)
  expect_equal(sum(pts$labels[[1]] == 0), 20)
  expect_equal(pts$modnames, "m1")
  expect_equal(pts$dsids, 1)
})

test_that("format_points() round-trips a complete table exactly", {
  d <- .test_scores_labels()
  orig <- auc(evalmod(scores = d$scores, labels = d$labels))
  tbl <- .complete_points_table(d$scores, d$labels, d$np, d$nn)

  pts <- suppressMessages(format_points(tbl,
    threshold_col = "threshold", tpr_col = "tpr", fpr_col = "fpr",
    np = d$np, nn = d$nn
  ))

  expect_equal(sort(pts$scores[[1]]), sort(d$scores))
  expect_equal(sort(pts$labels[[1]]), sort(d$labels))

  back <- auc(evalmod(mmdata(pts$scores, pts$labels)))
  expect_equal(back$aucs, orig$aucs)
})

test_that("format_points() round-trips recall and precision exactly", {
  d <- .test_scores_labels()
  orig <- auc(evalmod(scores = d$scores, labels = d$labels))
  tbl <- .complete_points_table(d$scores, d$labels, d$np, d$nn)

  pts <- suppressMessages(format_points(tbl,
    threshold_col = "threshold", rec_col = "recall", prec_col = "precision",
    np = d$np, nn = d$nn
  ))

  back <- auc(evalmod(mmdata(pts$scores, pts$labels)))
  expect_equal(back$aucs, orig$aucs)
})

test_that("format_points() accepts a threshold that runs the other way", {
  d <- .test_scores_labels()
  orig <- auc(evalmod(scores = d$scores, labels = d$labels))
  tbl <- .complete_points_table(d$scores, d$labels, d$np, d$nn)
  tbl$threshold <- -tbl$threshold

  pts <- suppressMessages(format_points(tbl,
    threshold_col = "threshold", tpr_col = "tpr", fpr_col = "fpr",
    np = d$np, nn = d$nn
  ))

  back <- auc(evalmod(mmdata(pts$scores, pts$labels)))
  expect_equal(back$aucs, orig$aucs)
})

test_that("format_points() anchors a table that stops short", {
  # Only half the positives and a fifth of the negatives are ever admitted
  df <- data.frame(
    threshold = c(0.9, 0.6), tpr = c(0.25, 0.5), fpr = c(0.05, 0.2)
  )

  pts <- suppressMessages(format_points(df,
    threshold_col = "threshold", tpr_col = "tpr", fpr_col = "fpr",
    np = 40, nn = 100
  ))

  expect_equal(length(pts$scores[[1]]), 140)
  expect_equal(sum(pts$labels[[1]] == 1), 40)
  expect_equal(min(pts$scores[[1]]), 0.6 - 1)
})

test_that("format_points() splits by model and dataset", {
  df <- data.frame(
    mod = rep(c("b", "a"), each = 4),
    ds = rep(c(2, 2, 5, 5), 2),
    threshold = rep(c(0.8, 0.2), 4),
    tpr = rep(c(0.5, 1), 4), fpr = rep(c(0.2, 1), 4)
  )

  pts <- suppressMessages(format_points(df,
    threshold_col = "threshold", tpr_col = "tpr", fpr_col = "fpr",
    mod_col = "mod", dsid_col = "ds", np = 10, nn = 10
  ))

  expect_equal(length(pts$scores), 4)
  expect_equal(pts$modnames, c("a", "a", "b", "b"))
  expect_equal(pts$dsids, c(1, 2, 1, 2))
})

test_that("format_points() takes the totals from a column", {
  df <- data.frame(
    ds = rep(c(1, 2), each = 2),
    threshold = rep(c(0.8, 0.2), 2),
    tpr = rep(c(0.5, 1), 2), fpr = rep(c(0.5, 1), 2),
    np = rep(c(10, 20), each = 2), nn = rep(c(30, 40), each = 2)
  )

  pts <- suppressMessages(format_points(df,
    threshold_col = "threshold", tpr_col = "tpr", fpr_col = "fpr",
    dsid_col = "ds", np = "np", nn = "nn"
  ))

  expect_equal(lengths(pts$scores), c(40, 60))
})

test_that("format_points() takes one total per group", {
  df <- data.frame(
    ds = rep(c(1, 2), each = 2),
    threshold = rep(c(0.8, 0.2), 2),
    tpr = rep(c(0.5, 1), 2), fpr = rep(c(0.5, 1), 2)
  )

  pts <- suppressMessages(format_points(df,
    threshold_col = "threshold", tpr_col = "tpr", fpr_col = "fpr",
    dsid_col = "ds", np = c(10, 20), nn = c(30, 40)
  ))

  expect_equal(lengths(pts$scores), c(40, 60))
})

test_that("format_points() rejects invalid input", {
  df <- data.frame(
    threshold = c(0.9, 0.5), tpr = c(0.5, 1), fpr = c(0.5, 1)
  )
  call_with <- function(points_df = df, threshold_col = "threshold",
                        tpr_col = "tpr", fpr_col = "fpr",
                        rec_col = NULL, prec_col = NULL,
                        np = 10, nn = 10) {
    format_points(
      points_df, threshold_col, tpr_col, fpr_col, rec_col, prec_col, np, nn
    )
  }

  expect_error(call_with(points_df = 1:3), class = "precrec_error_invalid_points_df")
  expect_error(call_with(points_df = df[0, ]), class = "precrec_error_invalid_points_df")
  expect_error(call_with(np = NULL), class = "precrec_error_invalid_np")
  expect_error(call_with(nn = NULL), class = "precrec_error_invalid_nn")
  expect_error(call_with(np = list(1)), class = "precrec_error_invalid_np")
  expect_error(call_with(np = c(1, 2)), class = "precrec_error_invalid_np")
  expect_error(call_with(threshold_col = "nope"), class = "precrec_error_invalid_threshold_col")
  expect_error(call_with(tpr_col = NULL), class = "precrec_error_invalid_points_df")
  expect_error(
    call_with(rec_col = "tpr", prec_col = "fpr"),
    class = "precrec_error_invalid_points_df"
  )
})

test_that("format_points() rejects a table it cannot reconstruct", {
  base_df <- data.frame(
    threshold = c(0.9, 0.5), tpr = c(0.5, 1), fpr = c(0.5, 1)
  )
  call_with <- function(d, ...) {
    format_points(d,
      threshold_col = "threshold", tpr_col = "tpr", fpr_col = "fpr",
      np = 10, nn = 10, ...
    )
  }

  # A rate outside 0 to 1
  bad_rate <- base_df
  bad_rate$tpr <- c(0.5, 1.4)
  expect_error(call_with(bad_rate), class = "precrec_error_invalid_tpr_col")

  # One rate grows with the threshold while the other falls
  not_monotone <- base_df
  not_monotone$fpr <- c(1, 0.5)
  expect_error(call_with(not_monotone), class = "precrec_error_invalid_points_df")

  # The same threshold twice
  repeated <- base_df
  repeated$threshold <- c(0.9, 0.9)
  expect_error(call_with(repeated), class = "precrec_error_invalid_threshold_col")

  # Precision so low that the negatives it implies outnumber nn
  too_many <- data.frame(
    threshold = c(0.9, 0.5), recall = c(0.5, 1), precision = c(0.5, 0.1)
  )
  expect_error(
    format_points(too_many,
      threshold_col = "threshold", rec_col = "recall", prec_col = "precision",
      np = 10, nn = 20
    ),
    class = "precrec_error_invalid_points_df"
  )

  # A total that changes from row to row inside one group
  varying <- data.frame(
    ds = c(1, 1), threshold = c(0.9, 0.5),
    tpr = c(0.5, 1), fpr = c(0.5, 1), np = c(10, 20)
  )
  expect_error(
    format_points(varying,
      threshold_col = "threshold", tpr_col = "tpr", fpr_col = "fpr",
      dsid_col = "ds", np = "np", nn = 10
    ),
    class = "precrec_error_invalid_np"
  )
})

test_that("format_points() reports rounding when the rates are coarse", {
  df <- data.frame(
    threshold = c(0.9, 0.5), tpr = c(0.333, 1), fpr = c(0.111, 1)
  )

  expect_message(
    format_points(df,
      threshold_col = "threshold", tpr_col = "tpr", fpr_col = "fpr",
      np = 7, nn = 9
    ),
    "rounded"
  )
})

test_that("format_points() feeds every mode of evalmod()", {
  d <- .test_scores_labels()
  tbl <- .complete_points_table(d$scores, d$labels, d$np, d$nn)
  pts <- suppressMessages(format_points(tbl,
    threshold_col = "threshold", tpr_col = "tpr", fpr_col = "fpr",
    np = d$np, nn = d$nn
  ))

  expect_s3_class(evalmod(mmdata(pts$scores, pts$labels)), "sscurves")
  expect_s3_class(
    evalmod(mmdata(pts$scores, pts$labels), mode = "basic"), "sspoints"
  )
  expect_s3_class(
    evalmod(mmdata(pts$scores, pts$labels, mode = "aucroc"), mode = "aucroc"),
    "aucroc"
  )
})
