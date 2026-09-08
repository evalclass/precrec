# ETC utils: autoplot structure
#
# These tests state directly what a plot should be - how many panels it has,
# which metric each one draws, how it is titled and labeled. The snapshots
# in _snaps/ catch anything that changes without being asked to; these say
# what is supposed to be true, so a failure reads as a broken expectation
# rather than as a diff to go and interpret. Prefer adding to these for
# anything you can state outright.

skip_on_cran()

ap0_check_libs <- function() {
  requireNamespace("ggplot2", quietly = TRUE)
}

ap0_basic_points <- function() {
  data(P10N10)
  evalmod(
    mode = "basic", scores = P10N10$scores,
    labels = P10N10$labels
  )
}

ap0_curves <- function() {
  data(P10N10)
  evalmod(scores = P10N10$scores, labels = P10N10$labels)
}

test_that("autoplot draws one panel per basic metric by default", {
  if (!ap0_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  metrics <- names(.basic_metric_names())
  p <- suppressWarnings(ggplot2::autoplot(ap0_basic_points()))

  expect_equal(length(gg_panels(p)), length(metrics))
  # The y label of each panel is the metric it draws, so this pins both the
  # membership of the default grid and its order.
  expect_equal(gg_labs(p, "y"), metrics)
})

test_that("every default panel is titled after the metric it draws", {
  if (!ap0_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  p <- suppressWarnings(ggplot2::autoplot(ap0_basic_points()))
  titles <- gg_labs(p, "title")

  expect_false(any(is.na(titles)))
  expect_equal(
    titles,
    vapply(gg_labs(p, "y"), .get_metric_title, character(1),
      USE.NAMES = FALSE
    )
  )
})

test_that("autoplot draws only the metrics it was asked for", {
  if (!ap0_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  points <- ap0_basic_points()

  p2 <- suppressWarnings(
    ggplot2::autoplot(points, c("precision", "kappa"))
  )
  expect_equal(gg_labs(p2, "y"), c("precision", "kappa"))
  expect_equal(gg_labs(p2, "title"), c("Precision", "Kappa"))

  p1 <- suppressWarnings(ggplot2::autoplot(points, "npv"))
  expect_equal(length(gg_panels(p1)), 1)
  expect_equal(gg_labs(p1, "y"), "npv")
  expect_equal(gg_labs(p1, "title"), "NPV")
})

test_that("basic-metric panels are drawn against normalized rank", {
  if (!ap0_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  p <- suppressWarnings(ggplot2::autoplot(ap0_basic_points()))
  expect_true(all(gg_labs(p, "x") == "normalized rank"))
})

test_that("autoplot draws ROC and PRC with the conventional axes", {
  if (!ap0_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  p <- suppressWarnings(ggplot2::autoplot(ap0_curves()))

  expect_equal(length(gg_panels(p)), 2)
  # The curve titles carry the class balance, which is the whole point of the
  # package - a PRC is only readable next to the prevalence that produced it.
  expect_equal(
    gg_labs(p, "title"),
    c("ROC - P: 10, N: 10", "Precision-Recall - P: 10, N: 10")
  )
  expect_equal(gg_labs(p, "x"), c("1 - Specificity", "Recall"))
  expect_equal(gg_labs(p, "y"), c("Sensitivity", "Precision"))
})

test_that("the curve titles report the counts of the data plotted", {
  if (!ap0_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  data(P10N10)
  curves <- evalmod(
    scores = P10N10$scores[1:15],
    labels = P10N10$labels[1:15]
  )
  np <- sum(P10N10$labels[1:15] == 1)
  nn <- 15 - np

  expect_equal(
    gg_labs(suppressWarnings(ggplot2::autoplot(curves)), "title"),
    paste0(
      c("ROC", "Precision-Recall"), " - P: ", np, ", N: ", nn
    )
  )
})

test_that("autoplot draws a single curve type on its own", {
  if (!ap0_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  curves <- ap0_curves()

  p_roc <- suppressWarnings(ggplot2::autoplot(curves, "ROC"))
  expect_equal(length(gg_panels(p_roc)), 1)
  expect_equal(gg_labs(p_roc, "title"), "ROC - P: 10, N: 10")
  expect_equal(gg_labs(p_roc, "y"), "Sensitivity")

  p_prc <- suppressWarnings(ggplot2::autoplot(curves, "PRC"))
  expect_equal(length(gg_panels(p_prc)), 1)
  expect_equal(gg_labs(p_prc, "title"), "Precision-Recall - P: 10, N: 10")
  expect_equal(gg_labs(p_prc, "y"), "Precision")
})

test_that("plotting does not warn about the arguments it cannot act on", {
  if (!ap0_check_libs()) {
    skip("Libraries cannot be loaded")
  }

  # The fortify methods take raw_curves and reduce_points for a common
  # interface, but a single dataset has no average to contrast a raw curve
  # with and the basic metrics have no point reduction. ggplot2's fortify
  # generic runs check_dots_used(), which reported every one of those as a
  # possible misspelling until the methods consumed them explicitly.
  ap0_build <- function(n_mod, n_ds, mode) {
    n <- n_mod * n_ds
    scores <- lapply(seq_len(n), function(i) seq_len(20))
    labels <- lapply(seq_len(n), function(i) rep(c(1, 0), 10))
    mdat <- mmdata(join_scores(scores), join_labels(labels),
      modnames = rep(paste0("m", seq_len(n_mod)), each = n_ds),
      dsids = rep(seq_len(n_ds), n_mod)
    )
    evalmod(mdat, mode = mode)
  }

  for (mode in c("rocprc", "basic")) {
    for (n_mod in c(1, 2)) {
      for (n_ds in c(1, 2)) {
        obj <- ap0_build(n_mod, n_ds, mode)
        expect_no_warning(ggplot2::fortify(obj))
        expect_no_warning(ggplot2::autoplot(obj))
      }
    }
  }
})
