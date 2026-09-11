# G: auc_delong(), auc_ci.aucdelong() and auc_diff.aucdelong()

## The O(m * n) definition of the structural components, written out.
## `.delong_components()` reads the same numbers off midranks, and this is
## what says the shortcut is the same calculation.
delong_slow <- function(x, y) {
  psi <- outer(x, y, function(a, b) (a > b) + 0.5 * (a == b))
  list(auc = mean(psi), v10 = rowMeans(psi), v01 = colMeans(psi))
}

two_normals <- function(n, seed = 1) {
  set.seed(seed)
  list(
    scores = c(rnorm(n / 2, 1), rnorm(n / 2, 0)),
    labels = rep(c(1, 0), each = n / 2)
  )
}

test_that("the midranks give the components the definition gives", {
  # Ties are where a shortcut of this kind goes wrong, so they are most of
  # the cases here
  cases <- list(
    distinct = list(x = c(3.1, 2.2, 5.0), y = c(1.0, 4.4, 0.5, 2.9)),
    some_ties = list(x = c(2, 2, 3, 1), y = c(2, 1, 3, 3, 0)),
    all_tied = list(x = rep(1, 5), y = rep(1, 4)),
    one_each = list(x = 1, y = 0)
  )

  for (case in names(cases)) {
    x <- cases[[case]][["x"]]
    y <- cases[[case]][["y"]]
    values <- c(x, y)
    pos <- rep(c(TRUE, FALSE), c(length(x), length(y)))

    fast <- precrec:::.delong_components(values, pos, !pos)
    slow <- delong_slow(x, y)

    expect_equal(fast[["auc"]], slow[["auc"]], info = case)
    expect_equal(fast[["v10"]], slow[["v10"]], info = case)
    expect_equal(fast[["v01"]], slow[["v01"]], info = case)
  }
})

test_that("auc_delong() reports the AUC that auc() reports", {
  # Read off the ranks the pipeline already assigned, so the tie and NA
  # policy is the package's own rather than a second opinion
  for (case in c("clean", "tied", "with_na")) {
    set.seed(3)
    scores <- rnorm(300)
    if (case == "tied") scores <- round(scores, 1)
    if (case == "with_na") scores[c(5, 50, 122)] <- NA
    labels <- rep(c(1, 0), each = 150)

    delong <- auc_delong(scores = scores, labels = labels)
    observed <- auc(evalmod(scores = scores, labels = labels))

    expect_equal(
      delong$aucs, observed$aucs[observed$curvetypes == "ROC"],
      info = case
    )
  }
})

test_that("auc_delong() returns one row per model and the covariance", {
  samps <- create_sim_samples(1, 100, 100, c("poor_er", "good_er"))
  mdat <- mmdata(samps[["scores"]], samps[["labels"]],
    modnames = samps[["modnames"]]
  )
  delong <- auc_delong(mdat)

  expect_s3_class(delong, "aucdelong")
  expect_s3_class(delong, "data.frame")
  expect_equal(names(delong), c("modnames", "curvetypes", "aucs", "error"))
  expect_equal(nrow(delong), 2)

  # The ROC AUC only: the precision-recall AUC is not a U statistic
  expect_equal(unique(delong$curvetypes), "ROC")

  covm <- attr(delong, "cov")
  expect_equal(dim(covm), c(2L, 2L))
  expect_equal(covm, t(covm))
  expect_equal(delong$error, sqrt(diag(covm)), ignore_attr = TRUE)
  expect_equal(attr(delong, "np"), 100L)
  expect_equal(attr(delong, "nn"), 100L)
})

test_that("the standard error is the one the AUC actually has", {
  # Simulate the sampling distribution and compare its spread with what
  # DeLong claims. Nothing here shares code with the implementation.
  for (cfg in list(c(50, 50), c(30, 170))) {
    np <- cfg[1]
    nn <- cfg[2]
    set.seed(1)
    reps <- replicate(400, {
      scores <- c(rnorm(np, 1), rnorm(nn, 0))
      labels <- rep(c(1, 0), c(np, nn))
      delong <- auc_delong(scores = scores, labels = labels)
      c(delong$aucs, delong$error)
    })

    expect_equal(mean(reps[2, ]), sd(reps[1, ]), tolerance = 0.06)
  }
})

test_that("DeLong and the bootstrap agree on the same data", {
  # Two routes to the same standard error, one exact and one resampled
  d <- two_normals(300, seed = 5)
  delong <- auc_delong(scores = d$scores, labels = d$labels)
  booted <- auc_ci(auc_boot(
    scores = d$scores, labels = d$labels, boot_n = 1500, seed = 1
  ))

  expect_equal(
    delong$error, booted$error[booted$curvetypes == "ROC"],
    tolerance = 0.1
  )
})

test_that("auc_ci() on a DeLong object gives a normal interval", {
  d <- two_normals(200)
  delong <- auc_delong(scores = d$scores, labels = d$labels)
  ci <- auc_ci(delong)

  expect_equal(
    names(ci),
    c(
      "modnames", "curvetypes", "aucs", "baselines", "error",
      "lower_bound", "upper_bound", "n"
    )
  )
  expect_equal(ci$n, 200)
  expect_equal(ci$aucs, delong$aucs)

  # The interval is the AUC give or take z standard errors
  expect_equal(ci$lower_bound, delong$aucs - qnorm(0.975) * delong$error)
  expect_equal(ci$upper_bound, delong$aucs + qnorm(0.975) * delong$error)

  # A wider interval covers more
  wide <- auc_ci(delong, alpha = 0.01)
  expect_true(wide$lower_bound < ci$lower_bound)
  expect_true(wide$upper_bound > ci$upper_bound)
})

test_that("the interval is clipped where an AUC cannot go", {
  # A near-perfect classifier: the normal interval would run past 1
  set.seed(2)
  scores <- c(rnorm(40, 8), rnorm(40, 0))
  labels <- rep(c(1, 0), each = 40)
  ci <- auc_ci(auc_delong(scores = scores, labels = labels))

  expect_equal(ci$upper_bound, 1)
  expect_true(ci$lower_bound >= 0)
})

test_that("dtype is refused, since the variance is an asymptotic one", {
  d <- two_normals(100)
  delong <- auc_delong(scores = d$scores, labels = d$labels)

  expect_error(auc_ci(delong, dtype = "t"),
    class = "precrec_error_invalid_dtype"
  )
  expect_error(auc_ci(delong, dtype = "normal"),
    class = "precrec_error_invalid_dtype"
  )
})

test_that("auc_diff() on a DeLong object compares every pair", {
  samps <- create_sim_samples(1, 150, 150, c("poor_er", "good_er", "excel"))
  mdat <- mmdata(samps[["scores"]], samps[["labels"]],
    modnames = samps[["modnames"]]
  )
  diffs <- auc_diff(auc_delong(mdat))

  expect_equal(
    names(diffs),
    c(
      "curvetypes", "modnames1", "modnames2", "diffs", "lower_bound",
      "upper_bound", "z_values", "p_values", "n"
    )
  )
  expect_equal(nrow(diffs), 3) # three pairs, ROC only
  expect_equal(unique(diffs$curvetypes), "ROC")
  expect_equal(unique(diffs$n), 300)

  # The difference is the difference of the two AUCs
  delong <- auc_delong(mdat)
  expect_equal(
    diffs$diffs[1], delong$aucs[1] - delong$aucs[2]
  )
})

test_that("the comparison is paired through the covariance", {
  # Two models that move together: ignoring the covariance would report a
  # difference more uncertain than it is
  set.seed(12)
  labels <- rep(c(1, 0), each = 200)
  base <- c(rnorm(200, 1), rnorm(200, 0))
  first <- base + rnorm(400, 0, 0.1)
  second <- base + rnorm(400, 0, 0.1)

  mdat <- mmdata(list(first, second), list(labels, labels),
    modnames = c("a", "b")
  )
  delong <- auc_delong(mdat)
  covm <- attr(delong, "cov")

  expect_true(covm[1, 2] > 0)

  paired <- precrec:::.delong_diff_se(covm, 1, 2)
  unpaired <- sqrt(covm[1, 1] + covm[2, 2])
  expect_true(paired < unpaired)

  diffs <- auc_diff(delong)
  expect_equal(diffs$z_values, diffs$diffs / paired)
})

test_that("auc_diff() reads the tail alternative asks for", {
  samps <- create_sim_samples(1, 150, 150, c("poor_er", "good_er"))
  mdat <- mmdata(samps[["scores"]], samps[["labels"]],
    modnames = samps[["modnames"]]
  )
  delong <- auc_delong(mdat)

  both <- auc_diff(delong)
  up <- auc_diff(delong, alternative = "greater")
  down <- auc_diff(delong, alternative = "less")

  expect_equal(up$z_values, both$z_values)
  expect_equal(down$z_values, both$z_values)
  expect_equal(up$p_values + down$p_values, 1)
  expect_equal(both$p_values, 2 * pmin(up$p_values, down$p_values))

  # The interval is unaffected by which tail is asked for
  expect_equal(up$lower_bound, both$lower_bound)
  expect_equal(down$upper_bound, both$upper_bound)
})

test_that("two models ranked alike leave nothing to divide by", {
  d <- two_normals(120)
  mdat <- mmdata(list(d$scores, d$scores), list(d$labels, d$labels),
    modnames = c("A", "B")
  )
  diffs <- auc_diff(auc_delong(mdat))

  expect_equal(diffs$diffs, 0)
  expect_equal(diffs$lower_bound, 0)
  expect_equal(diffs$upper_bound, 0)
  expect_true(is.na(diffs$z_values))
  expect_true(is.na(diffs$p_values))
})

test_that("auc_delong() refuses input it cannot describe", {
  d <- two_normals(100)

  # Several test sets: the variation between them is the better estimate
  samps <- create_sim_samples(4, 50, 50, "good_er")
  expect_error(
    auc_delong(mmdata(samps$scores, samps$labels, dsids = samps$dsids)),
    class = "precrec_error_invalid_mdat"
  )

  # Too few of one class for a variance
  expect_error(
    auc_delong(scores = c(3, 2, 1), labels = c(1, 0, 0)),
    class = "precrec_error_invalid_mdat"
  )

  # Models scored on different observations cannot be paired
  expect_error(
    auc_delong(mmdata(
      list(d$scores, d$scores), list(d$labels, rev(d$labels)),
      modnames = c("A", "B")
    )),
    class = "precrec_error_invalid_mdat"
  )

  # The fast AUC path keeps no ranks to read the components off
  expect_error(
    auc_delong(mmdata(d$scores, d$labels, mode = "aucroc")),
    class = "precrec_error_invalid_mdat"
  )
})

test_that("auc_diff() needs two models and an object it knows", {
  d <- two_normals(100)
  delong <- auc_delong(scores = d$scores, labels = d$labels)

  expect_error(auc_diff(delong), class = "precrec_error_invalid_x")
  expect_error(auc_diff(data.frame(a = 1)), class = "precrec_error_invalid_x")
  expect_error(auc_diff(delong, alternative = "twosided"),
    class = "precrec_error_invalid_alternative"
  )
})

test_that("auc_ci() on a DeLong object reports the ROC baseline", {
  d <- two_normals(200)
  ci <- auc_ci(auc_delong(scores = d$scores, labels = d$labels))

  # DeLong covers the ROC AUC only, whose chance level is 0.5 whatever the
  # class balance is
  expect_equal(ci$baselines, 0.5)
})
