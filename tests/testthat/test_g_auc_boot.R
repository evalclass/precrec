# G: auc_boot(), auc_ci.aucboot() and auc_diff()

## DeLong's analytic standard error for the ROC AUC. An independent
## reference: it comes from the U-statistic's structural components and
## touches none of the resampling the bootstrap does.
delong_se <- function(scores, labels, pos_val) {
  x <- scores[labels == pos_val]
  y <- scores[labels != pos_val]
  psi <- outer(x, y, function(a, b) (a > b) + 0.5 * (a == b))
  sqrt(var(rowMeans(psi)) / length(x) + var(colMeans(psi)) / length(y))
}

two_normals <- function(n, seed = 1) {
  set.seed(seed)
  list(
    scores = c(rnorm(n / 2, 1), rnorm(n / 2, 0)),
    labels = rep(c(1, 0), each = n / 2)
  )
}

test_that("auc_boot() returns one row per model, curve type and resample", {
  d <- two_normals(100)
  booted <- auc_boot(
    scores = d$scores, labels = d$labels, boot_n = 50,
    seed = 1
  )

  expect_s3_class(booted, "aucboot")
  expect_s3_class(booted, "data.frame")
  expect_equal(
    names(booted), c("modnames", "curvetypes", "boot_id", "aucs")
  )
  expect_equal(nrow(booted), 50 * 2) # ROC and PRC
  expect_equal(sort(unique(booted$boot_id)), 1:50)
  expect_equal(attr(booted, "boot_n"), 50L)
})

test_that("the resampling is stratified", {
  # Thirty positives against a hundred and seventy negatives: an
  # unstratified bootstrap would vary the balance from resample to
  # resample, and could draw one with no positives at all.
  set.seed(4)
  scores <- c(rnorm(30, 1), rnorm(170, 0))
  labels <- rep(c(1, 0), c(30, 170))
  booted <- auc_boot(scores = scores, labels = labels, boot_n = 20, seed = 1)

  expect_equal(attr(booted, "np"), 30L)
  expect_equal(attr(booted, "nn"), 170L)
  expect_false(anyNA(booted$aucs))
})

test_that("the bootstrap standard error agrees with DeLong's", {
  # The strongest check available: an analytic variance for the ROC AUC
  # derived a completely different way.
  for (n in c(100, 400)) {
    d <- two_normals(n, seed = n)
    ci <- auc_ci(auc_boot(
      scores = d$scores, labels = d$labels,
      boot_n = 1500, seed = 1
    ))
    boot_se <- ci$error[ci$curvetypes == "ROC"]

    expect_equal(boot_se, delong_se(d$scores, d$labels, 1), tolerance = 0.08)
  }
})

test_that("a seed makes the result reproducible", {
  d <- two_normals(100)
  args <- list(scores = d$scores, labels = d$labels, boot_n = 40)

  first <- do.call(auc_boot, c(args, seed = 99))
  second <- do.call(auc_boot, c(args, seed = 99))
  other <- do.call(auc_boot, c(args, seed = 100))

  expect_equal(first$aucs, second$aucs)
  expect_false(isTRUE(all.equal(first$aucs, other$aucs)))
})

test_that("a seeded call leaves the caller's random stream alone", {
  d <- two_normals(100)

  set.seed(5)
  expected <- runif(3)

  set.seed(5)
  invisible(auc_boot(
    scores = d$scores, labels = d$labels, boot_n = 20,
    seed = 1
  ))
  expect_equal(runif(3), expected)
})

test_that("auc_ci() on a bootstrap gives a percentile interval", {
  d <- two_normals(200)
  booted <- auc_boot(
    scores = d$scores, labels = d$labels, boot_n = 500,
    seed = 1
  )
  ci <- auc_ci(booted)

  expect_equal(
    names(ci),
    c(
      "modnames", "curvetypes", "aucs", "mean", "error", "lower_bound",
      "upper_bound", "n"
    )
  )
  expect_true(all(ci$lower_bound < ci$upper_bound))
  expect_true(all(ci$lower_bound >= 0 & ci$upper_bound <= 1))
  expect_equal(ci$n, rep(500, 2))

  # The bounds are quantiles of the resampled values, so they are values
  # the statistic actually took
  roc <- booted$aucs[booted$curvetypes == "ROC"]
  expect_equal(
    ci$lower_bound[ci$curvetypes == "ROC"],
    unname(quantile(roc, 0.025, names = FALSE))
  )

  # A wider interval covers more
  wide <- auc_ci(booted, alpha = 0.01)
  expect_true(all(wide$lower_bound <= ci$lower_bound))
  expect_true(all(wide$upper_bound >= ci$upper_bound))
})

test_that("auc_ci() reports the observed AUC alongside the resampled mean", {
  d <- two_normals(200)
  booted <- auc_boot(
    scores = d$scores, labels = d$labels, boot_n = 200,
    seed = 1
  )
  observed <- auc(evalmod(scores = d$scores, labels = d$labels))
  ci <- auc_ci(booted)

  for (curvetype in c("ROC", "PRC")) {
    expect_equal(
      ci$aucs[ci$curvetypes == curvetype],
      observed$aucs[observed$curvetypes == curvetype]
    )
  }
})

test_that("dtype is refused, since a percentile interval assumes none", {
  d <- two_normals(100)
  booted <- auc_boot(
    scores = d$scores, labels = d$labels, boot_n = 20,
    seed = 1
  )

  expect_error(auc_ci(booted, dtype = "t"),
    class = "precrec_error_invalid_dtype"
  )
})

test_that("auc_diff() pairs the models on the same resamples", {
  # Two models given identical scores. Paired, every difference is exactly
  # zero; resampled independently, they would not be.
  d <- two_normals(120)
  mdat <- mmdata(list(d$scores, d$scores), list(d$labels, d$labels),
    modnames = c("A", "B")
  )
  diffs <- auc_diff(auc_boot(mdat, boot_n = 100, seed = 5))

  expect_true(all(diffs$diffs == 0))
  expect_true(all(diffs$lower_bound == 0))
  expect_true(all(diffs$upper_bound == 0))
  expect_true(all(diffs$p_values == 1))
})

test_that("auc_diff() finds a difference that is really there", {
  set.seed(8)
  labels <- rep(c(1, 0), each = 150)
  strong <- c(rnorm(150, 2), rnorm(150, 0))
  weak <- c(rnorm(150, 0.15), rnorm(150, 0))

  mdat <- mmdata(list(strong, weak), list(labels, labels),
    modnames = c("strong", "weak")
  )
  diffs <- auc_diff(auc_boot(mdat, boot_n = 500, seed = 1))
  roc <- diffs[diffs$curvetypes == "ROC", ]

  expect_true(roc$diffs > 0)
  expect_true(roc$lower_bound > 0) # interval clear of zero
  expect_true(roc$p_values < 0.05)
})

test_that("auc_diff() reports the columns it documents", {
  d <- two_normals(100)
  mdat <- mmdata(list(d$scores, rev(d$scores)), list(d$labels, d$labels),
    modnames = c("A", "B")
  )
  diffs <- auc_diff(auc_boot(mdat, boot_n = 50, seed = 1))

  expect_equal(
    names(diffs),
    c(
      "curvetypes", "modnames1", "modnames2", "diffs", "lower_bound",
      "upper_bound", "p_values", "n"
    )
  )
  expect_equal(nrow(diffs), 2) # one pair, two curve types

  # The p-value cannot be reported below what the resample count supports
  expect_true(all(diffs$p_values >= 2 / (50 + 1)))
})

test_that("auc_boot() refuses input it cannot resample", {
  d <- two_normals(100)

  # Several test sets: the variation between them is the better estimate
  samps <- create_sim_samples(4, 50, 50, "good_er")
  expect_error(
    auc_boot(mmdata(samps$scores, samps$labels, dsids = samps$dsids)),
    class = "precrec_error_invalid_mdat"
  )

  # Too few of one class to resample
  expect_error(
    auc_boot(scores = c(3, 2, 1), labels = c(1, 0, 0)),
    class = "precrec_error_invalid_mdat"
  )

  # Models scored on different observations cannot be paired
  expect_error(
    auc_boot(mmdata(
      list(d$scores, d$scores), list(d$labels, rev(d$labels)),
      modnames = c("A", "B")
    )),
    class = "precrec_error_invalid_mdat"
  )

  expect_error(
    auc_boot(scores = d$scores, labels = d$labels, boot_n = 1),
    class = "precrec_error_invalid_boot_n"
  )
})

test_that("auc_diff() needs two models and an aucboot", {
  d <- two_normals(100)
  booted <- auc_boot(
    scores = d$scores, labels = d$labels, boot_n = 20,
    seed = 1
  )

  expect_error(auc_diff(booted), class = "precrec_error_invalid_x")
  expect_error(auc_diff(data.frame(a = 1)), class = "precrec_error_invalid_x")
})
