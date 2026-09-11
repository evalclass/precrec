# G: prbe
# Test prbe(curves), .find_break_even(rec, prec)

prbe_curves <- function(...) {
  set.seed(1)
  samps <- create_sim_samples(2, 50, 50, c("poor_er", "good_er"))
  mdat <- mmdata(samps[["scores"]], samps[["labels"]],
    modnames = samps[["modnames"]], dsids = samps[["dsids"]]
  )
  evalmod(mdat, raw_curves = TRUE, ...)
}

test_that("prbe() returns one row per curve", {
  res <- prbe(prbe_curves())

  expect_true(is.data.frame(res))
  expect_false(is(res, "data.table"))
  expect_equal(names(res), c("modnames", "dsids", "prbe", "baselines"))
  expect_equal(nrow(res), 4)
  expect_equal(as.character(res[["modnames"]]), rep(
    c("poor_er", "good_er"), 2
  ))
})

test_that("precision crosses recall at the reported point", {
  curves <- prbe_curves()
  res <- prbe(curves)

  # Read off the supporting points rather than through approx(): recall
  # repeats along a precision-recall curve, and approx() averages the
  # precision over the repeats instead of keeping the curve
  for (i in seq_along(curves[["prcs"]])) {
    rec <- curves[["prcs"]][[i]][["x"]]
    prec <- curves[["prcs"]][[i]][["y"]]
    at <- res[["prbe"]][i]

    before <- prec[rec < at] - rec[rec < at]
    after <- prec[rec > at] - rec[rec > at]

    # Precision is above recall on one side of the point and below it on the
    # other, which is what a crossing is
    expect_true(any(before > 0))
    expect_true(any(after < 0))
  }
})

test_that("prbe() rejects an object of the wrong class", {
  expect_error(prbe(evalmod(
    scores = c(0.1, 0.2, 0.3), labels = c(1, 0, 1), mode = "basic"
  )))
  expect_error(prbe("not a curve object"))
})

test_that("prbe() says so when the curves were not kept", {
  set.seed(1)
  samps <- create_sim_samples(2, 20, 20, "good_er")
  mdat <- mmdata(samps[["scores"]], samps[["labels"]],
    modnames = samps[["modnames"]], dsids = samps[["dsids"]]
  )
  curves <- evalmod(mdat, calc_avg = TRUE, raw_curves = FALSE)

  expect_error(prbe(curves), "raw_curves")
})

# --- .find_break_even() -----------------------------------------------------

test_that(".find_break_even() finds an exact crossing", {
  # precision equals recall at 0.5
  expect_equal(
    .find_break_even(c(0, 0.5, 1), c(1, 0.5, 0)), 0.5
  )
})

test_that(".find_break_even() interpolates between supporting points", {
  # precision - recall goes 0.8 -> -0.2, so the crossing is 4/5 of the way
  expect_equal(
    .find_break_even(c(0, 1), c(0.8, 0.8)), 0.8
  )
})

test_that(".find_break_even() reports every crossing, in order", {
  out <- .find_break_even(
    c(0, 0.25, 0.5, 0.75, 1),
    c(0.5, 0.1, 0.9, 0.4, 0.4)
  )

  expect_true(length(out) > 1)
  expect_false(is.unsorted(out))
  expect_equal(out, unique(out))
})

test_that(".find_break_even() returns NA when there is no crossing", {
  # Precision stays above recall throughout, so the two are never equal
  expect_true(is.na(.find_break_even(c(0, 0.4, 0.8), c(1, 1, 1))))
  expect_true(is.na(.find_break_even(0.5, 0.5)))
  expect_true(is.na(.find_break_even(numeric(0), numeric(0))))
})

test_that(".find_break_even() counts a touch at the end as a crossing", {
  # A perfect classifier reaches precision 1 at recall 1, which is a genuine
  # break-even point rather than an artifact
  expect_equal(.find_break_even(c(0, 0.5, 1), c(1, 1, 1)), 1)
})

test_that(".find_break_even() ignores points that are not finite", {
  expect_equal(
    .find_break_even(c(0, 0.5, 1, NA), c(1, 0.5, 0, NaN)), 0.5
  )
})

test_that("prbe() does not report the origin as a break-even point", {
  # A curve is anchored at recall 0, where precision is 0 when the
  # top-ranked instance is a negative. Precision and recall are equal there,
  # but nothing has been retrieved and the classifier is not balancing them.
  set.seed(4)
  scores <- c(rnorm(20, 1), rnorm(980))
  labels <- rep(c(1, 0), c(20, 980))

  expect_equal(labels[order(scores, decreasing = TRUE)][1], 0)

  curves <- evalmod(scores = scores, labels = labels, raw_curves = TRUE)
  res <- prbe(curves)

  expect_false(any(res$prbe == 0, na.rm = TRUE))
  expect_equal(res$prbe, 0.2)
})

test_that("prbe() still finds a crossing when the top instance is positive", {
  set.seed(4)
  scores <- c(rnorm(20, 2.5), rnorm(980))
  labels <- rep(c(1, 0), c(20, 980))

  expect_equal(labels[order(scores, decreasing = TRUE)][1], 1)

  res <- prbe(evalmod(scores = scores, labels = labels, raw_curves = TRUE))

  expect_equal(res$prbe, 0.75)
})

test_that("prbe() is NA when precision never catches up with recall", {
  # The curve leaves the origin below the diagonal and stays there, so
  # there is no recall above zero at which the two are equal. This is what
  # the origin crossing used to be reported as 0 instead of.
  set.seed(1)
  scores <- rnorm(1000)
  labels <- rep(c(1, 0), c(20, 980))

  res <- prbe(evalmod(scores = scores, labels = labels, raw_curves = TRUE))

  expect_equal(nrow(res), 1)
  expect_true(is.na(res$prbe))
})

test_that("prbe() reports the prevalence as the baseline", {
  # At chance the curve is flat at the proportion of positives, so it meets
  # the diagonal at that recall
  scores <- list(c(rnorm(10, 1), rnorm(90)), c(rnorm(30, 1), rnorm(70)))
  labels <- list(rep(c(1, 0), c(10, 90)), rep(c(1, 0), c(30, 70)))
  curves <- evalmod(
    mmdata(scores, labels, modnames = c("m1", "m1"), dsids = c(1, 2)),
    raw_curves = TRUE
  )

  res <- prbe(curves)

  expect_equal(res$baselines[res$dsids == 1], 0.1)
  expect_equal(res$baselines[res$dsids == 2], 0.3)
})

test_that("a random ranker breaks even at the prevalence", {
  found <- vapply(seq_len(40), function(i) {
    set.seed(i)
    res <- prbe(evalmod(
      scores = rnorm(10000), labels = rep(c(1, 0), c(200, 9800)),
      raw_curves = TRUE
    ))
    res$prbe[1]
  }, numeric(1))

  expect_equal(median(found, na.rm = TRUE), 0.02, tolerance = 0.02)
})
