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
  expect_equal(names(res), c("modnames", "dsids", "prbe"))
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
