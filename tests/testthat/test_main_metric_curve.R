# MA 2: metric_curve
# Test metric_curve(mdat, scores, labels, x_metric, y_metric, ...),
#      .joinable_pairs(), .joinable_curve(x_metric, y_metric),
#      .validate_metric_arg(metric, arg)

mc_samps <- function(n = 1, modes = "good_er") {
  set.seed(1)
  create_sim_samples(n, 50, 50, modes)
}

mc_mdat <- function() {
  samps <- mc_samps(3, c("poor_er", "good_er"))
  mmdata(samps[["scores"]], samps[["labels"]],
    modnames = samps[["modnames"]], dsids = samps[["dsids"]]
  )
}

mc_single <- function(...) {
  samps <- mc_samps()
  metric_curve(scores = samps[["scores"]], labels = samps[["labels"]], ...)
}

# --- The registry -----------------------------------------------------------

test_that(".joinable_pairs() lists the pairs that have an interpolation", {
  pairs <- .joinable_pairs()

  expect_equal(names(pairs), c("x", "y", "curve"))
  expect_equal(pairs$curve, c("ROC", "PRC"))
  expect_true(all(pairs$x %in% .get_metric_names("basic_all")))
  expect_true(all(pairs$y %in% .get_metric_names("basic_all")))
})

test_that(".joinable_curve() is ordered - x and y are not interchangeable", {
  expect_equal(.joinable_curve("fpr", "sensitivity"), "ROC")
  expect_equal(.joinable_curve("sensitivity", "precision"), "PRC")

  # A precision-recall curve drawn with the axes swapped has no defined
  # interpolation, so it is not the registered pair
  expect_true(is.na(.joinable_curve("sensitivity", "fpr")))
  expect_true(is.na(.joinable_curve("precision", "sensitivity")))
  expect_true(is.na(.joinable_curve("score", "accuracy")))
})

# --- The gate: a registered pair is the curve pipeline's own answer ---------

test_that("the registered ROC pair is identical to evalmod(mode = 'rocprc')", {
  mdat <- mc_mdat()
  xy <- metric_curve(mdat, x_metric = "fpr", y_metric = "sensitivity")

  curves <- as.data.frame(evalmod(mdat, raw_curves = TRUE))
  roc <- curves[curves$type == "ROC", c("x", "y", "modname", "dsid")]
  got <- as.data.frame(xy)[, c("x", "y", "modname", "dsid")]

  expect_equal(got, roc, ignore_attr = TRUE)
})

test_that("the registered PRC pair is identical to evalmod(mode = 'rocprc')", {
  mdat <- mc_mdat()
  xy <- metric_curve(mdat, x_metric = "sensitivity", y_metric = "precision")

  curves <- as.data.frame(evalmod(mdat, raw_curves = TRUE))
  prc <- curves[curves$type == "PRC", c("x", "y", "modname", "dsid")]
  got <- as.data.frame(xy)[, c("x", "y", "modname", "dsid")]

  expect_equal(got, prc, ignore_attr = TRUE)
})

test_that("a registered pair honors x_bins and interpolate", {
  mdat <- mc_mdat()
  xy <- metric_curve(mdat, x_bins = 4)
  curves <- as.data.frame(evalmod(mdat, raw_curves = TRUE, x_bins = 4))
  roc <- curves[curves$type == "ROC", c("x", "y")]

  expect_equal(as.data.frame(xy)[, c("x", "y")], roc, ignore_attr = TRUE)
})

# --- The unregistered path --------------------------------------------------

test_that("an unregistered pair reads the basic metrics", {
  samps <- mc_samps()
  xy <- mc_single(
    x_metric = "predicted_positive_rate", y_metric = "lift"
  )
  pts <- evalmod(
    scores = samps[["scores"]], labels = samps[["labels"]],
    mode = "basic", metrics = c("predicted_positive_rate", "lift")
  )
  df <- as.data.frame(pts)

  expect_equal(
    as.data.frame(xy)[["x"]],
    df[df$type == "predicted_positive_rate", "y"]
  )
  expect_equal(as.data.frame(xy)[["y"]], df[df$type == "lift", "y"])
})

test_that("the score is available as an axis, the way ROCR uses cutoff", {
  xy <- mc_single(x_metric = "score", y_metric = "precision")

  expect_true(is.na(attr(xy, "curve")))
  expect_equal(attr(xy, "x_metric"), "score")
})

test_that("the same metric on both axes is accepted", {
  xy <- mc_single(x_metric = "accuracy", y_metric = "accuracy")

  df <- as.data.frame(xy)
  expect_equal(df[["x"]], df[["y"]])
})

# --- Class variants ---------------------------------------------------------

test_that("metric_curve() picks the class the way evalmod() does", {
  samps <- mc_samps(3, c("poor_er", "good_er"))

  expect_s3_class(mc_single(), "ssxycurves")
  expect_s3_class(
    metric_curve(mmdata(
      join_scores(samps[["scores"]][[1]], samps[["scores"]][[2]]),
      samps[["labels"]]
    )),
    "msxycurves"
  )
  expect_s3_class(metric_curve(mc_mdat()), "mmxycurves")
})

test_that("metric_curve() keeps one curve per test dataset", {
  xy <- metric_curve(mc_mdat())

  expect_length(xy[["xy"]], 6)
  expect_equal(
    levels(as.data.frame(xy)[["modname"]]), c("poor_er", "good_er")
  )
})

# --- Arguments --------------------------------------------------------------

test_that("both axis arguments accept the ROCR identifiers", {
  expect_equal(attr(mc_single(x_metric = "fall"), "x_metric"), "fpr")
  expect_equal(
    attr(mc_single(y_metric = "rpp"), "y_metric"),
    "predicted_positive_rate"
  )
  expect_equal(attr(mc_single(x_metric = "tpr"), "x_metric"), "sensitivity")
})

test_that("an axis argument must name a metric that exists", {
  expect_error(mc_single(x_metric = "nonesuch"),
    class = "precrec_error_invalid_x_metric"
  )
  expect_error(mc_single(y_metric = "nonesuch"),
    class = "precrec_error_invalid_y_metric"
  )
  expect_error(mc_single(x_metric = 1),
    class = "precrec_error_invalid_x_metric"
  )
  expect_error(mc_single(x_metric = c("fpr", "lift")),
    class = "precrec_error_invalid_x_metric"
  )
})

test_that("a near miss on an axis argument suggests the metric meant", {
  err <- expect_error(mc_single(y_metric = "sensitivty"),
    class = "precrec_error_invalid_y_metric"
  )
  expect_match(conditionMessage(err), "sensitivity")
})

test_that("metric_curve() validates the data arguments", {
  samps <- mc_samps()

  expect_error(
    metric_curve(
      scores = samps[["scores"]], labels = samps[["labels"]],
      modnames = 1
    ),
    class = "precrec_error_invalid_modnames"
  )
  expect_error(
    metric_curve(
      scores = samps[["scores"]], labels = samps[["labels"]],
      x_bins = -1
    ),
    class = "precrec_error_invalid_x_bins"
  )
})

# --- The methods ------------------------------------------------------------

test_that("as.data.frame() returns the columns the curve objects return", {
  df <- as.data.frame(mc_single())

  expect_true(is.data.frame(df))
  expect_false(is(df, "data.table"))
  expect_equal(names(df), c("x", "y", "modname", "dsid", "type"))
  expect_equal(levels(df[["type"]]), "sensitivity vs fpr")
})

test_that("fortify() agrees with as.data.frame()", {
  xy <- mc_single()
  expect_equal(fortify(xy), as.data.frame(xy))
})

test_that("autoplot() draws a line for a registered pair and points else", {
  skip_if_not_installed("ggplot2")

  p1 <- autoplot(mc_single())
  p2 <- autoplot(mc_single(x_metric = "score", y_metric = "precision"))

  expect_s3_class(p1, "ggplot")
  expect_true(is(p1[["layers"]][[1]][["geom"]], "GeomLine"))
  expect_true(is(p2[["layers"]][[1]][["geom"]], "GeomPoint"))
})

test_that("autoplot(type = ) overrides the registry's choice", {
  skip_if_not_installed("ggplot2")

  p <- autoplot(
    mc_single(x_metric = "score", y_metric = "precision"),
    type = "l"
  )
  expect_true(is(p[["layers"]][[1]][["geom"]], "GeomLine"))
})

test_that("plot() draws without error for every class variant", {
  withr::local_pdf(NULL)

  expect_silent(plot(mc_single()))
  expect_silent(plot(metric_curve(mc_mdat())))
  expect_silent(plot(mc_single(x_metric = "rpp", y_metric = "lift")))
})

test_that("print() names the pair and says whether it is joined", {
  expect_output(print(mc_single()), "Sensitivity vs FPR")
  expect_output(print(mc_single()), "ROC")
  expect_output(
    print(mc_single(x_metric = "rpp", y_metric = "lift")),
    "not joined by a line"
  )
})

test_that("the object validates and reports its own contents", {
  xy <- mc_single()

  expect_equal(attr(xy, "x_metric"), "fpr")
  expect_equal(attr(xy, "y_metric"), "sensitivity")
  expect_equal(attr(xy, "curve"), "ROC")
  expect_true(attr(xy, "validated"))
})

test_that("metric_curve() passes the cost weights through", {
  xy1 <- mc_single(x_metric = "score", y_metric = "cost")
  xy2 <- mc_single(
    x_metric = "score", y_metric = "cost",
    cost_fp = 3, cost_fn = 0.5
  )

  expect_false(isTRUE(all.equal(
    as.data.frame(xy1)[["y"]], as.data.frame(xy2)[["y"]]
  )))
})

test_that("metric_curve() rejects a negative cost", {
  expect_error(mc_single(cost_fp = -1),
    class = "precrec_error_invalid_cost_fp"
  )
})
