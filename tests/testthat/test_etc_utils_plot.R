# ETC utils: plot helpers
# Test .pmatch_curvetype_basic(vals)

test_that(".pmatch_curvetype_basic() resolves the measure names", {
  expect_equal(.pmatch_curvetype_basic("error"), "error")
  expect_equal(.pmatch_curvetype_basic("accuracy"), "accuracy")
  expect_equal(.pmatch_curvetype_basic("balanced_accuracy"),
    "balanced_accuracy"
  )
  expect_equal(.pmatch_curvetype_basic("npv"), "npv")
  expect_equal(.pmatch_curvetype_basic("informedness"), "informedness")
  expect_equal(.pmatch_curvetype_basic("markedness"), "markedness")
  expect_equal(.pmatch_curvetype_basic("kappa"), "kappa")
})

test_that(".pmatch_curvetype_basic() accepts the common aliases", {
  expect_equal(.pmatch_curvetype_basic("recall"), "sensitivity")
  expect_equal(.pmatch_curvetype_basic("ppv"), "precision")
  expect_equal(.pmatch_curvetype_basic("bacc"), "balanced_accuracy")
  expect_equal(.pmatch_curvetype_basic("balanced accuracy"),
    "balanced_accuracy"
  )
  expect_equal(.pmatch_curvetype_basic("negative predictive value"), "npv")
  expect_equal(.pmatch_curvetype_basic("youden"), "informedness")
  expect_equal(.pmatch_curvetype_basic("cohen's kappa"), "kappa")
})

test_that(".pmatch_curvetype_basic() keeps 'm' pointing at mcc", {
  # "matthews" is the longer of the two names that start with m, so
  # markedness only takes over once the abbreviation stops being ambiguous
  expect_equal(.pmatch_curvetype_basic("m"), "mcc")
  expect_equal(.pmatch_curvetype_basic("mcc"), "mcc")
  expect_equal(.pmatch_curvetype_basic("mark"), "markedness")
})

test_that(".pmatch_curvetype_basic() takes several values at once", {
  expect_equal(
    .pmatch_curvetype_basic(c("prec", "youden", "kappa")),
    c("precision", "informedness", "kappa")
  )
})

# Test .get_metric_title(curvetype)

test_that(".get_metric_title() capitalizes or spells out the name", {
  expect_equal(.get_metric_title("accuracy"), "Accuracy")
  expect_equal(.get_metric_title("kappa"), "Kappa")
  expect_equal(.get_metric_title("mcc"), "MCC")
  expect_equal(.get_metric_title("npv"), "NPV")
  expect_equal(.get_metric_title("balanced_accuracy"), "Balanced accuracy")
  expect_equal(.get_metric_title("label"), "Label (1:pos, -1:neg)")
})

test_that(".get_metric_title() reads a factor by its label, not its code", {
  # The plot code pulls the curve type out of a data frame, where it is a
  # factor. Indexing the lookup vector by a factor would pick the integer
  # level code instead of the name, which silently retitled the panels.
  metrics <- names(.basic_metric_names())
  as_factor <- function(x) factor(x, levels = metrics)

  expect_equal(
    .get_metric_title(as_factor("label")), "Label (1:pos, -1:neg)"
  )
  expect_equal(.get_metric_title(as_factor("mcc")), "MCC")
  expect_equal(.get_metric_title(as_factor("npv")), "NPV")
  expect_equal(
    .get_metric_title(as_factor("balanced_accuracy")), "Balanced accuracy"
  )
  expect_equal(
    vapply(as_factor(metrics), .get_metric_title, character(1),
      USE.NAMES = FALSE
    ),
    vapply(metrics, .get_metric_title, character(1), USE.NAMES = FALSE)
  )
})

# Test .is_signed_metric(curvetype)

test_that(".is_signed_metric() picks out the measures that can go negative", {
  expect_true(all(.is_signed_metric(
    c("mcc", "informedness", "markedness", "kappa", "label")
  )))
  expect_false(any(.is_signed_metric(
    c("accuracy", "precision", "npv", "balanced_accuracy", "fscore")
  )))
})

# Test .get_plot_ncol(nplots)

test_that(".get_plot_ncol() keeps the panel grid roughly square", {
  expect_equal(.get_plot_ncol(1), 1)
  expect_equal(.get_plot_ncol(2), 2)
  expect_equal(.get_plot_ncol(3), 3)
  expect_equal(.get_plot_ncol(4), 2)
  expect_equal(.get_plot_ncol(6), 3)
  expect_equal(.get_plot_ncol(9), 3)
  expect_equal(.get_plot_ncol(14), 4)
})
