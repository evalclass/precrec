# ETC: Shared helpers
# Test .map(x, f), .map_dbl(x, f), .map_int(x, f), .map_chr(x, f),
#      .map_lgl(x, f), .map_idx(x, f), .keep(x, p), .flatten(x)

test_that(".map() returns a list of the same length", {
  expect_equal(.map(1:3, function(i) i * 2), list(2, 4, 6))
  expect_equal(.map(list(), identity), list())
  expect_type(.map(1:2, identity), "list")
})

test_that(".map() passes extra arguments through", {
  expect_equal(.map(list(c(1, NA)), sum, na.rm = TRUE), list(1))
})

test_that("the typed helpers return an atomic vector of that type", {
  expect_type(.map_dbl(1:3, function(i) i + 0.5), "double")
  expect_type(.map_int(list(1:2, 1:5), length), "integer")
  expect_type(.map_chr(1:2, function(i) "a"), "character")
  expect_type(.map_lgl(1:3, function(i) i > 1), "logical")
  expect_equal(.map_int(list(1:2, 1:5), length), c(2L, 5L))
})

test_that("the typed helpers reject a result of the wrong type or length", {
  expect_error(.map_dbl(1:2, function(i) "a"))
  expect_error(.map_int(1:2, function(i) c(1L, 2L)))
})

test_that(".map_idx() maps over positions, not elements", {
  x <- list("a", "b", "c")
  expect_equal(.map_idx(x, function(i) i), list(1L, 2L, 3L))
  expect_equal(.map_idx(list(), function(i) i), list())
})

test_that(".map_idx() drops the names its input carries", {
  # The callers store the result as an S3 object whose class items are
  # positional, so a stray name from the input would leak into it.
  expect_null(names(.map_idx(list(a = 1, b = 2), function(i) i)))
})

test_that(".keep() selects the elements the predicate accepts", {
  expect_equal(.keep(1:5, function(i) i %% 2 == 0), c(2L, 4L))
  expect_equal(.keep(list(), function(i) TRUE), list())
  expect_equal(.keep(1:3, function(i) FALSE), integer(0))
})

test_that(".flatten() removes exactly one level of nesting", {
  expect_equal(
    .flatten(list(list(1, 2), list(3))),
    list(1, 2, 3)
  )
  expect_equal(
    .flatten(list(list(list(1)))),
    list(list(1))
  )
  expect_null(.flatten(list()))
})

# Test .basic_metric_table(), .basic_metric_names(metrics),
#      .resolve_metrics(metrics), .get_metric_names(mode),
#      .get_obj_metrics(obj), .pmatch_metric_names(metrics)

test_that("the metric table is internally consistent", {
  tab <- .basic_metric_table()

  expect_false(any(duplicated(tab$name)))
  expect_false(any(duplicated(tab$short)))
  expect_true(all(tab$range %in% c("unit", "signed", "free")))
  expect_equal(sum(tab$default), 14L)
})

test_that("every alias points at a name the table holds", {
  tab <- .basic_metric_table()
  expect_true(all(.basic_metric_aliases() %in% tab$name))
  expect_false(any(names(.basic_metric_aliases()) %in% tab$name))
})

test_that(".basic_metric_names() still returns the fourteen defaults", {
  # The panel count of an existing caller's plot depends on this
  expect_equal(
    names(.basic_metric_names()),
    c(
      "score", "label", "error", "accuracy", "specificity", "sensitivity",
      "precision", "mcc", "fscore", "balanced_accuracy", "npv",
      "informedness", "markedness", "kappa"
    )
  )
  expect_equal(.get_metric_names("basic"), names(.basic_metric_names()))
})

test_that(".get_metric_names('basic_all') adds the ROCR measures", {
  all_names <- .get_metric_names("basic_all")

  # The defaults come first and in their own order, so an added measure
  # cannot displace one
  expect_equal(all_names[seq_along(.get_metric_names("basic"))],
    .get_metric_names("basic")
  )
  expect_equal(
    setdiff(all_names, .get_metric_names("basic")),
    c(
      "fpr", "fnr", "false_discovery_rate", "false_omission_rate",
      "predicted_positive_rate", "predicted_negative_rate", "lift", "odds",
      "mi", "chisq", "cost"
    )
  )
})

test_that(".resolve_metrics() keeps the default set whatever it is given", {
  defaults <- .get_metric_names("basic")

  expect_equal(.resolve_metrics(NULL), defaults)
  expect_equal(.resolve_metrics("all"), .get_metric_names("basic_all"))
  expect_equal(.resolve_metrics("lift"), c(defaults, "lift"))
  expect_equal(.resolve_metrics("accuracy"), defaults)
})

test_that(".resolve_metrics() returns the measures in table order", {
  expect_equal(
    .resolve_metrics(c("odds", "fpr")),
    c(.get_metric_names("basic"), "fpr", "odds")
  )
})

test_that(".resolve_metrics() resolves the ROCR identifiers", {
  expect_equal(.resolve_metrics("fall"), .resolve_metrics("fpr"))
  expect_equal(.resolve_metrics("rpp"), .resolve_metrics(
    "predicted_positive_rate"
  ))
  expect_equal(.resolve_metrics("pcmiss"), .resolve_metrics(
    "false_omission_rate"
  ))
})

test_that(".resolve_metrics() rejects a name it does not know", {
  expect_error(.resolve_metrics("nonesuch"),
    class = "precrec_error_invalid_metrics"
  )
  expect_error(.resolve_metrics(1), class = "precrec_error_invalid_metrics")
  expect_error(.resolve_metrics(list("fpr")),
    class = "precrec_error_invalid_metrics"
  )
})

test_that(".get_obj_metrics() falls back to the default set", {
  # An object built before `metrics` existed carries no attribute
  expect_equal(.get_obj_metrics(structure(list())), .get_metric_names("basic"))
})
