# ETC: Validate arguments
# Test .stop_invalid_arg(msg, arg),
#      .assert_internal(...),
#      .assert_flag(x, arg), .assert_string(x, arg, values, allow_na),
#      .assert_number(x, arg, min, max, whole, allow_na),
#      .assert_choice(x, arg, values), .assert_vector(x, arg, type, len),
#      .nearest_value(x, values)

test_that(".stop_invalid_arg() attaches precrec condition classes", {
  err <- expect_error(.stop_invalid_arg("bad {.arg mode}", "mode"))

  expect_s3_class(err, "precrec_error_invalid_mode")
  expect_s3_class(err, "precrec_error_invalid_arg")
  expect_s3_class(err, "precrec_error")
  expect_equal(err$arg, "mode")
})

test_that(".assert_internal() passes when every condition is TRUE", {
  expect_true(.assert_internal(TRUE, 1 == 1, is.numeric(1)))
  expect_true(.assert_internal())
})

test_that(".assert_internal() reports the first failing expression", {
  x <- list(tp = c(1, 2))

  err <- expect_error(
    .assert_internal(is.numeric(x[["tp"]]), length(x[["tp"]]) == 5L),
    class = "precrec_error_internal"
  )

  expect_match(conditionMessage(err), 'length(x[["tp"]]) == 5L', fixed = TRUE)
  expect_s3_class(err, "precrec_error")
})

test_that(".assert_flag() accepts only TRUE or FALSE", {
  expect_true(.assert_flag(TRUE, "calc_avg"))
  expect_true(.assert_flag(FALSE, "calc_avg"))

  expect_err_cls <- function(x) {
    expect_error(
      .assert_flag(x, "calc_avg"),
      class = "precrec_error_invalid_calc_avg"
    )
  }

  expect_err_cls(NA)
  expect_err_cls(0)
  expect_err_cls("TRUE")
  expect_err_cls(c(TRUE, FALSE))
})

test_that(".assert_string() checks the type and the allowed values", {
  expect_true(.assert_string("basic", "mode"))
  expect_true(.assert_string("basic", "mode", c("rocprc", "basic")))

  expect_error(
    .assert_string(1, "mode"), "single string",
    class = "precrec_error_invalid_mode"
  )
  expect_error(
    .assert_string(c("a", "b"), "mode"), "single string",
    class = "precrec_error_invalid_mode"
  )
  expect_error(
    .assert_string("prc", "mode", c("rocprc", "basic")), "must be one of",
    class = "precrec_error_invalid_mode"
  )
})

test_that(".assert_string() rejects NA unless allow_na is TRUE", {
  expect_error(
    .assert_string(NA_character_, "modname"),
    class = "precrec_error_invalid_modname"
  )
  expect_true(.assert_string(NA_character_, "modname", allow_na = TRUE))
})

test_that(".assert_number() checks the type, wholeness and the range", {
  expect_true(.assert_number(0.5, "alpha", min = 0, max = 1))

  expect_error(
    .assert_number("1", "alpha"), "single number",
    class = "precrec_error_invalid_alpha"
  )
  expect_error(
    .assert_number(1.5, "x_bins", whole = TRUE), "whole number",
    class = "precrec_error_invalid_x_bins"
  )
  expect_error(
    .assert_number(-0.1, "alpha", min = 0, max = 1), "between 0 and 1",
    class = "precrec_error_invalid_alpha"
  )
  expect_error(
    .assert_number(0, "x_bins", min = 1), "1 or larger",
    class = "precrec_error_invalid_x_bins"
  )
})

test_that(".validate_x_bins() caps x_bins", {
  # The ceiling is a resource guard: every stage sized by x_bins allocates a
  # vector of that length per curve, so an unbounded value dies in the
  # allocator instead of at the argument.
  expect_true(.validate_x_bins(1e6))
  expect_true(.validate_x_bins(1000))
  expect_true(.validate_x_bins(0, allow_zero = TRUE))

  for (x in c(1e6 + 1, 1e9)) {
    err <- expect_error(
      .validate_x_bins(x),
      class = "precrec_error_invalid_x_bins"
    )
    expect_match(conditionMessage(err), "between 1 and 1e\\+06")
  }
})

test_that(".assert_number() rejects NA unless allow_na is TRUE", {
  expect_error(
    .assert_number(NA_real_, "dsid"),
    class = "precrec_error_invalid_dsid"
  )
  expect_true(.assert_number(NA_real_, "dsid", allow_na = TRUE))
})

test_that(".assert_number() rejects an infinite whole number cleanly", {
  # `Inf %% 1` is NaN, which used to reach `if (NaN != 0)` and fail with R's
  # own "missing value where TRUE/FALSE needed" instead of a precrec error.
  for (x in c(Inf, -Inf)) {
    err <- expect_error(
      .assert_number(x, "x_bins", min = 1, whole = TRUE),
      class = "precrec_error_invalid_x_bins"
    )
    expect_match(conditionMessage(err), "whole number")
  }
})

test_that(".assert_string() and .assert_number() keep NA typed", {
  # checkmate's `na.ok` accepts an NA of any type; these arguments have always
  # wanted one of their own type, so a logical NA is still an error.
  expect_error(
    .assert_string(NA, "modname", allow_na = TRUE),
    class = "precrec_error_invalid_modname"
  )
  expect_error(
    .assert_number(NA_character_, "dsid", allow_na = TRUE),
    class = "precrec_error_invalid_dsid"
  )

  expect_true(.assert_string(NA_character_, "modname", allow_na = TRUE))
  expect_true(.assert_number(NA_integer_, "dsid", allow_na = TRUE))
})

test_that(".assert_string() suggests the nearest allowed value", {
  err <- expect_error(
    .assert_string("sensitivty", "curvetype", .get_metric_names("basic")),
    class = "precrec_error_invalid_curvetype"
  )
  expect_match(conditionMessage(err), "Did you mean", fixed = TRUE)
  expect_match(conditionMessage(err), "sensitivity", fixed = TRUE)
})

test_that(".assert_string() offers nothing when nothing is close", {
  err <- expect_error(
    .assert_string("xx", "mode", c("rocprc", "basic", "aucroc")),
    class = "precrec_error_invalid_mode"
  )
  expect_false(grepl("Did you mean", conditionMessage(err), fixed = TRUE))
})

test_that(".nearest_value() scales its threshold with the typed length", {
  values <- c("rocprc", "basic", "aucroc")

  expect_equal(.nearest_value("rocpr", values), "rocprc")
  expect_equal(.nearest_value("ROCPRC", values), "rocprc")
  expect_null(.nearest_value("xx", values))
  expect_null(.nearest_value("something_else_entirely", values))
  expect_null(.nearest_value("a", character(0)))
  expect_null(.nearest_value(1, values))
  expect_null(.nearest_value(NA_character_, values))
})

test_that(".assert_choice() accepts values that are not strings", {
  expect_true(.assert_choice(2, "dsid", c(1, 2, 3)))

  err <- expect_error(
    .assert_choice(5, "dsid", c(1, 2, 3)),
    class = "precrec_error_invalid_dsid"
  )
  expect_match(conditionMessage(err), "must be one of")
  expect_false(grepl("Did you mean", conditionMessage(err), fixed = TRUE))
})

test_that(".assert_vector() checks the type and the length", {
  expect_true(.assert_vector(c("a", "b"), "modnames", "character"))
  expect_true(.assert_vector(c(1, 2), "dsids", "numeric", len = 2))

  expect_error(
    .assert_vector(c(1, 2), "modnames", "character"),
    class = "precrec_error_invalid_modnames"
  )
  expect_error(
    .assert_vector(c("a", "b"), "modnames", "character", len = 3),
    class = "precrec_error_invalid_modnames"
  )

  # `is.vector()` rejects anything carrying an attribute other than names,
  # which is what keeps a matrix out of a vector argument.
  expect_error(
    .assert_vector(matrix(1:4, 2), "dsids", "numeric"),
    class = "precrec_error_invalid_dsids"
  )
})
