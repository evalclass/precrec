# ETC: Validate arguments
# Test .stop_invalid_arg(msg, arg),
#      .assert_internal(...),
#      .assert_flag(x, arg), .assert_string(x, arg, values, allow_na),
#      .assert_number(x, arg, min, max, whole, allow_na)

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

test_that(".assert_number() rejects NA unless allow_na is TRUE", {
  expect_error(
    .assert_number(NA_real_, "dsid"),
    class = "precrec_error_invalid_dsid"
  )
  expect_true(.assert_number(NA_real_, "dsid", allow_na = TRUE))
})
