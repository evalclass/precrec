#' @importFrom precrec

context("PL 1: Pipeline main")
# Test .pmatch_mode(val)
#      .make_prefix(model_type, data_type)
#      pl_main(mdat, mode, calc_avg, cb_alpha, raw_curves, x_bins)
#

test_that(".pmatch_mode() returns 'rocprc', 'basic' or 'aucroc'", {
  expect_equal(.pmatch_mode("rocprc"), "rocprc")
  expect_equal(.pmatch_mode("prcroc"), "rocprc")
  expect_equal(.pmatch_mode("basic"), "basic")
  expect_equal(.pmatch_mode("aucroc"), "aucroc")

  expect_equal(.pmatch_mode("r"), "rocprc")
  expect_equal(.pmatch_mode("p"), "rocprc")
  expect_equal(.pmatch_mode("b"), "basic")
  expect_equal(.pmatch_mode("a"), "aucroc")

  expect_equal(.pmatch_mode("A"), "A")
  expect_equal(.pmatch_mode(1), 1)
  expect_equal(.pmatch_mode(NULL), NULL)
})

test_that(".make_prefix() takes 'model_type' and 'data_type'", {
  expect_equal(.make_prefix("single", "single"), "ss")
  expect_equal(.make_prefix("multiple", "single"), "ms")
  expect_equal(.make_prefix("single", "multiple"), "sm")
  expect_equal(.make_prefix("multiple", "multiple"), "mm")

  expect_equal(.make_prefix("sing", "multi"), "")
  expect_equal(.make_prefix("s", "m"), "")
})

pl1_create_mdat_ms <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mmdata(scores, labels)
}

pl1_create_mdat_sm <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mmdata(scores, labels, expd_first = "dsids")
}

pl1_create_mdat_mm <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  s4 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3, s4)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  l4 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3, l4)

  mmdata(scores, labels, modnames = c("m1", "m2"), dsids = c(1, 2),
         expd_first = "modnames")
}

pl1_create_mdat_mm <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  s4 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3, s4)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  l4 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3, l4)

  mmdata(scores, labels, modnames = c("m1", "m2"), dsids = c(1, 2),
         expd_first = "modnames")
}

test_that("pl_main() returns 'sscurves', 'sspoints', 'aucroc'", {
  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)

  mdat <- mmdata(s1, l1)

  pl <- pl_main(mdat)
  expect_true(is(pl, "sscurves"))

  pl <- pl_main(mdat, "basic")
  expect_true(is(pl, "sspoints"))

  pl <- pl_main(mdat, "aucroc")
  expect_true(is(pl, "aucroc"))
})

test_that("pl_main() returns 'mscurves', 'mspoints', 'aucroc'", {
  mdat <- pl1_create_mdat_ms()

  pl <- pl_main(mdat)
  expect_true(is(pl, "mscurves"))

  pl <- pl_main(mdat, "basic")
  expect_true(is(pl, "mspoints"))

  pl <- pl_main(mdat, "aucroc")
  expect_true(is(pl, "aucroc"))
})

test_that("pl_main() returns 'smcurves', 'smpoints', 'aucroc'", {
  mdat <- pl1_create_mdat_sm()

  pl <- pl_main(mdat)
  expect_true(is(pl, "smcurves"))

  pl <- pl_main(mdat, "basic")
  expect_true(is(pl, "smpoints"))

  pl <- pl_main(mdat, "aucroc")
  expect_true(is(pl, "aucroc"))
})

test_that("pl_main() returns 'mmcurves', 'mmpoints', 'aucroc'", {
  mdat <- pl1_create_mdat_mm()

  pl <- pl_main(mdat)
  expect_true(is(pl, "mmcurves"))

  pl <- pl_main(mdat, "basic")
  expect_true(is(pl, "mmpoints"))

  pl <- pl_main(mdat, "aucroc")
  expect_true(is(pl, "aucroc"))
})

test_that("pl_main() accepts 'calc_avg'", {

  f_check_calc_avg1 <- function(mdat, val1 = "logical", val2 = "logical") {
    for (ct in c("rocs", "prcs")) {
      pl1 <- pl_main(mdat, calc_avg = TRUE, raw_curves = TRUE)
      expect_equal(typeof(attr(pl1[[ct]], "avgcurves")), val1)

      pl2 <- pl_main(mdat, calc_avg = FALSE, raw_curves = TRUE)
      expect_equal(typeof(attr(pl2[[ct]], "avgcurves")), val2)
    }
  }

  f_check_calc_avg2 <- function(mdat, val1 = "logical", val2 = "logical") {
    for (et in c("err", "acc", "sp", "sn", "prec")) {
      pl1 <- pl_main(mdat, "basic", calc_avg = TRUE, raw_curves = TRUE)
      expect_equal(typeof(attr(pl1[[et]], "avgcurves")), val1)

      pl2 <- pl_main(mdat, "basic", calc_avg = FALSE, raw_curves = TRUE)
      expect_equal(typeof(attr(pl2[[et]], "avgcurves")), val2)
    }
  }

  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)
  mdat1 <- mmdata(s1, l1)
  f_check_calc_avg1(mdat1)
  f_check_calc_avg2(mdat1)

  mdat2 <- pl1_create_mdat_ms()
  f_check_calc_avg1(mdat2)
  f_check_calc_avg2(mdat2)

  mdat3 <- pl1_create_mdat_sm()
  f_check_calc_avg1(mdat3, "list")
  f_check_calc_avg2(mdat3, "list")

  mdat4 <- pl1_create_mdat_mm()
  f_check_calc_avg1(mdat4, "list")
  f_check_calc_avg2(mdat4, "list")
})

test_that("pl_main() accepts 'cb_alpha'", {

  f_check_cb_alpha1 <- function(mdat) {
    for (ct in c("rocs", "prcs")) {
      pl1 <- pl_main(mdat, cb_alpha = 0.05, raw_curves = TRUE)
      expect_equal(attr(attr(pl1[[ct]], "avgcurves"), "cb_zval"), 1.96,
                   tolerance = 1e-2)

      pl2 <- pl_main(mdat, cb_alpha = 0.01, raw_curves = TRUE)
      expect_equal(attr(attr(pl2[[ct]], "avgcurves"), "cb_zval"), 2.575,
                   tolerance = 1e-3)
    }
  }

  f_check_cb_alpha2 <- function(mdat) {
    for (et in c("err", "acc", "sp", "sn", "prec")) {
      pl1 <- pl_main(mdat, "basic", cb_alpha = 0.05, raw_curves = TRUE)
      expect_equal(attr(attr(pl1[[et]], "avgcurves"), "cb_zval"), 1.96,
                   tolerance = 1e-2)

      pl2 <- pl_main(mdat, "basic", cb_alpha = 0.01, raw_curves = TRUE)
      expect_equal(attr(attr(pl2[[et]], "avgcurves"), "cb_zval"), 2.575,
                   tolerance = 1e-3)
    }
  }

  mdat1 <- pl1_create_mdat_sm()
  f_check_cb_alpha1(mdat1)
  f_check_cb_alpha2(mdat1)

  mdat2 <- pl1_create_mdat_mm()
  f_check_cb_alpha1(mdat2)
  f_check_cb_alpha2(mdat2)
})

test_that("pl_main() accepts 'raw_curves'", {

  f_check_raw_curves1 <- function(mdat, val1 = "list", val2 = "list") {
    for (ct in c("rocs", "prcs")) {
      pl1 <- pl_main(mdat, raw_curves = FALSE)
      expect_equal(typeof(pl1[[ct]]), val1)

      pl2 <- pl_main(mdat, raw_curves = TRUE)
      expect_equal(typeof(pl2[[ct]]), val2)
    }
  }

  f_check_raw_curves2 <- function(mdat, val1 = "list", val2 = "list") {
    for (et in c("err", "acc", "sp", "sn", "prec")) {
      pl1 <- pl_main(mdat, "basic", raw_curves = FALSE)
      expect_equal(typeof(pl1[[et]]), val1)

      pl2 <- pl_main(mdat, "basic", raw_curves = TRUE)
      expect_equal(typeof(pl2[[et]]), val2)
    }
  }

  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)
  mdat1 <- mmdata(s1, l1)
  f_check_raw_curves1(mdat1)
  f_check_raw_curves2(mdat1)

  mdat2 <- pl1_create_mdat_ms()
  f_check_raw_curves1(mdat2)
  f_check_raw_curves2(mdat2)

  mdat3 <- pl1_create_mdat_sm()
  f_check_raw_curves1(mdat3, "logical")
  f_check_raw_curves2(mdat3, "logical")

  mdat4 <- pl1_create_mdat_mm()
  f_check_raw_curves1(mdat4, "logical")
  f_check_raw_curves2(mdat4, "logical")
})

test_that("pl_main() accepts 'x_bins'", {
  f_check_x_bins <- function(mdat) {
    for (ct in c("rocs", "prcs")) {
      pl1 <- pl_main(mdat, x_bins = 10, raw_curves = TRUE)
      expect_equal(attr(pl1[[ct]][[1]], "args")[["x_bins"]], 10)

      pl2 <- pl_main(mdat, x_bins = NA, raw_curves = TRUE)
      expect_equal(attr(pl2[[ct]][[1]], "args")[["x_bins"]], 1)

      pl3 <- pl_main(mdat, x_bins = NULL, raw_curves = TRUE)
      expect_equal(attr(pl3[[ct]][[1]], "args")[["x_bins"]], 1)
    }
  }

  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)
  mdat1 <- mmdata(s1, l1)
  f_check_x_bins(mdat1)

  mdat2 <- pl1_create_mdat_ms()
  f_check_x_bins(mdat2)

  mdat3 <- pl1_create_mdat_sm()
  f_check_x_bins(mdat3)

  mdat4 <- pl1_create_mdat_mm()
  f_check_x_bins(mdat4)

})
