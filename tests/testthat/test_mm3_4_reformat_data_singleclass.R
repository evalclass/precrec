#' @importFrom precrec

context("MM 3: Reformat labels for single class datasets")
# Test .factor_labels(labels, posclass)

test_that(".factor_labels() reterns positive label when pos vec is given", {
  expect_pos_label <- function(labels) {
    fmtlbs <- .factor_labels(labels, NULL)
    eval(bquote(expect_equal(fmtlbs[["nn"]], 0)))
    eval(bquote(expect_equal(fmtlbs[["np"]], length(labels))))
  }

  expect_pos_label(c(2L, 2L, 2L))
  expect_pos_label(c(1, 1, 1))
  expect_pos_label(c(1.0, 1.0, 1.0))
  expect_pos_label(c(TRUE, TRUE, TRUE))
  expect_pos_label(c("P", "P", "P"))
})

test_that(".factor_labels() reterns nagative label when neg vec is given", {
  expect_neg_label <- function(labels) {
    fmtlbs <- .factor_labels(labels, NULL)
    eval(bquote(expect_equal(fmtlbs[["nn"]], length(labels))))
    eval(bquote(expect_equal(fmtlbs[["np"]], 0)))
  }

  expect_neg_label(c(1L, 1L, 1L))
  expect_neg_label(c(0L, 0L, 0L))
  expect_neg_label(c(-1L, -1L, -1L))
  expect_neg_label(c(0.0, 0.0, 0.0))
  expect_neg_label(c(-1.0, -1.0, -1.0))
  expect_neg_label(c(FALSE, FALSE, FALSE))
  expect_neg_label(c("0", "0", "0"))
  expect_neg_label(c("-1", "-1", "-1"))
  expect_neg_label(c("N", "N", "N"))
})

test_that(".factor_labels() reterns default label when pos vec is given", {
  expect_defpos_label <- function(labels, def_pos) {
    fmtlbs <- .factor_labels(labels, def_pos)
    eval(bquote(expect_equal(fmtlbs[["nn"]], 0)))
    eval(bquote(expect_equal(fmtlbs[["np"]], length(labels))))
  }

  expect_defpos_label(c(2L, 2L, 2L), 2L)
  expect_defpos_label(c(1L, 1L, 1L), 1L)
  expect_defpos_label(c(1.0, 1.0, 1.0), 1.0)
  expect_defpos_label(c(TRUE, TRUE, TRUE), TRUE)
  expect_defpos_label(c("1", "1", "1"), "1")
  expect_defpos_label(c("P", "P", "P"), "P")
})

test_that(".factor_labels() reterns non-default label when neg vec is given", {
  expect_defneg_label <- function(labels, def_pos) {
    fmtlbs <- .factor_labels(labels, def_pos)
    eval(bquote(expect_equal(fmtlbs[["nn"]], length(labels))))
    eval(bquote(expect_equal(fmtlbs[["np"]], 0)))
  }

  expect_defneg_label(c(1L, 1L, 1L), 2L)
  expect_defneg_label(c(0L, 0L, 0L), 2L)
  expect_defneg_label(c(-1L, -1L, -1L), 2L)
  expect_defneg_label(c(0.0, 0.0, 0.0), 1.0)
  expect_defneg_label(c(-1.0, -1.0, -1.0), 1.0)
  expect_defneg_label(c(FALSE, FALSE, FALSE), TRUE)
  expect_defneg_label(c("0", "0", "0"), "1")
  expect_defneg_label(c("-1", "-1", "-1"), "1")
  expect_defneg_label(c("N", "N", "N"), "P")
})
