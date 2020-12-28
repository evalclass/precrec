#' @importFrom precrec

context("PL 4: Calculate evaluation measures")
# Test calc_measures(cmats, scores, labels)

test_that("calc_measures() reterns an 'pevals' object", {
  pevals1 <- calc_measures(scores = c(0.1, 0.2, 0), labels = c(1, 0, 1))

  data(P10N10)
  fmdat <- reformat_data(P10N10$scores, P10N10$labels)
  cmats <- create_confmats(fmdat)
  pevals2 <- calc_measures(cmats)
  pevals3 <- calc_measures(scores = P10N10$scores, labels = P10N10$labels)

  expect_true(is(pevals1, "pevals"))
  expect_true(is(pevals2, "pevals"))
  expect_true(is(pevals3, "pevals"))
})

test_that("'cmats' must be a 'cmats' object", {
  expect_err_msg <- function(cmats) {
    err_msg <- "Unrecognized class for .validate()"
    eval(bquote(expect_error(calc_measures(cmats), err_msg)))
  }

  expect_err_msg(list())
  expect_err_msg(data.frame())
})

test_that("calc_measures() can directly take scores and labels", {
  cmats <- create_confmats(scores = c(0.1, 0.2, 0.2, 0),
                           labels = c(1, 0, 1, 1))
  pevals1 <- calc_measures(cmats)
  pevals2 <- calc_measures(scores = c(0.1, 0.2, 0.2, 0),
                           labels = c(1, 0, 1, 1))

  expect_equal(pevals1, pevals2)
})

test_that("calc_measures() accepts arguments for reformat_data()", {
  err_msg <- "Invalid arguments: na.rm"
  expect_error(calc_measures(scores = c(0.1, 0.2, 0.2, 0),
                             labels = c(1, 0, 1, 1), na.rm = TRUE),
               err_msg)

  pevals <- calc_measures(scores = c(0.1, 0.2, 0),
                          labels = c(1, 0, 1),
                          na_worst = TRUE,
                          ties_method = "first",
                          keep_fmdat = TRUE)

  expect_equal(.get_obj_arg(pevals, "fmdat", "na_worst"), TRUE)
  expect_equal(.get_obj_arg(pevals, "fmdat", "ties_method"), "first")
})


test_that("calc_measures() accepts na_worst argument", {
  expect_equal_ranks <- function(scores, na_worst, ranks) {
    pevals <- calc_measures(scores = scores,
                            labels = c(1, 0, 1),
                            na_worst = na_worst,
                            keep_fmdat = TRUE)

    fmdat <- .get_obj(pevals, "fmdat")

    eval(bquote(expect_equal(.get_obj_arg(pevals, NULL, "na_worst"), na_worst)))
    eval(bquote(expect_equal(.get_obj_arg(fmdat, NULL, "na_worst"), na_worst)))
    eval(bquote(expect_equal(fmdat[["ranks"]], ranks)))

    sranks <- .rank_scores(scores, na_worst = na_worst)
    eval(bquote(expect_equal(sranks[["ranks"]], ranks)))
  }

  na1_scores <- c(NA, 0.2, 0.1)
  na2_scores <- c(0.2, NA, 0.1)
  na3_scores <- c(0.2, 0.1, NA)

  expect_equal_ranks(na1_scores, TRUE, c(3, 1, 2))
  expect_equal_ranks(na1_scores, FALSE, c(1, 2, 3))

  expect_equal_ranks(na2_scores, TRUE, c(1, 3, 2))
  expect_equal_ranks(na2_scores, FALSE, c(2, 1, 3))

  expect_equal_ranks(na3_scores, TRUE, c(1, 2, 3))
  expect_equal_ranks(na3_scores, FALSE, c(2, 3, 1))
})

test_that("calc_measures() accepts ties_method argument", {

  expect_equal_ranks <- function(ties_method, ranks) {
    pevals <- calc_measures(scores = c(0.1, 0.2, 0.2, 0.2, 0.3),
                            labels = c(1, 0, 1, 1, 1),
                            ties_method = ties_method,
                            keep_fmdat = TRUE)

    fmdat <- .get_obj(pevals, "fmdat")

    eval(bquote(expect_equal(.get_obj_arg(pevals, NULL, "ties_method"),
                             ties_method)))
    eval(bquote(expect_equal(.get_obj_arg(fmdat, NULL, "ties_method"),
                             ties_method)))
    eval(bquote(expect_equal(fmdat[["ranks"]], ranks)))
  }

  expect_equal_ranks("equiv", c(5, 2, 2, 2, 1))
  expect_equal_ranks("first", c(5, 2, 3, 4, 1))

})

test_that("'pevals' contains a list with 1 item", {
  pevals <- calc_measures(scores = c(0.1, 0.2, 0), labels = c(1, 0, 1))

  expect_true(is.list(pevals))
  expect_equal(length(pevals), 1)
})

test_that("calc_measures() reterns correct evaluation values", {
  pevals <- calc_measures(scores = c(0.1, 0.2, 0, 0.3),
                          labels = c(1, 0, 0, 1))
  pb <- pevals[["basic"]]

#   "TPs" c(0, 1, 1, 2, 2)
#   "FNs" c(2, 1, 1, 0, 0)
#   "FPs" c(0, 0, 1, 1, 2)
#   "TNs" c(2, 2, 1, 1, 0)
  expect_equal(pb[["error"]], c(0.5, 0.25, 0.5, 0.25, 0.5))
  expect_equal(pb[["accuracy"]], c(0.5, 0.75, 0.5, 0.75, 0.5))
  expect_equal(pb[["specificity"]], c(1, 1, 0.5, 0.5, 0))
  expect_equal(pb[["sensitivity"]], c(0, 0.5, 0.5, 1, 1))
  expect_equal(pb[["precision"]], c(1, 1, 0.5, 2/3, 0.5))
  expect_equal(pb[["mcc"]], c(NA, 0.5773503, 0, 0.5773503, NA),
               tolerance = 1e-4)
  expect_equal(pb[["fscore"]], c(0, 2/3, 0.5, 0.8, 2/3), tolerance = 1e-4)

})

pl4_create_ms_dat <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  list(scores = scores, labels = labels)
}

pl4_create_sm_dat <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  list(scores = scores, labels = labels)
}

pl4_create_mm_dat <- function() {
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

  list(scores = scores, labels = labels)
}

test_that("ss test data", {
  pevals <- calc_measures(scores = c(1, 2, 3, 4),
                          labels = c(1, 0, 1, 0))
  pb <- pevals[["basic"]]

  expect_equal(pb[["error"]], c(0.5, 0.75, 0.5, 0.75, 0.5))
  expect_equal(pb[["accuracy"]], c(0.5, 0.25, 0.5, 0.25, 0.5))
  expect_equal(pb[["specificity"]], c(1, 0.5, 0.5, 0, 0))
  expect_equal(pb[["sensitivity"]], c(0, 0, 0.5, 0.5, 1))
  expect_equal(pb[["precision"]], c(0, 0, 0.5, 1/3, 0.5))
  expect_equal(pb[["mcc"]], c(NA, -0.5773503, 0, -0.5773503, NA),
               tolerance = 1e-4)
  expect_equal(pb[["fscore"]], c(0, 0, 0.5, 0.4, 2/3), tolerance = 1e-4)
})

test_that("ms test data", {
  msdat <- pl4_create_ms_dat()

  pevals1 <- calc_measures(scores = msdat[["scores"]][[1]],
                           labels = msdat[["labels"]][[1]])
  pb1 <- pevals1[["basic"]]
  expect_equal(pb1[["error"]], c(0.75, 0.5, 0.25, 0.5, 0.25))
  expect_equal(pb1[["accuracy"]], c(0.25, 0.5, 0.75, 0.5, 0.75))
  expect_equal(pb1[["specificity"]], c(1, 1, 1, 0, 0))
  expect_equal(pb1[["sensitivity"]], c(0, 1/3, 2/3, 2/3, 1))
  expect_equal(pb1[["precision"]], c(1, 1, 1, 2/3, 0.75))
  expect_equal(pb1[["mcc"]], c(NA, 1/3, 0.5773503, -1/3, NA),
               tolerance = 1e-4)
  expect_equal(pb1[["fscore"]], c(0, 0.5, 0.8, 2/3, 0.8571429),
               tolerance = 1e-4)

  pevals2 <- calc_measures(scores = msdat[["scores"]][[2]],
                           labels = msdat[["labels"]][[2]])
  pb2 <- pevals2[["basic"]]
  expect_equal(pb2[["error"]], c(0.75, 0.5, 0.25, 0, 0.25))
  expect_equal(pb2[["accuracy"]], c(0.25, 0.5, 0.75, 1, 0.75))
  expect_equal(pb2[["specificity"]], c(1, 1, 1, 1, 0))
  expect_equal(pb2[["sensitivity"]], c(0, 1/3, 2/3, 1, 1))
  expect_equal(pb2[["precision"]], c(1, 1, 1, 1, 0.75))
  expect_equal(pb2[["mcc"]], c(NA, 1/3, 0.5773503, 1, NA),
               tolerance = 1e-4)
  expect_equal(pb2[["fscore"]], c(0, 0.5, 0.8, 1, 0.8571429), tolerance = 1e-4)

  pevals3 <- calc_measures(scores = msdat[["scores"]][[3]],
                           labels = msdat[["labels"]][[3]])
  pb3 <- pevals3[["basic"]]
  expect_equal(pb3[["error"]], c(0.75, 0.5, 0.75, 0.5, 0.25))
  expect_equal(pb3[["accuracy"]], c(0.25, 0.5, 0.25, 0.5, 0.75))
  expect_equal(pb3[["specificity"]], c(1, 1, 0, 0, 0))
  expect_equal(pb3[["sensitivity"]], c(0, 1/3, 1/3, 2/3, 1))
  expect_equal(pb3[["precision"]], c(1, 1, 0.5, 2/3, 0.75))
  expect_equal(pb3[["mcc"]], c(NA, 1/3, -0.5773503, -1/3, NA),
               tolerance = 1e-4)
  expect_equal(pb3[["fscore"]], c(0, 0.5, 0.4, 2/3, 0.8571429),
               tolerance = 1e-4)

})

test_that("sm test data", {
  smdat <- pl4_create_sm_dat()

  pevals1 <- calc_measures(scores = smdat[["scores"]][[1]],
                           labels = smdat[["labels"]][[1]])
  pb1 <- pevals1[["basic"]]
  expect_equal(pb1[["error"]], c(0.75, 0.5, 0.25, 0.5, 0.25))
  expect_equal(pb1[["accuracy"]], c(0.25, 0.5, 0.75, 0.5, 0.75))
  expect_equal(pb1[["specificity"]], c(1, 1, 1, 0, 0))
  expect_equal(pb1[["sensitivity"]], c(0, 1/3, 2/3, 2/3, 1))
  expect_equal(pb1[["precision"]], c(1, 1, 1, 2/3, 0.75))
  expect_equal(pb1[["mcc"]], c(NA, 1/3, 0.5773503, -1/3, NA),
               tolerance = 1e-4)
  expect_equal(pb1[["fscore"]], c(0, 0.5, 0.8, 2/3, 0.8571429),
               tolerance = 1e-4)

  pevals2 <- calc_measures(scores = smdat[["scores"]][[2]],
                           labels = smdat[["labels"]][[2]])
  pb2 <- pevals2[["basic"]]
  expect_equal(pb2[["error"]], c(0.75, 0.5, 0.25, 0, 0.25))
  expect_equal(pb2[["accuracy"]], c(0.25, 0.5, 0.75, 1, 0.75))
  expect_equal(pb2[["specificity"]], c(1, 1, 1, 1, 0))
  expect_equal(pb2[["sensitivity"]], c(0, 1/3, 2/3, 1, 1))
  expect_equal(pb2[["precision"]], c(1, 1, 1, 1, 0.75))
  expect_equal(pb2[["mcc"]], c(NA, 1/3, 0.5773503, 1, NA),
               tolerance = 1e-4)
  expect_equal(pb2[["fscore"]], c(0, 0.5, 0.8, 1, 0.8571429), tolerance = 1e-4)

  pevals3 <- calc_measures(scores = smdat[["scores"]][[3]],
                           labels = smdat[["labels"]][[3]])
  pb3 <- pevals3[["basic"]]
  expect_equal(pb3[["error"]], c(0.75, 0.5, 0.75, 0.5, 0.25))
  expect_equal(pb3[["accuracy"]], c(0.25, 0.5, 0.25, 0.5, 0.75))
  expect_equal(pb3[["specificity"]], c(1, 1, 0, 0, 0))
  expect_equal(pb3[["sensitivity"]], c(0, 1/3, 1/3, 2/3, 1))
  expect_equal(pb3[["precision"]], c(1, 1, 0.5, 2/3, 0.75))
  expect_equal(pb3[["mcc"]], c(NA, 1/3, -0.5773503, -1/3, NA),
               tolerance = 1e-4)
  expect_equal(pb3[["fscore"]], c(0, 0.5, 0.4, 2/3, 0.8571429),
               tolerance = 1e-4)

})

test_that("mm test data", {
  mmdat <- pl4_create_mm_dat()

  pevals1 <- calc_measures(scores = mmdat[["scores"]][[1]],
                           labels = mmdat[["labels"]][[1]])
  pb1 <- pevals1[["basic"]]
  expect_equal(pb1[["error"]], c(0.75, 0.5, 0.25, 0.5, 0.25))
  expect_equal(pb1[["accuracy"]], c(0.25, 0.5, 0.75, 0.5, 0.75))
  expect_equal(pb1[["specificity"]], c(1, 1, 1, 0, 0))
  expect_equal(pb1[["sensitivity"]], c(0, 1/3, 2/3, 2/3, 1))
  expect_equal(pb1[["precision"]], c(1, 1, 1, 2/3, 0.75))
  expect_equal(pb1[["mcc"]], c(NA, 1/3, 0.5773503, -1/3, NA),
               tolerance = 1e-4)
  expect_equal(pb1[["fscore"]], c(0, 0.5, 0.8, 2/3, 0.8571429),
               tolerance = 1e-4)

  pevals2 <- calc_measures(scores = mmdat[["scores"]][[2]],
                           labels = mmdat[["labels"]][[2]])
  pb2 <- pevals2[["basic"]]
  expect_equal(pb2[["error"]], c(0.75, 0.5, 0.25, 0, 0.25))
  expect_equal(pb2[["accuracy"]], c(0.25, 0.5, 0.75, 1, 0.75))
  expect_equal(pb2[["specificity"]], c(1, 1, 1, 1, 0))
  expect_equal(pb2[["sensitivity"]], c(0, 1/3, 2/3, 1, 1))
  expect_equal(pb2[["precision"]], c(1, 1, 1, 1, 0.75))
  expect_equal(pb2[["mcc"]], c(NA, 1/3, 0.5773503, 1, NA),
               tolerance = 1e-4)
  expect_equal(pb2[["fscore"]], c(0, 0.5, 0.8, 1, 0.8571429), tolerance = 1e-4)

  pevals3 <- calc_measures(scores = mmdat[["scores"]][[3]],
                           labels = mmdat[["labels"]][[3]])
  pb3 <- pevals3[["basic"]]
  expect_equal(pb3[["error"]], c(0.75, 0.5, 0.75, 0.5, 0.25))
  expect_equal(pb3[["accuracy"]], c(0.25, 0.5, 0.25, 0.5, 0.75))
  expect_equal(pb3[["specificity"]], c(1, 1, 0, 0, 0))
  expect_equal(pb3[["sensitivity"]], c(0, 1/3, 1/3, 2/3, 1))
  expect_equal(pb3[["precision"]], c(1, 1, 0.5, 2/3, 0.75))
  expect_equal(pb3[["mcc"]], c(NA, 1/3, -0.5773503, -1/3, NA),
               tolerance = 1e-4)
  expect_equal(pb3[["fscore"]], c(0, 0.5, 0.4, 2/3, 0.8571429),
               tolerance = 1e-4)

  pevals4 <- calc_measures(scores = mmdat[["scores"]][[3]],
                           labels = mmdat[["labels"]][[3]])
  pb4 <- pevals4[["basic"]]
  expect_equal(pb4[["error"]], c(0.75, 0.5, 0.75, 0.5, 0.25))
  expect_equal(pb4[["accuracy"]], c(0.25, 0.5, 0.25, 0.5, 0.75))
  expect_equal(pb4[["specificity"]], c(1, 1, 0, 0, 0))
  expect_equal(pb4[["sensitivity"]], c(0, 1/3, 1/3, 2/3, 1))
  expect_equal(pb4[["precision"]], c(1, 1, 0.5, 2/3, 0.75))
  expect_equal(pb4[["mcc"]], c(NA, 1/3, -0.5773503, -1/3, NA),
               tolerance = 1e-4)
  expect_equal(pb4[["fscore"]], c(0, 0.5, 0.4, 2/3, 0.8571429),
               tolerance = 1e-4)

})
