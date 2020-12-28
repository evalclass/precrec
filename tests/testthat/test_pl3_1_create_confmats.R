#' @importFrom precrec

context("PL 3: Create confusion matrices")
# Test create_confmats(fmdat, scores, labels)

test_that("create_confmats() reterns a 'cmats' object", {
  cmats1 <- create_confmats(scores = c(0.1, 0.2, 0), labels = c(1, 0, 1))

  data(P10N10)
  fmdat <- reformat_data(P10N10$scores, P10N10$labels)
  cmats2 <- create_confmats(fmdat)
  cmats3 <- create_confmats(scores = P10N10$scores, labels = P10N10$labels)

  expect_true(is(cmats1, "cmats"))
  expect_true(is(cmats2, "cmats"))
  expect_true(is(cmats3, "cmats"))
})

test_that("'fmdat' must be a 'fmdat' object", {
  expect_err_msg <- function(fmdat) {
    err_msg <- "Unrecognized class for .validate()"
    eval(bquote(expect_error(create_confmats(fmdat), err_msg)))
  }

  expect_err_msg(list())
  expect_err_msg(data.frame())
})

test_that("create_confmats() can directly take scores and labels", {
  fmdat <- reformat_data(c(0.1, 0.2, 0.2, 0), c(1, 0, 1, 1))
  cmats1 <- create_confmats(fmdat)
  cmats2 <- create_confmats(scores = c(0.1, 0.2, 0.2, 0),
                            labels = c(1, 0, 1, 1))

  expect_equal(cmats1, cmats2)
})

test_that("create_confmats() accepts arguments for reformat_data()", {
  err_msg <- "Invalid arguments: na.rm"
  expect_error(create_confmats(scores = c(0.1, 0.2, 0.2, 0),
                               labels = c(1, 0, 1, 1), na.rm = TRUE),
               err_msg)

  cmats <- create_confmats(scores = c(0.1, 0.2, 0),
                           labels = c(1, 0, 1),
                           na_worst = TRUE,
                           ties_method = "first",
                           keep_fmdat = TRUE)

  expect_equal(.get_obj_arg(cmats, "fmdat", "na_worst"), TRUE)
  expect_equal(.get_obj_arg(cmats, "fmdat", "ties_method"), "first")
})

test_that("create_confmats() accepts na_worst argument", {
  expect_equal_ranks <- function(scores, na_worst, ranks) {
    cmats <- create_confmats(scores = scores,
                             labels = c(1, 0, 1),
                             na_worst = na_worst,
                             keep_fmdat = TRUE)

    fmdat <- .get_obj(cmats, "fmdat")

    eval(bquote(expect_equal(.get_obj_arg(cmats, NULL, "na_worst"), na_worst)))
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

test_that("create_confmats() accepts ties_method argument", {

  expect_equal_ranks <- function(ties_method, ranks) {
    cmats <- create_confmats(scores = c(0.1, 0.2, 0.2, 0.2, 0.3),
                             labels = c(1, 0, 1, 1, 1),
                             ties_method = ties_method,
                             keep_fmdat = TRUE)

    fmdat <- .get_obj(cmats, "fmdat")

    eval(bquote(expect_equal(.get_obj_arg(cmats, NULL, "ties_method"),
                             ties_method)))
    eval(bquote(expect_equal(.get_obj_arg(fmdat, NULL, "ties_method"),
                             ties_method)))
    eval(bquote(expect_equal(fmdat[["ranks"]], ranks)))
  }

  expect_equal_ranks("equiv", c(5, 2, 2, 2, 1))
  expect_equal_ranks("first", c(5, 2, 3, 4, 1))

})

test_that("'cmats' contains a list with 7 items", {
  cmats <- create_confmats(scores = c(0.1, 0.2, 0), labels = c(1, 0, 1))

  expect_true(is.list(cmats))
  expect_equal(length(cmats), 7)
})

test_that("TPs, FNs, FPs, and TNs must be the same length", {
  cmats <- create_confmats(scores = c(0.1, 0.2, 0), labels = c(1, 0, 1))
  vec_size <- length(cmats[["ranks"]])

  expect_true(vec_size != 0)
  expect_equal(vec_size, length(cmats[["tp"]]))
  expect_equal(vec_size, length(cmats[["fn"]]))
  expect_equal(vec_size, length(cmats[["fp"]]))
  expect_equal(vec_size, length(cmats[["tn"]]))
})

test_that("'cmats' contains correct items", {
  cmats <- create_confmats(scores = c(0.1, 0.2, 0), labels = c(1, 0, 1))
  np <- cmats[["pos_num"]]
  nn <- cmats[["neg_num"]]
  vec_size <- length(cmats[["ranks"]])

  expect_equal(cmats[["tp"]][1], 0)
  expect_equal(cmats[["fn"]][1], np)
  expect_equal(cmats[["fp"]][1], 0)
  expect_equal(cmats[["tn"]][1], nn)

  expect_equal(cmats[["tp"]][vec_size], np)
  expect_equal(cmats[["fn"]][vec_size], 0)
  expect_equal(cmats[["fp"]][vec_size], nn)
  expect_equal(cmats[["tn"]][vec_size], 0)
})

test_that("create_confmats() reterns correct matrices", {
  cmats <- create_confmats(scores = c(0.1, 0.2, 0, 0.3),
                           labels = c(1, 0, 0, 1))

  expect_equal(cmats[["pos_num"]], 2)
  expect_equal(cmats[["neg_num"]], 2)
  expect_equal(cmats[["tp"]], c(0, 1, 1, 2, 2))
  expect_equal(cmats[["fn"]], c(2, 1, 1, 0, 0))
  expect_equal(cmats[["fp"]], c(0, 0, 1, 1, 2))
  expect_equal(cmats[["tn"]], c(2, 2, 1, 1, 0))
})

test_that("create_confmats() handles tied scores 1", {
  cmats <- create_confmats(scores = c(0.3, 0.2, 0.2, 0.2, 0.2, 0.1),
                           labels = c(0, 1, 0, 1, 0, 1))
  expect_equal(cmats[["tp"]], c(0, 0, 0.5, 1, 1.5, 2, 3))
  expect_equal(cmats[["fp"]], c(0, 1, 1.5, 2, 2.5, 3, 3))
})

test_that("create_confmats() handles tied scores 2", {
  cmats <- create_confmats(scores = c(0.3, 0.2, 0.2, 0.2, 0.2),
                           labels = c(0, 1, 0, 1, 0))
  expect_equal(cmats[["tp"]], c(0, 0, 0.5, 1, 1.5, 2))
  expect_equal(cmats[["fp"]], c(0, 1, 1.5, 2, 2.5, 3))
})

pl3_create_ms_dat <- function() {
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

pl3_create_sm_dat <- function() {
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

pl3_create_mm_dat <- function() {
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
  cmats <- create_confmats(scores = c(1, 2, 3, 4),
                           labels = c(1, 0, 1, 0))

  expect_equal(cmats[["pos_num"]], 2)
  expect_equal(cmats[["neg_num"]], 2)
  expect_equal(cmats[["tp"]], c(0, 0, 1, 1, 2))
  expect_equal(cmats[["fn"]], c(2, 2, 1, 1, 0))
  expect_equal(cmats[["fp"]], c(0, 1, 1, 2, 2))
  expect_equal(cmats[["tn"]], c(2, 1, 1, 0, 0))

})

test_that("ms test data", {
  msdat <- pl3_create_ms_dat()

  cmats1 <- create_confmats(scores = msdat[["scores"]][[1]],
                            labels = msdat[["labels"]][[1]])
  expect_equal(cmats1[["pos_num"]], 3)
  expect_equal(cmats1[["neg_num"]], 1)
  expect_equal(cmats1[["tp"]], c(0, 1, 2, 2, 3))
  expect_equal(cmats1[["fn"]], c(3, 2, 1, 1, 0))
  expect_equal(cmats1[["fp"]], c(0, 0, 0, 1, 1))
  expect_equal(cmats1[["tn"]], c(1, 1, 1, 0, 0))

  cmats2 <- create_confmats(scores = msdat[["scores"]][[2]],
                            labels = msdat[["labels"]][[2]])
  expect_equal(cmats2[["pos_num"]], 3)
  expect_equal(cmats2[["neg_num"]], 1)
  expect_equal(cmats2[["tp"]], c(0, 1, 2, 3, 3))
  expect_equal(cmats2[["fn"]], c(3, 2, 1, 0, 0))
  expect_equal(cmats2[["fp"]], c(0, 0, 0, 0, 1))
  expect_equal(cmats2[["tn"]], c(1, 1, 1, 1, 0))

  cmats3 <- create_confmats(scores = msdat[["scores"]][[3]],
                            labels = msdat[["labels"]][[3]])
  expect_equal(cmats3[["pos_num"]], 3)
  expect_equal(cmats3[["neg_num"]], 1)
  expect_equal(cmats3[["tp"]], c(0, 1, 1, 2, 3))
  expect_equal(cmats3[["fn"]], c(3, 2, 2, 1, 0))
  expect_equal(cmats3[["fp"]], c(0, 0, 1, 1, 1))
  expect_equal(cmats3[["tn"]], c(1, 1, 0, 0, 0))

})

test_that("sm test data", {
  smdat <- pl3_create_sm_dat()

  cmats1 <- create_confmats(scores = smdat[["scores"]][[1]],
                            labels = smdat[["labels"]][[1]])
  expect_equal(cmats1[["pos_num"]], 3)
  expect_equal(cmats1[["neg_num"]], 1)
  expect_equal(cmats1[["tp"]], c(0, 1, 2, 2, 3))
  expect_equal(cmats1[["fn"]], c(3, 2, 1, 1, 0))
  expect_equal(cmats1[["fp"]], c(0, 0, 0, 1, 1))
  expect_equal(cmats1[["tn"]], c(1, 1, 1, 0, 0))

  cmats2 <- create_confmats(scores = smdat[["scores"]][[2]],
                            labels = smdat[["labels"]][[2]])
  expect_equal(cmats2[["pos_num"]], 3)
  expect_equal(cmats2[["neg_num"]], 1)
  expect_equal(cmats2[["tp"]], c(0, 1, 2, 3, 3))
  expect_equal(cmats2[["fn"]], c(3, 2, 1, 0, 0))
  expect_equal(cmats2[["fp"]], c(0, 0, 0, 0, 1))
  expect_equal(cmats2[["tn"]], c(1, 1, 1, 1, 0))

  cmats3 <- create_confmats(scores = smdat[["scores"]][[3]],
                            labels = smdat[["labels"]][[3]])
  expect_equal(cmats3[["pos_num"]], 3)
  expect_equal(cmats3[["neg_num"]], 1)
  expect_equal(cmats3[["tp"]], c(0, 1, 1, 2, 3))
  expect_equal(cmats3[["fn"]], c(3, 2, 2, 1, 0))
  expect_equal(cmats3[["fp"]], c(0, 0, 1, 1, 1))
  expect_equal(cmats3[["tn"]], c(1, 1, 0, 0, 0))

})

test_that("mm test data", {
  mmdat <- pl3_create_mm_dat()

  cmats1 <- create_confmats(scores = mmdat[["scores"]][[1]],
                            labels = mmdat[["labels"]][[1]])
  expect_equal(cmats1[["pos_num"]], 3)
  expect_equal(cmats1[["neg_num"]], 1)
  expect_equal(cmats1[["tp"]], c(0, 1, 2, 2, 3))
  expect_equal(cmats1[["fn"]], c(3, 2, 1, 1, 0))
  expect_equal(cmats1[["fp"]], c(0, 0, 0, 1, 1))
  expect_equal(cmats1[["tn"]], c(1, 1, 1, 0, 0))

  cmats2 <- create_confmats(scores = mmdat[["scores"]][[2]],
                            labels = mmdat[["labels"]][[2]])
  expect_equal(cmats2[["pos_num"]], 3)
  expect_equal(cmats2[["neg_num"]], 1)
  expect_equal(cmats2[["tp"]], c(0, 1, 2, 3, 3))
  expect_equal(cmats2[["fn"]], c(3, 2, 1, 0, 0))
  expect_equal(cmats2[["fp"]], c(0, 0, 0, 0, 1))
  expect_equal(cmats2[["tn"]], c(1, 1, 1, 1, 0))

  cmats3 <- create_confmats(scores = mmdat[["scores"]][[3]],
                            labels = mmdat[["labels"]][[3]])
  expect_equal(cmats3[["pos_num"]], 3)
  expect_equal(cmats3[["neg_num"]], 1)
  expect_equal(cmats3[["tp"]], c(0, 1, 1, 2, 3))
  expect_equal(cmats3[["fn"]], c(3, 2, 2, 1, 0))
  expect_equal(cmats3[["fp"]], c(0, 0, 1, 1, 1))
  expect_equal(cmats3[["tn"]], c(1, 1, 0, 0, 0))

  cmats4 <- create_confmats(scores = mmdat[["scores"]][[4]],
                            labels = mmdat[["labels"]][[4]])
  expect_equal(cmats4[["pos_num"]], 3)
  expect_equal(cmats4[["neg_num"]], 1)
  expect_equal(cmats4[["tp"]], c(0, 1, 1, 2, 3))
  expect_equal(cmats4[["fn"]], c(3, 2, 2, 1, 0))
  expect_equal(cmats4[["fp"]], c(0, 0, 1, 1, 1))
  expect_equal(cmats4[["tn"]], c(1, 1, 0, 0, 0))
})
