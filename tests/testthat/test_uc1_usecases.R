#' @importFrom precrec

context("UC 1: Use cases")

test_that("calculation of correct avg and cb when x_bins = 8", {

  # Data creation
  # > samps1 <- create_sim_samples(2, 10, 10, "good_er")
  # > sscores1 <- round(samps3[["scores"]][[1]][[1]], 3) * 1000
  # > sscores1 <- paste(as.character(sscores1), collapse = ", ")
  # > sscores2 <- round(samps3[["scores"]][[2]][[1]], 3) * 1000
  # > sscores2 <- paste(as.character(sscores2), collapse = ", ")
  # > label12 <- paste(as.character(samps3[["labels"]]), collapse = ", ")

  # Data preparation
  sscores1 <- c(997, 522, 721, 895, 783, 91, 198, 467, 13, 61,
                8, 66, 175, 127, 103, 188, 320, 132, 85, 318)
  sscores2 <- c(404, 247, 490, 661, 70, 846, 333, 613, 332, 379,
                289, 388, 185, 50, 146, 242, 226, 584, 389, 55)
  score12 <- join_scores(sscores1, sscores2)
  label12 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

  smmdat <- mmdata(score12, label12, dsids = c(1, 2))

  # Calculation with x_bins
  smcurves <- evalmod(smmdat, x_bins = 8)

  # Results
  smdf <- as.data.frame(smcurves)
  ys <- subset(smdf, x %in% c(0.25, 0.75), select = y, drop = TRUE)

  # Test
  expect_false(any(ys == 0.0))

})

test_that("as.data.frame issue when n-fold datasets are used", {

  crv5f = evalmod(nfold_df = M2N50F5,
                  score_cols = c(1, 2),
                  lab_col = 3,
                  fold_col = 4,
                  modnames = c("score1", "score2"),
                  dsids = 1:5)

  crv5f_df <- as.data.frame(crv5f)
  modnames <- table(crv5f_df$modname)
  modnames_tbl  <- table(crv5f_df$modname)

  expect_true(modnames_tbl[1] > 2000)
  expect_true(modnames_tbl[2] > 2000)

})

test_that("a factor by c() returns another factor", {

  emod <- "basic"
  scores <- list(c(0.1007308,0.5804833,0.3817939,0.2826659,0.4325302))
  labels <- list(c(1, 1, 1, 2, 1))
  mnames <- "m1"
  ds_id <- 5


  smmod <- evalmod(mode = emod, scores = scores, labels = labels,
                   modnames = mnames, dsids = ds_id)

  mdat <- mmdata(scores, labels,
                 modnames = mnames, dsids = ds_id, posclass = NULL,
                 na_worst = TRUE, ties_method = "equiv",
                 mode = emod)

  cdat <- create_confmats(mdat[[1]], keep_fmdat = TRUE)
  #pevals <- calc_measures(cdat)


  expect_equal(cdat$tp, c(0, 0, 0, 0, 1, 1))
  expect_equal(cdat$fp, c(0, 1, 2, 3, 3, 4))
  expect_equal(cdat$tn, c(4, 3, 2, 1, 1, 0))
  expect_equal(cdat$fn, c(1, 1, 1, 1, 0, 0))

  expect_equal(attr(cdat, "src")[["scores"]], scores[[1]])
  expect_equal(attr(cdat, "src")[["labels"]], labels[[1]])

  #
  # lscores <- join_scores(scores, chklen = FALSE)
  # llabels <- join_labels(labels, chklen = FALSE)
  #
  #
  # cdat <- list()
  # cdat <- c(cdat, labels)

})
