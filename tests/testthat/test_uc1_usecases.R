library(precrec)

context("UC 1: Test actual cases")

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
  ys <- subset(smdf, x %in% c(0.25, 0.75), select=y, drop = TRUE)

  # Test
  expect_false(any(ys==0.0))

})
