fold <- c(1, 3, 1, 3, 5, 5, 4, 4, 3, 5,
          2, 3, 2, 2, 2, 1, 1, 5, 5, 1,
          2, 2, 4, 3, 4, 1, 4, 1, 3, 4,
          3, 4, 1, 3, 2, 1, 5, 3, 2, 2,
          5, 3, 1, 4, 5, 5, 4, 4, 2, 5)

M2N50F5 <- data.frame(
  score1 = rnorm(50),
  score2 = rnorm(50),
  label = rep(c("pos", "neg"), c(25, 25)),
  fold = fold
)

devtools::use_data(M2N50F5, overwrite = TRUE)
