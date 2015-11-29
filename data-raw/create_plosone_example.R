
scores <- c(20, 19, 18, 17, 16, 15, 14, 14, 14, 14, 14, 14,  8,  7,  6,  5,  5,  5,  5,  5)
labs   <- c( 1,  1, -1,  1,  1,  1,  1,  1, -1, -1, -1, -1,  1, -1,  1, -1,  1, -1, -1, -1)

P10N10 <- list(
  np = 10,
  nn = 10,
  labels = labs,
  scores = scores
)

devtools::use_data(P10N10, overwrite = TRUE)

