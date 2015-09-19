
create_samples <- function(np, nn) {
  labels <- c(rep(1, np), rep(0, nn))

  random_scores <- c(rnorm(np, 0, 1), rnorm(nn, 0, 1))
  poor_er_scores <- c(rbeta(np, 4, 1), rbeta(nn, 1, 1))
  good_er_scores <- c(rbeta(np, 1, 1), rbeta(nn, 1, 4))
  excel_scores <- c(rnorm(np, 3, 1), rnorm(nn, 0, 1))
  perf_scores <- c(rep(1, np), rep(0, nn))

  list(np = np,
       nn = nn,
       labels = labels,
       random_scores = random_scores,
       poor_er_scores = poor_er_scores,
       good_er_scores = good_er_scores,
       excel_scores = excel_scores,
       perf_scores = perf_scores
  )
}

B500 <- create_samples(500, 500)
B1000 <- create_samples(1000, 1000)
IB500 <- create_samples(500, 5000)
IB1000 <- create_samples(1000, 10000)

devtools::use_data(B500, B1000, IB500, IB1000, overwrite = TRUE)
