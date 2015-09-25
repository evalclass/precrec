create_test_samples <- function(np, nn) {
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

rep_test_all_levels <- function(n, np, nn) {
  labels <- c(rep(1, np), rep(0, nn))

  rfunc <- function() {
    samp <- create_test_samples(np, nn)
    list(random_scores = samp[["random_scores"]],
         poor_er_scores = samp[["poor_er_scores"]],
         good_er_scores = samp[["good_er_scores"]],
         excel_scores = samp[["excel_scores"]],
         perf_scores = samp[["perf_scores"]])
  }
  replicate(n, rfunc(), simplify = FALSE)
}
