create_sim_samples <- function(n_repeat, np, nn, score_names = "random") {
  # === Validate input arguments ===
  choices <- c("random", "poor_er", "good_er", "excel", "perf")
  if (.is_single_string(score_names) && score_names == "all") {
    score_names <- choices
  } else if (!.is_char_vec(score_names) || !(score_names %in% choices)) {
    stop(gettextf("'score_names' should be one of %s",
                  paste(dQuote(choices), collapse = ", ")))
  }
  snames <- paste0(score_names, "_scores")

  # === Sample random variables ===
  afunc <- function() {
    simdat <- sample_rnd5levs(np, nn)
    ffunc <- function(i) {
      if (names(simdat[i]) %in% snames) {
        TRUE
      } else {
        FALSE
      }
    }
    list_ids <- Filter(ffunc, seq_along(simdat))
    sd <- simdat[list_ids]
    names(sd) <- NULL
    sd
  }
  mscreos <- replicate(n_repeat, afunc(), simplify = FALSE)
  labels <- c(rep(1, np), rep(0, nn))

  # === Make a list ===
  list(scores = mscreos,
       labels = labels,
       model_names = score_names,
       data_nos = seq(n_repeat))
}

sample_rnd5levs <- function(np, nn) {
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

rep_one_level <- function(n, score_name, np, nn) {
  labels <- c(rep(1, np), rep(0, nn))

  rfunc <- function(np, nn) {
    samp <- sample_rnd5levs(np, nn)
    samp[[level_name]]
  }
  scores <- replicate(n, rfunc(np, nn), simplify = FALSE)

  list(scores = scores, labels = labels)
}

rep_all_levels <- function(n, np, nn) {
  labels <- c(rep(1, np), rep(0, nn))

  rfunc <- function(np, nn) {
    samp <- sample_rnd5levs(np, nn)
    list(random_scores = samp[["random_scores"]],
         poor_er_scores = samp[["poor_er_scores"]],
         good_er_scores = samp[["good_er_scores"]],
         excel_scores = samp[["excel_scores"]],
         perf_scores = samp[["perf_scores"]])
  }
  scores <- replicate(n, rfunc(np, nn), simplify = FALSE)

  list(scores = scores, labels = labels)
}
