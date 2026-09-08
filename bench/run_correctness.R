#!/usr/bin/env Rscript
#
# Correctness harness for the C++ layer.
#
#   Rscript bench/run_correctness.R
#
# The E4 optimizations rewrite how the C++ code allocates and fills its
# results. This harness pins the properties those rewrites could silently
# break, so it should be run before and after every one of them:
#
#   1. the C++ data-frame builders agree with the pure-R fallback
#      (`use_rcpp = FALSE` in `.dataframe_common`)
#   2. ALTREP inputs (compact sequences, deferred coercions) give the same
#      answers as the materialized vectors they stand for
#   3. shifting every score by a constant leaves the curves unchanged, and
#      NAs are ranked as `na_worst` asks - the invariant the DBL_MIN
#      sentinel bug broke
#
# Exits non-zero if any check fails.

.bench_dir <- "bench"
source(file.path(.bench_dir, "harness.R"))
source(file.path(.bench_dir, "datasets.R"))

.checks <- new.env(parent = emptyenv())
.checks$pass <- 0L
.checks$fail <- character()

check <- function(label, ok) {
  ok <- isTRUE(ok)
  cat(if (ok) "  ok   " else "  FAIL ", label, "\n", sep = "")
  if (ok) {
    .checks$pass <- .checks$pass + 1L
  } else {
    .checks$fail <- c(.checks$fail, label)
  }
  invisible(ok)
}

same <- function(a, b) isTRUE(all.equal(a, b, tolerance = 1e-12))

# --- 1. C++ builders against the pure-R fallback ----------------------------

check_rcpp_fallback <- function(d, tag) {
  curves <- evalmod(scores = d[["scores"]], labels = d[["labels"]])
  points <- evalmod(
    scores = d[["scores"]], labels = d[["labels"]],
    mode = "basic"
  )

  check(
    paste0("rocprc data frame, C++ == R [", tag, "]"),
    same(
      as.data.frame(curves),
      suppressWarnings(as.data.frame(curves, use_rcpp = FALSE))
    )
  )
  check(
    paste0("basic data frame, C++ == R [", tag, "]"),
    same(
      as.data.frame(points),
      suppressWarnings(as.data.frame(points, use_rcpp = FALSE))
    )
  )
  check(
    paste0("fortify, C++ == R [", tag, "]"),
    same(
      fortify(curves),
      suppressWarnings(fortify(curves, use_rcpp = FALSE))
    )
  )
}

check_rcpp_fallback_avg <- function(m, tag) {
  mdat <- mmdata(
    join_scores(m[["scores"]]), join_labels(m[["labels"]]),
    expd_first = "dsids"
  )
  avg <- evalmod(mdat, calc_avg = TRUE, raw_curves = FALSE)

  check(
    paste0("averaged data frame, C++ == R [", tag, "]"),
    same(
      as.data.frame(avg, raw_curves = FALSE),
      suppressWarnings(as.data.frame(avg, raw_curves = FALSE,
        use_rcpp = FALSE
      ))
    )
  )
}

# --- 2. ALTREP inputs -------------------------------------------------------

# 1:n and seq_len(n) are compact sequences; as.numeric() of one is a
# deferred coercion. None of them hold a real data buffer until something
# asks for one, so they are the inputs most likely to trip up C++ that
# reaches for a pointer.
check_altrep <- function(n = 2000L) {
  labels <- rep(c(0L, 1L), length.out = n)

  altrep_int <- seq_len(n)
  altrep_dbl <- as.numeric(seq_len(n))
  plain_dbl <- as.numeric(seq_len(n)) + 0

  check(
    "ALTREP compact integer scores rank like a plain vector",
    same(
      .rank_scores(altrep_int)[["ranks"]],
      .rank_scores(plain_dbl)[["ranks"]]
    )
  )
  check(
    "ALTREP deferred double scores rank like a plain vector",
    same(
      .rank_scores(altrep_dbl)[["ranks"]],
      .rank_scores(plain_dbl)[["ranks"]]
    )
  )
  check(
    "ALTREP scores give the same curves as a plain vector",
    same(
      as.data.frame(evalmod(scores = altrep_dbl, labels = labels)),
      as.data.frame(evalmod(scores = plain_dbl, labels = labels))
    )
  )
  check(
    "ALTREP labels give the same curves as a plain vector",
    same(
      as.data.frame(evalmod(scores = plain_dbl, labels = rep(0:1, n / 2))),
      as.data.frame(evalmod(scores = plain_dbl, labels = labels))
    )
  )
}

# --- 3. Ranking invariants --------------------------------------------------

check_invariants <- function(d, tag) {
  shifted <- d
  shifted[["scores"]] <- d[["scores"]] + 1000

  check(
    paste0("curves are invariant under a score shift [", tag, "]"),
    same(
      as.data.frame(evalmod(scores = d[["scores"]], labels = d[["labels"]])),
      as.data.frame(evalmod(
        scores = shifted[["scores"]],
        labels = shifted[["labels"]]
      ))
    )
  )

  # All-negative scores are the case the DBL_MIN sentinel got wrong
  neg <- d
  neg[["scores"]] <- d[["scores"]] - max(d[["scores"]], na.rm = TRUE) - 1

  check(
    paste0("all-negative scores match their shifted copy [", tag, "]"),
    same(
      as.data.frame(evalmod(
        scores = neg[["scores"]],
        labels = neg[["labels"]]
      )),
      as.data.frame(evalmod(
        scores = neg[["scores"]] + 1000,
        labels = neg[["labels"]]
      ))
    )
  )

  for (na_worst in c(TRUE, FALSE)) {
    ranks_neg <- .rank_scores(neg[["scores"]], na_worst = na_worst)[["ranks"]]
    ranks_pos <- .rank_scores(neg[["scores"]] + 1000,
      na_worst = na_worst
    )[["ranks"]]
    check(
      paste0(
        "ranks ignore the sign of the scores, na_worst = ", na_worst,
        " [", tag, "]"
      ),
      same(ranks_neg, ranks_pos)
    )
  }
}

# --- main -------------------------------------------------------------------

main <- function() {
  bench_load_precrec()

  shapes <- list(
    balanced = list(n = 5000),
    imbalanced = list(n = 5000, prevalence = 0.01),
    ties = list(n = 5000, ties = 0.5),
    nas = list(n = 5000, nas = 0.05),
    mixed = list(n = 5000, prevalence = 0.01, ties = 0.5, nas = 0.05)
  )

  for (tag in names(shapes)) {
    cat("\n", tag, "\n", sep = "")
    d <- do.call(.bench_make_data, shapes[[tag]])
    check_rcpp_fallback(d, tag)
    check_invariants(d, tag)
  }

  cat("\naveraged curves\n")
  check_rcpp_fallback_avg(.bench_make_multi(5000, k = 5), "multi5")

  cat("\nALTREP inputs\n")
  check_altrep()

  cat("\n", .checks$pass, " checks passed", sep = "")
  if (length(.checks$fail) > 0L) {
    cat(", ", length(.checks$fail), " FAILED:\n", sep = "")
    cat(paste0("  ", .checks$fail, collapse = "\n"), "\n")
    quit(status = 1L)
  }
  cat(", none failed.\n")
  invisible(NULL)
}

if (sys.nframe() == 0L || identical(environment(), globalenv())) {
  main()
}
