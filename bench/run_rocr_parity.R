#!/usr/bin/env Rscript
#
# Cross-check the basic evaluation measures against ROCR.
#
#   Rscript bench/run_rocr_parity.R
#
# ROCR is the reference implementation for the measures added in 0.16.0, so
# comparing against it is a stronger check than any internal invariant: it is
# the thing being matched. ROCR is deliberately NOT a dependency of this
# package, not even in Suggests - install it yourself to run this, and skip
# the script when it is absent.
#
#   install.packages("ROCR")
#
# Two differences are expected and are asserted here rather than tolerated
# silently:
#
#   1. `odds` and `chisq` are NA in `precrec` wherever the 2x2 table has an
#      empty cell, which is the top and bottom rank of every dataset. ROCR
#      reports Inf or NaN there. `mi` is 0 at those two points rather than
#      NaN, because a cutoff that predicts one class for everything carries
#      no information about the labels - that value is defined, not missing.
#      Only the region both call finite is compared.
#   2. ROCR reports one row per distinct cutoff; `precrec` reports one per
#      rank. Where scores are tied the two therefore have different row
#      counts, and are lined up on the cutoff. ROCR's row for a cutoff counts
#      every instance scoring at or above it, which is `precrec`'s LAST row
#      at that cutoff - taking any other one compares different confusion
#      matrices and fails for every measure, the long-standing ones included.
#
# Exits non-zero if any check fails.

.bench_dir <- "bench"
source(file.path(.bench_dir, "harness.R"))

if (!requireNamespace("ROCR", quietly = TRUE)) {
  cat("ROCR is not installed - nothing to compare against.\n")
  cat("  install.packages(\"ROCR\")\n")
  quit(status = 0)
}

bench_load_precrec()

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

# precrec measure -> ROCR measure identifier
.measure_map <- function() {
  c(
    accuracy = "acc",
    error = "err",
    sensitivity = "tpr",
    specificity = "tnr",
    precision = "ppv",
    npv = "npv",
    mcc = "phi",
    fscore = "f",
    fpr = "fpr",
    fnr = "fnr",
    false_discovery_rate = "pcfall",
    false_omission_rate = "pcmiss",
    predicted_positive_rate = "rpp",
    predicted_negative_rate = "rnp",
    lift = "lift",
    odds = "odds",
    mi = "mi",
    chisq = "chisq",
    cost = "cost"
  )
}

# The measures of one precrec object, one column per measure, indexed by the
# score they were calculated at. The leading row is the empty prediction set,
# which has no cutoff and no counterpart in ROCR.
.precrec_by_cutoff <- function(scores, labels) {
  pts <- evalmod(
    scores = scores, labels = labels, mode = "basic",
    metrics = "all"
  )
  df <- as.data.frame(pts)
  wide <- lapply(split(df$y, df$type), identity)
  out <- as.data.frame(wide, stringsAsFactors = FALSE)
  out$cutoff <- wide[["score"]]
  out[!is.na(out$cutoff), ]
}

.compare_measure <- function(pc, rocr_pred, pc_name, rocr_name, tag) {
  # ROCR calculates `chisq` through `stats::chisq.test()`, which warns about
  # the approximation on every sparse 2x2 table it is handed - once per
  # cutoff. The warnings are ROCR's, not this package's, and they bury the
  # output.
  perf <- suppressWarnings(
    ROCR::performance(rocr_pred, measure = rocr_name)
  )
  rocr <- data.frame(
    cutoff = perf@x.values[[1]],
    value = perf@y.values[[1]]
  )

  # The last precrec row at a cutoff is the one that counts every instance
  # scoring at or above it, which is what ROCR's single row for that cutoff
  # means. Without this every measure disagrees on tied scores.
  last <- !duplicated(pc$cutoff, fromLast = TRUE)
  merged <- merge(rocr, pc[last, c("cutoff", pc_name)], by = "cutoff")
  keep <- is.finite(merged$value) & is.finite(merged[[pc_name]])
  if (sum(keep) == 0L) {
    return(check(
      paste0(pc_name, " vs ROCR:", rocr_name, " [", tag, "]"), FALSE
    ))
  }

  check(
    paste0(
      pc_name, " vs ROCR:", rocr_name, " [", tag, "], ",
      sum(keep), " point(s)"
    ),
    isTRUE(all.equal(
      merged$value[keep], merged[[pc_name]][keep],
      tolerance = 1e-10
    ))
  )
}

.check_odds_ends <- function(scores, labels, tag) {
  pts <- evalmod(
    scores = scores, labels = labels, mode = "basic", metrics = "odds"
  )
  df <- as.data.frame(pts)
  odds <- df$y[df$type == "odds"]

  check(
    paste0("odds is NA at both ends and never infinite [", tag, "]"),
    is.na(odds[1]) && is.na(odds[length(odds)]) && !any(is.infinite(odds))
  )
}

# ROCR reports Inf where precrec reports NA, and this is the assertion that
# the difference is exactly that and nothing else.
.check_odds_convention <- function(pc, rocr_pred, tag) {
  perf <- ROCR::performance(rocr_pred, measure = "odds")
  rocr <- data.frame(
    cutoff = perf@x.values[[1]],
    value = perf@y.values[[1]]
  )
  last <- !duplicated(pc$cutoff, fromLast = TRUE)
  merged <- merge(rocr, pc[last, c("cutoff", "odds")], by = "cutoff")
  disagree <- xor(is.finite(merged$value), is.finite(merged$odds))

  check(
    paste0("odds differs from ROCR only where ROCR is not finite [", tag, "]"),
    all(!is.finite(merged$value[disagree]) & is.na(merged$odds[disagree]))
  )
}

# `pc` has had the leading no-cutoff row dropped so it can be joined to ROCR,
# so this reads the measure off the object rather than off that frame.
.check_mi_ends <- function(scores, labels, tag) {
  pts <- evalmod(
    scores = scores, labels = labels, mode = "basic", metrics = "mi"
  )
  df <- as.data.frame(pts)
  mi <- df$y[df$type == "mi"]

  check(
    paste0("mi is 0 where the prediction is constant [", tag, "]"),
    isTRUE(all.equal(mi[[1]], 0)) &&
      isTRUE(all.equal(mi[[length(mi)]], 0)) && !any(is.na(mi))
  )
}

# `cost` takes two weights, which the measure map cannot carry, so it is
# checked on its own against ROCR's own weighted call.
.check_cost_weights <- function(scores, labels, rocr_pred, tag) {
  perf <- ROCR::performance(rocr_pred,
    measure = "cost", cost.fp = 3, cost.fn = 0.5
  )
  rocr <- data.frame(
    cutoff = perf@x.values[[1]],
    value = perf@y.values[[1]]
  )

  pts <- evalmod(
    scores = scores, labels = labels, mode = "basic",
    metrics = c("cost", "score"), cost_fp = 3, cost_fn = 0.5
  )
  df <- as.data.frame(pts)
  pc <- data.frame(
    cutoff = df$y[df$type == "score"],
    cost = df$y[df$type == "cost"]
  )
  pc <- pc[!is.na(pc$cutoff), ]
  last <- !duplicated(pc$cutoff, fromLast = TRUE)
  merged <- merge(rocr, pc[last, ], by = "cutoff")

  check(
    paste0("cost with weights 3/0.5 vs ROCR [", tag, "]"),
    isTRUE(all.equal(merged$value, merged$cost, tolerance = 1e-10))
  )
}

# --- Datasets --------------------------------------------------------------

set.seed(20260901)
cases <- list(
  balanced = list(
    scores = c(runif(200), runif(200)),
    labels = rep(c(1, 0), each = 200)
  ),
  imbalanced = list(
    scores = c(runif(50), runif(950)),
    labels = rep(c(1, 0), times = c(50, 950))
  ),
  ties = list(
    scores = round(runif(400), 1),
    labels = sample(c(0, 1), 400, replace = TRUE)
  )
)

for (tag in names(cases)) {
  cat("\n", tag, "\n", sep = "")
  d <- cases[[tag]]
  pc <- .precrec_by_cutoff(d[["scores"]], d[["labels"]])
  pred <- ROCR::prediction(d[["scores"]], d[["labels"]])

  for (pc_name in names(.measure_map())) {
    .compare_measure(pc, pred, pc_name, .measure_map()[[pc_name]], tag)
  }
  .check_odds_ends(d[["scores"]], d[["labels"]], tag)
  .check_odds_convention(pc, pred, tag)
  .check_mi_ends(d[["scores"]], d[["labels"]], tag)
  .check_cost_weights(d[["scores"]], d[["labels"]], pred, tag)
}

cat("\n")
if (length(.checks$fail) > 0L) {
  cat(
    .checks$pass, "checks passed,", length(.checks$fail), "failed:\n"
  )
  cat(paste0("  ", .checks$fail, collapse = "\n"), "\n")
  quit(status = 1)
}
cat(.checks$pass, "checks passed, none failed.\n")
