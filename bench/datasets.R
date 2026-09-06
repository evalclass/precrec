# Seeded datasets for the precrec benchmarks.
#
# Every dataset is rebuilt from a fixed seed, so a run on one machine is
# comparable with a run on another and with a stored baseline.

.bench_seed <- 20260830

# Build a single (scores, labels) pair.
#
# prevalence  share of positive labels (0.5 balanced, 0.01 imbalanced)
# ties        share of scores rounded onto a coarse grid, creating ties
# nas         share of scores replaced by NA
.bench_make_data <- function(n, prevalence = 0.5, ties = 0, nas = 0,
                             seed = .bench_seed) {
  set.seed(seed)

  labels <- stats::rbinom(n, 1L, prevalence)
  # A degenerate draw would make the pipeline error out rather than run
  if (all(labels == 0L)) labels[1L] <- 1L
  if (all(labels == 1L)) labels[1L] <- 0L

  scores <- stats::rnorm(n, mean = labels * 0.8)

  if (ties > 0) {
    idx <- seq_len(round(n * ties))
    scores[idx] <- round(scores[idx], 1)
  }
  if (nas > 0) {
    scores[sample.int(n, round(n * nas))] <- NA_real_
  }

  list(scores = scores, labels = labels)
}

# Build k datasets of n/k rows each, for the averaging benchmarks.
.bench_make_multi <- function(n, k = 5, prevalence = 0.5,
                              seed = .bench_seed) {
  per <- max(2L, as.integer(n / k))
  scores <- vector("list", k)
  labels <- vector("list", k)
  for (i in seq_len(k)) {
    d <- .bench_make_data(per, prevalence = prevalence, seed = seed + i)
    scores[[i]] <- d[["scores"]]
    labels[[i]] <- d[["labels"]]
  }
  list(scores = scores, labels = labels, k = k)
}

# Format 1e5 as "1e5" for dataset names
.bench_size_tag <- function(n) {
  sprintf("1e%d", as.integer(round(log10(n))))
}

# The dataset catalogue.
#
# sizes     the size sweep, run with the balanced shape
# shape_n   the size at which the shape sweep (imbalance, ties, NAs) runs
bench_datasets <- function(sizes = c(1e4, 1e5, 1e6), shape_n = 1e5) {
  out <- list()

  for (n in sizes) {
    tag <- .bench_size_tag(n)
    out[[paste0("balanced_", tag)]] <- list(
      n = n, args = list(n = n)
    )
  }

  tag <- .bench_size_tag(shape_n)
  shapes <- list(
    imbalanced = list(n = shape_n, prevalence = 0.01),
    ties = list(n = shape_n, ties = 0.5),
    nas = list(n = shape_n, nas = 0.05),
    mixed = list(n = shape_n, prevalence = 0.01, ties = 0.5, nas = 0.05)
  )
  for (nm in names(shapes)) {
    out[[paste0(nm, "_", tag)]] <- list(n = shape_n, args = shapes[[nm]])
  }

  out
}

# Materialise one catalogue entry
bench_load_dataset <- function(entry) {
  do.call(.bench_make_data, entry[["args"]])
}
