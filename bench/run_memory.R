#!/usr/bin/env Rscript
#
# Peak resident memory of one pass through the pipeline.
#
#   Rscript bench/run_memory.R                  # every case at 1e6
#   Rscript bench/run_memory.R --n 1e5
#   Rscript bench/run_memory.R --case basic
#
# `bench::mark()` reports `mem_alloc`, which counts R-level allocation only.
# The C++ layer holds its intermediates in std::vector, on the C++ heap,
# where that counter cannot see them - so the copy-then-wrap pattern the E4
# work removes is invisible to `run_bench.R`. Peak RSS is what moved.
#
# One case per process, because peak RSS is a high-water mark and never
# comes back down within a process. Run it from the package root.

.bench_dir <- "bench"
source(file.path(.bench_dir, "harness.R"))
source(file.path(.bench_dir, "datasets.R"))

# The cases, each a function of an mmdata object
.mem_cases <- list(
  rocprc = function(mdat) as.data.frame(evalmod(mdat)),
  basic = function(mdat) as.data.frame(evalmod(mdat, mode = "basic")),
  aucroc = function(mdat) evalmod(mdat, mode = "aucroc")
)

# Peak resident set size of this process, in MB
#
# VmHWM is Linux-only; elsewhere fall back to what R itself accounts for,
# which at least moves in the right direction.
.peak_rss_mb <- function() {
  status <- "/proc/self/status"
  if (file.exists(status)) {
    line <- grep("^VmHWM:", readLines(status), value = TRUE)
    if (length(line) == 1L) {
      return(as.numeric(sub("^VmHWM:\\s*(\\d+).*$", "\\1", line)) / 1024)
    }
  }
  sum(gc()[, "max used"] * c(56, 8)) / 1024^2
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  i <- match("--n", args)
  n <- if (is.na(i)) 1e6 else as.numeric(args[[i + 1L]])
  i <- match("--case", args)
  cases <- if (is.na(i)) names(.mem_cases) else args[[i + 1L]]

  unknown <- setdiff(cases, names(.mem_cases))
  if (length(unknown) > 0L) {
    stop("unknown case: ", paste(unknown, collapse = ", "), call. = FALSE)
  }

  if (length(cases) > 1L) {
    # Peak RSS only ever goes up, so each case needs its own process
    self <- file.path(.bench_dir, "run_memory.R")
    for (case in cases) {
      system2("Rscript", c(self, "--n", n, "--case", case))
    }
    return(invisible(NULL))
  }

  bench_load_precrec()
  d <- .bench_make_data(n, prevalence = 0.5)
  mdat <- mmdata(d[["scores"]], d[["labels"]])
  gc(full = TRUE)

  before <- .peak_rss_mb()
  invisible(.mem_cases[[cases]](mdat))
  cat(sprintf(
    "%-8s n = %-8s peak %7.0f MB  (%.0f MB before the run)\n",
    cases, format(n, scientific = TRUE), .peak_rss_mb(), before
  ))
  invisible(NULL)
}

if (identical(environment(), globalenv())) {
  main()
}
