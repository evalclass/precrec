#!/usr/bin/env Rscript
#
# Run the precrec benchmark suite.
#
#   Rscript bench/run_bench.R                          # run and print
#   Rscript bench/run_bench.R --save bench/baseline/develop.json
#   Rscript bench/run_bench.R --compare bench/baseline/develop.json
#   Rscript bench/run_bench.R --sizes 1e4,1e5 --quick
#   Rscript bench/run_bench.R --big                    # add the 1e7 sweep
#
# Run it from the package root.

.bench_dir <- "bench"
source(file.path(.bench_dir, "harness.R"))
source(file.path(.bench_dir, "datasets.R"))
source(file.path(.bench_dir, "cases.R"))

.bench_opt <- function(args, flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i)) {
    return(default)
  }
  if (i == length(args)) {
    stop("'", flag, "' needs a value", call. = FALSE)
  }
  args[[i + 1L]]
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  .bench_require()

  quick <- "--quick" %in% args
  big <- "--big" %in% args
  save_to <- .bench_opt(args, "--save")
  compare_to <- .bench_opt(args, "--compare")

  sizes_arg <- .bench_opt(args, "--sizes")
  if (!is.null(sizes_arg)) {
    sizes <- as.numeric(strsplit(sizes_arg, ",", fixed = TRUE)[[1L]])
  } else if (big) {
    sizes <- c(1e4, 1e5, 1e6, 1e7)
  } else if (quick) {
    sizes <- c(1e4)
  } else {
    sizes <- c(1e4, 1e5, 1e6)
  }

  shape_n <- if (quick) 1e4 else 1e5
  min_time <- if (quick) 0.05 else 0.5
  max_iterations <- if (quick) 3L else 20L

  bench_load_precrec()

  catalogue <- bench_datasets(sizes = sizes, shape_n = shape_n)
  results <- list()

  for (nm in names(catalogue)) {
    entry <- catalogue[[nm]]
    message("dataset ", nm, " (n = ", format(entry[["n"]], scientific = TRUE),
      ")",
      appendLF = TRUE
    )
    d <- bench_load_dataset(entry)

    for (case_nm in names(cases <- bench_cases(d))) {
      message("  ", case_nm)
      results[[length(results) + 1L]] <- bench_measure(
        case_nm, nm, entry[["n"]], cases[[case_nm]],
        min_time = min_time, max_iterations = max_iterations
      )
    }
    rm(d, cases)
    invisible(gc(FALSE))
  }

  # The averaging paths need several datasets per model
  for (n in sizes) {
    nm <- paste0("multi5_", .bench_size_tag(n))
    message("dataset ", nm)
    m <- .bench_make_multi(n, k = 5)

    for (case_nm in names(cases <- bench_avg_cases(m))) {
      message("  ", case_nm)
      results[[length(results) + 1L]] <- bench_measure(
        case_nm, nm, n, cases[[case_nm]],
        min_time = min_time, max_iterations = max_iterations
      )
    }
    rm(m, cases)
    invisible(gc(FALSE))
  }

  results <- do.call(rbind, results)

  cat("\n")
  bench_print(results)

  if (!is.null(save_to)) {
    bench_write(results, save_to)
    cat("\nSaved baseline to ", save_to, "\n", sep = "")
  }
  if (!is.null(compare_to)) {
    bench_compare(results, compare_to)
  }

  invisible(results)
}

if (sys.nframe() == 0L || identical(environment(), globalenv())) {
  main()
}
