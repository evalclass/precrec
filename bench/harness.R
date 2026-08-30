# Timing harness and baseline storage for the precrec benchmarks.

.bench_require <- function(pkgs = c("bench", "jsonlite")) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "The benchmarks need the '", pkg, "' package. Install it with ",
        "install.packages(\"", pkg, "\")",
        call. = FALSE
      )
    }
  }
}

# Load the package from the working tree, so the benchmarks measure the
# source as it stands rather than an installed copy.
bench_load_precrec <- function() {
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(".", quiet = TRUE, export_all = TRUE)
  } else {
    library("precrec")
  }
  invisible(NULL)
}

# Time one zero-argument function and return a single-row data frame.
# A function rather than an expression keeps the call site free of
# non-standard evaluation; one closure call is noise next to the work.
bench_measure <- function(benchmark, dataset, n, fn,
                          min_time = 0.5, max_iterations = 20) {
  # One untimed call, so that first-use costs (loading data.table, warming
  # the allocator) do not land on the first measured iteration
  invisible(fn())

  res <- bench::mark(
    fn(),
    min_time = min_time,
    max_iterations = max_iterations,
    check = FALSE,
    memory = TRUE,
    filter_gc = FALSE
  )

  data.frame(
    benchmark = benchmark,
    dataset = dataset,
    n = as.numeric(n),
    median_s = as.numeric(res[["median"]]),
    min_s = as.numeric(res[["min"]]),
    mem_alloc = as.numeric(res[["mem_alloc"]]),
    n_itr = as.integer(res[["n_itr"]]),
    n_gc = as.integer(res[["n_gc"]]),
    stringsAsFactors = FALSE
  )
}

.bench_git_commit <- function() {
  out <- suppressWarnings(
    tryCatch(system2("git", c("rev-parse", "--short", "HEAD"),
      stdout = TRUE, stderr = FALSE
    ), error = function(e) NA_character_)
  )
  if (length(out) == 0L) NA_character_ else out[[1L]]
}

bench_metadata <- function() {
  list(
    created = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    git_commit = .bench_git_commit(),
    r_version = R.version.string,
    platform = R.version$platform,
    precrec_version = as.character(utils::packageVersion("precrec")),
    bench_version = as.character(utils::packageVersion("bench"))
  )
}

bench_write <- function(results, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(
    list(meta = bench_metadata(), results = results),
    path,
    auto_unbox = TRUE, pretty = TRUE, digits = 8
  )
  invisible(path)
}

bench_read <- function(path) {
  if (!file.exists(path)) {
    stop("No baseline at '", path, "'. Record one with --save first.",
      call. = FALSE
    )
  }
  jsonlite::read_json(path, simplifyVector = TRUE)
}

bench_print <- function(results) {
  out <- results
  out[["median_ms"]] <- round(out[["median_s"]] * 1000, 3)
  out[["mem_MB"]] <- round(out[["mem_alloc"]] / 1024^2, 2)
  print(out[, c("benchmark", "dataset", "median_ms", "mem_MB", "n_itr")],
    row.names = FALSE
  )
  invisible(results)
}

# Compare a fresh run against a stored baseline.
#
# tolerance is the fraction a median may grow before it is called a
# regression (0.10 = 10% slower). Cases whose baseline median is under
# noise_floor_ms are timed but never flagged - at that scale the run-to-run
# spread swamps any real change.
bench_compare <- function(results, baseline_path, tolerance = 0.10,
                          noise_floor_ms = 1) {
  base <- bench_read(baseline_path)
  old <- base[["results"]]

  key <- function(x) paste(x[["benchmark"]], x[["dataset"]], sep = "/")
  idx <- match(key(results), key(old))

  cmp <- data.frame(
    benchmark = results[["benchmark"]],
    dataset = results[["dataset"]],
    base_ms = round(old[["median_s"]][idx] * 1000, 3),
    new_ms = round(results[["median_s"]] * 1000, 3),
    base_MB = round(old[["mem_alloc"]][idx] / 1024^2, 2),
    new_MB = round(results[["mem_alloc"]] / 1024^2, 2),
    stringsAsFactors = FALSE
  )
  cmp[["time_x"]] <- round(cmp[["new_ms"]] / cmp[["base_ms"]], 3)
  mem_x <- round(cmp[["new_MB"]] / cmp[["base_MB"]], 3)
  # A baseline of 0 MB carries no ratio worth printing
  mem_x[!is.finite(mem_x)] <- NA_real_
  cmp[["mem_x"]] <- mem_x

  cat("\nBaseline: ", baseline_path, "\n", sep = "")
  cat("  recorded ", base[["meta"]][["created"]],
    " at ", base[["meta"]][["git_commit"]], "\n\n",
    sep = ""
  )
  print(cmp[, c(
    "benchmark", "dataset", "base_ms", "new_ms", "time_x", "mem_x"
  )], row.names = FALSE)

  missing <- is.na(idx)
  if (any(missing)) {
    cat("\n", sum(missing), " case(s) not in the baseline:\n", sep = "")
    cat(paste0("  ", key(results)[missing], collapse = "\n"), "\n", sep = "")
  }

  timed <- !missing & cmp[["base_ms"]] >= noise_floor_ms
  if (any(!missing & !timed)) {
    cat("\n", sum(!missing & !timed), " case(s) under the ",
      noise_floor_ms, " ms noise floor, not compared.\n",
      sep = ""
    )
  }

  slower <- timed & cmp[["time_x"]] > 1 + tolerance
  faster <- timed & cmp[["time_x"]] < 1 - tolerance
  if (any(faster)) {
    cat("\nFaster by more than ", tolerance * 100, "%:\n", sep = "")
    print(cmp[faster, c("benchmark", "dataset", "time_x")], row.names = FALSE)
  }
  if (any(slower)) {
    cat("\nREGRESSION - slower by more than ", tolerance * 100, "%:\n",
      sep = ""
    )
    print(cmp[slower, c("benchmark", "dataset", "time_x")], row.names = FALSE)
  } else {
    cat("\nNo timing regression beyond ", tolerance * 100, "%.\n", sep = "")
  }

  invisible(cmp)
}
