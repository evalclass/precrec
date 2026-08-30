# Benchmarks

Development scripts for the E4 optimisation work in
`.claude/plans/enhancements-2026.md`. This directory is `.Rbuildignore`d and
is not part of the package — nothing here is a dependency of `precrec`
itself.

It needs `bench`, `jsonlite` and `pkgload`, none of which are package
dependencies:

```r
install.packages(c("bench", "jsonlite", "pkgload"))
```

Run everything from the package root. The scripts load the package with
`pkgload::load_all()`, so they measure the working tree rather than an
installed copy.

## Timing

```sh
Rscript bench/run_bench.R                                    # run and print
Rscript bench/run_bench.R --compare bench/baseline/develop.json
Rscript bench/run_bench.R --save bench/baseline/mine.json
Rscript bench/run_bench.R --sizes 1e4,1e5                    # pick sizes
Rscript bench/run_bench.R --big                              # adds 1e7
Rscript bench/run_bench.R --quick                            # smoke test
```

`--quick` cuts the iteration count and the shape sweep down to 1e4. Use it
to check the scripts still run, not to compare against a baseline — three
iterations do not settle enough for the 10% threshold to mean anything.

`--big` adds the 1e7 sweep. The `rocprc` and `basic` cases build curve
objects with a row per observation, so 1e7 needs several GB; it is opt-in
for that reason.

## The workflow for an optimisation

```sh
Rscript bench/run_correctness.R                              # before
Rscript bench/run_bench.R --compare bench/baseline/develop.json
# ... make the change ...
Rscript bench/run_correctness.R                              # after
Rscript bench/run_bench.R --compare bench/baseline/develop.json
```

`bench/baseline/develop.json` is the committed reference, recorded on
`develop` before any E4 optimisation landed. Timings are machine-specific,
so the ratios are what carry across machines, not the absolute numbers —
record your own baseline with `--save` if you want a like-for-like
comparison on your hardware.

## Correctness

```sh
Rscript bench/run_correctness.R
```

The E4 optimisations rewrite how the C++ code allocates and fills its
results, which is exactly the kind of change that stays silent when it goes
wrong. The harness pins three properties and exits non-zero if any fails:

1. **C++ against the pure-R fallback.** `.dataframe_common` keeps an R
   implementation behind `use_rcpp = FALSE`; the two must agree for
   `as.data.frame`, `fortify` and the averaged data frames.
2. **ALTREP inputs.** Compact sequences (`seq_len(n)`) and deferred
   coercions do not hold a data buffer until something asks for one, so
   they are what trips up C++ that reaches straight for a pointer.
3. **Ranking invariants.** Adding a constant to every score must leave the
   curves unchanged, and NAs must be ranked as `na_worst` asks. This is the
   property the `DBL_MIN` sentinel bug broke — reintroducing that bug fails
   6 of these checks.

## Layout

| File | What it holds |
| --- | --- |
| `datasets.R` | seeded generators and the dataset catalogue |
| `cases.R` | the benchmark cases, each labelled with the C++ entry point it exercises |
| `harness.R` | timing, JSON baselines, baseline comparison |
| `run_bench.R` | timing entry point |
| `run_correctness.R` | correctness entry point |
| `baseline/` | committed baseline JSON |

## Datasets

A size sweep (`1e4`, `1e5`, `1e6`, and `1e7` under `--big`) with balanced
classes, plus a shape sweep at `1e5`:

| Name | Shape |
| --- | --- |
| `balanced_*` | 50% positive, no ties, no NAs |
| `imbalanced_1e5` | 1% positive |
| `ties_1e5` | half the scores rounded onto a coarse grid |
| `nas_1e5` | 5% NA scores |
| `mixed_1e5` | all three at once |
| `multi5_*` | five datasets per model, for the averaging paths |

Everything is drawn from a fixed seed, so two runs on the same machine see
the same numbers.
