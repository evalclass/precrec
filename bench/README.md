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

### Build flags

`pkgload::load_all()` compiles `src/` through `pkgbuild`, which by default
adds its development flags — `-UNDEBUG -Wall -pedantic -g -O0`. Those
*override* R's own `-O2`, so the obvious `load_all()` benchmarks an
unoptimised build in which the C++ runs roughly an order of magnitude
slower than the copy a user installs. `bench_load_precrec()` therefore sets
`options(pkg.build_extra_flags = FALSE)` and rebuilds `src/` from clean
unless `bench/.build-mode` (gitignored) says the objects on disk were
already built that way.

The stamp is written *after* the build, so any object file newer than it
was produced by something else and the next run rebuilds. That matters: a
plain `devtools::test()` or `devtools::load_all()` silently recompiles
`src/` with the development flags, and without the mtime check the
benchmarks would happily measure that build.

The practical consequences:

- The first benchmark run after an ordinary `devtools::load_all()` session
  recompiles `src/`, and vice versa. That is the stamp file doing its job,
  not a bug.
- Numbers recorded before this was fixed are not comparable with numbers
  recorded after it. `bench/baseline/develop.json` is a release-flag
  baseline; anything older has been re-recorded.

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
Rscript bench/run_memory.R                                   # peak RSS
# ... make the change ...
Rscript bench/run_correctness.R                              # after
Rscript bench/run_bench.R --compare bench/baseline/develop.json
Rscript bench/run_memory.R
```

### How much to believe

The 10% threshold is tighter than this kind of measurement supports. On the
machine the baseline was recorded on, running *identical* code twice moves
`evalmod()` by about 3% and `evalmod(mode = "basic")` by up to 11%, and
separate batches drift against each other over minutes. During the E4 work
a 5% "regression" survived three alternating rounds and a reversed run
order, and then evaporated once the change it was blamed on was isolated.

So treat `--compare` as a screen, not a verdict. Anything under roughly
1.3x on a single case wants an alternating A/B — build the variant with one
changed function, run old/new/old/new in one sitting, and compare medians.
A change worth keeping usually shows up far outside the noise:
`as.data.frame()` went from 6.2 ms to 4.5 ms in every run of every batch.

`bench/baseline/develop.json` is the committed reference, recorded on
`develop` before any E4 optimisation landed. Timings are machine-specific,
so the ratios are what carry across machines, not the absolute numbers —
record your own baseline with `--save` if you want a like-for-like
comparison on your hardware.

## Memory

```sh
Rscript bench/run_memory.R              # every case at 1e6
Rscript bench/run_memory.R --n 1e5
Rscript bench/run_memory.R --case basic
```

`bench::mark()` reports `mem_alloc`, which counts R-level allocation only.
The C++ layer holds its intermediates in `std::vector`, on the C++ heap,
where that counter cannot see them — during the E4 work the `mem_x` column
sat at exactly 1.000 through a change that cut a third off the peak. This
script reports peak RSS instead, one case per process, because a
high-water mark never comes back down within a process.

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
| `run_memory.R` | peak-RSS entry point |
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
