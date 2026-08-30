# precrec enhancement plan (2026-08)

Five enhancements, each with current state, design decisions, work plan, and
risks. Suggested sequencing at the end. Effort: S ≈ days, M ≈ 1–2 weeks,
L ≈ 3–6 weeks, XL ≈ 2+ months.

---

## E1. Store all data-frame data as `data.table` — **M** — **DONE (2026-08-30)**

### Outcome

Implemented on `feature/DataTable`. The internal tables are built with
`data.table`; the public contract is unchanged (`as.data.frame`, `fortify`,
`auc`, `pauc`, `auc_ci` all still hand back plain data frames), and
`as.data.table.<class>` is the new opt-in.

**Where the win actually was.** The default `Rcpp` path never had the
quadratic problem, so the standard benchmarks move by less than 1% at 1e6.
The whole payoff is in the R fallback: over 20 test datasets
(118k rows), `.dataframe_curve` went from **706 ms / 698 MB to 18 ms /
16 MB** — 38x faster, 43x less memory. That path runs whenever
`use_rcpp = FALSE`.

**Two deviations from the plan below, both deliberate:**

1. `fortify.*` returns a plain data frame, not a data.table. The plan
   floated returning the data.table directly; `fortify` is documented
   public API here, `setDF()` on a freshly built table is free, and the
   only gain would have been skipping that free call. The stated principle
   — internal data.table, external contract unchanged — is better served
   this way. Verified either way: the January vdiffr snapshots pass
   untouched.
2. `fortify.fmdat` / `.cmats` / `.pevals` still build plain data frames.
   They are built once and returned, with no loop to fix and nothing to
   convert back from.

**What the reference semantics actually cost.** Accessors that hand back a
stored table (`auc`, `pauc`, the `aucroc` frame) now copy it, because
`setDF()` converts in place and would otherwise leave the caller holding a
handle into the object. On these tables that is tens of microseconds
(`auc()` went from 21 to 66 us, both far below anything a caller notices),
and it buys back the guarantee that a returned frame can never mutate the
object it came from — which is what a data.frame gave for free before.

The `[.data.table` NSE collision the plan warned about did bite once, in
`auc_ci()`: `aucs[aucs$modnames == modname, ]` resolves `aucs` to the
column of that name inside `[.data.table`, not the table. Fixed by taking
a plain data frame view at the top of the function.

### Current state

`data.table` is already in `Imports` (only `frank()` is used, in
`pl3_calc_auc_with_u.R`). Data frames are built in ~12 places:

- `data_info` — `mm1_create_mmdat.R:224`
- AUC tables — `.gather_aucs` (`pl2_pipeline_main_rocprc.R:151`),
  `.summarize_uauc_results` (`pl2_pipeline_main_aucroc.R:110`),
  `.summarize_basic` (`pl2_pipeline_main_basic.R:155`)
- pAUC tables — `g_part.R:318,351`
- CI tables — `g_auc_ci.R:137,164` (**`rbind` inside a loop**)
- `as.data.frame`/`fortify` outputs — fast path builds `Rcpp::DataFrame` in
  C++ (`convert_curve_df`/`convert_curve_avg_df` in `precrec_misc.cpp`);
  R fallback `.dataframe_curve*` (`etc_utils_dataframe.R:371,405`) does
  **`rbind` in a nested loop**
- `fortify.fmdat/cmats/pevals` — `g_fortify.R`
- `print.*` methods call `print.data.frame` — `g_print.R`

### Key decision: what does the public API return?

`as.data.frame()` is a base generic with a strong contract. A `data.table`
*is* a `data.frame`, but printing, `[`-semantics, and copy-on-modify
behaviour differ, and reverse dependencies / user scripts may rely on base
semantics.

**Recommendation:** internal representation `data.table`, external contract
unchanged by default:

1. All internal tables built with `data.table()` / `rbindlist()` /
   `setDT()`.
2. `as.data.frame.*` calls `data.table::setDF()` at the boundary — zero-copy,
   returns a plain `data.frame` (existing tests keep passing).
3. Add `as.data.table.<class>` S3 methods (registering against the
   `data.table::as.data.table` generic) for users who want data.tables —
   zero-copy the other way.
4. `fortify.*` may return data.table directly (ggplot2 handles it), but
   verify vdiffr snapshots don't shift.

### Work plan

1. Replace the two `rbind`-in-loop sites (`etc_utils_dataframe.R`,
   `g_auc_ci.R`) with build-list-then-`rbindlist()`. This alone removes the
   worst quadratic behaviour and the R fallback's performance penalty.
2. Convert table constructors stage by stage (mm1 → pl2 → g_) to
   `data.table`; use `setattr()` where attribute copies matter.
3. C++ boundary: `Rcpp::DataFrame` results get `setDT()` applied in the
   calling R wrapper (cheap, by reference) — no C++ changes needed.
4. Add `as.data.table` methods + docs + `_pkgdown.yml` reference entry.
5. `print.*`: keep `print.data.frame(...)` calls explicit so output format
   (regression-tested in `test_g_print.R`) does not change.
6. Tests: add class assertions (`inherits(x, "data.frame")` and not
   `data.table` for `as.data.frame` output); run full suite + vdiffr locally.

### Risks

- Silent behaviour changes from data.table's reference semantics — audit any
  site that mutates a table after creation (e.g. `data_info[["nn"]][i] <- ...`
  in `mmdata`).
- `.validate.*` methods check item/attribute names; class changes on internal
  objects must be reflected there.

---

## E2. Multi-label (multiclass) support, including single-label — **XL** — **DONE (2026-08-31)**

### Outcome

Implemented on `feature/Multiclass`. One-vs-rest, classes on the model axis,
exactly as decided below. Binary behaviour is unchanged: the whole existing
suite passes untouched, and `mmdata()` reads binary input by the same path it
always did.

**The decomposition.** `R/mm5_expand_multiclass.R` turns an n x K score
matrix and K-level labels into K binary (scores, labels) pairs before
`reformat_data()` ever runs, so no C++ changed. Score columns are matched to
classes by name when the column names name the classes, and by position
otherwise. Class names come from a factor's levels when the labels are a
factor, which keeps a class that is missing from one fold of a cross
validation from disappearing along with it. Several score matrices are read
as several datasets rather than several models, because the model axis is
about to be spent on the classes; supply `modnames` to get `class:model`
names instead.

`multiclass` is detected from the input when unset: `"ovr"` when the labels
hold more than two classes and the scores hold one column per class. That
cannot change how any existing call is read, because labels with more than
two classes were an outright error before.

**Two things had to give.** `.validate.mdat()` checked that every dataset
sharing a `dsid` had the same `np`/`nn`; one-vs-rest decompositions differ in
class balance by construction, so the check is skipped for them. And the
precision-recall baseline was drawn unconditionally even though
`.get_pn_info()` already worked out whether the datasets share a prevalence -
harmless while prevalence was always shared, wrong the moment it is not. Both
plot backends now draw it only when `is_consistant`. That is the pitfall this
package's own paper is about, so a line that fits none of the classes is
worse than no line.

**Macro-average.** `auc()` gained a `macro` argument, default `TRUE`, which
appends the unweighted mean of the per-class AUCs per dataset and curve type
under the model name `macro-average` (or `macro-average:<model>`). It is
ignored for binary objects, so `auc()` on existing input is untouched.
Classes that could not be evaluated carry NA and are left out of the mean.

**Single-class datasets.** `mode = "basic"` warns and calculates rather than
stopping: accuracy and error rate are defined, and specificity without
negatives and sensitivity without positives come back as NA, which
`calc_basic_measures()` already produced. The curve pipelines still stop by
default, and `on_single_class = "na"` asks them to warn and hand back a
placeholder instead - `.create_na_curves()` and `.create_na_uauc()`. A
degenerate fold then keeps its row in `auc()` with NA in it rather than
aborting the run, and `.calc_ci_stats()` drops those NAs and reports how many
datasets the interval was actually built from.

Worth recording: feeding the existing C++ curve builders a specificity column
of NAs **segfaults**. That is unreachable from the package - the pipeline
guard has always stood in front of it - which is why the placeholder is built
in R rather than by relaxing the guard and letting `create_curves()` run.

### Not done

- **Micro-average** (decision c called it optional). Pooling the K
  decompositions into one concatenated dataset would count every observation
  K times, which is a different measure from what the macro rows report and
  wants its own explanation. Left out rather than shipped half-explained.
- **`auc_ci()` macro rows.** Per-class intervals come free, because the
  classes are models. An interval around a macro-average is not the average
  of the per-class intervals, so it needs its own estimator rather than a
  row.
- **Multiclass with `nfold_df`.** Rejected with an error that says to pass a
  list of score matrices instead. `format_nfold()` is built around one score
  column per model, and widening it is its own change.
- **Faceting by class.** Classes are models, so the existing multi-model
  rendering colours them already; a class-aware legend title would be
  cosmetic.

### Cost

No measurable change on any benchmark. The rocprc and basic paths gained one
`attr()` test each and a branch before the `mmdata()` loop. The 1e5 shapes
straddle 1.0 across runs, which is the noise floor phase 6 documented, not a
change: `evalmod_rocprc` on `nas_1e5` read 1.306 and then 1.098 on the same
code, and `imbalanced_1e5` and `ties_1e5` read 0.89 in the same run.

---

## E2. Multi-label (multiclass) support, including single-label — **XL**

### Current state

Strictly binary, enforced in C++: `make_new_labels()`
(`precrec_mmx.cpp`) maps labels to {1 = negative, 2 = positive} and returns
`"invalid-labels"` on a third distinct value. A **single**-class input is
already accepted by `mmdata()`/`reformat_data()` (see
`test_mm3_4_reformat_data_singleclass.R`), but every pipeline aborts on it:
`.pl_main_rocprc`/`_basic`/`_aucroc` `stop()` when `np == 0 || nn == 0`.

The S3 class system encodes exactly two axes (models × datasets) in
two-letter prefixes (`ss`/`ms`/`sm`/`mm`); a class axis does not exist.

### Key decisions

**a) Decomposition strategy: one-vs-rest (OvR).** Each of K classes becomes a
binary problem (class = positive, rest = negative). This reuses the entire
existing binary pipeline — including the accurate PRC interpolation — and is
the standard approach (scikit-learn, pROC's `multiclass.roc` uses pairwise;
OvR is more natural for PRC). One-vs-one can come later if ever.

**b) Representation: map classes onto the existing model axis** rather than
adding a third axis. A third axis would multiply the S3 surface (8 methods ×
4 generics × new prefixes) and is not worth it for v1. Instead:

- Input: `scores` = n×K matrix (one column per class, colnames = class
  names), `labels` = one vector/factor with K levels.
- `mmdata()` gains `multiclass = c("none", "ovr")` (default `"none"`,
  auto-detected when `scores` is a matrix whose colnames match label
  levels and K > 2).
- Decomposition happens in a new `mm5_expand_multiclass.R` **before**
  `reformat_data()`: K binary (scores, labels) pairs with
  modnames = class names (or `class:model` when real multiple models are
  also present). No C++ changes required for the core.
- Result objects get new attributes `classnames` and `multiclass = "ovr"` so
  plots can label/facet by class and `auc()` can report per-class +
  macro-averaged AUC.

**c) Macro/micro summaries.** Add `auc()` output rows for macro-average
(mean of per-class AUCs) and optionally micro-average (pool all
decompositions — implementable as one extra concatenated dataset).

**d) Single-label case.** Make degenerate inputs evaluable instead of fatal:

- `mode = "basic"`: accuracy/error are defined; specificity or sensitivity
  already produce `NA` when `nn`/`np` = 0 (`calc_basic_measures` handles it).
  Change `.pl_main_basic` to warn instead of `stop()`.
- `mode = "rocprc"` / `"aucroc"`: curves/AUC are mathematically undefined —
  keep the error by default, add `on_single_class = c("error", "na")` so
  n-fold runs where one fold is degenerate return `NA` rows rather than
  aborting the whole evaluation.

### Work plan

1. Relax validators (`.validate_labels`, `mmdata` arg checks) to accept K>2
   factors when `multiclass != "none"`; keep the strict binary error message
   otherwise (it is one of the package's selling points).
2. Implement `mm5` decomposition + `classnames` plumbing; `posclass` is
   ignored (with a warning) in multiclass mode.
3. `data_info` gains a `class` column (prints via `print.mdat`).
4. Single-class handling per (d): touch the three `.pl_main_*` guards +
   `.summarize_*` to emit NA rows.
5. `auc()`/`auc_ci()`: per-class rows + macro row; `autoplot`/`plot`: color
   or facet by class (reuses existing multi-model rendering).
6. Tests: new `test_mm5_*`, multiclass use-case file, iris-style example
   dataset in `data-raw/`; vignette section "5. Multiclass evaluation".
7. Docs: `evalmod`/`mmdata` roxygen, README feature list, pkgdown reference.

### Risks

- Biggest user-facing change in the package's history; keep binary behaviour
  byte-identical (full snapshot suite must pass untouched).
- Averaging across datasets × classes interacts with `calc_avg` — v1 should
  average within class across datasets only, and document that.
- Prevalence differs per OvR decomposition, so PRC baselines differ per
  class — document prominently (this package's own paper is about exactly
  this pitfall).

---

## E3. Modernize per r-pkgs.org — **M**

### Current state / gaps found

- `DESCRIPTION`: `Depends: R (>= 3.2.1)` (2015-era), `Date:` field
  (usethis drops it), no `Config/testthat/edition`, non-markdown roxygen.
- `assertthat` used ~100× (mostly `etc_utils_validate_args.R`) —
  soft-deprecated ecosystem-wide; tidyverse guidance is rlang/cli.
- Errors are `stop(paste0(...), call. = FALSE)` — no condition classes.
- `tests/testthat.R` has a stray `#' @importFrom precrec` line; testthat
  edition 1/2 semantics.
- CI workflows trigger on `[main, _develop]` — **intentional**: the
  underscore disables CI on `develop` because running the full matrix on
  every develop push is too heavy. Leave the filters alone; only
  `actions/checkout@v3` and action versions need updating. (If lighter
  develop feedback is ever wanted: a single ubuntu-release job on
  `develop`, or `workflow_dispatch` for on-demand runs.)
- `_pkgdown.yml` uses the deprecated `templates: params: bootswatch:`
  syntax (pre-Bootstrap-5).
- `@docType package` in `R/precrec.R` alongside `"_PACKAGE"` — roxygen2 ≥ 7.3
  warns on this.
- No `inst/WORDLIST` for `devtools::spell_check()`; no lintr/styler config
  committed despite CodeFactor badge.

### Work plan (each step is an independent PR)

1. **CI refresh**: bump action versions (`checkout@v4`, r-lib/actions
   current), re-generate workflows via `usethis::use_github_action()`.
   Keep the `_develop` filters — CI on `develop` is disabled by design.
2. **DESCRIPTION**: drop `Date:`; bump to `R (>= 4.1)` (matches oldrel-1
   testing matrix; announce in NEWS); add `Config/testthat/edition: 3`,
   `Config/testthat/parallel: true`, `Roxygen: list(markdown = TRUE)`;
   `usethis::use_tidy_description()`.
3. **testthat 3e**: fix deprecated expectations, remove any `context()`,
   clean `testthat.R`. The `check_ggplot_fig()` helper keeps working; note
   `testthat:::on_ci()` is a `:::` call into unexported API — replace with
   `Sys.getenv("CI") != ""` while there.
4. **roxygen markdown**: `roxygen2md::roxygen2md()`, fix `@docType`, add
   `@returns` to all exported functions (CRAN now expects it).
5. **assertthat → rlang/cli**: mechanical but wide. Introduce
   `cli::cli_abort(class = "precrec_error_*")` in the validators; keep
   *message text identical* where tests match on it, or update tests to
   `expect_error(class = ...)` (better). Drop assertthat from Imports.
6. **pkgdown**: `template: bootstrap: 5`, refresh `_pkgdown.yml`.
7. **Hygiene**: `inst/WORDLIST`, `.lintr`, styler pre-commit or a
   `usethis::use_tidy_style()` pass, `Rcpp` skeleton comments refresh.

### Risks

- Step 5 touches every error path — do it after the test suite is on
  edition 3 and green, and lean on `expect_error(class=)` so wording can
  evolve.
- cli adds a dependency; acceptable (it is ggplot2's dependency already, so
  the effective footprint is zero).

---

## E4. Optimize the Rcpp layer — **M** — **DONE (2026-08-30)**

### Correctness findings to fix before optimizing

1. **`DBL_MIN` NA sentinel bug** — **FIXED (2026-08-30).** `na_worst` maps
   NA to `DBL_MIN`, the smallest *positive* double rather than the most
   negative, so NAs outrank every negative score. Reproduced standalone:
   scores `c(-1,-2,NA,-3,-4)` / labels `c(2,2,2,1,1)` returns AUC **1.0**
   where the correct value is 0.667.

   **The original assessment that this was dormant was wrong.** The same
   sentinel appears twice, and only one of the two sites is unreachable:

   - `calc_uauc` (`precrec_plx.cpp`) — genuinely dormant, as analysed:
     reachable only via `ustat_method = "sort"`, which `.pl_main_aucroc`
     cannot select, and the default `calc_uauc_frank` path is correct.
   - `make_index_pairs` (`precrec_misc.cpp`) — **on the main pipeline
     path.** `get_score_ranks` → `.rank_scores` → `reformat_data`, i.e.
     every `evalmod()` call. Verified end to end: `evalmod()` on
     `scores = c(-1,-2,NA,-3,-4)`, `labels = c(1,1,1,0,0)` returned
     ROC 1.0 / PRC 1.0 instead of 0.667 / 0.851, and disagreed with the
     same scores shifted by a constant.

   So published results *were* affected whenever scores contained both NAs
   and negative values and `na_worst = TRUE` (the default). NEWS describes
   it as a results-changing fix, not an internal one. `na_worst = FALSE`
   uses `DBL_MAX` and was always correct.

   Fixed with `std::numeric_limits<double>::lowest()` at both sites.
   Regression tests: negative-scores + NA cases in
   `test_mm3_1_reformat_data_scores.R` (rank level, both `na_worst`
   values), `test_pl3_2_calc_auc_with_u.R` (both `ustat_method`s, closing
   the sort-path coverage gap) and `test_main_evalmod.R` (end to end).
2. **Averaging variance** — **FIXED (2026-08-30).** `calc_avg_curve` *and*
   `calc_avg_points` (`precrec_plx.cpp`) both used the `E[x²]−E[x]²` formula
   with negatives clamped to zero — catastrophic cancellation territory.
   Both now use Welford's algorithm. The full suite passes unchanged, so CI
   bands move at most ~1e-8; regression tests in `test_pl6_1_*` /
   `test_pl6_2_*` pin large-magnitude y values (offset 1e10) where the old
   formula returned an SE of 64 instead of 0.707.
3. Audit `get_yval_single` — **DONE (2026-08-30).** `std::set<double> x_set`
   was declared and never used; removed.

### Optimization plan (benchmark-driven, in order of expected payoff)

0. **Infrastructure first** — **DONE (2026-08-30).** `bench/`
   (Rbuildignored, `bench/README.md` documents it): seeded datasets over a
   1e4–1e7 size sweep plus a shape sweep (imbalanced, ties, NAs, all three),
   `bench::mark()` cases labelled with the C++ entry point each exercises,
   JSON baselines with a `--compare` mode that flags regressions past 10%
   above a 1 ms noise floor, and `run_correctness.R` covering the
   `use_rcpp = FALSE` fallback, ALTREP inputs, and the ranking invariants.
   The correctness harness was checked against a deliberately reintroduced
   `DBL_MIN` bug: it fails 6 checks, so it is not vacuous.

   **The harness was measuring a debug build, and the first baseline was
   wrong.** `pkgload::load_all()` compiles `src/` through `pkgbuild`, which
   adds `-UNDEBUG -Wall -pedantic -g -O0` and *overrides* R's `-O2`. Every
   number recorded before this was found describes C++ running roughly an
   order of magnitude slower than the copy a user installs — which made the
   C++ look far more dominant than it is. `bench_load_precrec()` now sets
   `options(pkg.build_extra_flags = FALSE)` and rebuilds from clean unless
   `bench/.build-mode` says the objects on disk were already built that
   way; the stamp is written after the build, so an object file newer than
   it (an ordinary `devtools::test()`, say) forces a rebuild. Without that
   mtime check a stale `-O0` build slips straight through — it did once,
   and produced a full set of plausible-looking numbers.

   `bench/baseline/develop.json` has been re-recorded with release flags.

   **What the corrected baseline says.** Slowest cases at 1e6 (median ms,
   and how the debug numbers had ranked them):

   | case | release ms | debug ms |
   | --- | --- | --- |
   | `evalmod_avg_basic` / multi5_1e6 | 1855 | 9887 |
   | `evalmod_basic` / balanced_1e6 | 411 | 631 |
   | `as_data_frame_basic` / balanced_1e6 | 336 | 912 |
   | `evalmod_avg_rocprc` / multi5_1e6 | 316 | 861 |
   | `evalmod_rocprc` / balanced_1e6 | 305 | 812 |
   | `mmdata` / balanced_1e6 | 117 | 531 |

   `calc_avg_points` is still the worst path, but by 4.5x rather than 15x,
   and `mmdata` drops from a headline case to a minor one. **Reordering the
   list to put `calc_avg_points` first still holds** — it was done.

   **`bench::mark()`'s `mem_alloc` cannot see this work.** It counts
   R-level allocation only, and the copy-then-wrap pattern item 1 removes
   holds its intermediates in `std::vector`, on the C++ heap. The
   `mem_x` column sat at exactly 1.000 through changes that cut a third of
   the peak. `bench/run_memory.R` was added to measure peak RSS instead,
   one case per process.

1. **Eliminate copy-then-wrap** — **DONE, for the converters.**
   `convert_curve_df` and `convert_curve_avg_df` now fill
   `Rcpp::NumericVector(Rcpp::no_init(n))` directly. The copy helpers in
   `precrec_misc.cpp` were templated on their destination to allow it, and
   a `trim_vec` helper handles the reduced-points path, where the real
   length is only known at the end (the common path returns the vector
   itself and copies nothing).

   Peak RSS at 1e6, `evalmod()` plus `as.data.frame()`, reproducible to the
   megabyte:

   | case | before | after |
   | --- | --- | --- |
   | `mode = "basic"` | 897 MB | 584 MB |
   | `mode = "rocprc"` | 470 MB | 424 MB |

   `as.data.frame()` is **27% faster** (6.2 ms to 4.5 ms at 2e5), in every
   run of every batch.

   **The same change to `calc_basic_measures` was measured and reverted.**
   Attributing the memory saving showed the converters account for all of
   it: 897 -> 584 MB from the converters alone, and 596 MB with
   `calc_basic_measures` converted too — *worse*. It saved 19 MB on the
   `rocprc` path and cost 12 MB on `basic`, for a wash, extra code, and a
   slower `as.data.frame`. The eight columns it builds are handed straight
   back to R, so the wrapped copy it was making is short-lived and reuses
   memory the allocator has already got.

   **`Rcpp::Vector::operator[]` is not a free replacement for
   `std::vector::operator[]`,** which is worth recording for whoever tries
   this again. The first version of `calc_basic_measures` indexed the Rcpp
   vectors directly and was **13% slower end to end** — far outside the
   noise. `Rcpp::Vector` reaches its data through a cached pointer held
   inside the object, and the compiler cannot prove that storing a double
   does not clobber that cache, so it reloads all eight base pointers after
   every store. Taking `double* p = v.begin()` once, before the loop,
   recovered it. Anything writing several Rcpp vectors in one loop needs
   that.
2. **`get_score_ranks`** — **PARTLY DONE.** `sort_indices` passed its
   comparator as a *function pointer*, which cannot be inlined into
   `std::sort`'s inner loop; it is a function object now. `mmdata` is
   **22–26% faster** across every shape and size. Both comparators order by
   score alone, so the sequence of comparisons, and therefore the resulting
   permutation, is unchanged.

   The index-vector sort the plan proposed was **not** done: the existing
   `vector<pair<unsigned, double>>` keeps the key beside the index, where an
   index sort would chase a pointer into the score array on every
   comparison. Writing `ranks` / `rank_idx` into `Rcpp::IntegerVector` was
   also skipped — 4 MB each at 1e6 against a 424 MB peak, and item 1 shows
   the change is not free.
3. **`calc_basic_measures` reciprocals** — **PARTLY DONE.** The
   loop-invariant `nn == 0` / `np == 0` branches and the int-to-double
   conversions are hoisted; no measurable effect, kept because it is free
   and clearer. The divisions stay divisions: multiplying by a precomputed
   reciprocal would move every published measure by an ulp, and the loop is
   dominated by a `sqrt` and eight stores, not by three divides.
4. **`interpolate_prc`** — **MEASURED, NOT KEPT.** Hoisting every
   loop-invariant read and the two differences out of the interpolation
   loop measured at **exactly zero** across three alternating rounds. The
   loop body usually runs no iterations at all — most adjacent point pairs
   have no bin boundary between them — so there is nothing there to hoist
   out of. Reverted rather than carried as unmeasurable complexity.
5. **Binary size**: not attempted. See the note under Risks.
6. **Parallelism**: not attempted, as planned.

### Outcome

| case | change |
| --- | --- |
| `as.data.frame()` (convert_curve_df) | **0.57–0.85x** |
| `evalmod(calc_avg = TRUE, mode = "basic")` | **0.61–0.84x** |
| `mmdata()` | **0.74–0.81x** |
| `evalmod()`, `evalmod(mode = "basic")` | no measurable change |
| peak RSS, `basic` / `rocprc` at 1e6 | **−35% / −10%** |

**The noise floor is the main lesson.** The harness flags anything past
10%, and that is far too tight here: the same case moves 3% run-to-run on
`evalmod()` and up to 11% on `evalmod(mode = "basic")` with *identical*
code, and batches drift against each other over minutes. A 5% "regression"
on `evalmod()` survived three alternating rounds, survived reversing the
order, and then evaporated when the change it was attributed to was
isolated — it tracked nothing in the executed code. Every number above was
settled by an alternating A/B with the variant built from a single changed
function, and nothing under about 1.3x on a single case should be believed
without one. Raising the default tolerance, or teaching `--compare` to
want several runs, is the obvious follow-up.

### Risks

- ~~Item 1 changes allocation patterns near the R GC — every touched
  function needs the correctness harness run before/after.~~ Done; 40/40
  before and after, and the full suite is unchanged at 0 failures.
- Item 5 (binary size) was left alone. It is a standing CRAN NOTE, not a
  regression, and the "keep unstripped" decision from 0.10.1 still stands.
- ~~The fix in (1) does not change any public-API result.~~ It does — the
  `make_index_pairs` site is on the `evalmod()` path. See item (1).

---

## E5. Additional evaluation metrics — **M** (tiered) — **DONE (2026-08-31)**

### Outcome

Implemented on `feature/Metrics`. Tiers 1 and 2 shipped; tier 3 stays
deferred as planned.

**Tier 1.** Balanced accuracy, NPV, informedness (Youden's J), markedness and
Cohen's kappa are calculated in the same C++ loop as the existing measures,
and `fscore` became F-beta through a new `beta` argument on `evalmod()`
(default 1, so existing results are byte-identical). NPV needed the mirror of
the trick precision already used: nothing is predicted negative at the last
rank, so that cell is undefined and is taken from its neighbour, and the two
markedness values built from the patched cells are recomputed after the loop.

**The touch list was the work, as predicted — so it got shorter.** Rather
than add five names to each of the eight places that hardcoded the measure
list, the list moved into one helper, `.basic_metric_names()` in
`R/etc_utils.R`, which maps each long name to its internal short one. The
pipeline, the data-frame converter, the validator, `print`, `fortify`, the
plot titles and `.check_curvetype` all read it now. Two more shared helpers
came out of the same exercise: `.get_metric_title()` (which `plot` and
`autoplot` had each been doing on their own) and `.is_signed_metric()` (the
`-1..1` axis range that used to be spelled `curvetype == "mcc" || curvetype
== "label"` in four places).

`.set_layout()` in `R/etc_utils_plot.R` had a hardcoded ladder that stopped
at nine panels and failed with "object 'mat1' not found" at fourteen. It now
computes the grid, and shares its column count with the ggplot2 side through
`.get_plot_ncol()`, so a set of measures is laid out the same way whichever
draws it.

**Tier 2.** `prob_metrics()` returns the Brier score and the log loss per
model and dataset; `prob_metrics_ci()` summarises them over multiple
datasets. Both take the same input as `evalmod()` — an `mmdata()` object or
raw scores and labels — because that is where the raw scores live; the curve
objects do not keep them. Scores outside [0, 1] are rejected with a
`precrec_error_invalid_scores` condition, since both measures read values
rather than ranks. The log loss clamps scores away from 0 and 1 by `eps`
(default 1e-15), which is what every other implementation does; without it a
single confident and wrong prediction makes the whole sample infinite.

The CI machinery the plan wanted generalized turned out to be a clean
extraction: `.calc_ci_stats()` and `.pmatch_dtype()` moved out of
`auc_ci.aucs()` into `R/etc_utils.R`, with the clipping range as an argument.
`auc_ci()` clips to [0, 1]; the Brier score does too, and the log loss is
clipped below at 0 and left unbounded above.

### Cost

Five more measures is five more vectors the length of the input, per dataset,
through the whole basic pipeline. Measured at 1e6 (release flags):

| case | before | after | |
|------|--------|-------|-|
| `evalmod(mode = "basic")` | 411 ms | 526 ms | 1.28-1.44x across shapes |
| `as.data.frame()` on basic points | 336 ms | 333 ms | unchanged |
| peak RSS, basic | 581 MB | 817 MB | +41% |
| `evalmod()` (rocprc) | 305 ms | 238 ms | unchanged by E5 |
| peak RSS, rocprc | 401 MB | 402 MB | unchanged |

The first cut of this work did regress `evalmod()` by 14-24%, because the
curve pipeline calls `calc_measures()` too and was paying for five measures
no curve is drawn from. `calc_measures()` and `calc_basic_measures()` grew an
`extra_measures` flag, and `.pl_main_rocprc()` passes `FALSE`. Isolated at
1e6: 179 ms for the full table, 86.6 ms for the reduced one. The rocprc
numbers above are the phase-6 optimisations showing through, not an E5 gain.

### Not done

Tier 3 (lift/gain, DET, precision-recall-gain) stays out of scope, as the
plan recommended.


### Current state

`calc_basic_measures` (`precrec_plx.cpp:281`) computes per-rank: error,
accuracy, specificity, sensitivity, precision, MCC, F1. Exposed via
`mode = "basic"`; names flow through `.get_metric_names()` (`etc_utils.R`),
`.dataframe_common` `curvetype_names`, the plot/autoplot/fortify helpers,
and `print.beval_info`.

### Tier 1 — confusion-matrix metrics (drop-in columns) — S/M

All computable in the same loop from tp/fp/tn/fn:

- **Balanced accuracy** = (sensitivity + specificity)/2
- **NPV** (negative predictive value); FDR/FOR come free as 1−precision,
  1−NPV — include NPV, skip trivially-derivable complements
- **Youden's J / informedness** = sn + sp − 1
- **Markedness** = precision + NPV − 1
- **Cohen's kappa**
- **F-beta**: generalize `fscore` with a `beta` argument to `evalmod`
  (default 1 — backward compatible)

Touch list per metric (this is the real cost, not the math):
`calc_basic_measures` + `calc_avg_points` compatibility →
`.get_metric_names()` → `curvetype_names` in `.dataframe_common` /
fortify/autoplot/plot helpers (`etc_utils_*.R`) → `print.beval_info`
summary → tests (`test_pl4_*`, `test_g_*`) → docs + vignette + pkgdown.
Grep-driven checklist; recommend adding **one metric end-to-end first**
(balanced accuracy) to establish the recipe, then batch the rest.

### Tier 2 — scalar, score-based metrics — M

**Brier score** and **log loss** are single numbers per (model, dataset),
not per-rank curves — they don't fit `mode = "basic"`. Design: a new
`scores()`-style S3 generic (or extend `auc()`'s table with a `metric`
column) computed from raw scores+labels; requires scores to be
probabilities, so validate range [0,1] and error otherwise. C++ helper is
~20 lines each. CI via the existing `auc_ci` bootstrap/normal machinery
generalized to `metric_ci()`.

### Tier 3 — new curve types — L (separate project)

Lift/gain charts, DET curves (FNR vs FPR), precision-recall-gain (PRG,
Flach & Kull 2015). Each needs: C++ curve builder + interpolation decision,
new `curvetype`, plot/autoplot/fortify/dataframe support, partial-AUC
semantics. Only PRG has strong scientific pull given the package's niche;
scope it as its own plan if wanted.

### Recommendation

Ship Tier 1 (+ F-beta) and Tier 2 (Brier, log loss). Defer Tier 3.

---

## Sequencing

```
1. E3 steps 1–3        CI refresh + testthat 3e → safe ground for everything else  [DONE]
2. E4 correctness      DBL_MIN bug, Welford   → cheap, S-sized, ride on step 1     [DONE]
3. E4 step 0           benchmark harness      → baseline                        [DONE]
4. E1                  data.table internals   → touches df sites E5 also touches  [DONE]
5. E3 steps 4–7        roxygen/cli/pkgdown    → mechanical, any time           [DONE]
6. E4 optimizations    guided by benchmarks                            [DONE]
7. E5 tiers 1–2        metrics on the new table plumbing         [DONE]
8. E2                  multiclass             → largest, lands on modernized base [DONE]
```

Rationale: E2 rewires input handling that E1/E5 touch — doing it last avoids
double work; the E4 correctness fixes are small and independent, so they ride
along with the first branch rather than needing one of their own. Since CI is intentionally off on `develop`,
run `devtools::check()` locally before each feature merge.

Each enhancement = one `feature/<Name>` branch off `develop` per the
existing git-flow, with its own NEWS entries; E2 and the `calc_uauc` fix
warrant a minor-version bump (0.15.0) rather than a patch.
