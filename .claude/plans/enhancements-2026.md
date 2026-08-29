# precrec enhancement plan (2026-08)

Five enhancements, each with current state, design decisions, work plan, and
risks. Suggested sequencing at the end. Effort: S ≈ days, M ≈ 1–2 weeks,
L ≈ 3–6 weeks, XL ≈ 2+ months.

---

## E1. Store all data-frame data as `data.table` — **M**

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

## E4. Optimize the Rcpp layer — **M** (correctness items are S and come first)

### Correctness findings to fix before optimizing

1. **`calc_uauc` NA handling bug** (`precrec_plx.cpp:154`) — **verified,
   but dormant.** `na_worst` maps NA to `DBL_MIN`, the smallest *positive*
   double rather than the most negative, so NAs outrank every negative score.
   Reproduced standalone: scores `c(-1,-2,NA,-3,-4)` / labels
   `c(2,2,2,1,1)` returns AUC **1.0** where the correct value is 0.667.
   *However*, `calc_uauc` is only reached via `ustat_method = "sort"` or when
   data.table is unavailable — and `.pl_main_aucroc` calls
   `calc_auc_with_u()` without `...`, so the public API can never select it,
   while data.table is a hard `Imports` dependency. The default path,
   `calc_uauc_frank`, delegates ranking to `data.table::frank` and is
   correct. So: **no published result is affected**; this is a latent
   fallback bug plus a test-coverage gap (the four `ustat_method = "sort"`
   tests in `test_pl3_2_calc_auc_with_u.R` use only positive scores).
   Fix: `std::numeric_limits<double>::lowest()`, and add a negative-scores +
   NA case to the existing sort-path tests. One line; effort S.
2. **`calc_avg_curve` variance** (`precrec_plx.cpp`): uses the
   `E[x²]−E[x]²` formula and clamps negatives — catastrophic cancellation
   territory. Switch to Welford's algorithm; CI bands change only at
   ~1e-8 level but become numerically sound.
3. Audit `get_yval_single`: `std::set<double> x_set` appears declared but
   unused — remove or justify.

### Optimization plan (benchmark-driven, in order of expected payoff)

0. **Infrastructure first**: `bench/` directory (Rbuildignored) with
   `bench::mark()` scripts over seeded datasets (1e4–1e7 rows; balanced +
   imbalanced; with/without ties and NAs), storing baseline JSON so every
   later change shows a before/after. Also add an ALTREP-safety and
   correctness harness comparing against the pure-R fallback
   (`use_rcpp = FALSE` path already exists in `.dataframe_common` — handy).
1. **Eliminate copy-then-wrap**: pervasive pattern is `std::vector<double>`
   filled, then `Rcpp::wrap()` copies into a new SEXP (e.g.
   `convert_curve_df`, `create_roc_curve`, `calc_avg_curve`). Write directly
   into `Rcpp::NumericVector(Rcpp::no_init(n))`. Halves peak memory on the
   biggest paths; measurable time win at 1e6+.
2. **`get_score_ranks`**: replace the `vector<pair<unsigned,double>>` sort
   with an index-vector sort using a lambda comparator (less memory traffic);
   only `stable_sort` when `ties_method` requires stability.
3. **`calc_basic_measures`**: hoist `1.0/(np+nn)`, `1.0/nn`, `1.0/np`
   reciprocals out of the loop; the `nn==0`/`np==0` branches too (they are
   loop-invariant). Minor but free.
4. **`interpolate_prc`**: profile; it runs per adjacent point pair with
   nonlinear steps — check for redundant recomputation of loop-invariant
   terms.
5. **Binary size**: the 4 MB `.so` (standing CRAN NOTE) is mostly template
   instantiations (`make_new_labels` × 4 SEXP types) and Rcpp headers.
   Try `-Os`-equivalent via `src/Makevars` only if it doesn't reconflict with
   the "keep unstripped" decision from 0.10.1 — otherwise accept.
6. **Parallelism**: don't add OpenMP in C++. The natural unit is
   per-(model, dataset) `lapply` in `.pl_main_*` — if wanted, offer
   opt-in `future.apply` at R level later. Not part of this pass.

### Risks

- Item 1 changes allocation patterns near the R GC — every touched function
  needs the correctness harness run before/after.
- The fix in (1) does **not** change any public-API result (the buggy path
  is unreachable from `evalmod()`); NEWS should describe it as an internal
  fallback fix, not a results-changing one.

---

## E5. Additional evaluation metrics — **M** (tiered)

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
1. E3 steps 1–3        CI refresh + testthat 3e → safe ground for everything else
2. E4 correctness      DBL_MIN bug, Welford   → cheap, S-sized, ride on step 1
3. E4 step 0           benchmark harness      → baseline
4. E1                  data.table internals   → touches df sites E5 also touches
5. E3 steps 4–7        roxygen/cli/pkgdown    → mechanical, any time
6. E4 optimizations    guided by benchmarks
7. E5 tiers 1–2        metrics on the new table plumbing
8. E2                  multiclass             → largest, lands on modernized base
```

Rationale: E2 rewires input handling that E1/E5 touch — doing it last avoids
double work; the E4 correctness fixes are small and independent, so they ride
along with the first branch rather than needing one of their own. Since CI is intentionally off on `develop`,
run `devtools::check()` locally before each feature merge.

Each enhancement = one `feature/<Name>` branch off `develop` per the
existing git-flow, with its own NEWS entries; E2 and the `calc_uauc` fix
warrant a minor-version bump (0.15.0) rather than a patch.
