# precrec: ROCR measure parity + free-form metric curves (2026-08)

Three threads, planned together because they touch the same files:

- **R1** — add the ROCR measures precrec is missing.
- **R2** — a new function that takes an x-metric and a y-metric and draws
  one against the other, the way `ROCR::performance()` does.
- **R3** — two cross-cutting refactors the maintainer asked for at the same
  time: retire the `apply` family, and put argument checking on a common
  footing.

All the decisions that gate phases 1-4 are settled; see **Decisions taken**
at the end. The only dependency change in the whole plan is `checkmate`
(+ `backports`) added to `Imports` in phase 1.

Effort key as in [enhancements-2026.md](enhancements-2026.md):
S ≈ days, M ≈ 1–2 weeks, L ≈ 3–6 weeks.

---

## R1. Measure gap against ROCR

Source: `?ROCR::performance`. ROCR exposes 28 measures behind 35 identifiers
(7 are aliases: `fall`, `rec`, `sens`, `miss`, `spec`, `prec`, `mat`), plus
`cutoff` as the default x-axis. Below, ROCR's identifier on the left,
precrec's name on the right; the four tables account for all 36.

### Already covered — 17 identifiers, no work

| ROCR | precrec | Where |
| --- | --- | --- |
| `cutoff` | `score` | `mode = "basic"` |
| `acc` | `accuracy` | `mode = "basic"` |
| `err` | `error` | `mode = "basic"` |
| `tpr`, `rec`, `sens` | `sensitivity` | `mode = "basic"` |
| `tnr`, `spec` | `specificity` | `mode = "basic"` |
| `ppv`, `prec` | `precision` | `mode = "basic"` |
| `npv` | `npv` | `mode = "basic"` |
| `phi`, `mat` | `mcc` | `mode = "basic"` |
| `f` | `fscore` (with `beta`) | `mode = "basic"` |
| `auc` | `auc(curvetype = "ROC")` | `mode = "rocprc"` |
| `aucpr` | `auc(curvetype = "PRC")` | `mode = "rocprc"` |
| `mxe` | `logloss` | `prob_metrics()` |

precrec already carries measures ROCR has no equivalent for:
`balanced_accuracy`, `informedness`, `markedness`, `kappa`, Brier score,
partial AUC, `auc_ci()`, averaging across datasets/folds, one-vs-rest
multiclass, and correct non-linear PR interpolation.

### Tier A — 11 identifiers, algebra on vectors already computed — **S**

Each is a vectorized one-liner in `calc_measures()`, derived from columns the
`pevals` table already holds or from the `cmats` counts. No C++ change, no
new state, no new pass over the data. See **Decisions taken** below.

| ROCR | Definition | In precrec's terms |
| --- | --- | --- |
| `fpr`, `fall` | `FP/N` | `1 - specificity` |
| `fnr`, `miss` | `FN/P` | `1 - sensitivity` |
| `pcfall` | `FP/(TP+FP)` | `1 - precision` |
| `pcmiss` | `FN/(TN+FN)` | `1 - npv` |
| `rpp` | `(TP+FP)/n` | rate of positive predictions |
| `rnp` | `(TN+FN)/n` | rate of negative predictions |
| `lift` | `P(Ŷ=⊕\|Y=⊕)/P(Ŷ=⊕)` | `sensitivity / rpp` |
| `odds` | `(TP·TN)/(FN·FP)` | odds ratio |
| `rmse` | `√(Σ(y−ŷ)²/n)` | `sqrt(brier)`, in `prob_metrics()` |

### Tier B — 3 identifiers, new per-cutoff computation, still O(n) — **M**

| ROCR | Definition | Note |
| --- | --- | --- |
| `mi` | `I(Ŷ;Y) = H(Y) − H(Y\|Ŷ)` | entropy of the 2×2 table at each cutoff |
| `chisq` | Pearson χ² of the 2×2 table | same table, different statistic |
| `cost` | `cost.fp·FP/N + cost.fn·FN/P` | needs two new user arguments |

### Tier C — 5 identifiers, curve-level or scalar, new algorithms — **L**

| ROCR | Definition | Why it is harder |
| --- | --- | --- |
| `prbe` | precision-recall break-even point | root-find where `prec == rec`; interacts with PR interpolation |
| `rch` | ROC convex hull | new curve type, new S3 surface |
| `sar` | `(acc + auc + (1 − rmse))/3` | mixes a cutoff measure, a curve scalar and a score scalar |
| `cal` | calibration error | sliding window over sorted scores; `window.size` argument |
| `ecost` | expected-cost curve (Drummond & Holte) | a curve in its own space, with an obligatory x-axis |

### How the new measures land

**Naming — standard where a standard exists, spelled out where ROCR is
idiosyncratic.** precrec already ships `mcc` and `npv`, so abbreviations are
not a new precedent; but `rpp`/`rnp`/`pcfall`/`pcmiss` are ROCR-local jargon
with perfectly good standard names. Canonical name on the left, accepted
aliases on the right.

| Canonical | Aliases | Definition |
| --- | --- | --- |
| `fpr` | `fall` | `FP/N` |
| `fnr` | `miss` | `FN/P` |
| `false_discovery_rate` | `fdr`, `pcfall` | `FP/(TP+FP)` |
| `false_omission_rate` | `for`, `pcmiss` | `FN/(TN+FN)` |
| `predicted_positive_rate` | `ppr`, `rpp` | `(TP+FP)/n` |
| `predicted_negative_rate` | `pnr`, `rnp` | `(TN+FN)/n` |
| `lift` | — | `sensitivity / predicted_positive_rate` |
| `odds` | `odds_ratio` | `(TP·TN)/(FN·FP)` |
| `mi` | `mutual_information` | `H(Y) − H(Y\|Ŷ)` |
| `chisq` | — | Pearson χ² of the 2×2 table |

Aliases resolve through the existing `.pmatch_curvetype_basic()` machinery,
so ROCR's own identifiers keep working as input everywhere a measure name is
accepted. Panel titles use the canonical name via `.get_metric_title()`.

**The new measures are opt-in; the default set stays at 14.**
`autoplot.*points` and `plot.*points` default to
`curvetype = .get_metric_names("basic")`, which today returns all 14 measures.
Adding Tier A/B to that would take an existing user's default plot from 14
panels to 22, then 25, with no code change on their side. So
`.basic_metric_names()` gains a per-measure `default` flag:
`.get_metric_names("basic")` keeps returning the same 14, and a new
`.get_metric_names("basic_all")` returns everything for validation and for
`metric_curve()`. Named explicitly, the new measures work everywhere the old
ones do.

**Derive the new measures in R, not in C++.** This replaces the original
"one line each next to the C++ loop" idea, and it is what makes opt-in free.
`calc_measures(cmats, ..., extra_measures = TRUE)` already receives the
confusion matrices, and `.validate.cmats()` confirms they carry
`pos_num`, `neg_num`, `tp`, `fp`, `tn`, `fn`, `ranks`. Every Tier A and
Tier B measure is a vectorized one-liner over those:

- `fpr` = `1 - specificity`, `fnr` = `1 - sensitivity`,
  `false_discovery_rate` = `1 - precision`,
  `false_omission_rate` = `1 - npv` — pure transforms of columns the
  `pevals` table already holds.
- `predicted_positive_rate`, `predicted_negative_rate`, `lift`, `odds`,
  `mi`, `chisq` — arithmetic on `tp`/`fp`/`tn`/`fn`.

Consequences, all good: **no C++ change for Tier A or Tier B**, so no new
`.o` churn and no Rcpp signature to regenerate; nothing is computed unless
requested, so an unused measure costs zero time and zero memory; and the
`n_extra` gating in `src/precrec_plx.cpp` stays exactly as it is. `cost`
(Tier B) is the same shape, just with two user-supplied weights.

The one thing to watch: these must be computed **before** the `x_bins`
reduction, on the same rank grid as the measures they derive from, or a
derived column will not line up with its siblings. Phase 3 pins that with a
test that every basic column has equal length after reduction — the check
`calc_measures()` already performs at `R/pl4_calc_measures.R:97`.

**`odds` is `NA` at the ends, not `Inf`.** At the extreme cutoffs `FP` or
`FN` is zero by construction, so the odds ratio is undefined for the first
and last point of *every* dataset — not an edge case. `NA` matches how
`precision` and `npv` already patch their own undefined end
(`src/precrec_plx.cpp:407-412`), and keeps `Inf` out of any shared axis.
ROCR returns `Inf`; the difference is documented, and the parity script
(below) compares only the finite region.

**Unbounded measures need a third axis range.** `R/etc_utils_autoplot.R:717-727`
hard-codes `[0, 1]` or, via `.is_signed_metric()`, `[-1, 1]`, with a third
branch leaving `ylim = NULL` for `score`. `lift`, `odds`, `mi` and `chisq`
are unbounded above. Replace `.is_signed_metric()` with
`.metric_range()` returning `"unit"`, `"signed"` or `"free"`; the `"free"`
branch is the `ylim = NULL` path that already exists, so this is a rename
plus one lookup table, not new plotting logic.

## R2. A free-form x-vs-y metric plot

### The shape of it

`evalmod(mode = "basic")` already computes every measure at every cutoff and
stores them as parallel vectors indexed by rank. A ROCR-style
`performance(pred, measure, x.measure)` plot is a *projection* of that table:
pick two columns, plot one against the other. That is the whole feature, and
framing it as a projection rather than a new pipeline is what keeps it cheap.

Public entry point:

```r
metric_curve(mdat = NULL, scores = NULL, labels = NULL,
             x_metric = "fpr", y_metric = "sensitivity", ...)
```

returning `<ss|ms|sm|mm>xycurves` per the existing class scheme, with
`plot`, `autoplot`, `fortify` and `as.data.frame` methods. The defaults
reproduce ROCR's most common call, `performance(pred, "tpr", "fpr")`; both
arguments accept the aliases in the naming table, so `x_metric = "fall"` and
`y_metric = "tpr"` work too.

Because the projection reads the same table `mode = "basic"` produces, the
four class variants come almost free — `ss`/`ms`/`sm`/`mm`, averaging and
one-vs-rest multiclass all work because the underlying table already
supports them.

### The joinable-pair registry — the real risk here, and how it is handled

precrec exists because linear interpolation between PR points is wrong;
`src/precrec_plx.cpp` interpolates ROC and PR curves specially
(`interpolate_roc` and its PR counterpart). The basic-measure table holds
**raw per-cutoff points with no interpolation**.

So a naive `metric_curve(x_metric = "sensitivity", y_metric = "precision")`
would draw a curve that visibly disagrees with `evalmod(mode = "rocprc")` on
the same data — a package whose entire selling point is PR-curve correctness
would ship a second, wrong PR curve behind a friendlier interface.

**Design: an internal registry of pairs that may be joined by a line.** Not
two hard-coded special cases — a table, so the set can grow as interpolation
is defined for more pairs without touching the plotting code.

```r
#
# Metric pairs whose points have a defined interpolation
#
# Only a pair listed here may be joined into a curve; every other pair is
# drawn as points, because joining raw per-cutoff points with straight lines
# is exactly the error precrec exists to avoid. Adding a row here is how a
# new joinable pair is registered - the plotting code reads this and nothing
# else.
#
.joinable_pairs <- function() {
  data.frame(
    x     = c("fpr",         "sensitivity"),
    y     = c("sensitivity", "precision"),
    curve = c("ROC",         "PRC"),
    stringsAsFactors = FALSE
  )
}
```

Rules, in order:

1. Resolve both metric names through the alias table to canonical names.
2. If the ordered pair is in the registry, **delegate to the existing curve
   code** for that `curve` type. `metric_curve()` then returns exactly what
   `evalmod(mode = "rocprc")` returns for that curve — one implementation,
   one answer, no possibility of drift.
3. Otherwise draw points (`geom = "point"`), with `geom = "line"` available
   as an explicit opt-in for anyone who knows what they are asking for.

Phase 4 pins rule 2 with a test asserting that `metric_curve()` and
`evalmod(mode = "rocprc")` produce *identical* curve data for both
registered pairs. That test is the guard against precrec ever shipping two
different PR curves.

### D5 — cost measures need arguments the pipeline has no slot for

`cost` (`cost.fp`, `cost.fn`), `f` (`alpha`), `cal` (`window.size`) and
`auc` (`fpr.stop`) all take measure-specific parameters. precrec threads
`beta` through `evalmod()` for `fscore` already, so the precedent is
"one named argument per measure", which does not scale past about three.
Decide in Phase 5 whether Tier B/C measures get their own arguments or a
single `metric_args = list(...)`. **Still open, but it does not block
phases 1-4.**

## R3. The two cross-cutting refactors

### R3a. Retire the `apply` family

Current state: **44 calls across 13 files** — 37 `lapply`, 7 `vapply`. No
`sapply`, `mapply`, `apply` or `tapply` anywhere, so the codebase is already
half-disciplined; the type-unstable ones are gone.

**Decided: internal `.map_*` helpers, no new dependency.** `CLAUDE.md` says
*"Don't add hard dependencies. `Imports` is deliberately small"*, and `purrr`
would pull in `vctrs`, `lifecycle` and `magrittr`. Since `purrr`'s main win
over `lapply` is type stability — which `vapply` already provides — a small
internal family in `R/etc_utils.R` gives the call-site readability without
the install cost:

```r
.map(x, f, ...)        # lapply
.map_dbl(x, f, ...)    # vapply(..., double(1))
.map_int(x, f, ...)    # vapply(..., integer(1))
.map_chr(x, f, ...)    # vapply(..., character(1))
.map_lgl(x, f, ...)    # vapply(..., logical(1))
.map2(x, y, f, ...)    # Map, returning a plain list
.imap(x, f, ...)       # over seq_along(x) with names
```

Roughly 40 lines. The names are `purrr`'s, so if precrec ever does take the
dependency the swap is an import change and not a call-site change. Where
the return type is known — most of the 37 `lapply` calls — conversion goes
to a typed helper, which is a genuine strictness gain over the status quo,
not just a rename.

**The gate that makes this safe:** this refactor must be a *pure no-op*. All
35 plot snapshots stay byte-identical, all 2989 assertions pass, and
`bench/run_bench.R --compare` moves within its ~11% noise floor. If any
snapshot moves, the refactor changed behavior and is wrong. This is a
stronger gate than anything available for the feature phases, which is why
it should land before them (see Sequencing).

### R3b. Argument checking

Current state is better than the request implies. `R/etc_utils_validate_args.R`
already has `.stop_invalid_arg()` (typed condition classes), `.assert_flag()`,
`.assert_string(values = )`, `.assert_number(min, max, whole)` and
`.assert_internal()`. Tests match on class, not message text.

The gaps worth closing. Items 1-2 are free because `rlang` is already
imported; items 3-4 build on `checkmate` (see below):

1. **`rlang::arg_match()` for enum arguments.** `.assert_string(values = )`
   rejects a bad value; `arg_match()` rejects it *and* suggests the nearest
   valid one. Straight upgrade for `mode`, `curvetype`, `multiclass`,
   `ties_method`, and both new `x_metric`/`y_metric` arguments — where the
   valid set will be ~25 names and a typo suggestion genuinely matters.
2. **`rlang::check_required()`** for mandatory arguments, replacing
   `missing()` checks.
3. **A declarative spec per exported function** instead of a hand-written
   check block: one named list of `arg = type + constraint`, walked by a
   single `.check_args()`. This is the "more common approach" the request
   is reaching for, and it is what makes adding `metric_curve()`'s dozen
   arguments cheap.
4. `.assert_choice()` and `.assert_numeric_vector()` to round out the family.

**Decided: adopt `checkmate`.** Its footprint is small enough to clear the
`CLAUDE.md` bar — version 2.3.4, `Imports: backports (>= 1.1.0), utils`, no
`LinkingTo`, and `backports` itself has no hard dependencies. Two small
packages, neither compiled against anything else.

**But not `checkmate::assert_*()`.** Those throw a plain `simpleError`, which
would break precrec's `precrec_error_invalid_<arg>` contract and every
`expect_error(..., class = )` test in the suite. Use the `check_*()`
predicates instead — they return `TRUE` or a failure *string* — and route the
string through the existing `.stop_invalid_arg()`:

```r
.assert_number <- function(x, arg, ...) {
  res <- checkmate::check_number(x, ...)
  if (!isTRUE(res)) .stop_invalid_arg(res, arg)
  invisible(x)
}
```

The `.assert_*` signatures do not change, so this is an internal swap: the
helpers get checkmate's breadth and message quality, callers and tests see
nothing new. New helpers (`.assert_choice()`, `.assert_numeric_vector()`)
are then almost free to add.

Per `CLAUDE.md`, existing `stop(msg, call. = FALSE)` domain errors are **not**
converted wholesale. New code uses the new helpers; old checks migrate only
where a phase is already editing that function.

---

## Phases

Each phase is one `feature/<Name>` branch off `develop`, merged `--no-ff`,
with its own `NEWS.md` bullets. CI is off on `develop` by design, so
`devtools::check()` runs locally before every merge.

| # | Phase | Branch | Effort | Gate |
| --- | --- | --- | --- | --- |
| 1 | `checkmate` behind `.assert_*`, near-miss hints, new helpers | `feature/ArgChecks` | S | **DONE (2026-09-01)** - see outcome below |
| 2 | `.map_*` helpers, retire the 44 `apply` calls | `feature/MapHelpers` | M | **Snapshots byte-identical**; bench within noise |
| 3 | Tier A measures, `default` flag, `.metric_range()` | `feature/MetricsTierA` | M | `.get_metric_names("basic")` still returns the same 14; parity script |
| 4 | `metric_curve()`, `.joinable_pairs()`, methods | `feature/MetricCurve` | M | Registered pairs produce data **identical** to `evalmod(mode = "rocprc")` |
| 5 | Tier B measures + `cost` arguments | `feature/MetricsTierB` | M | Parity script |
| 6 | Tier C: `prbe`, `rch`, `sar`, `cal`, `ecost` | `feature/MetricsTierC` | L | Per-measure; **candidate for deferral** |
| 7 | Vignette, pkgdown, release prep | `feature/Docs0160` | S | `check()`, `spell_check()`, `_pkgdown.yml` reference sections |

### Phase 1 outcome (2026-09-01)

Landed on `feature/ArgChecks`. `checkmate` (+ `backports`) is in `Imports`
and drives the predicates inside `.assert_flag()`, `.assert_string()`,
`.assert_number()` and the new `.assert_choice()`. Two guard clauses were
needed: `checkmate`'s `na.ok` accepts an NA of *any* type, so `is.character()`
and `is.numeric()` still ride alongside it to keep a logical `NA` out of a
name argument and a character `NA` out of a numeric one. Every message and
every condition class is unchanged.

**Gate.** A 429-line behavioural probe recorded the exact class and message of
all eight assert entry points across 33 inputs before the change, and was
diffed against the same probe afterwards. Two lines moved, both the intended
bug fix. Full suite `FAIL 0 | WARN 0 | PASS 2989 + 21 new`; `R CMD check`
`0 errors | 0 warnings | 1 note` (the local `-mno-omit-leaf-frame-pointer`);
no snapshot moved.

**One real bug found and fixed.** `.assert_number(x, whole = TRUE)` computed
`x %% 1 != 0`, and `Inf %% 1` is `NaN`, so `evalmod(x_bins = Inf)` failed with
R's own "missing value where TRUE/FALSE needed" and no `precrec` class
attached.

**Near-miss hints** are hand-rolled on `utils::adist()` rather than
`rlang::arg_match()`: `arg_match()` raises its own `rlang_error` and would
have had to be caught and re-raised to keep the condition classes, and the
threshold wanted tuning anyway. It scales with the length of what was typed,
so `"sensitivty"` offers `"sensitivity"` while `"xx"` offers nothing. This is
aimed squarely at phase 4, where `x_metric` and `y_metric` have ~25 valid
values.

**`.check_args()` was deliberately not built.** The declarative spec was
planned as item 3 of R3b, but the `.validate_<arg>()` functions already *are*
a per-argument registry reused across call sites, and several take a second
argument (`.validate_modnames(modnames, datalen)`,
`.validate_cb_alpha(cb_alpha, calc_avg)`) that a name-dispatch table cannot
carry without escape hatches. Building a second mechanism before
`metric_curve()` exists would be guessing at its shape. Revisit in phase 4,
where there is a real consumer to design against.

### Why this order

- **Phase 2 before 3.** The refactor's correctness gate is "no snapshot
  moves". Phase 3 changes snapshots on purpose. Doing the no-op refactor
  first keeps that gate clean and unambiguous; doing it after would mean
  diffing a refactor against a moving baseline.
- **Phase 1 before 4.** `metric_curve()` is the function with the most new
  arguments in the package; it should be the first consumer of the new
  checking spec, not a retrofit.
- **Phase 3 before 4.** `metric_curve()`'s defaults are `fpr`/`sensitivity`,
  and `fpr` does not exist as a basic measure yet. Phase 3 also introduces
  the `default` flag that keeps the new measures opt-in, which phase 4's
  `x_metric`/`y_metric` validation reads.
- **Phase 6 last, and possibly never.** Tier C is five loosely related
  algorithms; `rch` and `ecost` are new curve types, which
  [enhancements-2026.md](enhancements-2026.md) already deferred once as
  "E5 tier 3 — separate project". Nothing in phases 1–5 depends on it.

### Version

Phases 1–2 are internal: patch. Phase 3 onward adds public measures and a
public function: **0.16.0**, bumped when phase 3 merges.

---

## Anti-degradation strategy

The request explicitly asks to avoid regression. What is already in place:

- **2989 assertions**, `FAIL 0 | WARN 0`.
- **35 machine-independent plot snapshots** — text digests of
  `ggplot2::layer_data()`, so they compare structure, not pixels, and are
  stable across machines. These are what make phase 2 verifiable.
- `bench/run_bench.R --compare` (noise floor ~11%, so read it directionally)
  and `bench/run_correctness.R`.

What this plan adds:

- **`bench/run_rocr_parity.R`** — an offline script, not a test, that runs
  ROCR and precrec on the same data and asserts numeric agreement for every
  mapped measure in the table above. ROCR does **not** become a dependency,
  not even `Suggests`; the script is for the maintainer to run when a
  measure lands. This is the strongest possible check on R1, because the
  reference implementation is the thing being matched.
- **Per-phase snapshot discipline.** A snapshot that changes in phases 1, 2
  or 7 is a bug. A snapshot that changes in 3–6 is reviewed line by line
  before acceptance — the panel-title bug in 0.15.0 got in precisely
  because a snapshot was accepted without reading the diff.
- **Every fix validated by reverting it** and confirming the new test fails
  with a readable message.

## Decisions taken

| | Question | Decision |
| --- | --- | --- |
| D1 | Axis range for unbounded measures | `.metric_range()` returning `"unit"` / `"signed"` / `"free"` |
| D2 | `odds` at the curve ends | `NA`, documented; parity script compares the finite region |
| D3 | Keep new measures off the hot path | Derive in R in `calc_measures()`; no C++ change, nothing computed unless asked for |
| D4 | Interpolation for registered pairs | `.joinable_pairs()` registry; registered pairs delegate to the real curve code, everything else draws points |
| D6 | `purrr` vs base vs internal helpers | Internal `.map_*` helpers, no new dependency |
| D7 | Argument checking library | `checkmate` via `check_*()` routed through `.stop_invalid_arg()`, preserving condition classes |
| D8 | Do new measures join the default panel set | No — opt-in; `.get_metric_names("basic")` still returns 14 |
| D9 | Measure naming | Standard abbreviations where standard; ROCR jargon spelled out; ROCR ids accepted as aliases |
| D10 | Name of the new function | `metric_curve()` |

## Still open

| | Question | When it must be settled |
| --- | --- | --- |
| D5 | Measure-specific parameters: one argument each, or `metric_args = list(...)` | Phase 5 — does not block 1-4 |
| D11 | Whether Tier C ships at all | Phase 6 — nothing depends on it |
