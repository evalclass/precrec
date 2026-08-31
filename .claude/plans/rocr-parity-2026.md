# precrec: ROCR measure parity + free-form metric curves (2026-08)

Three threads, planned together because they touch the same files:

- **R1** — add the ROCR measures precrec is missing.
- **R2** — a new function that takes an x-metric and a y-metric and draws
  one against the other, the way `ROCR::performance()` does.
- **R3** — two cross-cutting refactors the maintainer asked for at the same
  time: retire the `apply` family, and put argument checking on a common
  footing.

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

Each is one line next to the existing `sp`/`sn`/`prec`/`npv` loop in
`src/precrec_plx.cpp`. No new state, no new pass over the data.

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

### Decisions this raises

**D1 — unbounded measures break an axis assumption.** Today every basic
measure is either `[0, 1]` or, via `.is_signed_metric()`, `[-1, 1]`;
`R/etc_utils_autoplot.R:717-727` hard-codes those two ranges, with a third
branch that leaves `ylim = NULL` for `score`. `lift`, `odds`, `mi` and
`chisq` are unbounded above, so they need that third branch, and
`.is_signed_metric()` should become a three-way
`.metric_range()` returning `"unit"`, `"signed"` or `"free"`.

**D2 — `odds` is `Inf` at both ends of every curve.** At the most extreme
cutoffs `FP` or `FN` is zero by construction, so the odds ratio is infinite
for at least the first and last point of *every* dataset — not an edge case.
Decide once: `NA` (consistent with how `precision` and `npv` already patch
their undefined end), `Inf` (matches ROCR), or a Haldane–Anscombe +0.5
correction. Recommend `NA` plus a documented note, because the existing
`.is_signed_metric` / NA-handling machinery already copes and `Inf` would
wreck any shared axis.

**D3 — keep the new measures out of the curve hot path.** The C++ already
splits "always computed" from "extra", sizing the extra vectors `n_extra`
(`src/precrec_plx.cpp:313-317`) so `evalmod(mode = "rocprc")` never pays for
measures it does not draw. Every Tier A/B measure goes in the extra set.
This is the single most important performance guardrail in R1.

---

## R2. A free-form x-vs-y metric plot

### The shape of it

`evalmod(mode = "basic")` already computes every measure at every cutoff and
stores them as parallel vectors indexed by rank. A ROCR-style
`performance(pred, measure, x.measure)` plot is a *projection* of that table:
pick two columns, plot one against the other. That is the whole feature, and
framing it as a projection rather than a new pipeline is what keeps it cheap.

Proposed public entry point:

```r
metric_curve(mdat = NULL, scores = NULL, labels = NULL,
             x_metric = "fpr", y_metric = "tpr", ...)
```

returning `<ss|ms|sm|mm>xycurves` per the existing class scheme, with
`plot`, `autoplot`, `fortify` and `as.data.frame` methods. Defaults chosen
to reproduce ROCR's most common call, `performance(pred, "tpr", "fpr")`.

Name alternatives if `metric_curve` reads wrong: `xy_curve`,
`perf_curve`, `performance_curve`. **Open — maintainer's call.**

### D4 — the interpolation trap, and it is the real risk here

precrec exists because linear interpolation between PR points is wrong;
`src/precrec_plx.cpp` interpolates ROC and PR curves specially
(`interpolate_roc` and its PR counterpart). The basic-measure table holds
**raw per-cutoff points with no interpolation**.

So `metric_curve(x_metric = "rec", y_metric = "prec")` would draw a curve
that visibly disagrees with `evalmod(mode = "rocprc")` on the same data — a
package whose entire selling point is PR-curve correctness would ship a
second, wrong PR curve behind a friendlier interface. Same, less severely,
for `fpr`/`tpr`.

Three ways out, in order of preference:

1. **Special-case the two known pairs.** `(fpr, tpr)` and `(rec, prec)`
   delegate to the existing curve code; everything else draws raw points.
   Users get the right answer for the pairs they ask for most, and precrec
   keeps one PR curve.
2. **Draw points, not lines, by default** for arbitrary pairs, with
   `geom = "line"` opt-in. Honest, but makes the common call look unlike
   ROCR.
3. Draw raw lines for everything and document the difference. Cheapest,
   and the one that will generate bug reports.

Recommend 1, with 2's point geometry as the default for pairs that have no
defined interpolation.

### D5 — cost measures need arguments the pipeline has no slot for

`cost` (`cost.fp`, `cost.fn`), `f` (`alpha`), `cal` (`window.size`) and
`auc` (`fpr.stop`) all take measure-specific parameters. precrec threads
`beta` through `evalmod()` for `fscore` already, so the precedent is
"one named argument per measure", which does not scale past about three.
Decide in Phase 5 whether Tier B/C measures get their own arguments or a
single `metric_args = list(...)`.

---

## R3. The two cross-cutting refactors

### R3a. Retire the `apply` family

Current state: **44 calls across 13 files** — 37 `lapply`, 7 `vapply`. No
`sapply`, `mapply`, `apply` or `tapply` anywhere, so the codebase is already
half-disciplined; the type-unstable ones are gone.

**D6 — `purrr` would be a new hard dependency, and `CLAUDE.md` forbids
that.** The rule reads: *"Don't add hard dependencies. `Imports` is
deliberately small."* precrec currently imports 10 packages and already has
`rlang` and `cli`. Adding `purrr` pulls in `vctrs`, `lifecycle` and
`magrittr` transitively. Three options:

| | New deps | Call sites read like | Notes |
| --- | --- | --- | --- |
| **a. Adopt `purrr`** | +4 | `map(x, f)`, `map_dbl(x, f)` | What was asked for. Overrides a standing rule — maintainer's call, and a legitimate one. |
| **b. Standardize on base** | 0 | `vapply(x, f, double(1))` | `purrr`'s main win is type stability, which `vapply` already gives. Converts 37 `lapply` → `vapply` where the type is known. |
| **c. Internal `.map_*` helpers** | 0 | `.map_dbl(x, f)` | `purrr`-shaped names, base implementations, ~40 lines in `R/etc_utils.R`. Swappable for real `purrr` later without touching call sites. |

Recommend **c**: it delivers the readability the request is really about,
costs nothing at install time, keeps `CLAUDE.md`'s rule intact, and leaves
the door open. If the maintainer wants genuine `purrr`, **a** is fine — but
`Imports`, never `Suggests`, since these are hot-path calls.

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

The gaps worth closing, all of them free because `rlang` is already imported:

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

Explicitly **not** proposed: `checkmate`. It is the obvious library answer
and it is very good, but it is another hard dependency for something
`rlang` + 40 lines already covers.

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
| 1 | Argument-checking foundation (R3b) | `feature/ArgChecks` | S | Additive only; suite green; snapshots unchanged |
| 2 | Retire the `apply` family (R3a) | `feature/MapHelpers` | M | **Snapshots byte-identical**; bench within noise |
| 3 | Tier A measures (R1) | `feature/MetricsTierA` | M | ROCR parity script; new snapshots reviewed line by line |
| 4 | `metric_curve()` + methods (R2) | `feature/MetricCurve` | M/L | Pairs `(fpr,tpr)`/`(rec,prec)` match `evalmod()` exactly |
| 5 | Tier B measures + `cost` args (R1) | `feature/MetricsTierB` | M | ROCR parity script |
| 6 | Tier C: `prbe`, `rch`, `sar`, `cal`, `ecost` (R1) | `feature/MetricsTierC` | L | Per-measure; **candidate for deferral** |
| 7 | Vignette, pkgdown, release prep | `feature/Docs0160` | S | `check()`, `spell_check()`, `_pkgdown.yml` reference sections |

### Why this order

- **Phase 2 before 3.** The refactor's correctness gate is "no snapshot
  moves". Phase 3 changes snapshots on purpose. Doing the no-op refactor
  first keeps that gate clean and unambiguous; doing it after would mean
  diffing a refactor against a moving baseline.
- **Phase 1 before 4.** `metric_curve()` is the function with the most new
  arguments in the package; it should be the first consumer of the new
  checking spec, not a retrofit.
- **Phase 3 before 4.** `metric_curve()`'s defaults are `fpr`/`tpr`, and
  `fpr` does not exist as a basic measure yet.
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

## Open decisions

| | Question | Recommendation |
| --- | --- | --- |
| D1 | Axis range for unbounded measures | Three-way `.metric_range()` |
| D2 | `odds` at the curve ends | `NA`, documented |
| D3 | Keep new measures out of the curve path | Yes — extend the `n_extra` set |
| D4 | Interpolation for `(rec, prec)` / `(fpr, tpr)` | Delegate to the real curve code |
| D5 | Measure-specific parameters | Decide at phase 5 |
| D6 | `purrr` vs base vs internal helpers | Internal `.map_*` helpers |
| — | Name for the new function | `metric_curve()` |
