# precrec architecture

## Source file prefixes (`R/`)

| Prefix | Role |
| --- | --- |
| `main_` | `evalmod()` and `metric_curve()` — the user-facing entry points |
| `mm1`–`mm5` | **m**ake **m**odel data: input joining, validation, reformatting, one-vs-rest expansion |
| `pl1`–`pl6` | **p**ipe**l**ine stages that turn reformatted data into results |
| `g_` | S3 **g**enerics and their methods (`plot`, `autoplot`, `auc`, …) |
| `etc_utils*` | shared helpers: arg validation, object validation, plot/df utils |
| `etc_create_samples` | `create_sim_samples()` test-data generator |
| `precrec.R` | package-level roxygen docs, dataset docs, `.onUnload` |
| `RcppExports.R` | **generated** R stubs for the C++ layer |

Numeric prefixes encode call order, so `pl3` may call `pl4` helpers but not
the reverse. Put new code in the file matching its stage.

## The pipeline

`evalmod()` (`main_evalmod.R`) validates args, builds an `mdat` object if the
user passed raw `scores`/`labels`, then hands off to `pl_main()`
(`pl1_pipeline_main.R`), which dispatches on **mode**:

```
scores/labels
  └─ mmdata()            mm1_create_mmdat.R    -> "mdat" (list of "fmdat"/"sdat")
       ├─ join_scores/join_labels              mm2_join_data.R
       ├─ format_nfold()                       mm4_format_nfold.R
       └─ reformat_data()                      mm3_reformat_data.R
            (rank scores, factor labels; C++ get_score_ranks/format_labels)

pl_main(mdat, mode=)     pl1_pipeline_main.R
  ├─ mode "rocprc" ─ .pl_main_rocprc           pl2_pipeline_main_rocprc.R
  │     per dataset:  create_confmats()        pl3_create_confmats.R  -> "cmats"
  │                   calc_metrics()          pl4_calc_metrics.R    -> "pevals"
  │                   create_curves()          pl5_create_curves.R    -> "curves"
  │     then:         .calc_avg_common()       pl6_calc_average.R     (avg + CI band)
  │     result class: <pf>curves
  ├─ mode "basic"  ─ .pl_main_basic            pl2_pipeline_main_basic.R
  │     same cmats/pevals stages, no curve fitting
  │     result class: <pf>points
  └─ mode "aucroc" ─ .pl_main_aucroc           pl2_pipeline_main_aucroc.R
        calc_auc_with_u()                      pl3_calc_auc_with_u.R
        fast AUC via the Mann–Whitney U statistic, no curve at all
        result class: aucroc
```

`part()` (`g_part.R`) post-processes a `*curves` object in place, adding
partial AUCs and `xlim`/`ylim` attributes and flipping `attr(x, "partial")`.
`prbe()` (`g_prbe.R`) reads the precision-recall break-even point off the
already-interpolated PR curve of such an object.

`metric_curve()` (`main_metric_curve.R`) is a second entry point that
projects two basic metrics against each other. It does not add a pipeline
stage: it calls `evalmod()` and takes two columns. See **The joinable-pair
registry** below, which is the load-bearing part.

### Modes

- `"rocprc"` (default, alias `"prcroc"`) — ROC + precision-recall curves.
- `"basic"` — per-rank basic metrics. `.get_metric_names("basic")` is the
  fourteen an object holds by default; `.get_metric_names("basic_all")` is
  every metric `.basic_metric_table()` knows, and `evalmod(metrics = )`
  chooses between them per object. See **The metric table** below.
- `"aucroc"` — ROC AUC only, the fast path. Note `mmdata(mode = "aucroc")`
  produces `sdat` rather than `fmdat`, and the other modes reject it.

Modes accept partial matches via `.pmatch_mode()`; several args have similar
`.pmatch_*` helpers, so compare against the *normalized* value internally.

## S3 class naming

Result classes are `<prefix><kind>` where the two-letter prefix comes from
`.make_prefix(model_type, dataset_type)` — first letter is models, second is
datasets, `s` = single, `m` = multiple:

| | single dataset | multiple datasets |
| --- | --- | --- |
| **single model** | `sscurves` / `sspoints` | `smcurves` / `smpoints` |
| **multiple models** | `mscurves` / `mspoints` | `mmcurves` / `mmpoints` |

`metric_curve()` adds a third kind on the same grid: `ssxycurves`,
`smxycurves`, `msxycurves`, `mmxycurves`.

`curves` objects also carry `curve_info` and `aucs`; `points` objects carry
`beval_info`; `xycurves` objects carry `xycurve_info`. This is why generics come in sets of eight (`g_plot.R`,
`g_autoplot.R`, `g_fortify.R`, `g_dataframe.R`) — **adding behavior usually
means editing all eight methods, or better, the shared helper they delegate
to in `etc_utils_plot.R` / `etc_utils_autoplot.R` / `etc_utils_dataframe.R` /
`etc_utils_fortify.R`.**

Averaging and confidence bands only apply when the dataset axis is `m`. For a
single dataset `.pl_main_rocprc` silently forces `calc_avg = FALSE` and
`raw_curves = TRUE`.

## Object protocol

Every intermediate object is a `structure(list, class = ...)` with a common
attribute contract:

- `modname`, `dsid`, `np`, `nn` — provenance and class counts
- `args` — the normalized arguments used to build it
- `src` — the upstream object, or `NA` when `keep_*` was `FALSE`.
  `.get_obj()` / `.get_obj_arg()` (`etc_utils.R`) walk this chain.
- `validated` — set by `.validate()`, which dispatches to the
  `.validate.<class>` method defined beside the constructor. Validators
  check item names, attribute names, and allowed arg names, then return the
  object. They short-circuit when `validated` is already `TRUE`.

Each pipeline stage function also accepts bare `scores`/`labels` instead of
its upstream object, via `.create_src_obj()`, so stages are individually
testable.

## R ↔ C++ boundary

All heavy computation is in `src/`, exposed with `// [[Rcpp::export]]`:

| File | Contents |
| --- | --- |
| `precrec_mmx.cpp` | input prep: `format_labels`, `get_score_ranks` (radix sort on an order-preserving `double`→`uint64_t` key) |
| `precrec_plx.cpp` | the pipeline: `create_confusion_matrices`, `calc_basic_metrics`, `create_roc_curve`, `create_prc_curve` (with non-linear interpolation), `calc_auc`, `calc_uauc`, `calc_uauc_frank`, `calc_avg_curve`, `calc_avg_points` |
| `precrec_misc.cpp/.h` | tie shuffling, point reduction, `convert_curve_df` / `convert_curve_avg_df` for `as.data.frame` |
| `RcppExports.*` | **generated** |

Conventions in this layer:

- `#define STRICT_R_HEADERS` at the top of every `.cpp` — required, CRAN
  checks depend on it.
- C++ functions return a list with an `errmsg` element instead of throwing.
  The R side must call `.check_cpp_func_error(obj, "func_name")` right after,
  which converts a non-empty `errmsg` into a `stop()`.
- Use R's RNG (`unif_rand()` via `randWrapper`), never `std::random_shuffle`
  or `std::rand` — reproducibility under `set.seed()` and CRAN policy.
- Each C++ block is commented with the R file and R function that calls it;
  keep those headers accurate when moving code.
- A loop that scans a whole `Rcpp` vector reads it through a raw pointer
  (`const double* p = v.begin()`), not through `operator[]`. That operator
  goes through a proxy whose bounds check is a call to `warning()`, and in a
  translation unit the size of `precrec_plx.cpp` the compiler leaves the call
  out of line — worth up to 1.8x on the loops that read several vectors. Say
  in a comment what makes the index safe; `precrec_plx.cpp` has the pattern.

## The metric table

`.basic_metric_table()` in `etc_utils.R` is the single list of basic
evaluation metrics. One row per metric, with:

| Column | Used by |
| --- | --- |
| `name` | everything the user names a metric by |
| `short` | the class-item names of a `points` object, and the `Meas.` column `print` shows |
| `desc` | the legend `print.beval_info()` writes |
| `default` | whether an object holds it when `evalmod(metrics = )` is not given |
| `range` | `"unit"`, `"signed"` or `"free"` — the y axis the metric needs |

**Two bugs in two consecutive releases came from a lookup keyed on one of
the two naming schemes and missing the other**: the 0.15.0 panel-title bug
read a factor by its level code, and `.is_signed_metric()` listed the long
names while the base-R plotting path holds the short ones, so informedness
and markedness were drawn on the wrong axis. `.metric_range()` accepts both.
Prefer the table over a fresh list of names.

Fourteen metrics are `default = TRUE`; the rest are the ROCR-parity
metrics, off unless asked for. They are derived in R in
`.add_derived_metrics()` (`pl4_calc_metrics.R`) from columns the C++ layer
already produced or from the confusion-matrix counts behind them — no C++
change, and nothing computed for a metric nobody asked for. The length
check in `.validate.pevals()` is what pins a derived column to the same rank
grid as its siblings.

`.basic_metric_aliases()` holds the other names a metric answers to, ROCR's
identifiers included. `.pmatch_added_metric()` is tried **last** in
`.pmatch_curvetype_basic()`, so a prefix that used to reach one of the
original metrics still does: `"f"` is the F-score, `"l"` is the label.

## The joinable-pair registry

`.joinable_pairs()` in `main_metric_curve.R` lists the ordered metric pairs
that may be joined by a line. It exists because the basic-metric table
holds raw per-cutoff points with no interpolation, so joining an arbitrary
pair of them with straight lines is the error this package exists to
prevent.

A pair in the registry is **not calculated by `metric_curve()` at all** — it
is handed to the curve pipeline, and the result is what that pipeline
produced. One implementation, one answer. Everything else is drawn as
points. Adding a row is how a new joinable pair is registered; the drawing
code reads this table and nothing else.

## Curve accuracy notes

The package's reason for existing is that naive PR-curve code is wrong. The
behaviors below are load-bearing — changing them changes published results:

- Non-linear interpolation between supporting points for PR curves
  (`interpolate_prc` in `precrec_plx.cpp`), linear for ROC.
- Elongation to the y-axis to estimate the first PR point.
- Score-wise thresholds, not fixed bins. `x_bins` controls only the
  *output* sampling of an already-exact curve; `interpolate = FALSE` disables
  binning and averaging entirely.
- Ties are resolved by `ties_method` (`"equiv"`, `"random"`, `"first"`) and
  missing scores by `na_worst`, both applied at ranking time in `mm3`.
