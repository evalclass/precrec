## Version 0.23.1

An update of the published package `precrec` (0.14.5 -> 0.23.1). 0.15.0,
0.16.0, 0.16.1, 0.16.2, 0.17.0, 0.18.0, 0.19.0, 0.20.0, 0.21.0, 0.21.1,
0.21.2, 0.22.0, 0.22.1, 0.22.2 and 0.23.0 were prepared but not submitted,
so this release carries all sixteen sets of changes.

- Support datasets with more than two classes by one-vs-rest decomposition
- Add the evaluation metrics `ROCR` provides, opt-in through
  `evalmod(metrics = )`, plus confusion-matrix metrics, F-beta, and
  probability-based metrics
- Add one new dependency, `checkmate`, for the argument checks
- Add `metric_curve()`, which draws one evaluation metric against another
- Add `prbe()`, the precision-recall break-even point, and
  `average_precision()`, the step estimator of the area under the
  precision-recall curve
- Add the `roc_dist` and `sedi` metrics, and the `macro_weight` argument of
  `auc()` for weighting the per-class average by the class distribution
- Add the `jaccard` and likelihood-ratio metrics, and the D2 scores of
  `prob_metrics()`, for parity with `scikit-learn`
- Rename the evaluation quantities "measures" to "metrics" in the
  documentation and the `print()` output, matching the `metrics`
  arguments that have always selected them
- Add `classification_report()`, the per-class precision, recall and
  F-score table of `scikit-learn`'s function of that name
- Add `auc_boot()` and `auc_diff()`, which resample a single test set so
  that an AUC can carry a confidence interval, and two models can be
  compared, without several test sets
- Fix a buffer overrun in the curve interpolation, which sized its output
  by `x_bins` but built the points from a grid snapped to each gap
  separately, and so could emit more of them than it had allocated;
  `x_bins` is now capped at `1e6` as well
- Handle single-class datasets with a warning instead of an error
- Fix the ranking of `NA` scores, the panel titles of `autoplot()`, the
  y axis of `plot()` for the metrics that can go negative, and the order of
  instances with tied scores, which was left to the C++ standard library and
  so differed between platforms
- Raise the R dependency to >= 4.1, and modernize the tests and internals
- Replace the single long vignette with a short one; the rest of the
  documentation moved to the package website, so the tarball did not grow
- Speed up the C++ hot paths, so `evalmod()` runs about 1.5x faster on a
  million rows; the results are unchanged, bit for bit
- Let `reduce_points` thin the basic evaluation metrics for plotting, which
  carry one point per cutoff and so drew a mark per instance
- Add `evalmod(basic_ties = )` for what the basic metrics report at the
  cutoffs inside a run of tied scores; the default is the behavior the
  package has always had
- Add a Wald test to `auc_diff()` - the `z_values` and `p_values_wald`
  columns - together with an `alternative` argument for the tail it and the
  percentile p-value are read from; the percentile p-value `auc_diff()` has
  always reported is unchanged
- Add `metric_table()`, which returns every basic evaluation metric at
  every cutoff as one row per cutoff and one column per metric; it
  reshapes results `evalmod(mode = "basic")` already produces and
  calculates nothing new
- Add `best_cutoff()`, which picks the cutoff that optimizes one metric and
  returns the row of `metric_table()` at it, with the criteria documented in
  two groups by whether they know the prevalence
- Add `auc_delong()`, DeLong's analytic variance of the ROC AUC, which
  `auc_ci()` turns into a normal interval and `auc_diff()` into a paired
  comparison between models; `auc_diff()` becomes an S3 generic to take
  both it and the existing bootstrap object
- Document `print()`, whose six methods had no help page because nothing
  defined the topic they were attached to, and give them the invisible
  return value an S3 `print` method is expected to have
- Add `metric_table(at = )`, which reports the metrics of given thresholds
  rather than of every cutoff, so that a cutoff chosen on one dataset can
  be scored on another

`NEWS.md` has the full list.

## Test environments

- local Ubuntu 22.04 and macOS, R release
- win-builder, R devel
- GitHub Actions
    - macOS-latest (release)
    - windows-latest (release)
    - ubuntu-latest (devel, release, oldrel-1)

## R CMD check results

0 errors | 0 warnings | 1 note

- **NOTE** on Linux: sub-directories of 1Mb or more, because `precrec.so` is
  over 4 MB.
