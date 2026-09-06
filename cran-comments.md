## Version 0.18.0

An update of the published package `precrec` (0.14.5 -> 0.18.0). 0.15.0,
0.16.0, 0.16.1, 0.16.2 and 0.17.0 were prepared but not submitted, so this
release carries all six sets of changes.

- Support datasets with more than two classes by one-vs-rest decomposition
- Add the evaluation measures `ROCR` provides, opt-in through
  `evalmod(metrics = )`, plus confusion-matrix measures, F-beta, and
  probability-based metrics
- Add one new dependency, `checkmate`, for the argument checks
- Add `metric_curve()`, which draws one evaluation measure against another
- Add `prbe()`, the precision-recall break-even point, and
  `average_precision()`, the step estimator of the area under the
  precision-recall curve
- Add the `roc_dist` and `sedi` measures, and the `macro_weight` argument of
  `auc()` for weighting the per-class average by the class distribution
- Add the `jaccard` and likelihood-ratio measures, and the D2 scores of
  `prob_metrics()`, for parity with `scikit-learn`
- Handle single-class datasets with a warning instead of an error
- Fix the ranking of `NA` scores, the panel titles of `autoplot()`, the
  y axis of `plot()` for the measures that can go negative, and the order of
  instances with tied scores, which was left to the C++ standard library and
  so differed between platforms
- Raise the R dependency to >= 4.1, and modernize the tests and internals
- Replace the single long vignette with a short one; the rest of the
  documentation moved to the package website, so the tarball did not grow

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
