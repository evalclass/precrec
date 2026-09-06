# precrec 0.16.2

* Rewrite the package website. The single long `Introduction` vignette is now
  a short `Get started` page plus three sets of short articles, reachable
  from new `How-to`, `Measures` and `Plots` menus: one page per task, one
  page per family of measures, and one page per plot. The vignette shipped
  with the package is the `Get started` page; the rest are on the website
  only, so nothing was added to the size of the package.

* Update `README.md` to match, and to describe the measures added in 0.15.0
  and 0.16.0.

* Shrink the logo. `man/figures/logo.png` was 3392 x 3392 pixels and 430 KB,
  and ships inside the package; it is now 240 x 240 and 13 KB, which is the
  size the `README` and the website actually display. The favicons are
  regenerated from it.

# precrec 0.16.1

* Fix the order in which instances with tied scores are returned. The sort
  left the order of equal scores to the C++ standard library, which meant
  the `label` column of `as.data.frame(evalmod(mode = "basic"))`, and the
  label panel `plot()` and `autoplot()` draw from it, could come out in a
  different order on macOS than on Linux for the same data. Tied scores now
  keep the order they were given in, on every platform. No evaluation
  measure changes; only the order of the tied instances themselves.

# precrec 0.16.0

* Add `metric_curve()`, which takes the name of a measure for the x axis and
  the name of a measure for the y axis and draws one against the other, the
  way `ROCR::performance()` does. Every measure `evalmod()` can calculate is
  available on both axes, under its own name or under the `ROCR` identifier.
  It returns an `ssxycurves`, `msxycurves`, `smxycurves` or `mmxycurves`
  object, chosen the way `evalmod()` chooses between its own, and the object
  works with `print()`, `as.data.frame()`, `fortify()`, `plot()` and
  `autoplot()`.

  Only two pairs are joined by a line: false positive rate against
  sensitivity, which is the ROC curve, and sensitivity against precision,
  which is the precision-recall curve. Those two have a defined
  interpolation, and for them `metric_curve()` hands the work to the same
  code `evalmod(mode = "rocprc")` uses, so the two cannot disagree. Every
  other pair is drawn as points, because joining raw per-cutoff points with
  straight lines is the error this package was written to avoid; pass
  `type = "l"` to join them anyway.

  `metric_curve()` draws one curve per test dataset and does not average
  over them. An average needs a rule for interpolating between the points of
  each curve, which is what an unregistered pair does not have.

* Add the evaluation measures `ROCR` provides that `precrec` did not:
  `fpr`, `fnr`, `false_discovery_rate`, `false_omission_rate`,
  `predicted_positive_rate`, `predicted_negative_rate`, `lift`, `odds`,
  `mi`, `chisq` and `cost`. Each also answers to the identifier `ROCR` uses
  for it - `fall`, `miss`, `pcfall`, `pcmiss`, `rpp`, `rnp`,
  `mutual_information` - and to its standard abbreviation where it has one,
  so a call written against `ROCR` keeps working. `evalmod()` gains
  `cost_fp` and `cost_fn` for the two weights the `cost` measure takes;
  with the default weights of `1` it is the error rate.

  They are not calculated unless asked for. `evalmod()` gains a `metrics`
  argument that names the measures to add, or takes `"all"`; the default
  `NULL` is the fourteen measures the function has always returned, so an
  existing call gets the same object with the same fourteen `plot()` and
  `autoplot()` panels. A measure that was not calculated cannot be plotted,
  and the error says which argument asks for it.

  The odds ratio and the chi-square statistic are `NA` at the top and the
  bottom of every dataset, where the 2x2 table has an empty cell and neither
  is defined. `ROCR` reports an infinity or a `NaN` there. `NA` is what
  `precision` and `npv` already do with their own undefined end, and it
  keeps an infinity off a shared axis. The mutual information is `0` at
  those two points rather than `NA`, because a cutoff that predicts one
  class for everything carries no information about the labels - that value
  is defined, and it is zero.

* Add `prbe()`, which finds the points of a precision-recall curve at which
  precision and recall are equal. It takes the object `evalmod()` returns and
  gives back a data frame with one row per break-even point, in the manner of
  `auc()`.

  `ROCR::performance(pred, "prbe")` interpolates linearly between adjacent
  raw precision-recall points to find the crossing, which is not correct and
  is the reason this package exists; `prbe()` reads the crossing off the
  curve `evalmod()` has already interpolated properly.

* Add the `sar` measure, the mean of accuracy, the AUC of the ROC curve, and
  one minus the root mean squared error. Like the other added measures it is
  opt-in through `evalmod(metrics = )`. The RMSE reads the values of the
  scores rather than their ranks, so `sar` warns and returns `NA` when the
  scores are not probabilities between 0 and 1; every other measure asked for
  in the same call is still returned.

* Add the root mean squared error to `prob_metrics()` and
  `prob_metrics_ci()`, as the `"rmse"` metric. It is the square root of the
  Brier score, so each model and dataset now takes up three rows rather than
  two.

* Declare `stats` in `Imports`. It was used but not listed.

* Fix the y axis of `plot()` for informedness and markedness. Both run from
  -1 to 1, and both were drawn on a 0 to 1 axis, which cut off the negative
  half of the curve. `autoplot()` was never affected. The axis range of a
  measure now comes from one table rather than from a list of names that had
  the internal short names missing from it.

# precrec 0.15.0

* Support datasets with more than two classes, by one-vs-rest decomposition.
  Pass a matrix with one score column per class together with the class
  labels and `mmdata()` builds one binary problem per class, carried on the
  model axis, so `evalmod()`, `plot()`, `autoplot()`, `as.data.frame()` and
  the averaging all treat the classes as they would several models on one
  test set. `mmdata()` and `evalmod()` gain a `multiclass` argument
  (`"none"` or `"ovr"`), detected from the input when it is left unset;
  binary input is read exactly as before. `auc()` gains a `macro` argument
  and reports the macro-average of the per-class AUCs alongside them. The
  new `C3N150` sample dataset is a 3-class example.

  Each one-vs-rest decomposition has its own class balance, so the baseline
  of a precision-recall curve differs from class to class. The plots now
  leave that baseline out whenever the datasets do not share a prevalence,
  rather than drawing one line that fits none of them.

* Handle a dataset in which every label belongs to the same class instead of
  stopping on it. `evalmod(mode = "basic")` now warns and calculates what
  it can - accuracy and error rate are defined, specificity without
  negatives and sensitivity without positives come back as `NA` - where it
  used to stop. ROC and precision-recall curves remain undefined for such a
  dataset, so `evalmod()` still stops by default; the new
  `on_single_class = "na"` asks it to warn and return `NA` instead, so that
  one degenerate fold of an n-fold run no longer aborts the whole
  evaluation. `auc_ci()` and `prob_metrics_ci()` leave those `NA`s out of
  the interval and report how many datasets it was built from.

* Add five confusion-matrix measures to `evalmod(mode = "basic")`: balanced
  accuracy, negative predictive value, informedness (Youden's J), markedness,
  and Cohen's kappa. They are calculated in the same pass as the existing
  measures and appear as new `curvetype` values in `as.data.frame()`,
  `plot()`, `autoplot()`, `fortify()` and `print()`. Code that plots the
  default set of measures now gets fourteen panels instead of nine; pass
  `curvetype` to pick a subset. `evalmod(mode = "basic")` does about a third
  more work for them and holds about 40% more memory at peak;
  `evalmod()` in its default mode is unaffected, because the curves are drawn
  from three measures and the rest are no longer built for it.

* Add a `beta` argument to `evalmod()`, which generalizes `fscore` to the
  F-beta score. The default `beta = 1` is the F1 score the function has always
  returned, so existing results are unchanged.

* Add `prob_metrics()` and `prob_metrics_ci()`, which calculate the Brier
  score and the log loss of prediction scores together with their confidence
  intervals over multiple test datasets. Both measures read the values of the
  scores rather than their ranks, so the scores must be probabilities;
  anything outside the range 0 to 1 is rejected with a
  `precrec_error_invalid_scores` condition.

* Replace the `vdiffr` plot tests with machine-independent snapshots, and
  drop `vdiffr` from `Suggests`. The tests now record what a plot is made of
  -- its panels, their titles and axis labels, and the x/y/group data behind
  every layer -- as text, instead of comparing a rendered SVG. Rendering
  brought in the local font metrics, so a baseline belonged to one machine,
  could not be committed and had to be skipped on CI. The new baselines live
  in `tests/testthat/_snaps/` under version control, the comparison runs
  everywhere including CI, and a failure names what changed.

* Stop `autoplot()` and `fortify()` warning "Arguments in `...` must be used"
  on every call. The `fortify` methods accept `raw_curves` and
  `reduce_points` for a common interface, but a single test dataset has no
  average to contrast a raw curve with and the basic measures have no point
  reduction, so those methods never read the argument -- which the ggplot2
  `fortify` generic reported as a possible misspelling. The behavior is
  unchanged; the arguments are now consumed explicitly and documented as
  having no effect on those objects.

* Fix the panel titles of `autoplot()` in `mode = "basic"`. The measure name
  arrives from the plot data as a factor, and the title lookup read it by its
  level code instead of its name, so the `label` panel was titled "MCC" and
  the `mcc`, `npv` and `balanced_accuracy` panels were left untitled.

* Require R >= 4.1 (was R >= 3.2.1)

* Migrate the unit tests to testthat edition 3, and run test files in parallel

* Remove the deprecated `context()` calls from the unit tests

* Convert the roxygen2 documentation to markdown. Links to functions now
  render with parentheses, such as `evalmod()`, and links to other packages
  are qualified, such as `gridExtra::arrangeGrob()`. The rendered help pages
  are otherwise unchanged.

* Replace `assertthat` with `cli` and `rlang` for argument validation.
  `assertthat` is no longer a dependency. Errors raised by argument checks now
  carry condition classes, so they can be caught by class rather than by
  message: `precrec_error_invalid_<argument>`, `precrec_error_invalid_arg` and
  `precrec_error`. The wording of these messages has changed, but the same
  inputs are accepted and rejected as before.

* Suggest the nearest valid value when an argument is given one that is not
  allowed. `evalmod(mode = "ROCPRC")` now points at `rocprc` in the error, and
  a `curvetype` with a typo in it is matched against the measure
  names.
  A value that resembles nothing in the set is reported as before, without a
  guess.

* Fix the error raised when an argument that must be a whole number is given
  an infinite one. `evalmod(x_bins = Inf)` failed with R's own "missing value
  where TRUE/FALSE needed" rather than a `precrec` condition, because
  `Inf %% 1` is `NaN`.

* Use `checkmate` for the argument type checks behind the existing helpers.
  The messages and the condition classes are unchanged; `checkmate` and its
  only dependency, `backports`, are the new `Imports`.

* Replace the `apply` family with a small internal `.map_*` family. All 44
  `lapply`, `vapply` and `Filter` calls in the package now go through helpers
  named after their purrr equivalents, and most of them through a typed one
  that states what the call returns. No dependency was added and no behavior
  changed: the plot snapshots, the correctness suite and the benchmarks are
  the same before and after.

* Add the package website to `URL` in `DESCRIPTION`, and rebuild the pkgdown
  site with the Bootstrap 5 template.

* Add `inst/WORDLIST` and a spell-check test, plus a committed `.lintr`
  configuration.

* Fix the ranking of `NA` scores when `na_worst = TRUE`. The sentinel value
  used for `NA` was `DBL_MIN`, the smallest *positive* double, so `NA`s
  outranked every negative score instead of being ranked last. Results change
  only when the scores contain both `NA`s and negative values: `evalmod()`
  now returns the same curves and AUCs for such input as it does when a
  constant is added to every score. `na_worst = FALSE` was never affected.

* Calculate the standard errors of averaged curves and averaged points with
  Welford's algorithm instead of `E[x^2] - E[x]^2`. The previous formula lost
  precision through catastrophic cancellation and clamped the resulting
  negative variances to zero. Confidence bands from `evalmod(calc_avg = TRUE)`
  change at around the 1e-8 level.

* Add `as.data.table()` methods for the objects `evalmod()` returns, so the
  table `precrec` builds internally can be had without a conversion. They
  accept the same arguments as `as.data.frame()` and hold the same content.
  `data.table` was already a dependency.

* Build the internal tables with `data.table`. The public contract is
  unchanged: `as.data.frame()`, `fortify()`, `auc()`, `pauc()` and
  `auc_ci()` still return plain data frames, with the same columns, types
  and row order as before. Tables reached through the returned frame can no
  longer be modified by reference.

* Speed up the pure-R conversion path used when `use_rcpp = FALSE`. It grew
  the result with `rbind()` once per curve, copying everything collected so
  far on every pass. Over 20 test datasets it is now around 38 times faster
  and allocates around 43 times less memory. `auc_ci()` and the basic
  measure summary had the same pattern and got the same treatment. The
  default `Rcpp` path was never affected.

* Speed up `as.data.frame()` and `autoplot()` for large curve objects, and
  cut the memory they need. The C++ converter filled a C++ buffer and then
  copied it into the vectors it returned; it now fills those vectors
  directly. Converting a curve object built from 200,000 observations is
  around 27% faster, and one pass of `evalmod()` plus `as.data.frame()` over
  1,000,000 observations peaks at 584 MB instead of 897 MB in
  `mode = "basic"`, and 424 MB instead of 470 MB for ROC and
  precision-recall curves.

* Speed up `mmdata()` by around 25%. Scores are sorted through a comparison
  the sort can inline, rather than through a function pointer, which it
  could not. The resulting ranks are unchanged.

* Speed up the averaging of basic evaluation measures across datasets,
  which `evalmod(calc_avg = TRUE, mode = "basic")` performs. Collecting the
  distinct x values used a `std::set` and a `std::map`, each allocating a
  node per value and following a pointer per lookup, once for every point
  of every dataset; a sorted vector and a binary search replace them. It is
  around 1.6 times faster at 100,000 observations and above.

# precrec 0.14.5

* Restructure unit tests for svg comparisons with vdiff

* Reformat signatures of S3 methods

# precrec 0.14.4

* Update unit tests to avoid is.atomic(NULL) issue

* Update argument names of S3 functions to keep them consistent

# precrec 0.14.3

* Use new R setup configurations of GitHub actions

# precrec 0.14.2

* Update the signature of the fortify generic function 

# precrec 0.14.0

* Use tidy aesthetics with aes() and sym() instead of aes_string()

# precrec 0.13.1

* Use patchwork to combine multiple plots instead of using grid and gridExtra

* Test ggplot results with vdiffr

# precrec 0.12.9

* Fix another case of incorrect assignment of dsid_modnames

# precrec 0.12.8

* Fix incorrect dsid_modnames when data.frame is created by fortify()

# precrec 0.12.7

* Use STRICT_R_HEADER in Rcpp source files
* Skip several unit tests on CRAN submission

# precrec 0.12.5

* Update citation

# precrec 0.12.4

* Transfer the GitHub repository to evalclass/precrec

# precrec 0.12.2

* Skip ggplot2 unit tests
* Change default branch to main

# precrec 0.12.1

* Replace std::random_shuffle with a new function 

# precrec 0.12

* Improve error messages when a data set includes only one class  
* Improve code quality using the results from lintr and CodeFactor.io

# precrec 0.11.2

* format_nfold function returns labels as integer even given as factor  

# precrec 0.11.1

* Update test cases to treat c(factor) as factor since c(factor) does not return integer anymore 

# precrec 0.11

* Add auc\_ci function for CI calculation of AUC scores

# precrec 0.10.1

* Remove src/Makevars to keep .so file unstripped

# precrec 0.10

* Fix Rcpp header for STRICT_R_HEADERS
* Strip symbols of .so file

# precrec 0.9.1

* Fix a bug with as.data.frame when multiple datasets given

* Add format_nfold function to convert a dataframe with n-fold data to a list

# precrec 0.8

* Add 'aucroc' mode for fast AUC (ROC)

* Change how to treat 'show_cb' and 'raw_curves' options

# precrec 0.7.1

* Add precrec_init.c to avoid CRAN warnings

# precrec 0.7

* Add reduce_points option to autoplot

# precrec 0.6.2

* Fix mdat print

# precrec 0.6.1

* Improve GitHub pages

# precrec 0.6

* Improve as.data.frame with Rcpp

* Create GitHub pages with pkgdown

# precrec 0.5.2

* Update README

# precrec 0.5

* Add partial AUCs

# precrec 0.4

* Add new measures
    * Matthews correlation coefficient
    * F-score
    
* New generic function
    * as.data.frame works on precrec S3 objects

# precrec 0.3

* Improved the testing environment
    * unit tests
    * codecov
    
* Improved several documents


# precrec 0.2

* Improved several documents
    * several help files (.Rd)
    * package vignette
    * README

# precrec 0.1

* The first release version of `precrec`

* The package offers five functions
    * evalmod
    * mmdata
    * join\_scores
    * join\_labels
    * create\_sim\_samples
