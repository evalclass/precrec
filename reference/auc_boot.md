# Bootstrap AUCs from one test set

`auc_boot` resamples a single test set and recalculates the areas under
the ROC and precision-recall curves on each resample. The result carries
the whole bootstrap distribution, which
[`auc_ci()`](https://evalclass.github.io/precrec/reference/auc_ci.md)
turns into confidence intervals and
[`auc_diff()`](https://evalclass.github.io/precrec/reference/auc_diff.md)
into comparisons between models.

## Usage

``` r
auc_boot(mdat, scores = NULL, labels = NULL, boot_n = 1000, seed = NULL, ...)
```

## Arguments

- mdat:

  An `mdata` object created by
  [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
  holding exactly one dataset. It can be omitted when `scores` and
  `labels` are given.

- scores:

  A numeric vector, matrix, array, data frame, or list of scores. See
  [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
  for the accepted shapes.

- labels:

  A numeric, character, logical, or factor vector of observed labels, or
  a list of such vectors.

- boot_n:

  The number of bootstrap resamples. The default of 1000 is the usual
  compromise; the tails of a percentile interval are estimated from
  `alpha / 2` of these, so 25 resamples at each end of a 95% interval,
  and 2000 or more is worth the wait when the interval itself is the
  result being reported.

- seed:

  A seed for the resampling, so that a reported interval can be
  reproduced. The global random number state is restored afterwards, so
  a seeded call does not disturb the stream the caller is drawing from.
  `NULL`, the default, resamples from the current state.

- ...:

  Further arguments passed to
  [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
  when `scores` and `labels` are given instead of `mdat`.

## Value

An object of class `aucboot`: a data frame of one row per model, curve
type and resample, with the columns `modnames`, `curvetypes`, `boot_id`
and `aucs`. The AUCs of the original data are attached as the `observed`
attribute.

## Details

The rest of the package builds its intervals from the variation between
several test sets, so
[`auc_ci()`](https://evalclass.github.io/precrec/reference/auc_ci.md) on
an `evalmod` result needs several and says so. This is the answer for
the ordinary case of one test set, where the only sample there is has to
stand in for the population.

## Resampling

The resampling is **stratified**: positives are drawn from the positives
and negatives from the negatives, so every resample holds exactly the
class balance the original does. An unstratified bootstrap of an
imbalanced dataset produces resamples with a different balance, and
sometimes with no positives at all, which moves the precision-recall
baseline underneath the quantity being estimated.

Every model is resampled on the **same** draws. Comparisons between
models are then paired, which is what makes
[`auc_diff()`](https://evalclass.github.io/precrec/reference/auc_diff.md)
a statement about the difference rather than about two independent
intervals.

## See also

[`auc_ci()`](https://evalclass.github.io/precrec/reference/auc_ci.md)
for the intervals,
[`auc_diff()`](https://evalclass.github.io/precrec/reference/auc_diff.md)
for comparing two models,
[`auc_delong()`](https://evalclass.github.io/precrec/reference/auc_delong.md)
for an exact standard error of the ROC AUC, and
[`auc()`](https://evalclass.github.io/precrec/reference/auc.md) for the
point estimates.

## Examples

``` r

## One test set, two models
samps <- create_sim_samples(1, 100, 100, c("poor_er", "good_er"))
mdat <- mmdata(samps[["scores"]], samps[["labels"]],
  modnames = samps[["modnames"]]
)

booted <- auc_boot(mdat, boot_n = 200, seed = 42)
auc_ci(booted)
auc_diff(booted)
```
