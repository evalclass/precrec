# DeLong's standard error for the ROC AUC

`auc_delong` calculates the ROC AUC of every model on one test set
together with the variance and covariance of those AUCs, exactly rather
than by resampling.
[`auc_ci()`](https://evalclass.github.io/precrec/reference/auc_ci.md)
turns the result into confidence intervals and
[`auc_diff()`](https://evalclass.github.io/precrec/reference/auc_diff.md)
into comparisons between models.

## Usage

``` r
auc_delong(mdat, scores = NULL, labels = NULL, ...)
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

- ...:

  Further arguments passed to
  [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
  when `scores` and `labels` are given instead of `mdat`.

## Value

An object of class `aucdelong`: a data frame of one row per model, with
the columns `modnames`, `curvetypes`, `aucs` and `error`, the last being
the standard error of the AUC. The covariance matrix of the AUCs is
attached as the `cov` attribute, and is what makes the comparison in
[`auc_diff()`](https://evalclass.github.io/precrec/reference/auc_diff.md)
a paired one.

## Details

The ROC AUC is a Mann-Whitney U statistic, so its variance follows from
the structural components of that statistic and needs no bootstrap. That
makes this the exact counterpart of
[`auc_boot()`](https://evalclass.github.io/precrec/reference/auc_boot.md):
same input, same two functions reading the result, no `boot_n`, no seed,
nothing that changes between two runs, and no floor under the p-value.

## The ROC AUC only

There is no precision-recall counterpart of this. The ROC AUC is a U
statistic - the probability that a random positive outranks a random
negative - and everything below rests on that. The precision-recall AUC
is not one, and the interpolated area
[`auc()`](https://evalclass.github.io/precrec/reference/auc.md) reports
is further from being one still, so it has no analytic variance to
report and does not appear in the result.

[`auc_boot()`](https://evalclass.github.io/precrec/reference/auc_boot.md)
remains the answer for the precision-recall AUC, and the one to reach
for whenever the precision-recall curve is the point. This function is
here because the ROC AUC is what a great many readers and reviewers ask
about by name, and because it is exact where the bootstrap is
approximate.

## How the variance is calculated

Write `m` for the positives and `n` for the negatives. Each positive `i`
and each negative `j` contributes a structural component - the share of
the other class it beats:

    V10[i] = (1 / n) * sum over j of psi(x[i], y[j])
    V01[j] = (1 / m) * sum over i of psi(x[i], y[j])

where `psi` is 1 when the positive outranks the negative, 0.5 when they
are tied and 0 otherwise - the same half credit for a tie that the AUC
itself gives. The variance is then

    var(auc) = var(V10) / m + var(V01) / n

and for two models on the same test set the covariance is the same
expression with the covariance of their components. Both are computed
from midranks rather than from all `m * n` comparisons, so the cost is a
sort rather than a product.

The components are taken from the ranks `precrec` already assigns, so
ties and `NA` scores are handled exactly as they are everywhere else in
the package and the AUC reported here is the one
[`auc()`](https://evalclass.github.io/precrec/reference/auc.md) reports.

## What it assumes

The variance is an **asymptotic** one. It describes how the AUC would
move over repeated samples of the same size, and the interval built from
it is a normal one, so both get better as the test set grows. On a small
or badly imbalanced test set the percentile interval of
[`auc_boot()`](https://evalclass.github.io/precrec/reference/auc_boot.md)
is the safer reading, and comparing the two is a cheap way to find out
whether the sample is large enough for this one.

## See also

[`auc_boot()`](https://evalclass.github.io/precrec/reference/auc_boot.md)
for the resampling counterpart, which also covers the precision-recall
AUC,
[`auc_ci()`](https://evalclass.github.io/precrec/reference/auc_ci.md)
for the intervals and
[`auc_diff()`](https://evalclass.github.io/precrec/reference/auc_diff.md)
for comparing models.

## Examples

``` r

## One test set, two models
samps <- create_sim_samples(1, 100, 100, c("poor_er", "good_er"))
mdat <- mmdata(samps[["scores"]], samps[["labels"]],
  modnames = samps[["modnames"]]
)

delong <- auc_delong(mdat)
delong
#>   modnames curvetypes   aucs      error
#> 1  poor_er        ROC 0.7845 0.03210423
#> 2  good_er        ROC 0.8512 0.02794278

auc_ci(delong)
auc_diff(delong)
```
