# Compare bootstrapped AUCs between models

`auc_diff` takes the resamples of
[`auc_boot()`](https://evalclass.github.io/precrec/reference/auc_boot.md)
and reports, for every pair of models and every curve type, the
difference between their AUCs with a confidence interval around it.

## Usage

``` r
auc_diff(x, alpha = 0.05)
```

## Arguments

- x:

  An `aucboot` object from
  [`auc_boot()`](https://evalclass.github.io/precrec/reference/auc_boot.md).

- alpha:

  The interval covers `1 - alpha` of the resampled differences, so the
  default of 0.05 gives a 95% interval.

## Value

A data frame with one row per curve type and pair of models:

|  |  |
|----|----|
| `curvetypes` | ROC or PRC |
| `modnames1`, `modnames2` | The pair, in the order compared |
| `diffs` | Observed AUC of the first minus that of the second |
| `lower_bound`, `upper_bound` | Percentile interval of the resampled differences |
| `p_values` | See below |
| `n` | Resamples the row is based on |

## Details

The comparison is paired: both models are scored on the same resample,
so the difference is calculated within a resample and the spread of
those differences is what the interval describes. Two separate intervals
from
[`auc_ci()`](https://evalclass.github.io/precrec/reference/auc_ci.md)
cannot be read this way - they can overlap while the difference is still
clearly on one side of zero, because they say nothing about how the two
models move together.

## The interval and the p-value

The interval is the primary result, and is the `alpha / 2` and
`1 - alpha / 2` quantiles of the resampled differences.

`p_values` is the proportion of resampled differences falling on the
other side of zero from the observed one, doubled for a two-sided test,
and calculated as `2 * min(1 + sum(d <= 0), 1 + sum(d >= 0)) / (n + 1)`
so that it is never exactly zero - a bootstrap of `n` resamples cannot
report a p-value below about `2 / n`, and reporting one would be an
artifact of the resample count rather than evidence. It is a percentile
p-value, and the distribution is not shifted to sit under the null, so
read it as a companion to the interval rather than as an exact test.

## See also

[`auc_boot()`](https://evalclass.github.io/precrec/reference/auc_boot.md)
for the resampling and
[`auc_ci()`](https://evalclass.github.io/precrec/reference/auc_ci.md)
for one model at a time.

## Examples

``` r

samps <- create_sim_samples(1, 100, 100, c("poor_er", "good_er"))
mdat <- mmdata(samps[["scores"]], samps[["labels"]],
  modnames = samps[["modnames"]]
)

auc_diff(auc_boot(mdat, boot_n = 200, seed = 42))
```
