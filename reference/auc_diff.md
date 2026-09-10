# Compare AUCs between models

`auc_diff` reports, for every pair of models, the difference between
their AUCs with a confidence interval around it. It reads either of the
two estimates of uncertainty the package offers for a single test set:
the resamples of
[`auc_boot()`](https://evalclass.github.io/precrec/reference/auc_boot.md),
which cover both curve types, or the analytic covariance of
[`auc_delong()`](https://evalclass.github.io/precrec/reference/auc_delong.md),
which covers the ROC AUC exactly.

## Usage

``` r
auc_diff(x, alpha = NULL, alternative = NULL)

# S3 method for class 'aucboot'
auc_diff(x, alpha = 0.05, alternative = "two.sided")

# S3 method for class 'aucdelong'
auc_diff(x, alpha = 0.05, alternative = "two.sided")
```

## Arguments

- x:

  An `aucboot` object from
  [`auc_boot()`](https://evalclass.github.io/precrec/reference/auc_boot.md)
  or an `aucdelong` object from
  [`auc_delong()`](https://evalclass.github.io/precrec/reference/auc_delong.md).

- alpha:

  The interval covers `1 - alpha` of the resampled differences, so the
  default of 0.05 gives a 95% interval.

- alternative:

  The side of zero the alternative hypothesis is on, one of
  `"two.sided"`, `"greater"` and `"less"`. `"greater"` tests whether the
  first model of the pair has the larger AUC. It selects the tail of
  every p-value in the result and leaves `z_values` alone; the interval
  stays two-sided whatever it is set to.

## Value

A data frame with one row per curve type and pair of models.

From an `aucboot` object:

|  |  |
|----|----|
| `curvetypes` | ROC or PRC |
| `modnames1`, `modnames2` | The pair, in the order compared |
| `diffs` | Observed AUC of the first minus that of the second |
| `lower_bound`, `upper_bound` | Percentile interval of the resampled differences |
| `p_values` | Percentile p-value, see below |
| `z_values` | Wald statistic, `diffs / sd(d)`, see below |
| `p_values_wald` | Wald p-value, see below |
| `n` | Resamples the row is based on |

From an `aucdelong` object, where there is one p-value rather than two
and only the ROC AUC to report:

|  |  |
|----|----|
| `curvetypes` | ROC |
| `modnames1`, `modnames2` | The pair, in the order compared |
| `diffs` | AUC of the first minus that of the second |
| `lower_bound`, `upper_bound` | Normal interval of the difference, clipped to `[-1, 1]` |
| `z_values` | `diffs` over its standard error |
| `p_values` | DeLong's p-value, see below |
| `n` | Instances in the test set |

## Details

The comparison is paired either way. With a bootstrap both models are
scored on the same resample, so the difference is calculated within a
resample and the spread of those differences is what the interval
describes; with DeLong the pairing is carried by the covariance between
the two AUCs. Two separate intervals from
[`auc_ci()`](https://evalclass.github.io/precrec/reference/auc_ci.md)
cannot be read this way - they can overlap while the difference is still
clearly on one side of zero, because they say nothing about how the two
models move together.

## DeLong's comparison

Given an `aucdelong` object the standard error of the difference comes
out of the covariance matrix,

    se = sqrt(var(auc1) + var(auc2) - 2 * cov(auc1, auc2))
    z_values = diffs / se
    p_values = 2 * pnorm(-abs(z_values))

and the covariance is what makes it a paired test: two models scored on
the same test set rise and fall together, and dropping the last term
would overstate how uncertain their difference is.

This is a Wald test too, so `alternative` selects its tail the same way.
What it does not need is `boot_n`: the standard error is exact rather
than resampled, so the p-value has no floor and does not move between
two runs. What it does assume is that the variance is asymptotic - see
[`auc_delong()`](https://evalclass.github.io/precrec/reference/auc_delong.md) -
and it has nothing to say about the precision-recall AUC.

`z_values` is `NA` when two models rank every instance the same way and
there is no standard error to divide by, and `p_values` is `NA` with it.

## The interval and the two p-values

This section and the two that follow describe an `aucboot` object.

The interval is the primary result, and is the `alpha / 2` and
`1 - alpha / 2` quantiles of the resampled differences.

The two p-values are the two ways of getting one out of a bootstrap, and
each column is named for the method that produced it. Write `d` for the
vector of `n` resampled differences and `diffs` for the observed one.

`p_values` is the **percentile** p-value - the share of `d` on the other
side of zero from `diffs`, doubled for a two-sided test:

    p_values = 2 * min(1 + sum(d <= 0), 1 + sum(d >= 0)) / (n + 1)

The added ones keep it away from exactly zero. It cannot go below
`2 / (n + 1)`, so `boot_n` sets a floor under it.

`z_values` and `p_values_wald` are the **Wald** test - an estimate over
an estimate of its standard error, referred to a standard normal:

    z_values      = diffs / sd(d)
    p_values_wald = 2 * pnorm(-abs(z_values))

`sd(d)` is the standard error, because the spread of the bootstrap
distribution is what the bootstrap has to say about how far the
difference moves from sample to sample. Having a scale underneath it
rather than a count of resamples, the Wald p-value has no floor.

`alternative` replaces the doubled minimum with the matching one-sided
count, and `-abs()` with `-` or `+`, in the two formulas above.

## Why two p-values

Because they fail in opposite ways, and neither one on its own tells you
that it is failing.

The percentile p-value assumes nothing about the shape of `d`, and pays
for that with the floor. Once it reaches `2 / (n + 1)` it has stopped
measuring the models and started reporting `boot_n`: a difference that
is merely clear and one that is overwhelming both come out at 0.002 at
the default thousand resamples, and the number gives no sign of which it
is looking at.

The Wald p-value has no floor, and pays for that with an assumption. It
takes `d` to be roughly normal, and locates the null by reflecting the
sampling distribution rather than by enforcing it. Where `d` is skewed -
few positives, or either model near the ceiling of the precision-recall
AUC - it is confidently wrong, and again the number carries no warning.

Side by side they cover each other. When they agree, the normality the
Wald test assumes is doing no harm at this sample size and you can quote
its resolution. When they disagree sharply, `d` is not the shape the
Wald test needs, and the percentile p-value - floor and all - is the one
to trust. The comparison is the diagnostic; neither column is one on its
own.

There is a second, exact sense in which they are different answers. A
Wald test is the counterpart of the bootstrap **normal** interval,
`diffs` plus and minus `z` standard errors, while `lower_bound` and
`upper_bound` are the **percentile** interval. So `p_values_wald` is not
the dual of the bounds reported beside it, and need not agree with them.

Read the interval first. Both p-values are companions to it rather than
exact tests.

## Not a t statistic

`z_values` divides by a standard error, not by a standard error of a
mean, and is read off the normal rather than off a t.
`auc_ci(dtype = "t")` is the genuine t in this package, and the contrast
is exact: there the spread is taken over a handful of real test sets and
divided by the square root of how many there were, so `n - 1` degrees of
freedom mean something. Here the spread is taken over resamples and
divided by nothing - `boot_n` is a setting rather than a sample size,
and a t on `boot_n - 1` degrees of freedom would be a p-value that
shrinks when the caller resamples harder.

Nor is this the bootstrap-*t*, which forms a statistic of its own inside
every resample and takes its reference distribution from those rather
than from the normal.

`z_values` is `NA` when the resamples have no spread to divide by, which
happens when two models are given the same scores, and `p_values_wald`
is `NA` with it.

## See also

[`auc_boot()`](https://evalclass.github.io/precrec/reference/auc_boot.md)
for the resampling,
[`auc_delong()`](https://evalclass.github.io/precrec/reference/auc_delong.md)
for the analytic alternative, and
[`auc_ci()`](https://evalclass.github.io/precrec/reference/auc_ci.md)
for one model at a time.

## Examples

``` r

samps <- create_sim_samples(1, 100, 100, c("poor_er", "good_er"))
mdat <- mmdata(samps[["scores"]], samps[["labels"]],
  modnames = samps[["modnames"]]
)

booted <- auc_boot(mdat, boot_n = 200, seed = 42)
auc_diff(booted)

## Is the second model the better one?
auc_diff(booted, alternative = "less")

## The same comparison without resampling, for the ROC AUC
auc_diff(auc_delong(mdat))
```
