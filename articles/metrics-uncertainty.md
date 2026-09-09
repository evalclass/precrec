# Uncertainty from one test set

Every interval elsewhere in `precrec` is built from the variation
*between* test sets, so with one test set there is nothing to vary and
[`auc_ci()`](https://evalclass.github.io/precrec/reference/auc_ci.md)
says so.
[`auc_boot()`](https://evalclass.github.io/precrec/reference/auc_boot.md)
resamples the one sample you have instead.

``` r

library(precrec)

samps <- create_sim_samples(1, 100, 100, c("poor_er", "good_er"))
mdat <- mmdata(samps[["scores"]], samps[["labels"]],
  modnames = samps[["modnames"]]
)

booted <- auc_boot(mdat, boot_n = 500, seed = 42)
knitr::kable(auc_ci(booted))
```

| modnames | curvetypes |      aucs |      mean |     error | lower_bound | upper_bound |   n |
|:---------|:-----------|----------:|----------:|----------:|------------:|------------:|----:|
| poor_er  | ROC        | 0.8328000 | 0.8346590 | 0.0275162 |   0.7797475 |   0.8856250 | 500 |
| poor_er  | PRC        | 0.7860641 | 0.7950443 | 0.0419200 |   0.7162381 |   0.8786786 | 500 |
| good_er  | ROC        | 0.8180000 | 0.8178362 | 0.0307755 |   0.7488375 |   0.8709150 | 500 |
| good_er  | PRC        | 0.8574152 | 0.8571980 | 0.0243810 |   0.8021867 |   0.8986942 | 500 |

`aucs` is the AUC of the original data and `mean` the average over the
resamples; the gap between them is the bootstrap’s estimate of bias. The
bounds are quantiles of the resampled values - a percentile interval -
so they are values the AUC actually took, and cannot fall outside
`[0, 1]`. That is why `dtype` is refused here: there is no distribution
being assumed.

Pass `seed` to make an interval reproducible. It restores the random
stream afterwards, so a seeded call does not disturb whatever else you
are drawing from.

## Stratified, and shared

Positives are drawn from the positives and negatives from the negatives,
so every resample has exactly the class balance of the original. An
unstratified bootstrap of imbalanced data varies the balance from
resample to resample, and can draw one with no positives at all - which
moves the precision-recall baseline underneath the number being
estimated.

Every model is resampled on the *same* draws, which is what makes the
next section a comparison rather than two separate answers.

## Comparing two models

``` r

knitr::kable(auc_diff(booted))
```

| curvetypes | modnames1 | modnames2 | diffs | lower_bound | upper_bound | p_values | statistic | p_values_norm | n |
|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|
| ROC | poor_er | good_er | 0.014800 | -0.062155 | 0.0924050 | 0.6706587 | 0.3702657 | 0.7111846 | 500 |
| PRC | poor_er | good_er | -0.071351 | -0.154159 | 0.0278979 | 0.2155689 | -1.5169265 | 0.1292852 | 500 |

The difference is taken within each resample, so the interval describes
the difference itself. That is not the same as looking at two intervals
from
[`auc_ci()`](https://evalclass.github.io/precrec/reference/auc_ci.md)
and checking whether they overlap: two intervals can overlap while the
difference is clearly on one side of zero, because separate intervals
say nothing about how the two models move together.

`p_values` is the share of resampled differences on the far side of zero
from the observed one, doubled. It is floored at `2 / (n + 1)`, so 500
resamples cannot report anything below 0.004 - a smaller number would be
an artifact of the resample count rather than evidence. Read the
interval first; the p-value is a companion to it, not an exact test.

## The statistic and the second p-value

That floor is a problem when the result has to be written down as a
p-value. Take a pair of models that are not close:

``` r

clear <- create_sim_samples(1, 100, 100, c("poor_er", "excel"))
cmdat <- mmdata(clear[["scores"]], clear[["labels"]],
  modnames = clear[["modnames"]]
)

clear_diff <- auc_diff(auc_boot(cmdat, boot_n = 500, seed = 42))
# The normal p-value is far below what kable's default rounding shows
clear_diff[["p_values_norm"]] <- format(
  clear_diff[["p_values_norm"]],
  digits = 3
)

knitr::kable(clear_diff)
```

| curvetypes | modnames1 | modnames2 | diffs | lower_bound | upper_bound | p_values | statistic | p_values_norm | n |
|:---|:---|:---|---:|---:|---:|---:|---:|:---|---:|
| ROC | poor_er | excel | -0.2462000 | -0.3168000 | -0.1795725 | 0.003992 | -7.110897 | 1.15e-12 | 500 |
| PRC | poor_er | excel | -0.2729186 | -0.3449664 | -0.2004622 | 0.003992 | -7.289584 | 3.11e-13 | 500 |

The interval is nowhere near zero, and yet `p_values` sits on its floor
of 0.004, because that is the smallest number 500 resamples can support.
Getting below 0.001 that way means resampling ten thousand times or
more.

`statistic` and `p_values_norm` read a number off the resamples that are
already there. `statistic` divides the observed difference by the
standard deviation of the resampled ones, so the bootstrap spread stands
in for a standard error, and `p_values_norm` is that statistic read off
the normal distribution - here many orders of magnitude below the floor
the percentile p-value has run into.

The resolution is bought with an assumption, and it is worth knowing
which. Nothing is shuffled between the two models, so the null is never
enforced: the distribution being used is the sampling one, centered on
the observed difference and reflected to sit under zero. It also takes
the resampled differences to be roughly normal, which is weakest where
they are skewed - few positives, or either model near the ceiling of the
precision-recall AUC. The very small p-values it makes available are
therefore its least trustworthy numbers, and when the two p-values
disagree sharply it is the normal approximation to distrust.

`statistic` is `NA` when the resamples have no spread to divide by,
which is what two models given the same scores produce, and
`p_values_norm` is `NA` with it.

## One side or two

`alternative` picks the tail both p-values are read from. The default
`"two.sided"` asks whether the models differ; `"greater"` asks whether
the first of the pair has the larger AUC, and `"less"` whether the
second does. `poor_er` is the first of this pair, so `"less"` is the
question worth asking about it:

``` r

knitr::kable(auc_diff(booted, alternative = "less"))
```

| curvetypes | modnames1 | modnames2 | diffs | lower_bound | upper_bound | p_values | statistic | p_values_norm | n |
|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|
| ROC | poor_er | good_er | 0.014800 | -0.062155 | 0.0924050 | 0.6666667 | 0.3702657 | 0.6444077 | 500 |
| PRC | poor_er | good_er | -0.071351 | -0.154159 | 0.0278979 | 0.1077844 | -1.5169265 | 0.0646426 | 500 |

The interval stays two-sided whatever `alternative` is set to, so the
same bounds are there to read next to a one-sided p-value.

## How many resamples

`boot_n` defaults to 1000. The tails of a 95% interval are estimated
from about 25 resamples at each end of that, which is enough to see the
interval but coarse if the interval is the result you are publishing;
2000 or more is worth the wait then. Cost grows with `boot_n` and barely
with the size of the dataset, since the curves are computed in C++ and
all the resamples go through in one pass.

## When several test sets are better

If you have cross-validation folds or genuinely separate test sets, use
them:
[`auc_ci()`](https://evalclass.github.io/precrec/reference/auc_ci.md) on
an
[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
result measures variation between real samples, while the bootstrap can
only reuse the one sample it was given. A bootstrap interval inherits
whatever is unrepresentative about that sample, and cannot tell you so.

## Next

- [AUC and other curve
  summaries](https://evalclass.github.io/precrec/articles/metrics-auc.md)
- [Evaluate cross-validation
  folds](https://evalclass.github.io/precrec/articles/howto-cross-validation.md)
