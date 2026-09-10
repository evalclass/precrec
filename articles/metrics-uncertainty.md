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

| curvetypes | modnames1 | modnames2 | diffs | lower_bound | upper_bound | p_values | z_values | p_values_wald | n |
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

`p_values` is the *percentile* p-value. Writing `d` for the resampled
differences and `n` for how many there are, it is the share of `d` on
the far side of zero from the observed difference, doubled:

    p_values = 2 * min(1 + sum(d <= 0), 1 + sum(d >= 0)) / (n + 1)

The added ones keep it away from exactly zero, which floors it at
`2 / (n + 1)`: 500 resamples cannot report anything below 0.004, and a
smaller number would be an artifact of the resample count rather than
evidence. Read the interval first; the p-value is a companion to it, not
an exact test.

## The Wald test

That floor is a problem when the result has to be written down as a
p-value. Take a pair of models that are not close:

``` r

clear <- create_sim_samples(1, 100, 100, c("poor_er", "excel"))
cmdat <- mmdata(clear[["scores"]], clear[["labels"]],
  modnames = clear[["modnames"]]
)

clear_diff <- auc_diff(auc_boot(cmdat, boot_n = 500, seed = 42))
# The Wald p-value is far below what kable's default rounding shows
clear_diff[["p_values_wald"]] <- format(
  clear_diff[["p_values_wald"]],
  digits = 3
)

knitr::kable(clear_diff)
```

| curvetypes | modnames1 | modnames2 | diffs | lower_bound | upper_bound | p_values | z_values | p_values_wald | n |
|:---|:---|:---|---:|---:|---:|---:|---:|:---|---:|
| ROC | poor_er | excel | -0.2462000 | -0.3168000 | -0.1795725 | 0.003992 | -7.110897 | 1.15e-12 | 500 |
| PRC | poor_er | excel | -0.2729186 | -0.3449664 | -0.2004622 | 0.003992 | -7.289584 | 3.11e-13 | 500 |

The interval is nowhere near zero, and yet `p_values` sits on its floor
of 0.004, because that is the smallest number 500 resamples can support.
Getting below 0.001 that way means resampling ten thousand times or
more.

`z_values` and `p_values_wald` are a *Wald* test, read off the resamples
that are already there. A Wald statistic is an estimate over an estimate
of its standard error, referred to a standard normal:

    z_values      = diffs / sd(d)
    p_values_wald = 2 * pnorm(-abs(z_values))

`sd(d)` is the standard error, because the spread of the bootstrap
distribution is what the bootstrap has to say about how far the
difference moves from sample to sample. Having a scale underneath it
rather than a count of resamples, it has no floor, and lands many orders
of magnitude below the one the percentile p-value has run into.

`z_values` is `NA` when the resamples have no spread to divide by, which
is what two models given the same scores produce, and `p_values_wald` is
`NA` with it.

## Why two p-values

Because they fail in opposite ways, and neither one on its own tells you
that it is failing.

The percentile p-value assumes nothing about the shape of `d`, and pays
for that with the floor. Once it reaches `2 / (n + 1)` it has stopped
measuring the models and started reporting `boot_n` - a difference that
is merely clear and one that is overwhelming both come out at 0.004 in
the table above, and nothing in the number says which one you are
looking at.

The Wald p-value has no floor, and pays for that with an assumption. It
takes `d` to be roughly normal, and locates the null by reflecting the
sampling distribution rather than by enforcing it: nothing is shuffled
between the two models. Where `d` is skewed - few positives, or either
model near the ceiling of the precision-recall AUC - it is confidently
wrong, and again the number carries no warning.

Side by side they cover each other. When they agree, the normality the
Wald test assumes is doing no harm at this sample size, and its
resolution is yours to quote. When they disagree sharply, `d` is not the
shape the Wald test needs, and the percentile p-value - floor and all -
is the one to trust. The comparison is the diagnostic; neither column is
one by itself.

There is also an exact sense in which they answer different questions. A
Wald test is the counterpart of the *normal* interval, the difference
plus and minus so many standard errors, while `lower_bound` and
`upper_bound` are the *percentile* interval. `p_values_wald` is
therefore not the dual of the bounds next to it, and need not agree with
them.

## Not a t statistic

`z_values` divides by a standard error, not by a standard error of a
mean, and is read off the normal rather than off a t. The contrast with
`auc_ci(dtype = "t")` on cross-validation folds, which is a genuine t,
is exact: there the spread is taken over a handful of real test sets and
divided by the square root of how many there were, so `n - 1` degrees of
freedom mean something. Here the spread is taken over resamples and
divided by nothing - `boot_n` is a setting rather than a sample size,
and a t on `boot_n - 1` degrees of freedom would be a p-value that
shrinks when you resample harder.

Nor is it the bootstrap-*t*, which forms a statistic of its own inside
every resample and takes its reference distribution from those rather
than from the normal.

## One side or two

`alternative` picks the tail the percentile and the Wald p-value are
read from. The default `"two.sided"` asks whether the models differ;
`"greater"` asks whether the first of the pair has the larger AUC, and
`"less"` whether the second does. `poor_er` is the first of this pair,
so `"less"` is the question worth asking about it:

``` r

knitr::kable(auc_diff(booted, alternative = "less"))
```

| curvetypes | modnames1 | modnames2 | diffs | lower_bound | upper_bound | p_values | z_values | p_values_wald | n |
|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|
| ROC | poor_er | good_er | 0.014800 | -0.062155 | 0.0924050 | 0.6666667 | 0.3702657 | 0.6444077 | 500 |
| PRC | poor_er | good_er | -0.071351 | -0.154159 | 0.0278979 | 0.1077844 | -1.5169265 | 0.0646426 | 500 |

The interval stays two-sided whatever `alternative` is set to, so the
same bounds are there to read next to a one-sided p-value.

## DeLong, without resampling

The ROC AUC is a Mann-Whitney U statistic - the probability that a
random positive outranks a random negative - so its variance can be
written down rather than resampled.
[`auc_delong()`](https://evalclass.github.io/precrec/reference/auc_delong.md)
does that.

``` r

delong <- auc_delong(mdat)

knitr::kable(auc_ci(delong))
```

| modnames | curvetypes |   aucs |     error | lower_bound | upper_bound |   n |
|:---------|:-----------|-------:|----------:|------------:|------------:|----:|
| poor_er  | ROC        | 0.8328 | 0.0285145 |   0.7769127 |   0.8886873 | 200 |
| good_er  | ROC        | 0.8180 | 0.0299393 |   0.7593200 |   0.8766800 | 200 |

No `boot_n`, no seed, and nothing that moves between two runs. Compare
it with the bootstrap on the same data:

``` r

boot_roc <- subset(auc_ci(booted), curvetypes == "ROC")

knitr::kable(
  data.frame(
    modnames = boot_roc[["modnames"]],
    bootstrap = boot_roc[["error"]],
    delong = auc_ci(delong)[["error"]]
  ),
  digits = 5
)
```

| modnames | bootstrap |  delong |
|:---------|----------:|--------:|
| poor_er  |   0.02752 | 0.02851 |
| good_er  |   0.03078 | 0.02994 |

The comparison between two models works the same way, and the pairing is
carried by the covariance between the two AUCs rather than by resampling
them together.

``` r

knitr::kable(auc_diff(delong))
```

| curvetypes | modnames1 | modnames2 | diffs | lower_bound | upper_bound | z_values | p_values | n |
|:---|:---|:---|---:|---:|---:|---:|---:|---:|
| ROC | poor_er | good_er | 0.0148 | -0.0632001 | 0.0928001 | 0.3718902 | 0.7099746 | 200 |

Both routes put the difference at 0.083 with a p-value near 0.062. That
agreement is the useful part: when the exact answer and the resampled
one land in the same place, the normal approximation is doing no harm at
this sample size.

### The ROC AUC only

There is no precision-recall counterpart. The precision-recall AUC is
not a U statistic, and the interpolated area
[`auc()`](https://evalclass.github.io/precrec/reference/auc.md) reports
is further from being one still, so
[`auc_delong()`](https://evalclass.github.io/precrec/reference/auc_delong.md)
returns ROC rows and nothing else. For the precision-recall AUC - which
is the reason most people are here -
[`auc_boot()`](https://evalclass.github.io/precrec/reference/auc_boot.md)
remains the answer.

### Which to use

Reach for
[`auc_delong()`](https://evalclass.github.io/precrec/reference/auc_delong.md)
when the ROC AUC is what is being reported, when the result has to be
exactly reproducible, or when a reader is expecting “DeLong’s test” by
name. Reach for
[`auc_boot()`](https://evalclass.github.io/precrec/reference/auc_boot.md)
for the precision-recall AUC, and on a small or badly imbalanced test
set, where the percentile interval assumes less: DeLong’s variance is an
*asymptotic* one, and the interval built from it is a normal interval,
so both improve as the test set grows. Running the two and seeing
whether they agree is a cheap way of finding out whether the sample is
large enough for the exact one.

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

- [Choose an operating
  point](https://evalclass.github.io/precrec/articles/howto-operating-point.md)
- [AUC and other curve
  summaries](https://evalclass.github.io/precrec/articles/metrics-auc.md)
- [Evaluate cross-validation
  folds](https://evalclass.github.io/precrec/articles/howto-cross-validation.md)
