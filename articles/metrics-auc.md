# AUC and other curve summaries

These summarize a whole curve in one number, so they do not depend on
picking a cutoff.

``` r

library(precrec)

curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)

knitr::kable(auc(curves))
```

| modnames | dsids | curvetypes |      aucs | baselines |
|:---------|------:|:-----------|----------:|----------:|
| m1       |     1 | ROC        | 0.7200000 |       0.5 |
| m1       |     1 | PRC        | 0.7397716 |       0.5 |

| Curve | What the area means                                      |
|-------|----------------------------------------------------------|
| ROC   | Probability a random positive outranks a random negative |
| PRC   | Mean precision across all recall levels                  |

`precrec` computes the PRC area from the properly interpolated curve;
tools that join raw points with straight lines overestimate it.

## Read the area against its baseline

The `baselines` column is what the area would be by chance. For a ROC
curve that is 0.5 whatever the data, so a ROC AUC can be read on its
own. For a precision-recall curve it is the proportion of positives, so
a PRC AUC cannot. This is the same generator at three class balances:

``` r

compare <- function(np, nn) {
  set.seed(1)
  scores <- c(rnorm(np, 1.2), rnorm(nn, 0))
  labels <- rep(c(1, 0), c(np, nn))
  areas <- auc(evalmod(scores = scores, labels = labels))

  data.frame(
    positives = sprintf("%.0f%%", 100 * np / (np + nn)),
    roc = areas$aucs[areas$curvetypes == "ROC"],
    prc = areas$aucs[areas$curvetypes == "PRC"],
    prc_baseline = areas$baselines[areas$curvetypes == "PRC"]
  )
}

knitr::kable(
  do.call(rbind, list(compare(500, 500), compare(100, 900), compare(20, 980))),
  row.names = FALSE, digits = 3
)
```

| positives |   roc |   prc | prc_baseline |
|:----------|------:|------:|-------------:|
| 50%       | 0.807 | 0.801 |         0.50 |
| 10%       | 0.834 | 0.367 |         0.10 |
| 2%        | 0.850 | 0.129 |         0.02 |

The classifier is about as good in all three rows and the ROC AUC says
so, holding between 0.81 and 0.85. The PRC AUC falls to 0.129, which
reads as failure and is in fact six times chance. Quote the two numbers
together, and see [Balanced and imbalanced
data](https://evalclass.github.io/precrec/articles/howto-imbalanced-data.md)
for why the PRC column is the one that moved.

The baseline is taken per model and per test dataset, because a fold
need not hold the classes in the proportions the whole dataset does.

## Average precision

A different estimator of the same curve, not a different way of adding
up the same one: it joins the raw points with horizontal steps instead
of interpolating, so it reads high wherever the two disagree - here by
about 0.006. Prefer the interpolated area; this is here because other
packages report it under this name.

``` r

knitr::kable(average_precision(curves))
```

| modnames | dsids |       aps | baselines |
|:---------|------:|----------:|----------:|
| m1       |     1 | 0.7454008 |       0.5 |

## Partial areas

When only part of the curve matters - the low-false-positive end, say -
[`part()`](https://evalclass.github.io/precrec/reference/part.md)
restricts the range and
[`pauc()`](https://evalclass.github.io/precrec/reference/pauc.md)
reports the area over it. `spaucs` are standardized partial AUCs,
rescaled to 0 to 1 so ranges of different widths compare.

``` r

knitr::kable(pauc(part(curves, xlim = c(0, 0.25))))
```

| modnames | dsids | curvetypes |     paucs |    spaucs |
|:---------|------:|:-----------|----------:|----------:|
| m1       |     1 | ROC        | 0.1006250 | 0.4025000 |
| m1       |     1 | PRC        | 0.2345849 | 0.9383396 |

Plotting the restricted object draws the partial curve; see [partial
curves](https://evalclass.github.io/precrec/articles/plots-partial-curves.md).

## Confidence intervals

With several test sets,
[`auc_ci()`](https://evalclass.github.io/precrec/reference/auc_ci.md)
puts an interval around the mean area. `alpha` sets the level, and
`dtype = "t"` uses the t distribution, the better choice for a small
number of folds.

``` r

samps <- create_sim_samples(10, 100, 100, "good_er")
mdat <- mmdata(samps[["scores"]], samps[["labels"]], dsids = samps[["dsids"]])
mcurves <- evalmod(mdat)

knitr::kable(auc_ci(mcurves, alpha = 0.01, dtype = "t"))
```

| modnames | curvetypes |      mean |     error | lower_bound | upper_bound |   n |
|:---------|:-----------|----------:|----------:|------------:|------------:|----:|
| m1       | ROC        | 0.7955900 | 0.0260369 |   0.7695531 |   0.8216269 |  10 |
| m1       | PRC        | 0.8351332 | 0.0272381 |   0.8078951 |   0.8623712 |  10 |

## Break-even point

Where precision and recall are equal - the one point on the curve where
the two are in balance.
[`prbe()`](https://evalclass.github.io/precrec/reference/prbe.md) reads
the individual curves, so ask
[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
to keep them.

``` r

knitr::kable(prbe(evalmod(mdat, raw_curves = TRUE)))
```

| modnames | dsids | prbe |
|:---------|------:|-----:|
| m1       |     1 | 0.73 |
| m1       |     2 | 0.75 |
| m1       |     3 | 0.69 |
| m1       |     3 | 0.69 |
| m1       |     4 | 0.77 |
| m1       |     5 | 0.73 |
| m1       |     6 | 0.73 |
| m1       |     7 | 0.68 |
| m1       |     8 | 0.69 |
| m1       |     9 | 0.76 |
| m1       |    10 | 0.74 |

A curve crossing the diagonal more than once gets one row per crossing;
one that never does gets a single `NA`.
[`prbe()`](https://evalclass.github.io/precrec/reference/prbe.md) reads
the crossing off the interpolated curve - finding it by linear
interpolation between raw points, as some tools do, is the error this
package exists to avoid.

## Averaging over classes

For a multiclass evaluation
[`auc()`](https://evalclass.github.io/precrec/reference/auc.md) adds the
macro-average of the per-class areas, weighting every class equally.
`macro_weight = "prevalence"` weights each by the number of observations
it has instead, and names those rows `macro-average-weighted` to keep
the two apart.

``` r

mccurves <- evalmod(scores = C3N150$scores, labels = C3N150$labels)

knitr::kable(auc(mccurves, macro_weight = "prevalence"))
```

| modnames               | dsids | curvetypes |      aucs | baselines |
|:-----------------------|------:|:-----------|----------:|----------:|
| c1                     |     1 | ROC        | 0.9732000 | 0.5000000 |
| c1                     |     1 | PRC        | 0.9558435 | 0.3333333 |
| c2                     |     1 | ROC        | 0.7758000 | 0.5000000 |
| c2                     |     1 | PRC        | 0.6550357 | 0.3333333 |
| c3                     |     1 | ROC        | 0.5336000 | 0.5000000 |
| c3                     |     1 | PRC        | 0.4162555 | 0.3333333 |
| macro-average-weighted |     1 | ROC        | 0.7608667 | 0.5000000 |
| macro-average-weighted |     1 | PRC        | 0.6757116 | 0.3333333 |

Uniform if every class matters equally, prevalence if you care about the
average case. Other packages call these `roc_aunu` and `roc_aunp`. See
[more than two
classes](https://evalclass.github.io/precrec/articles/howto-multiclass.md).

The baseline is averaged over the classes the same way the areas are,
with the same weights, so a macro row stays readable. That matters most
when the classes are the unequal sizes a macro-average is usually
reached for:

``` r

set.seed(5)
rare <- rep(c("c1", "c2", "c3"), c(10, 40, 150))
noise <- cbind(rnorm(200), rnorm(200), rnorm(200))
noisy <- evalmod(mmdata(noise, rare, multiclass = "ovr"))

knitr::kable(
  subset(
    auc(noisy, macro_weight = "prevalence"),
    curvetypes == "PRC"
  ),
  row.names = FALSE, digits = 3
)
```

| modnames               | dsids | curvetypes |  aucs | baselines |
|:-----------------------|------:|:-----------|------:|----------:|
| c1                     |     1 | PRC        | 0.049 |     0.050 |
| c2                     |     1 | PRC        | 0.206 |     0.200 |
| c3                     |     1 | PRC        | 0.721 |     0.750 |
| macro-average-weighted |     1 | PRC        | 0.584 |     0.605 |

The scores here are random numbers. The weighted macro-average PRC area
is 0.584, which would be a respectable result if the baseline were not
0.605 sitting beside it.

## The fast path

If the ROC area is all you need, `mode = "aucroc"` computes it from the
U statistic of the Mann-Whitney test without building the curve. See
[large
datasets](https://evalclass.github.io/precrec/articles/howto-large-datasets.md).

``` r

knitr::kable(as.data.frame(
  evalmod(scores = P10N10$scores, labels = P10N10$labels, mode = "aucroc")
))
```

| modnames | dsids | aucs | ustats |
|:---------|:------|-----:|-------:|
| m1       | 1     | 0.72 |     72 |

## Next

- [Uncertainty from one test
  set](https://evalclass.github.io/precrec/articles/metrics-uncertainty.md)
- [Probability-based
  metrics](https://evalclass.github.io/precrec/articles/metrics-probability.md)
