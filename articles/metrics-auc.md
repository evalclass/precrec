# AUC and other curve summaries

These summarize a whole curve in one number, so they do not depend on
picking a cutoff.

``` r

library(precrec)

curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)

knitr::kable(auc(curves))
```

| modnames | dsids | curvetypes |      aucs |
|:---------|------:|:-----------|----------:|
| m1       |     1 | ROC        | 0.7200000 |
| m1       |     1 | PRC        | 0.7397716 |

| Curve | What the area means | Baseline |
|----|----|----|
| ROC | Probability a random positive outranks a random negative | 0.5 |
| PRC | Mean precision across all recall levels | Proportion of positives |

The ROC baseline is always 0.5. The precision-recall baseline moves with
the class balance, so a PRC area of 0.4 is poor on balanced data and
good when positives are 5% of the total. `precrec` computes that area
from the properly interpolated curve; tools that join raw points with
straight lines overestimate it.

## Average precision

A different estimator of the same curve, not a different way of adding
up the same one: it joins the raw points with horizontal steps instead
of interpolating, so it reads high wherever the two disagree - here by
about 0.006. Prefer the interpolated area; this is here because other
packages report it under this name.

``` r

knitr::kable(average_precision(curves))
```

| modnames | dsids |       aps |
|:---------|------:|----------:|
| m1       |     1 | 0.7454008 |

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
| m1       | ROC        | 0.8113400 | 0.0204715 |   0.7908685 |   0.8318115 |  10 |
| m1       | PRC        | 0.8472922 | 0.0183101 |   0.8289821 |   0.8656023 |  10 |

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
| m1       |     1 | 0.71 |
| m1       |     2 | 0.70 |
| m1       |     2 | 0.70 |
| m1       |     3 | 0.73 |
| m1       |     4 | 0.77 |
| m1       |     5 | 0.74 |
| m1       |     6 | 0.76 |
| m1       |     7 | 0.74 |
| m1       |     8 | 0.73 |
| m1       |     9 | 0.73 |
| m1       |    10 | 0.70 |

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

| modnames               | dsids | curvetypes |      aucs |
|:-----------------------|------:|:-----------|----------:|
| c1                     |     1 | ROC        | 0.9732000 |
| c1                     |     1 | PRC        | 0.9558435 |
| c2                     |     1 | ROC        | 0.7758000 |
| c2                     |     1 | PRC        | 0.6550357 |
| c3                     |     1 | ROC        | 0.5336000 |
| c3                     |     1 | PRC        | 0.4162555 |
| macro-average-weighted |     1 | ROC        | 0.7608667 |
| macro-average-weighted |     1 | PRC        | 0.6757116 |

Uniform if every class matters equally, prevalence if you care about the
average case. Other packages call these `roc_aunu` and `roc_aunp`. See
[more than two
classes](https://evalclass.github.io/precrec/articles/howto-multiclass.md).

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
