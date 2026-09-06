# AUC and other curve summaries

These summarize a whole curve in one number, so they do not depend on
picking a cutoff.

``` r

library(precrec)
library(ggplot2)

curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)
```

## Area under the curve

``` r

knitr::kable(auc(curves))
```

| modnames | dsids | curvetypes |      aucs |
|:---------|------:|:-----------|----------:|
| m1       |     1 | ROC        | 0.7200000 |
| m1       |     1 | PRC        | 0.7397716 |

| Curve | What the area means | Baseline |
|----|----|----|
| ROC | Probability a random positive outranks a random negative | 0.5 |
| PRC | Average precision over all recall levels | The proportion of positives |

The ROC baseline is always 0.5. The precision-recall baseline is not -
it moves with the class balance, so a PRC area of 0.4 is poor on
balanced data and good when positives are 5% of the total.

`precrec` computes the precision-recall area from the properly
interpolated curve. Tools that join raw points with straight lines
overestimate it.

## Partial areas

When only part of the curve matters - the low-false-positive end, say -
[`part()`](https://evalclass.github.io/precrec/reference/part.md)
restricts the range and
[`pauc()`](https://evalclass.github.io/precrec/reference/pauc.md)
reports the area over it.

``` r

partial <- part(curves, xlim = c(0, 0.25))

knitr::kable(pauc(partial))
```

| modnames | dsids | curvetypes |     paucs |    spaucs |
|:---------|------:|:-----------|----------:|----------:|
| m1       |     1 | ROC        | 0.1006250 | 0.4025000 |
| m1       |     1 | PRC        | 0.2345849 | 0.9383396 |

`spaucs` in that table are standardized partial AUCs, rescaled to 0 to 1
so that ranges of different widths can be compared. Plotting the
restricted object draws the partial curve; see [partial
curves](https://evalclass.github.io/precrec/articles/plots-partial-curves.md).

## Confidence intervals

With several test sets,
[`auc_ci()`](https://evalclass.github.io/precrec/reference/auc_ci.md)
puts an interval around the mean area.

``` r

samps <- create_sim_samples(10, 100, 100, "good_er")
mdat <- mmdata(samps[["scores"]], samps[["labels"]], dsids = samps[["dsids"]])
mcurves <- evalmod(mdat)

knitr::kable(auc_ci(mcurves))
```

| modnames | curvetypes |      mean |     error | lower_bound | upper_bound |   n |
|:---------|:-----------|----------:|----------:|------------:|------------:|----:|
| m1       | ROC        | 0.8113400 | 0.0123463 |   0.7989937 |   0.8236863 |  10 |
| m1       | PRC        | 0.8472922 | 0.0110427 |   0.8362495 |   0.8583350 |  10 |

`alpha` sets the level - `alpha = 0.01` for 99% - and `dtype = "t"` uses
the t distribution instead of the normal, which is the better choice for
a small number of folds.

``` r

knitr::kable(auc_ci(mcurves, alpha = 0.01, dtype = "t"))
```

| modnames | curvetypes |      mean |     error | lower_bound | upper_bound |   n |
|:---------|:-----------|----------:|----------:|------------:|------------:|----:|
| m1       | ROC        | 0.8113400 | 0.0204715 |   0.7908685 |   0.8318115 |  10 |
| m1       | PRC        | 0.8472922 | 0.0183101 |   0.8289821 |   0.8656023 |  10 |

## Break-even point

The break-even point is where precision and recall are equal - the one
point on the precision-recall curve where the two are in balance.

[`prbe()`](https://evalclass.github.io/precrec/reference/prbe.md) reads
the individual curves, so ask
[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
to keep them.

``` r

raw <- evalmod(mdat, raw_curves = TRUE)

knitr::kable(prbe(raw))
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

A curve can cross the diagonal more than once, and then the result holds
one row per crossing. A curve that never reaches equal precision and
recall gets a single row of `NA`.

[`prbe()`](https://evalclass.github.io/precrec/reference/prbe.md) reads
the crossing off the interpolated curve. Finding it by linear
interpolation between raw precision-recall points, as some tools do, is
the error this package exists to avoid.

## The fast path

If the ROC area is all you need, `mode = "aucroc"` computes it from the
U statistic of the Mann-Whitney test without building the curve at all.
See [working with large
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

- [Probability-based
  metrics](https://evalclass.github.io/precrec/articles/measures-probability.md)
- [Partial
  curves](https://evalclass.github.io/precrec/articles/plots-partial-curves.md)
