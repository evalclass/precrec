# Work with large datasets

The calculations are C++ and handle large datasets without special
metrics. Plotting is where the time goes, and there are two switches for
it.

``` r

library(precrec)
library(ggplot2)
```

## Fewer points to draw

A curve over a million instances has a million supporting points, and a
screen has a few thousand pixels.
[`plot()`](https://evalclass.github.io/precrec/reference/plot.md) and
[`autoplot()`](https://evalclass.github.io/precrec/reference/autoplot.md)
therefore thin the points before drawing, which is the default.

``` r

samps <- create_sim_samples(2, 20000, 20000)
curves <- evalmod(scores = samps$scores, labels = samps$labels)

# Thinned, the default
system.time(autoplot(curves))

# Every point
system.time(autoplot(curves, reduce_points = FALSE))
#>    user  system elapsed 
#>   0.095   0.013   0.108 
#>    user  system elapsed 
#>   0.081   0.006   0.087
```

The curve is calculated at full resolution either way - `reduce_points`
changes only what is drawn.

## AUC without the curve

If all you want is the area under the ROC curve, `mode = "aucroc"`
computes it from the U statistic of the Mann-Whitney test and never
builds the curve. It is much faster and uses much less memory.

``` r

aucs <- evalmod(
  scores = P10N10$scores, labels = P10N10$labels,
  mode = "aucroc"
)

knitr::kable(as.data.frame(aucs))
```

| modnames | dsids | aucs | ustats |
|:---------|:------|-----:|-------:|
| m1       | 1     | 0.72 |     72 |

This mode gives the ROC area only. There is no shortcut of the same kind
for the precision-recall area, which needs the curve.

## Coarser confidence bands

With many test sets, `x_bins` controls how many points the average curve
and its confidence band are evaluated at. Lowering it makes the
calculation cheaper; the default of 1000 is already modest.

``` r

mdat <- mmdata(samps[["scores"]], samps[["labels"]], dsids = samps[["dsids"]])

curves_coarse <- evalmod(mdat, x_bins = 100)
```

## Only the metrics you need

In `mode = "basic"`, each extra metric is another vector the length of
the dataset. The default set is fourteen; anything beyond it is opt-in
through `metrics =`. See the [metrics
overview](https://evalclass.github.io/precrec/articles/metrics-overview.md).

## Next

- [Confidence
  bands](https://evalclass.github.io/precrec/articles/plots-confidence-bands.md)
- [Get the numbers
  out](https://evalclass.github.io/precrec/articles/howto-results-as-data.md)
