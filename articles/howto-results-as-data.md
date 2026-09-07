# Get the numbers out

Every `precrec` object converts to a data frame, and every summary is a
data frame too. Nothing is locked inside the plotting code.

``` r

library(precrec)

curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)
```

## What is in the object

[`print()`](https://rdrr.io/r/base/print.html) summarizes the input and
the results.

``` r

curves
#> 
#>     === AUCs ===
#> 
#>      Model name Dataset ID Curve type       AUC
#>    1         m1          1        ROC 0.7200000
#>    2         m1          1        PRC 0.7397716
#> 
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1         m1          1             10             10
```

## The curve points

``` r

df <- as.data.frame(curves)
head(df)
#>       x   y modname dsid type
#> 1 0.000 0.0      m1    1  ROC
#> 2 0.000 0.1      m1    1  ROC
#> 3 0.000 0.2      m1    1  ROC
#> 4 0.001 0.2      m1    1  ROC
#> 5 0.002 0.2      m1    1  ROC
#> 6 0.003 0.2      m1    1  ROC
```

One row per supporting point, with the curve type and the model in their
own columns - the shape `ggplot2` and `dplyr` expect.
[`as.data.table()`](https://evalclass.github.io/precrec/reference/as.data.table.md)
returns the same thing as a `data.table` when that package is installed.

## The summaries

| Function | Returns |
|----|----|
| [`auc()`](https://evalclass.github.io/precrec/reference/auc.md) | Area under each curve |
| [`pauc()`](https://evalclass.github.io/precrec/reference/pauc.md) | Partial area, after [`part()`](https://evalclass.github.io/precrec/reference/part.md) |
| [`auc_ci()`](https://evalclass.github.io/precrec/reference/auc_ci.md) | Confidence interval of the area, over several test sets |
| [`prbe()`](https://evalclass.github.io/precrec/reference/prbe.md) | Precision-recall break-even point |
| [`prob_metrics()`](https://evalclass.github.io/precrec/reference/prob_metrics.md) | Brier score, RMSE and log loss |

``` r

knitr::kable(auc(curves))
```

| modnames | dsids | curvetypes |      aucs |
|:---------|------:|:-----------|----------:|
| m1       |     1 | ROC        | 0.7200000 |
| m1       |     1 | PRC        | 0.7397716 |

Each returns a plain data frame, so subsetting is ordinary R.

``` r

aucs <- auc(curves)
knitr::kable(subset(aucs, curvetypes == "PRC"))
```

|     | modnames | dsids | curvetypes |      aucs |
|:----|:---------|------:|:-----------|----------:|
| 2   | m1       |     1 | PRC        | 0.7397716 |

## Basic metrics per cutoff

`mode = "basic"` gives the per-cutoff metrics rather than the curves.

``` r

points <- evalmod(scores = P10N10$scores, labels = P10N10$labels,
  mode = "basic"
)

head(as.data.frame(points))
#>      x  y modname dsid  type
#> 1 0.00 NA      m1    1 score
#> 2 0.05 20      m1    1 score
#> 3 0.10 19      m1    1 score
#> 4 0.15 18      m1    1 score
#> 5 0.20 17      m1    1 score
#> 6 0.25 16      m1    1 score
```

The `x` column is the normalized rank and `y` the value of the metric
named in `type`.

## Feeding ggplot2 directly

[`fortify()`](https://evalclass.github.io/precrec/reference/fortify.md)
is the `ggplot2` hook, so a `precrec` object can go straight into
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html). See
[customizing
plots](https://evalclass.github.io/precrec/articles/plots-customizing.md).

## Next

- [Metrics
  overview](https://evalclass.github.io/precrec/articles/metrics-overview.md)
- [Plots
  overview](https://evalclass.github.io/precrec/articles/plots-overview.md)
