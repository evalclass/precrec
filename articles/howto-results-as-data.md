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

## Every metric at every cutoff

[`metric_table()`](https://evalclass.github.io/precrec/reference/metric_table.md)
returns one row per cutoff and one column per metric. This is the table
`pROC::coords()` and `ROCR`’s cutoff slots give, and the shape most work
downstream of a curve needs.

``` r

tab <- metric_table(scores = P10N10$scores, labels = P10N10$labels)

head(tab)
#>   modname dsid rank normalized_rank score label error accuracy specificity
#> 1      m1    1    0            0.00    NA    NA  0.50     0.50         1.0
#> 2      m1    1    1            0.05    20     1  0.45     0.55         1.0
#> 3      m1    1    2            0.10    19     1  0.40     0.60         1.0
#> 4      m1    1    3            0.15    18    -1  0.45     0.55         0.9
#> 5      m1    1    4            0.20    17     1  0.40     0.60         0.9
#> 6      m1    1    5            0.25    16     1  0.35     0.65         0.9
#>   sensitivity precision       mcc    fscore balanced_accuracy       npv
#> 1         0.0 1.0000000        NA 0.0000000              0.50 0.5000000
#> 2         0.1 1.0000000 0.2294157 0.1818182              0.55 0.5263158
#> 3         0.2 1.0000000 0.3333333 0.3333333              0.60 0.5555556
#> 4         0.2 0.6666667 0.1400280 0.3076923              0.55 0.5294118
#> 5         0.3 0.7500000 0.2500000 0.4285714              0.60 0.5625000
#> 6         0.4 0.8000000 0.3464102 0.5333333              0.65 0.6000000
#>   informedness markedness kappa
#> 1          0.0  0.5000000   0.0
#> 2          0.1  0.5263158   0.1
#> 3          0.2  0.5555556   0.2
#> 4          0.1  0.1960784   0.1
#> 5          0.2  0.3125000   0.2
#> 6          0.3  0.4000000   0.3
```

A row is the cutoff that calls the top `rank` instances positive, so
`score` is the score of the instance at that rank and the rule the row
stands for is `score >= that value`. `normalized_rank` is `rank / n`,
the x axis of the [basic metric
plots](https://evalclass.github.io/precrec/articles/plots-basic-metrics.md).
The first row calls nothing positive, which is why its `score` and
`label` are `NA` while its metrics are not.

Because it is a plain data frame, the question that usually follows is
ordinary R.

``` r

best <- tab[which.max(tab$fscore), ]

knitr::kable(best[, c("rank", "score", "sensitivity", "precision", "fscore")])
```

|     | rank | score | sensitivity | precision | fscore |
|:----|-----:|------:|------------:|----------:|-------:|
| 16  |   15 |     6 |         0.9 |       0.6 |   0.72 |

[`best_cutoff()`](https://evalclass.github.io/precrec/reference/best_cutoff.md)
is that line with the parts that are easy to get wrong done for you:
which direction the metric is better in, which of several tied cutoffs
to return, and one row per model per test dataset rather than one row
overall.

``` r

picked <- best_cutoff(
  scores = P10N10$scores, labels = P10N10$labels,
  metric = "fscore"
)

knitr::kable(
  picked[, c("metric", "value", "rank", "score", "sensitivity", "precision")]
)
```

| metric | value | rank | score | sensitivity | precision |
|:-------|------:|-----:|------:|------------:|----------:|
| fscore |  0.72 |   15 |     6 |         0.9 |       0.6 |

It takes any of the metrics, plus `"youden"` and `"topleft"` for the two
criteria the cutpoint literature names rather than the metric table
does. Which one to optimize is a bigger decision than it looks, and the
biggest one when positives are rare - see [Choose an operating
point](https://evalclass.github.io/precrec/articles/howto-operating-point.md).

Metrics beyond the default fourteen come the same way they do from
[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md).

``` r

lifted <- metric_table(
  scores = P10N10$scores, labels = P10N10$labels,
  metrics = c("lift", "jaccard")
)

head(lifted[, c("rank", "score", "precision", "lift", "jaccard")])
#>   rank score precision     lift   jaccard
#> 1    0    NA 1.0000000       NA 0.0000000
#> 2    1    20 1.0000000 2.000000 0.1000000
#> 3    2    19 1.0000000 2.000000 0.2000000
#> 4    3    18 0.6666667 1.333333 0.1818182
#> 5    4    17 0.7500000 1.500000 0.2727273
#> 6    5    16 0.8000000 1.600000 0.3636364
```

With several test datasets there is one block of rows per dataset and
nothing is averaged across them - a cutoff belongs to the dataset it was
read off. Build the object with `raw_curves = TRUE` if you pass one in
rather than letting
[`metric_table()`](https://evalclass.github.io/precrec/reference/metric_table.md)
build it.

## The same metrics, long

`mode = "basic"` on its own gives the long form, which is what the plots
use.

``` r

points <- evalmod(
  scores = P10N10$scores, labels = P10N10$labels,
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
named in `type`. It is the same data
[`metric_table()`](https://evalclass.github.io/precrec/reference/metric_table.md)
returns, on its side. Reach for this one to plot and for the table to
decide.

An object that has already been calculated can be passed straight in,
which saves calculating it twice.

``` r

identical(
  metric_table(points),
  metric_table(scores = P10N10$scores, labels = P10N10$labels)
)
#> [1] TRUE
```

## Feeding ggplot2 directly

[`fortify()`](https://evalclass.github.io/precrec/reference/fortify.md)
is the `ggplot2` hook, so a `precrec` object can go straight into
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html). See
[customizing
plots](https://evalclass.github.io/precrec/articles/plots-customizing.md).

## Next

- [Choose an operating
  point](https://evalclass.github.io/precrec/articles/howto-operating-point.md)
- [Metrics
  overview](https://evalclass.github.io/precrec/articles/metrics-overview.md)
- [Plots
  overview](https://evalclass.github.io/precrec/articles/plots-overview.md)
