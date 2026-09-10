# Get every evaluation metric at every cutoff

The `metric_table` function returns one row per cutoff and one column
per metric, in the manner of `pROC::coords` and the cutoff slots of a
`ROCR::performance` object. It is the table counterpart of
[`metric_curve()`](https://evalclass.github.io/precrec/reference/metric_curve.md),
which projects one metric against another.

## Usage

``` r
metric_table(x, scores = NULL, labels = NULL, metrics = NULL, ...)
```

## Arguments

- x:

  An `S3` object created by the
  [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
  function, or a basic-metric object created by
  `evalmod(mode = "basic")`. The `metric_table` function ignores
  `scores` and `labels` when `x` is specified. These arguments are
  internally passed to the
  [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
  function when `x` is unspecified. In that case, both `scores` and
  `labels` must be at least specified.

- scores:

  A numeric dataset of predicted scores. It can be a vector, a matrix,
  an array, a data frame, or a list.

- labels:

  A numeric, character, logical, or factor dataset of observed labels.
  It can be a vector, a matrix, an array, a data frame, or a list.

- metrics:

  A character vector of the metrics to calculate in addition to the
  default set, or the string `"all"` for every metric `precrec` knows,
  exactly as
  [`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
  takes it. It must be unspecified when `x` is already a basic-metric
  object, which carries the metrics it was built with.

- ...:

  These additional arguments are passed to
  [`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md),
  and through it to
  [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md),
  when this function builds the basic metrics itself. `beta`, `cost_fp`,
  `cost_fn`, `basic_ties`, `modnames`, `dsids` and `posclass` are the
  useful ones here.

## Value

The `metric_table` function returns a data frame with one row per cutoff
per model per test dataset, and the following columns.

|  |  |
|----|----|
| `modname` | Model name |
| `dsid` | Test dataset ID |
| `rank` | Number of instances called positive, `0` to `n` |
| `normalized_rank` | `rank / n`, the x axis of the basic metric plots |
| `score` | The cutoff itself, see below |
| `label` | `1` if the instance at this rank is positive, `-1` if it is negative |
| ... | One column per metric, named as [`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md) names them |

## What a row means

Row `rank = k` is the cutoff that calls the top `k` instances positive,
so `score` is the score of the instance at rank `k` and the rule the row
stands for is `score >= that value`. The metrics on the row are the
metrics of that rule.

The first row is `rank = 0` - call nothing positive. There is no
instance at that rank, so `score` and `label` are `NA` while the metrics
are defined, and it is kept because it is a real operating point.

Tied scores share one value of each metric rather than taking a value
that depends on the order the ties arrived in. `basic_ties` of
[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
controls that.

## Several test datasets

A cutoff belongs to the dataset it was read off, so the rows are per
dataset and nothing is averaged across them - unlike the basic metric
plots, which average by default. A basic-metric object passed as `x`
therefore has to have been built with `raw_curves = TRUE` when it holds
more than one dataset, since an object built without it keeps only the
average.

## See also

[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
for calculating the metrics,
[`metric_curve()`](https://evalclass.github.io/precrec/reference/metric_curve.md)
for projecting one metric against another, and
[`as.data.frame()`](https://evalclass.github.io/precrec/reference/as.data.frame.md)
for the long form the plots use.

## Examples

``` r

##################################################
### Single model & single test dataset
###

## Load a dataset with 10 positives and 10 negatives
data(P10N10)

## Get every metric at every cutoff
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

## The cutoff that maximizes the F-score
tab[which.max(tab$fscore), c("rank", "score", "precision", "fscore")]
#>    rank score precision fscore
#> 16   15     6       0.6   0.72


##################################################
### Add metrics beyond the default set
###

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


##################################################
### Reuse an object that is already calculated
###

points <- evalmod(
  scores = P10N10$scores, labels = P10N10$labels,
  mode = "basic"
)
head(metric_table(points))
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
