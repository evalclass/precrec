# Draw one evaluation metric against another

The `metric_curve` function takes the name of a metric for the x axis
and the name of a metric for the y axis and calculates one curve per
test dataset, in the manner of `ROCR::performance`. Every metric
[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
can calculate is available on both axes.

## Usage

``` r
metric_curve(
  mdat,
  scores = NULL,
  labels = NULL,
  x_metric = "fpr",
  y_metric = "sensitivity",
  modnames = NULL,
  dsids = NULL,
  posclass = NULL,
  na_worst = TRUE,
  ties_method = "equiv",
  x_bins = 1000,
  interpolate = TRUE,
  cost_fp = 1,
  cost_fn = 1,
  ...
)
```

## Arguments

- mdat:

  An `S3` object created by the
  [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
  function. It contains formatted scores and labels. The `metric_curve`
  function ignores `scores` and `labels` when `mdat` is specified. These
  arguments are internally passed to the
  [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
  function when `mdat` is unspecified. In that case, both `scores` and
  `labels` must be at least specified.

- scores:

  A numeric dataset of predicted scores. It can be a vector, a matrix,
  an array, a data frame, or a list.

- labels:

  A numeric, character, logical, or factor dataset of observed labels.
  It can be a vector, a matrix, an array, a data frame, or a list.

- x_metric:

  A string that specifies the metric of the x axis. It accepts every
  name
  [`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
  accepts for `metrics`, together with the identifiers `ROCR` uses. The
  default `"fpr"` with the default `y_metric` reproduces `ROCR`'s most
  common call.

- y_metric:

  A string that specifies the metric of the y axis.

- modnames:

  A character vector for the names of the models.

- dsids:

  A numeric vector for the dataset IDs.

- posclass:

  A string or a numeric value to specify the label of positives.

- na_worst:

  A Boolean value for the ties method of `NA`s.

- ties_method:

  A string for the ties method.

- x_bins:

  An integer for the number of supporting points of a registered pair,
  `1e6` or smaller. It is ignored for every other pair, which has no
  interpolation to place supporting points on.

- interpolate:

  A Boolean value to specify whether or not interpolation of a
  registered pair is performed.

- cost_fp:

  A numeric value for the cost of a false positive, used when one of the
  two axes is the `cost` metric. See
  [`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md).

- cost_fn:

  A numeric value for the cost of a false negative.

- ...:

  These additional arguments are passed to
  [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
  for data preparation.

## Value

The `metric_curve` function returns an `S3` object of one of the
following classes, chosen the way
[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
chooses between its own: `ssxycurves`, `msxycurves`, `smxycurves` and
`mmxycurves`. The object holds one curve per test dataset, and works
with `print`, `as.data.frame`, `fortify`, `plot` and `autoplot`.

## Which pairs are joined by a line

`precrec` exists because the points of a precision-recall curve must not
be joined by straight lines. The metrics this function reads are raw
per-cutoff values with no interpolation, so joining an arbitrary pair of
them would be the very error the package was written to avoid.

Two pairs have a defined interpolation, and only those two are drawn as
curves: `x_metric = "fpr"` with `y_metric = "sensitivity"`, which is the
ROC curve, and `x_metric = "sensitivity"` with `y_metric = "precision"`,
which is the precision-recall curve. For those, `metric_curve` hands the
work to the same code `evalmod(mode = "rocprc")` uses, so the two cannot
disagree.

Every other pair is drawn as points. Pass `type = "l"` to `plot` or
`autoplot` to join them anyway, having decided that the straight lines
mean something for the pair at hand.

## What this function does not do

`metric_curve` draws one curve per test dataset and does not average
over them. An average needs a rule for interpolating between the points
of each curve, which is exactly what an unregistered pair does not have.
Use `evalmod(calc_avg = TRUE)` for averaged ROC and precision-recall
curves.

## See also

[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
for the metrics themselves and for averaged ROC and precision-recall
curves.
[`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
for formatting input data.
[`autoplot()`](https://evalclass.github.io/precrec/reference/autoplot.md)
and [`plot()`](https://evalclass.github.io/precrec/reference/plot.md)
for the plots.

## Examples

``` r

##################################################
### The ROC curve, the way ROCR asks for it
###
samps <- create_sim_samples(1, 50, 50, "good_er")
xy1 <- metric_curve(
  scores = samps[["scores"]], labels = samps[["labels"]],
  x_metric = "fpr", y_metric = "sensitivity"
)
xy1
#> 
#>     === Sensitivity vs FPR ===
#> 
#>      A registered pair: this is the ROC curve, and is
#>      calculated by the same code as evalmod(mode = "rocprc").
#> 
#>      Model name Dataset ID # of points
#>    1         m1          1        1071
#> 
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1         m1          1             50             50
#> 

##################################################
### A pair with no interpolation, drawn as points
###
xy2 <- metric_curve(
  scores = samps[["scores"]], labels = samps[["labels"]],
  x_metric = "predicted_positive_rate", y_metric = "lift"
)
xy2
#> 
#>     === Lift vs Predicted positive rate ===
#> 
#>      The points of this pair are not joined by a line.
#>      No interpolation is defined between them; see
#>      ?metric_curve for the pairs that have one.
#> 
#>      Model name Dataset ID # of points
#>    1         m1          1         101
#> 
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1         m1          1             50             50
#> 

##################################################
### Multiple models and multiple test datasets
###
samps2 <- create_sim_samples(3, 50, 50, c("poor_er", "good_er"))
mdat <- mmdata(samps2[["scores"]], samps2[["labels"]],
  modnames = samps2[["modnames"]], dsids = samps2[["dsids"]]
)
xy3 <- metric_curve(mdat, x_metric = "score", y_metric = "precision")
xy3
#> 
#>     === Precision vs Score ===
#> 
#>      The points of this pair are not joined by a line.
#>      No interpolation is defined between them; see
#>      ?metric_curve for the pairs that have one.
#> 
#>      Model name Dataset ID # of points
#>    1    poor_er          1         101
#>    2    good_er          1         101
#>    3    poor_er          2         101
#>    4    good_er          2         101
#>    5    poor_er          3         101
#>    6    good_er          3         101
#> 
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1    poor_er          1             50             50
#>    2    good_er          1             50             50
#>    3    poor_er          2             50             50
#>    4    good_er          2             50             50
#>    5    poor_er          3             50             50
#>    6    good_er          3             50             50
#> 
```
