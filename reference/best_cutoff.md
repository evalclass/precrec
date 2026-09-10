# Choose an operating point

The `best_cutoff` function picks the cutoff that optimizes one
evaluation metric, in the manner of `pROC::coords(x, "best")`. It
returns one row per model per test dataset - a row of
[`metric_table()`](https://evalclass.github.io/precrec/reference/metric_table.md),
together with the criterion that chose it.

## Usage

``` r
best_cutoff(x, scores = NULL, labels = NULL, metric = "youden", ...)
```

## Arguments

- x:

  An `S3` object created by the
  [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
  function, or a basic-metric object created by
  `evalmod(mode = "basic")`. The `best_cutoff` function ignores `scores`
  and `labels` when `x` is specified. These arguments are internally
  passed to the
  [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
  function when `x` is unspecified. In that case, both `scores` and
  `labels` must be at least specified.

- scores:

  A numeric dataset of predicted scores. It can be a vector, a matrix,
  an array, a data frame, or a list.

- labels:

  A numeric, character, logical, or factor dataset of observed labels.
  It can be a vector, a matrix, an array, a data frame, or a list.

- metric:

  A string naming the metric to optimize. It can be any of the metrics
  [`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
  calculates, together with two names that come from the cutpoint
  literature rather than from the metric table.

  |             |                |             |
  |-------------|----------------|-------------|
  | **Name**    | **Metric**     | **Optimum** |
  | `"youden"`  | `informedness` | Maximum     |
  | `"topleft"` | `roc_dist`     | Minimum     |
  | `"fscore"`  | `fscore`       | Maximum     |
  | `"mcc"`     | `mcc`          | Maximum     |
  | `"cost"`    | `cost`         | Minimum     |

  The direction is a property of the metric, so it is not an argument -
  `error`, `cost`, `roc_dist`, the two error rates and the negative
  likelihood ratio are minimized, and the rest are maximized. `score`,
  `label`, `predicted_positive_rate` and `predicted_negative_rate`
  describe a cutoff rather than score it, and are refused.

- ...:

  These additional arguments are passed to
  [`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md),
  and through it to
  [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md),
  when this function builds the metrics itself. `beta` for `"fscore"`,
  `cost_fp` and `cost_fn` for `"cost"`, and `basic_ties`, `modnames`,
  `dsids` and `posclass` are the useful ones here.

## Value

The `best_cutoff` function returns a data frame with one row per model
per test dataset, and the following columns.

|  |  |
|----|----|
| `modname` | Model name |
| `dsid` | Test dataset ID |
| `metric` | The metric that was optimized |
| `value` | Its value at the chosen cutoff |
| `rank` | Number of instances called positive, `0` to `n` |
| `normalized_rank` | `rank / n` |
| `score` | The cutoff itself, the rule being `score >= ` it |
| `label` | `1` if the instance at this rank is positive, `-1` if it is negative |
| ... | One column per metric, as [`metric_table()`](https://evalclass.github.io/precrec/reference/metric_table.md) returns them |

The row is a row of
[`metric_table()`](https://evalclass.github.io/precrec/reference/metric_table.md),
so every other metric is there to be read at the same cutoff - which is
the point of returning the whole row rather than the threshold alone.

## The criterion matters more than the threshold

Youden's J and the closest point to the top left corner are the two
criteria most often reached for, and both are computed from sensitivity
and specificity alone. Both of those are conditioned on the true class,
so neither knows the prevalence, and on imbalanced data the cutoff they
choose can sit at a precision no one would deploy. That is the argument
this package makes about the ROC curve, one step further down the
pipeline.

|                      |                               |
|----------------------|-------------------------------|
| **Prevalence-blind** | `"youden"`, `"topleft"`       |
| **Prevalence-aware** | `"fscore"`, `"mcc"`, `"cost"` |

The default is `"youden"` because that is what a caller arriving from
another package expects. On imbalanced data it is the wrong default, and
`"mcc"` or a `"cost"` weighted by what the two mistakes actually cost is
the better choice. See
`vignette("howto-imbalanced-data", package = "precrec")`, which shows
the two disagreeing on the same dataset.

## Ties, and what is not here

Several cutoffs can share the optimum. The one with the smallest `rank`
is returned - the threshold that calls the fewest instances positive -
so the result is one row per model per test dataset whatever the data
does.

Rows that share a `score` are an exception, because they are one
threshold seen several times rather than several cutoffs: `score >= `
that value calls all of the tied instances positive, so the `rank`
reported is the last of the run and not the first. That is what
`evalmod(basic_ties = "hold")` produces throughout, since it gives every
cutoff in a run of tied scores the counts of the whole run.

A metric that has no interior optimum is optimized at an end of the
range, and no warning says so: `sensitivity` is largest when everything
is called positive, and `specificity` when nothing is. Those are the
correct answers to the question asked, and rarely the question meant.

A cutoff chosen on the same data the model is evaluated on is
optimistic, by however much the criterion was free to chase. Choosing it
on held-out data is the fix, and it is the caller's to make: nothing
here resamples or cross-validates the choice.

## See also

[`metric_table()`](https://evalclass.github.io/precrec/reference/metric_table.md)
for every metric at every cutoff, which this function takes one row of,
[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
for calculating the metrics, and
[`prbe()`](https://evalclass.github.io/precrec/reference/prbe.md) for
the precision-recall break-even point, which is an operating point
defined by a crossing rather than by an optimum.

## Examples

``` r

##################################################
### Single model & single test dataset
###

## Load a dataset with 10 positives and 10 negatives
data(P10N10)

## The cutoff that maximizes Youden's J
best_cutoff(scores = P10N10$scores, labels = P10N10$labels)
#>   modname dsid       metric value rank normalized_rank score label error
#> 1      m1    1 informedness   0.4    6             0.3    15     1   0.3
#>   accuracy specificity sensitivity precision       mcc fscore balanced_accuracy
#> 1      0.7         0.9         0.5 0.8333333 0.4364358  0.625               0.7
#>         npv informedness markedness kappa
#> 1 0.6428571          0.4  0.4761905   0.4


##################################################
### A criterion that knows the prevalence
###

best_cutoff(scores = P10N10$scores, labels = P10N10$labels, metric = "mcc")
#>   modname dsid metric     value rank normalized_rank score label error accuracy
#> 1      m1    1    mcc 0.4364358    6             0.3    15     1   0.3      0.7
#>   specificity sensitivity precision       mcc fscore balanced_accuracy
#> 1         0.9         0.5 0.8333333 0.4364358  0.625               0.7
#>         npv informedness markedness kappa
#> 1 0.6428571          0.4  0.4761905   0.4

## Weighted by what the two mistakes cost
best_cutoff(
  scores = P10N10$scores, labels = P10N10$labels,
  metric = "cost", cost_fp = 1, cost_fn = 5
)
#>   modname dsid metric value rank normalized_rank score label error accuracy
#> 1      m1    1   cost   0.5   20               1     5    -1   0.5      0.5
#>   specificity sensitivity precision mcc    fscore balanced_accuracy npv
#> 1           0           1       0.5  NA 0.6666667               0.5 0.8
#>   informedness markedness kappa cost
#> 1            0        0.3     0  0.5


##################################################
### Multiple models & multiple test datasets
###

samps <- create_sim_samples(2, 50, 50, c("poor_er", "good_er"))
mdat <- mmdata(samps[["scores"]], samps[["labels"]],
  modnames = samps[["modnames"]], dsids = samps[["dsids"]]
)
best_cutoff(mdat, metric = "fscore")[, 1:7]
#>   modname dsid metric     value rank normalized_rank     score
#> 1 poor_er    1 fscore 0.7567568   61            0.61 0.6253093
#> 2 poor_er    2 fscore 0.8333333   70            0.70 0.4347773
#> 3 good_er    1 fscore 0.7809524   55            0.55 0.1891361
#> 4 good_er    2 fscore 0.7674419   36            0.36 0.4968831
```
