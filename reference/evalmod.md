# Evaluate models and calculate performance evaluation metrics

The `evalmod` function calculates ROC and Precision-Recall curves for
specified prediction scores and binary labels. It also calculate several
basic performance evaluation metrics, such as accuracy, error rate, and
precision, by specifying `mode` as "basic".

## Usage

``` r
evalmod(
  mdat,
  mode = NULL,
  scores = NULL,
  labels = NULL,
  modnames = NULL,
  dsids = NULL,
  posclass = NULL,
  na_worst = TRUE,
  ties_method = "equiv",
  calc_avg = TRUE,
  cb_alpha = 0.05,
  raw_curves = FALSE,
  x_bins = 1000,
  interpolate = TRUE,
  beta = 1,
  on_single_class = "error",
  metrics = NULL,
  cost_fp = 1,
  cost_fn = 1,
  basic_ties = "split",
  ...
)
```

## Arguments

- mdat:

  An `S3` object created by the
  [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
  function. It contains formatted scores and labels. The `evalmod`
  function ignores the following arguments when `mdat` is specified.

  - `scores`

  - `labels`

  - `modnames`

  - `dsids`

  - `posclass`

  - `na_worst`

  - `ties_method`

  These arguments are internally passed to the
  [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
  function when `mdat` is unspecified. In that case, both `scores` and
  `labels` must be at least specified.

- mode:

  A string that specifies the types of evaluation metrics that the
  `evalmod` function calculates.

  "rocprc"

  :   ROC and Precision-Recall curves

  "prcroc"

  :   Same as above

  "basic"

  :   Normalized ranks vs. accuracy, error rate, specificity,
      sensitivity, precision, Matthews correlation coefficient, F-score,
      balanced accuracy, negative predictive value, informedness,
      markedness, and Cohen's kappa.

  "aucroc"

  :   Fast AUC(ROC) calculation with the U statistic

- scores:

  A numeric dataset of predicted scores. It can be a vector, a matrix,
  an array, a data frame, or a list. The
  [`join_scores()`](https://evalclass.github.io/precrec/reference/join_scores.md)
  function can be useful to make scores with multiple datasets.

- labels:

  A numeric, character, logical, or factor dataset of observed labels.
  It can be a vector, a matrix, an array, a data frame, or a list. The
  [`join_labels()`](https://evalclass.github.io/precrec/reference/join_labels.md)
  function can be useful to make labels with multiple datasets.

- modnames:

  A character vector for the names of the models. The `evalmod` function
  automatically generates default names as "m1", "m2", "m3", and so on
  when it is `NULL`.

- dsids:

  A numeric vector for test dataset IDs. The `evalmod` function
  automatically generates the default ID as `1` when it is `NULL`.

- posclass:

  A scalar value to specify the label of positives in `labels`. It must
  be the same data type as `labels`. For example, `posclass = -1`
  changes the positive label from `1` to `-1` when `labels` contains `1`
  and `-1`. The positive label will be automatically detected when
  `posclass` is `NULL`.

- na_worst:

  A Boolean value for controlling the treatment of NAs in `scores`.

  TRUE

  :   All NAs are treated as the worst scores

  FALSE

  :   All NAs are treated as the best scores

- ties_method:

  A string for controlling ties in `scores`.

  "equiv"

  :   Ties are equivalently ranked

  "first"

  :   Ties are ranked in an increasing order as appeared

  "random"

  :   Ties are ranked in random order

- calc_avg:

  A logical value to specify whether average curves should be
  calculated. It is effective only when `dsids` contains multiple
  dataset IDs. For instance, the function calculates the average for the
  model "m1" when `modnames` is `c("m1", "m1", "m1")` and `dsids` is
  `c(1, 2, 3)`. The calculation points are defined by `x_bins`.

- cb_alpha:

  A numeric value with range \[0, 1\] to specify the alpha value of the
  point-wise confidence bounds calculation. It is effective only when
  `calc_avg` is set to `TRUE`. For example, it should be `0.05` for the
  95% confidence level. The calculation points are defined by `x_bins`.

- raw_curves:

  A logical value to specify whether all raw curves should be discarded
  after the average curves are calculated. It is effective only when
  `calc_avg` is set to `TRUE`.

- x_bins:

  An integer value to specify the number of minimum bins on the x-axis.
  It is then used to define supporting points For instance, the x-values
  of the supporting points will be `c(0, 0.5, 1)` and
  `c(0, 0.25, 0.5, 0.75, 1)` when `x_bins = 2` and `x_bins = 4`,
  respectively. All corresponding y-values of the supporting points are
  calculated. `x_bins` places supporting points only when `mode` is set
  to `rocprc` or `prcroc`; with `mode = "basic"` there is no
  interpolation to place them on, and the value is instead the number of
  points kept per metric when a plot or a data frame is asked for with
  `reduce_points = TRUE`. It must be `1e6` or smaller; every stage sized
  by it allocates a vector of that length per curve, and a million
  supporting points is already finer than a plot resolves.

- interpolate:

  A Boolean value to specify whether or not interpolation of ROC and
  precision-recall curves are performed. `x_bins` and `calc_avg` are
  ignored and when `x_bins` is set to `FALSE`. `interpolate` is
  effective only when `mode` is set to `rocprc` or `prcroc`.

- beta:

  A numeric value to specify the beta of the F-beta score, which weights
  recall `beta` times as heavily as precision. The default `1` gives the
  F1 score. `beta` is effective only when `mode` is set to `basic`.

- on_single_class:

  A string that specifies what the `evalmod` function does with a
  dataset in which every label belongs to the same class.

  "error"

  :   Raise an error (default)

  "na"

  :   Warn, and return `NA` for the metrics that are undefined

  ROC and precision-recall curves are undefined for such a dataset, so
  `on_single_class` is effective only when `mode` is set to `rocprc`,
  `prcroc`, or `aucroc`. `mode = "basic"` always warns and calculates
  what it can, because accuracy and error rate are still defined.

- metrics:

  A character vector that names the basic evaluation metrics to
  calculate in addition to the default set, or the string `"all"` for
  every metric `precrec` knows. The default `NULL` is the fourteen
  metrics `evalmod` has always returned: `score`, `label`, `error`,
  `accuracy`, `specificity`, `sensitivity`, `precision`, `mcc`,
  `fscore`, `balanced_accuracy`, `npv`, `informedness`, `markedness` and
  `kappa`.

  The metrics that can be added are `fpr`, `fnr`,
  `false_discovery_rate`, `false_omission_rate`,
  `predicted_positive_rate`, `predicted_negative_rate`, `lift`, `odds`,
  `mi`, `chisq`, `cost` and `sar`. They are the metrics `ROCR` provides
  that `precrec` did not, and each of them also answers to the
  identifier `ROCR` uses for it - `fall`, `miss`, `pcfall`, `pcmiss`,
  `rpp`, `rnp` and `mutual_information` - and to its standard
  abbreviation where it has one.

  `roc_dist` and `sedi` can be added on the same footing. `roc_dist` is
  the distance from `(1 - specificity, sensitivity)` to the perfect
  corner of ROC space, and is the one metric here that is better when it
  is smaller. `sedi` is the symmetric extremal dependence index, a skill
  score built to stay informative when the positive class is rare.

  `jaccard`, `positive_likelihood_ratio` and `negative_likelihood_ratio`
  come from `scikit-learn`. `jaccard` is the Jaccard index, also called
  the critical success index: `TP / (TP + FP + FN)`, the confusion
  matrix with its true negative corner left out, which is the same
  omission `precision` and `sensitivity` make. The two likelihood ratios
  are `sensitivity / fpr` and `fnr / specificity`; `odds`, the
  diagnostic odds ratio, is their quotient.

  They are not calculated by default because each is another vector the
  size of the dataset, and because `plot` and `autoplot` draw one panel
  per metric the object holds. A metric that was not asked for cannot be
  plotted; `metrics` is effective only when `mode` is set to `basic`.

- cost_fp:

  A numeric value for the cost of a false positive, used by the `cost`
  metric. `cost` is not normalized, following `ROCR`: it is
  `cost_fp * FP / n + cost_fn * FN / n`, which with the default weights
  of `1` is the error rate. `cost_fp` is effective only when `mode` is
  set to `basic` and `metrics` asks for `cost`.

- cost_fn:

  A numeric value for the cost of a false negative.

- basic_ties:

  A string that specifies what the basic evaluation metrics report at
  the cutoffs inside a run of tied scores. `basic_ties` is effective
  only when `mode` is set to `basic`.

  "split"

  :   Spread the true and false positives of the run evenly over its
      cutoffs (default). This is the interpolation the ROC and
      precision-recall curves need, and is what `precrec` has always
      done.

  "hold"

  :   Give every cutoff in the run the counts it has once the whole run
      is taken, so tied instances share one value of every metric.

  A cutoff inside a tied run splits instances that share a score, so no
  threshold produces it. With `"split"` a perfect classifier whose
  scores are all `0` or `1` reports sensitivity climbing from `0` to `1`
  across the positives rather than reaching `1` at once. `"hold"` makes
  each metric a step function that changes only where the score does.
  The two agree whenever the scores are all distinct, and `"split"` is
  kept as the default because it is what every published `precrec`
  result was computed with.

- ...:

  These additional arguments are passed to
  [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
  for data preparation. `multiclass = "ovr"` asks for a one-vs-rest
  evaluation of a dataset with more than two classes; see
  [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md).

## Value

The `evalmod` function returns an `S3` object that contains performance
evaluation metrics. The number of models and the number of datasets can
be controlled by `modnames` and `dsids`. For example, the number of
models is "single" and the number of test datasets is "multiple" when
`modnames = c("m1", "m1", "m1")` and `dsids = c(1, 2, 3)` are specified.

Different `S3` objects have different default behaviors of `S3`
generics, such as
[`plot()`](https://evalclass.github.io/precrec/reference/plot.md),
[`autoplot()`](https://evalclass.github.io/precrec/reference/autoplot.md),
and
[`fortify()`](https://evalclass.github.io/precrec/reference/fortify.md).

1.  The `evalmod` function returns one of the following `S3`

    objects when `mode` is "prcroc". The objects contain ROC and
    Precision-Recall curves.

    |                 |                  |                         |
    |-----------------|------------------|-------------------------|
    | **`S3` object** | **\# of models** | **\# of test datasets** |
    | sscurves        | single           | single                  |
    | mscurves        | multiple         | single                  |
    | smcurves        | single           | multiple                |
    | mmcurves        | multiple         | multiple                |

2.  The `evalmod` function returns one of the following `S3`

    objects when `mode` is "basic". They contain the per-rank basic
    evaluation metrics; error rate, accuracy, specificity, sensitivity,
    precision, Matthews correlation coefficient, F-score, balanced
    accuracy, negative predictive value, informedness, markedness, and
    Cohen's kappa.

    |                 |                  |                         |
    |-----------------|------------------|-------------------------|
    | **`S3` object** | **\# of models** | **\# of test datasets** |
    | sspoints        | single           | single                  |
    | mspoints        | multiple         | single                  |
    | smpoints        | single           | multiple                |
    | mmpoints        | multiple         | multiple                |

3.  The `evalmod` function returns the `aucroc` S3 object

    when `mode` is "aucroc", which can be used with 'print' and
    'as.data.frame'.

## See also

[`plot()`](https://evalclass.github.io/precrec/reference/plot.md) for
plotting curves with the general R plot.
[`autoplot()`](https://evalclass.github.io/precrec/reference/autoplot.md)
and
[`fortify()`](https://evalclass.github.io/precrec/reference/fortify.md)
for plotting curves with ggplot2.
[`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
for formatting input data.
[`join_scores()`](https://evalclass.github.io/precrec/reference/join_scores.md)
and
[`join_labels()`](https://evalclass.github.io/precrec/reference/join_labels.md)
for formatting scores and labels with multiple datasets.
[`format_nfold()`](https://evalclass.github.io/precrec/reference/format_nfold.md)
for creating n-fold cross validation dataset from data frame.
[`create_sim_samples()`](https://evalclass.github.io/precrec/reference/create_sim_samples.md)
for generating random samples for simulations.

## Examples

``` r

##################################################
### Single model & single test dataset
###

## Load a dataset with 10 positives and 10 negatives
data(P10N10)

## Generate an sscurve object that contains ROC and Precision-Recall curves
sscurves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)
sscurves
#> 
#>     === AUCs ===
#> 
#>      Model name Dataset ID Curve type       AUC Baseline
#>    1         m1          1        ROC 0.7200000      0.5
#>    2         m1          1        PRC 0.7397716      0.5
#> 
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1         m1          1             10             10
#> 

## Generate an sspoints object that contains basic evaluation metrics
sspoints <- evalmod(
  mode = "basic", scores = P10N10$scores,
  labels = P10N10$labels
)
sspoints
#> 
#>     === Basic performance evaluation metrics ===
#> 
#>      ## Performance metrics
#>       rank:   normalized rank
#>       score:  score
#>       label:  label
#>       err:    error rate
#>       acc:    accuracy
#>       sp:     specificity
#>       sn:     sensitivity
#>       prec:   precision
#>       mcc:    Matthews correlation coefficient
#>       fscore: F-score
#>       bacc:   balanced accuracy
#>       npv:    negative predictive value
#>       infm:   informedness (Youden's J)
#>       mkd:    markedness
#>       kappa:  Cohen's kappa
#> 
#> 
#>      Model ID Metric       Min.    1st Qu.     Median       Mean    3rd Qu.
#>    1    m1  1   rank  0.0000000  0.2500000  0.5000000  0.5000000  0.7500000
#>    2    m1  1  score  5.0000000  5.7500000 14.0000000 11.7500000 15.2500000
#>    3    m1  1  label -1.0000000 -1.0000000  0.0000000  0.0000000  1.0000000
#>    4    m1  1    err  0.3000000  0.3500000  0.4000000  0.3952381  0.4400000
#>    5    m1  1    acc  0.5000000  0.5600000  0.6000000  0.6047619  0.6500000
#>    6    m1  1     sp  0.0000000  0.4000000  0.6333333  0.6047619  0.9000000
#>    7    m1  1     sn  0.0000000  0.4000000  0.6333333  0.6047619  0.9000000
#>    8    m1  1   prec  0.5000000  0.5750000  0.6333333  0.6892147  0.7619048
#>    9    m1  1    mcc  0.1376494  0.2238168  0.2666667  0.2755698  0.3367701
#>   10    m1  1 fscore  0.0000000  0.5333333  0.6333333  0.5579798  0.6758621
#>   11    m1  1   bacc  0.5000000  0.5600000  0.6000000  0.6047619  0.6500000
#>   12    m1  1    npv  0.5000000  0.6000000  0.6388889  0.6619921  0.8000000
#>   13    m1  1   infm  0.0000000  0.1200000  0.2000000  0.2095238  0.3000000
#>   14    m1  1    mkd  0.1960784  0.3000000  0.3333333  0.3512068  0.4000000
#>   15    m1  1  kappa  0.0000000  0.1200000  0.2000000  0.2095238  0.3000000
#>            Max.
#>    1  1.0000000
#>    2 20.0000000
#>    3  1.0000000
#>    4  0.5000000
#>    5  0.7000000
#>    6  1.0000000
#>    7  1.0000000
#>    8  1.0000000
#>    9  0.4364358
#>   10  0.7200000
#>   11  0.7000000
#>   12  0.8000000
#>   13  0.4000000
#>   14  0.5555556
#>   15  0.4000000
#> 
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1         m1          1             10             10
#> 

## Let tied scores share one value of every basic metric
tiedpoints <- evalmod(
  mode = "basic", scores = round(P10N10$scores, 1),
  labels = P10N10$labels, basic_ties = "hold"
)
tiedpoints
#> 
#>     === Basic performance evaluation metrics ===
#> 
#>      ## Performance metrics
#>       rank:   normalized rank
#>       score:  score
#>       label:  label
#>       err:    error rate
#>       acc:    accuracy
#>       sp:     specificity
#>       sn:     sensitivity
#>       prec:   precision
#>       mcc:    Matthews correlation coefficient
#>       fscore: F-score
#>       bacc:   balanced accuracy
#>       npv:    negative predictive value
#>       infm:   informedness (Youden's J)
#>       mkd:    markedness
#>       kappa:  Cohen's kappa
#> 
#> 
#>      Model ID Metric       Min.    1st Qu.     Median       Mean    3rd Qu.
#>    1    m1  1   rank  0.0000000  0.2500000  0.5000000  0.5000000  0.7500000
#>    2    m1  1  score  5.0000000  5.7500000 14.0000000 11.7500000 15.2500000
#>    3    m1  1  label -1.0000000 -1.0000000  0.0000000  0.0000000  1.0000000
#>    4    m1  1    err  0.3000000  0.4000000  0.4000000  0.4214286  0.5000000
#>    5    m1  1    acc  0.5000000  0.5000000  0.6000000  0.5785714  0.6000000
#>    6    m1  1     sp  0.0000000  0.4000000  0.5000000  0.5190476  0.9000000
#>    7    m1  1     sn  0.0000000  0.4000000  0.7000000  0.6380952  0.9000000
#>    8    m1  1   prec  0.5000000  0.5714286  0.5833333  0.6588959  0.7500000
#>    9    m1  1    mcc  0.1400280  0.2041241  0.2182179  0.2559654  0.3239094
#>   10    m1  1 fscore  0.0000000  0.5333333  0.6363636  0.5544563  0.6666667
#>   11    m1  1   bacc  0.5000000  0.5000000  0.6000000  0.5785714  0.6000000
#>   12    m1  1    npv  0.5000000  0.6000000  0.6250000  0.6594092  0.8000000
#>   13    m1  1   infm  0.0000000  0.0000000  0.2000000  0.1571429  0.2000000
#>   14    m1  1    mkd  0.1960784  0.2083333  0.3000000  0.3183050  0.4000000
#>   15    m1  1  kappa  0.0000000  0.0000000  0.2000000  0.1571429  0.2000000
#>            Max.
#>    1  1.0000000
#>    2 20.0000000
#>    3  1.0000000
#>    4  0.5000000
#>    5  0.7000000
#>    6  1.0000000
#>    7  1.0000000
#>    8  1.0000000
#>    9  0.4364358
#>   10  0.7200000
#>   11  0.7000000
#>   12  0.8000000
#>   13  0.4000000
#>   14  0.5555556
#>   15  0.4000000
#> 
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1         m1          1             10             10
#> 


##################################################
### Multiple models & single test dataset
###

## Create sample datasets with 100 positives and 100 negatives
samps <- create_sim_samples(1, 100, 100, "all")
mdat <- mmdata(samps[["scores"]], samps[["labels"]],
  modnames = samps[["modnames"]]
)

## Generate an mscurve object that contains ROC and Precision-Recall curves
mscurves <- evalmod(mdat)
mscurves
#> 
#>     === AUCs ===
#> 
#>      Model name Dataset ID Curve type       AUC Baseline
#>    1     random          1        ROC 0.5162000      0.5
#>    2     random          1        PRC 0.5066649      0.5
#>    3    poor_er          1        ROC 0.7457000      0.5
#>    4    poor_er          1        PRC 0.6591290      0.5
#>    5    good_er          1        ROC 0.7902000      0.5
#>    6    good_er          1        PRC 0.8330448      0.5
#>    7      excel          1        ROC 0.9875000      0.5
#>    8      excel          1        PRC 0.9892106      0.5
#>    9       perf          1        ROC 1.0000000      0.5
#>   10       perf          1        PRC 1.0000000      0.5
#> 
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1     random          1            100            100
#>    2    poor_er          1            100            100
#>    3    good_er          1            100            100
#>    4      excel          1            100            100
#>    5       perf          1            100            100
#> 

## Generate an mspoints object that contains basic evaluation metrics
mspoints <- evalmod(mdat, mode = "basic")
mspoints
#> 
#>     === Basic performance evaluation metrics ===
#> 
#>      ## Performance metrics
#>       rank:   normalized rank
#>       score:  score
#>       label:  label
#>       err:    error rate
#>       acc:    accuracy
#>       sp:     specificity
#>       sn:     sensitivity
#>       prec:   precision
#>       mcc:    Matthews correlation coefficient
#>       fscore: F-score
#>       bacc:   balanced accuracy
#>       npv:    negative predictive value
#>       infm:   informedness (Youden's J)
#>       mkd:    markedness
#>       kappa:  Cohen's kappa
#> 
#> 
#>        Model ID Metric         Min.     1st Qu.      Median       Mean
#>    1  random  1   rank  0.000000000  0.25000000  0.50000000 0.50000000
#>    2  random  1  score -2.088599920 -0.59347551 -0.02386908 0.02343299
#>    3  random  1  label -1.000000000 -1.00000000  0.00000000 0.00000000
#>    4  random  1    err  0.445000000  0.48000000  0.49500000 0.49194030
#>    5  random  1    acc  0.470000000  0.49500000  0.50500000 0.50805970
#>    6  random  1     sp  0.000000000  0.27000000  0.55000000 0.50805970
#>    7  random  1     sn  0.000000000  0.24000000  0.55000000 0.50805970
#>    8  random  1   prec  0.250000000  0.49152542  0.50515464 0.50239985
#>    9  random  1    mcc -0.126322788 -0.01801775  0.02060214 0.01520799
#>   10  random  1 fscore  0.000000000  0.31788079  0.54901961 0.45599050
#>   11  random  1   bacc  0.470000000  0.49500000  0.50500000 0.50805970
#>   12  random  1    npv  0.409090909  0.49444444  0.51200000 0.52153068
#>   13  random  1   infm -0.060000000 -0.01000000  0.01000000 0.01611940
#>   14  random  1    mkd -0.265957447 -0.02337541  0.02337541 0.02393053
#>   15  random  1  kappa -0.060000000 -0.01000000  0.01000000 0.01611940
#>   16 poor_er  1   rank  0.000000000  0.25000000  0.50000000 0.50000000
#>   17 poor_er  1  score  0.002985529  0.45782242  0.69633824 0.64144851
#>   18 poor_er  1  label -1.000000000 -1.00000000  0.00000000 0.00000000
#>   19 poor_er  1    err  0.295000000  0.32000000  0.36500000 0.37776119
#>   20 poor_er  1    acc  0.485000000  0.56500000  0.63500000 0.62223881
#>   21 poor_er  1     sp  0.000000000  0.44000000  0.67000000 0.62223881
#>   22 poor_er  1     sn  0.000000000  0.36000000  0.67000000 0.62223881
#>   23 poor_er  1   prec  0.000000000  0.60493827  0.66386555 0.62845635
#>   24 poor_er  1    mcc -0.123403510  0.21901763  0.31448545 0.28802621
#>   25 poor_er  1 fscore  0.000000000  0.48000000  0.66889632 0.57061654
#>   26 poor_er  1   bacc  0.485000000  0.56500000  0.63500000 0.62223881
#>   27 poor_er  1    npv  0.492385787  0.57333333  0.66990291 0.71987154
#>   28 poor_er  1   infm -0.030000000  0.13000000  0.27000000 0.24447761
#>   29 poor_er  1    mkd -0.507614213  0.28800000  0.35172344 0.34832790
#>   30 poor_er  1  kappa -0.030000000  0.13000000  0.27000000 0.24447761
#>   31 good_er  1   rank  0.000000000  0.25000000  0.50000000 0.50000000
#>   32 good_er  1  score  0.003093247  0.11440437  0.25141843 0.34506001
#>   33 good_er  1  label -1.000000000 -1.00000000  0.00000000 0.00000000
#>   34 good_er  1    err  0.255000000  0.29500000  0.33500000 0.35562189
#>   35 good_er  1    acc  0.500000000  0.58000000  0.66500000 0.64437811
#>   36 good_er  1     sp  0.000000000  0.38000000  0.70000000 0.64437811
#>   37 good_er  1     sn  0.000000000  0.44000000  0.70000000 0.64437811
#>   38 good_er  1   prec  0.500000000  0.58666667  0.70707071 0.74374779
#>   39 good_er  1    mcc  0.070888121  0.25928520  0.36147845 0.34450211
#>   40 good_er  1 fscore  0.000000000  0.59310345  0.68401487 0.60964952
#>   41 good_er  1   bacc  0.500000000  0.58000000  0.66500000 0.64437811
#>   42 good_er  1    npv  0.500000000  0.63087248  0.70588235 0.69702174
#>   43 good_er  1   infm  0.000000000  0.16000000  0.33000000 0.28875622
#>   44 good_er  1    mkd  0.203547543  0.36465036  0.44642857 0.44076952
#>   45 good_er  1  kappa  0.000000000  0.16000000  0.33000000 0.28875622
#>   46   excel  1   rank  0.000000000  0.25000000  0.50000000 0.50000000
#>   47   excel  1  score -2.443795138 -0.21332516  1.38505192 1.43120691
#>   48   excel  1  label -1.000000000 -1.00000000  0.00000000 0.00000000
#>   49   excel  1    err  0.045000000  0.13000000  0.25000000 0.25746269
#>   50   excel  1    acc  0.500000000  0.62500000  0.75000000 0.74253731
#>   51   excel  1     sp  0.000000000  0.50000000  0.94000000 0.74253731
#>   52   excel  1     sn  0.000000000  0.50000000  0.94000000 0.74253731
#>   53   excel  1   prec  0.500000000  0.66666667  0.94059406 0.84023777
#>   54   excel  1    mcc  0.070888121  0.38226007  0.57735027 0.56079139
#>   55   excel  1 fscore  0.000000000  0.66666667  0.76335878 0.70441689
#>   56   excel  1   bacc  0.500000000  0.62500000  0.75000000 0.74253731
#>   57   excel  1    npv  0.500000000  0.66666667  0.94117647 0.83916941
#>   58   excel  1   infm  0.000000000  0.25000000  0.50000000 0.48507463
#>   59   excel  1    mkd  0.500000000  0.57142857  0.66666667 0.67940718
#>   60   excel  1  kappa  0.000000000  0.25000000  0.50000000 0.48507463
#>   61    perf  1   rank  0.000000000  0.25000000  0.50000000 0.50000000
#>   62    perf  1  score  0.000000000  0.00000000  0.50000000 0.50000000
#>   63    perf  1  label -1.000000000 -1.00000000  0.00000000 0.00000000
#>   64    perf  1    err  0.000000000  0.12500000  0.25000000 0.25124378
#>   65    perf  1    acc  0.500000000  0.62500000  0.75000000 0.74875622
#>   66    perf  1     sp  0.000000000  0.50000000  1.00000000 0.74875622
#>   67    perf  1     sn  0.000000000  0.50000000  1.00000000 0.74875622
#>   68    perf  1   prec  0.500000000  0.66666667  1.00000000 0.84609623
#>   69    perf  1    mcc  0.070888121  0.38226007  0.57735027 0.57352524
#>   70    perf  1 fscore  0.000000000  0.66666667  0.76335878 0.71042736
#>   71    perf  1   bacc  0.500000000  0.62500000  0.75000000 0.74875622
#>   72    perf  1    npv  0.500000000  0.66666667  1.00000000 0.84609623
#>   73    perf  1   infm  0.000000000  0.25000000  0.50000000 0.49751244
#>   74    perf  1    mkd  0.500000000  0.57142857  0.66666667 0.69219247
#>   75    perf  1  kappa  0.000000000  0.25000000  0.50000000 0.49751244
#>         3rd Qu.      Max.
#>    1 0.75000000 1.0000000
#>    2 0.56142597 3.0499905
#>    3 1.00000000 1.0000000
#>    4 0.50500000 0.5300000
#>    5 0.52000000 0.5550000
#>    6 0.74000000 1.0000000
#>    7 0.77000000 1.0000000
#>    8 0.51898734 1.0000000
#>    9 0.04717707 0.1100055
#>   10 0.61600000 0.6711409
#>   11 0.52000000 0.5550000
#>   12 0.53333333 1.0000000
#>   13 0.04000000 0.1100000
#>   14 0.05482456 0.5050505
#>   15 0.04000000 0.1100000
#>   16 0.75000000 1.0000000
#>   17 0.87294326 0.9998229
#>   18 1.00000000 1.0000000
#>   19 0.43500000 0.5150000
#>   20 0.68000000 0.7050000
#>   21 0.86000000 1.0000000
#>   22 0.94000000 1.0000000
#>   23 0.68888889 0.7358491
#>   24 0.38507177 0.4599069
#>   25 0.72222222 0.7588933
#>   26 0.68000000 0.7050000
#>   27 0.88000000 1.0000000
#>   28 0.36000000 0.4100000
#>   29 0.50006758 0.5747126
#>   30 0.36000000 0.4100000
#>   31 0.75000000 1.0000000
#>   32 0.54798132 0.9895181
#>   33 1.00000000 1.0000000
#>   34 0.42000000 0.5000000
#>   35 0.70500000 0.7450000
#>   36 0.94000000 1.0000000
#>   37 0.88000000 1.0000000
#>   38 0.88888889 1.0000000
#>   39 0.45220164 0.5190783
#>   40 0.70718232 0.7226891
#>   41 0.70500000 0.7450000
#>   42 0.74468085 1.0000000
#>   43 0.41000000 0.4900000
#>   44 0.52137643 0.6097561
#>   45 0.41000000 0.4900000
#>   46 0.75000000 1.0000000
#>   47 2.93874871 5.5754949
#>   48 1.00000000 1.0000000
#>   49 0.37500000 0.5000000
#>   50 0.87000000 0.9550000
#>   51 1.00000000 1.0000000
#>   52 1.00000000 1.0000000
#>   53 1.00000000 1.0000000
#>   54 0.76431763 0.9111396
#>   55 0.87005650 0.9538462
#>   56 0.87000000 0.9550000
#>   57 1.00000000 1.0000000
#>   58 0.74000000 0.9100000
#>   59 0.78740157 0.9122807
#>   60 0.74000000 0.9100000
#>   61 0.75000000 1.0000000
#>   62 1.00000000 1.0000000
#>   63 1.00000000 1.0000000
#>   64 0.37500000 0.5000000
#>   65 0.87500000 1.0000000
#>   66 1.00000000 1.0000000
#>   67 1.00000000 1.0000000
#>   68 1.00000000 1.0000000
#>   69 0.77459667 1.0000000
#>   70 0.87640449 1.0000000
#>   71 0.87500000 1.0000000
#>   72 1.00000000 1.0000000
#>   73 0.75000000 1.0000000
#>   74 0.80000000 1.0000000
#>   75 0.75000000 1.0000000
#> 
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1     random          1            100            100
#>    2    poor_er          1            100            100
#>    3    good_er          1            100            100
#>    4      excel          1            100            100
#>    5       perf          1            100            100
#> 


##################################################
### Single model & multiple test datasets
###

## Create sample datasets with 100 positives and 100 negatives
samps <- create_sim_samples(4, 100, 100, "good_er")
mdat <- mmdata(samps[["scores"]], samps[["labels"]],
  modnames = samps[["modnames"]],
  dsids = samps[["dsids"]]
)

## Generate an smcurve object that contains ROC and Precision-Recall curves
smcurves <- evalmod(mdat)
smcurves
#> 
#>     === AUCs ===
#> 
#>      Model name Dataset ID Curve type       AUC Baseline
#>    1    good_er          1        ROC 0.8532000      0.5
#>    2    good_er          1        PRC 0.8822592      0.5
#>    3    good_er          2        ROC 0.8381000      0.5
#>    4    good_er          2        PRC 0.8756886      0.5
#>    5    good_er          3        ROC 0.8421000      0.5
#>    6    good_er          3        PRC 0.8761194      0.5
#>    7    good_er          4        ROC 0.8172000      0.5
#>    8    good_er          4        PRC 0.8617359      0.5
#> 
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1    good_er          1            100            100
#>    2    good_er          2            100            100
#>    3    good_er          3            100            100
#>    4    good_er          4            100            100
#> 

## Generate an smpoints object that contains basic evaluation metrics
smpoints <- evalmod(mdat, mode = "basic")
smpoints
#> 
#>     === Basic performance evaluation metrics ===
#> 
#>      ## Performance metrics
#>       rank:   normalized rank
#>       score:  score
#>       label:  label
#>       err:    error rate
#>       acc:    accuracy
#>       sp:     specificity
#>       sn:     sensitivity
#>       prec:   precision
#>       mcc:    Matthews correlation coefficient
#>       fscore: F-score
#>       bacc:   balanced accuracy
#>       npv:    negative predictive value
#>       infm:   informedness (Youden's J)
#>       mkd:    markedness
#>       kappa:  Cohen's kappa
#> 
#> 
#>        Model ID Metric          Min.    1st Qu.    Median      Mean   3rd Qu.
#>    1 good_er  1   rank  0.0000000000  0.2500000 0.5000000 0.5000000 0.7500000
#>    2 good_er  1  score  0.0017103979  0.1117255 0.2669684 0.3506574 0.5303036
#>    3 good_er  1  label -1.0000000000 -1.0000000 0.0000000 0.0000000 1.0000000
#>    4 good_er  1    err  0.2150000000  0.2450000 0.2950000 0.3242786 0.4000000
#>    5 good_er  1    acc  0.5000000000  0.6000000 0.7050000 0.6757214 0.7550000
#>    6 good_er  1     sp  0.0000000000  0.4200000 0.7600000 0.6757214 1.0000000
#>    7 good_er  1     sn  0.0000000000  0.5000000 0.7600000 0.6757214 0.9200000
#>    8 good_er  1   prec  0.5000000000  0.6133333 0.7647059 0.7757809 1.0000000
#>    9 good_er  1    mcc  0.0000000000  0.3121539 0.4501838 0.4141748 0.5401080
#>   10 good_er  1 fscore  0.0000000000  0.6578947 0.7089552 0.6401021 0.7542373
#>   11 good_er  1   bacc  0.5000000000  0.6000000 0.7050000 0.6757214 0.7550000
#>   12 good_er  1    npv  0.5000000000  0.6621622 0.7647059 0.7390155 0.8372093
#>   13 good_er  1   infm  0.0000000000  0.2000000 0.4100000 0.3514428 0.5100000
#>   14 good_er  1    mkd  0.0000000000  0.4679144 0.5291005 0.5147963 0.5711954
#>   15 good_er  1  kappa  0.0000000000  0.2000000 0.4100000 0.3514428 0.5100000
#>   16 good_er  2   rank  0.0000000000  0.2500000 0.5000000 0.5000000 0.7500000
#>   17 good_er  2  score  0.0008981645  0.1242699 0.2713954 0.3377941 0.5065014
#>   18 good_er  2  label -1.0000000000 -1.0000000 0.0000000 0.0000000 1.0000000
#>   19 good_er  2    err  0.2150000000  0.2500000 0.3100000 0.3317910 0.4050000
#>   20 good_er  2    acc  0.4950000000  0.5950000 0.6900000 0.6682090 0.7500000
#>   21 good_er  2     sp  0.0000000000  0.3900000 0.7500000 0.6682090 0.9900000
#>   22 good_er  2     sn  0.0000000000  0.4900000 0.7500000 0.6682090 0.8900000
#>   23 good_er  2   prec  0.4974874372  0.5933333 0.7474747 0.7705077 0.9795918
#>   24 good_er  2    mcc -0.0708881205  0.2955243 0.4253394 0.3968908 0.5138291
#>   25 good_er  2 fscore  0.0000000000  0.6533333 0.7072243 0.6338300 0.7413793
#>   26 good_er  2   bacc  0.4950000000  0.5950000 0.6900000 0.6682090 0.7500000
#>   27 good_er  2    npv  0.0000000000  0.6470588 0.7450980 0.7160724 0.7941176
#>   28 good_er  2   infm -0.0100000000  0.1900000 0.3800000 0.3364179 0.5000000
#>   29 good_er  2    mkd -0.5025125628  0.4285714 0.5002001 0.4865801 0.5778983
#>   30 good_er  2  kappa -0.0100000000  0.1900000 0.3800000 0.3364179 0.5000000
#>   31 good_er  3   rank  0.0000000000  0.2500000 0.5000000 0.5000000 0.7500000
#>   32 good_er  3  score  0.0014233844  0.1008353 0.2574960 0.3643232 0.6152085
#>   33 good_er  3  label -1.0000000000 -1.0000000 0.0000000 0.0000000 1.0000000
#>   34 good_er  3    err  0.2150000000  0.2400000 0.3100000 0.3298010 0.4150000
#>   35 good_er  3    acc  0.5000000000  0.5850000 0.6900000 0.6701990 0.7600000
#>   36 good_er  3     sp  0.0000000000  0.4000000 0.7600000 0.6701990 0.9700000
#>   37 good_er  3     sn  0.0000000000  0.4700000 0.7600000 0.6701990 0.9000000
#>   38 good_er  3   prec  0.5000000000  0.6000000 0.7600000 0.7714113 0.9508197
#>   39 good_er  3    mcc  0.0320256308  0.2723814 0.4465861 0.3985118 0.5247635
#>   40 good_er  3 fscore  0.0000000000  0.6266667 0.7054264 0.6356657 0.7542373
#>   41 good_er  3   bacc  0.5000000000  0.5850000 0.6900000 0.6701990 0.7600000
#>   42 good_er  3    npv  0.5000000000  0.6447368 0.7522936 0.7220218 0.8000000
#>   43 good_er  3   infm  0.0000000000  0.1700000 0.3800000 0.3403980 0.5200000
#>   44 good_er  3    mkd  0.1025641026  0.4079484 0.5208333 0.4934331 0.5758808
#>   45 good_er  3  kappa  0.0000000000  0.1700000 0.3800000 0.3403980 0.5200000
#>   46 good_er  4   rank  0.0000000000  0.2500000 0.5000000 0.5000000 0.7500000
#>   47 good_er  4  score  0.0007582405  0.1055548 0.2795836 0.3644613 0.5721920
#>   48 good_er  4  label -1.0000000000 -1.0000000 0.0000000 0.0000000 1.0000000
#>   49 good_er  4    err  0.2250000000  0.2600000 0.3350000 0.3421891 0.4150000
#>   50 good_er  4    acc  0.5000000000  0.5850000 0.6650000 0.6578109 0.7400000
#>   51 good_er  4     sp  0.0000000000  0.3700000 0.7500000 0.6578109 0.9800000
#>   52 good_er  4     sn  0.0000000000  0.4800000 0.7500000 0.6578109 0.8700000
#>   53 good_er  4   prec  0.5000000000  0.5800000 0.7500000 0.7610031 0.9574468
#>   54 good_er  4    mcc  0.0708881205  0.2575196 0.3878359 0.3758932 0.5070108
#>   55 good_er  4 fscore  0.0000000000  0.6357616 0.6962025 0.6241982 0.7222222
#>   56 good_er  4   bacc  0.5000000000  0.5850000 0.6650000 0.6578109 0.7400000
#>   57 good_er  4    npv  0.5000000000  0.6510067 0.7297297 0.7176431 0.7500000
#>   58 good_er  4   infm  0.0000000000  0.1700000 0.3300000 0.3156219 0.4800000
#>   59 good_er  4    mkd  0.2604166667  0.4016064 0.5025126 0.4786462 0.5642361
#>   60 good_er  4  kappa  0.0000000000  0.1700000 0.3300000 0.3156219 0.4800000
#>           Max.
#>    1 1.0000000
#>    2 0.9965829
#>    3 1.0000000
#>    4 0.5000000
#>    5 0.7850000
#>    6 1.0000000
#>    7 1.0000000
#>    8 1.0000000
#>    9 0.5784194
#>   10 0.7867299
#>   11 0.7850000
#>   12 1.0000000
#>   13 0.5700000
#>   14 0.6666667
#>   15 0.5700000
#>   16 1.0000000
#>   17 0.9768233
#>   18 1.0000000
#>   19 0.5050000
#>   20 0.7850000
#>   21 1.0000000
#>   22 1.0000000
#>   23 1.0000000
#>   24 0.5955947
#>   25 0.7675676
#>   26 0.7850000
#>   27 0.9411765
#>   28 0.5700000
#>   29 0.6546015
#>   30 0.5700000
#>   31 1.0000000
#>   32 0.9842874
#>   33 1.0000000
#>   34 0.5000000
#>   35 0.7850000
#>   36 1.0000000
#>   37 1.0000000
#>   38 1.0000000
#>   39 0.6054143
#>   40 0.7777778
#>   41 0.7850000
#>   42 1.0000000
#>   43 0.5700000
#>   44 0.6545115
#>   45 0.5700000
#>   46 1.0000000
#>   47 0.9976283
#>   48 1.0000000
#>   49 0.5000000
#>   50 0.7750000
#>   51 1.0000000
#>   52 1.0000000
#>   53 1.0000000
#>   54 0.5810859
#>   55 0.7537688
#>   56 0.7750000
#>   57 1.0000000
#>   58 0.5500000
#>   59 0.6370958
#>   60 0.5500000
#> 
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1    good_er          1            100            100
#>    2    good_er          2            100            100
#>    3    good_er          3            100            100
#>    4    good_er          4            100            100
#> 


##################################################
### Multiple models & multiple test datasets
###

## Create sample datasets with 100 positives and 100 negatives
samps <- create_sim_samples(4, 100, 100, "all")
mdat <- mmdata(samps[["scores"]], samps[["labels"]],
  modnames = samps[["modnames"]],
  dsids = samps[["dsids"]]
)

## Generate an mmcurve object that contains ROC and Precision-Recall curves
mmcurves <- evalmod(mdat)
mmcurves
#> 
#>     === AUCs ===
#> 
#>      Model name Dataset ID Curve type       AUC Baseline
#>    1     random          1        ROC 0.5146000      0.5
#>    2     random          1        PRC 0.5237073      0.5
#>    3    poor_er          1        ROC 0.7957000      0.5
#>    4    poor_er          1        PRC 0.7084909      0.5
#>    5    good_er          1        ROC 0.7602000      0.5
#>    6    good_er          1        PRC 0.7903612      0.5
#>    7      excel          1        ROC 0.9920000      0.5
#>    8      excel          1        PRC 0.9916963      0.5
#>    9       perf          1        ROC 1.0000000      0.5
#>   10       perf          1        PRC 1.0000000      0.5
#>   11     random          2        ROC 0.5276000      0.5
#>   12     random          2        PRC 0.5200409      0.5
#>   13    poor_er          2        ROC 0.7875000      0.5
#>   14    poor_er          2        PRC 0.7792567      0.5
#>   15    good_er          2        ROC 0.8350000      0.5
#>   16    good_er          2        PRC 0.8539610      0.5
#>   17      excel          2        ROC 0.9909000      0.5
#>   18      excel          2        PRC 0.9919093      0.5
#>   19       perf          2        ROC 1.0000000      0.5
#>   20       perf          2        PRC 1.0000000      0.5
#>   21     random          3        ROC 0.5498000      0.5
#>   22     random          3        PRC 0.5596541      0.5
#>   23    poor_er          3        ROC 0.8111000      0.5
#>   24    poor_er          3        PRC 0.7788746      0.5
#>   25    good_er          3        ROC 0.7795000      0.5
#>   26    good_er          3        PRC 0.8176583      0.5
#>   27      excel          3        ROC 0.9827000      0.5
#>   28      excel          3        PRC 0.9812225      0.5
#>   29       perf          3        ROC 1.0000000      0.5
#>   30       perf          3        PRC 1.0000000      0.5
#>   31     random          4        ROC 0.4849000      0.5
#>   32     random          4        PRC 0.5042369      0.5
#>   33    poor_er          4        ROC 0.8394000      0.5
#>   34    poor_er          4        PRC 0.7801863      0.5
#>   35    good_er          4        ROC 0.7823000      0.5
#>   36    good_er          4        PRC 0.8188463      0.5
#>   37      excel          4        ROC 0.9911000      0.5
#>   38      excel          4        PRC 0.9912694      0.5
#>   39       perf          4        ROC 1.0000000      0.5
#>   40       perf          4        PRC 1.0000000      0.5
#> 
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1     random          1            100            100
#>    2    poor_er          1            100            100
#>    3    good_er          1            100            100
#>    4      excel          1            100            100
#>    5       perf          1            100            100
#>    6     random          2            100            100
#>    7    poor_er          2            100            100
#>    8    good_er          2            100            100
#>    9      excel          2            100            100
#>   10       perf          2            100            100
#>   11     random          3            100            100
#>   12    poor_er          3            100            100
#>   13    good_er          3            100            100
#>   14      excel          3            100            100
#>   15       perf          3            100            100
#>   16     random          4            100            100
#>   17    poor_er          4            100            100
#>   18    good_er          4            100            100
#>   19      excel          4            100            100
#>   20       perf          4            100            100
#> 

## Generate an mmpoints object that contains basic evaluation metrics
mmpoints <- evalmod(mdat, mode = "basic")
mmpoints
#> 
#>     === Basic performance evaluation metrics ===
#> 
#>      ## Performance metrics
#>       rank:   normalized rank
#>       score:  score
#>       label:  label
#>       err:    error rate
#>       acc:    accuracy
#>       sp:     specificity
#>       sn:     sensitivity
#>       prec:   precision
#>       mcc:    Matthews correlation coefficient
#>       fscore: F-score
#>       bacc:   balanced accuracy
#>       npv:    negative predictive value
#>       infm:   informedness (Youden's J)
#>       mkd:    markedness
#>       kappa:  Cohen's kappa
#> 
#> 
#>        Model ID Metric         Min.      1st Qu.      Median         Mean
#>    1  random  1   rank  0.000000000  0.250000000  0.50000000  0.500000000
#>    2  random  1  score -2.369997103 -0.668307093 -0.02469509  0.031345356
#>    3  random  1  label -1.000000000 -1.000000000  0.00000000  0.000000000
#>    4  random  1    err  0.455000000  0.485000000  0.49500000  0.492736318
#>    5  random  1    acc  0.480000000  0.495000000  0.50500000  0.507263682
#>    6  random  1     sp  0.000000000  0.280000000  0.50000000  0.507263682
#>    7  random  1     sn  0.000000000  0.240000000  0.50000000  0.507263682
#>    8  random  1   prec  0.444444444  0.494505495  0.50806452  0.520719735
#>    9  random  1    mcc -0.123403510 -0.011020090  0.02001602  0.018919664
#>   10  random  1 fscore  0.000000000  0.320000000  0.50251256  0.456630995
#>   11  random  1   bacc  0.480000000  0.495000000  0.50500000  0.507263682
#>   12  random  1    npv  0.000000000  0.496000000  0.50537634  0.503548076
#>   13  random  1   infm -0.040000000 -0.010000000  0.01000000  0.014527363
#>   14  random  1    mkd -0.507614213 -0.012268433  0.02029221  0.024267811
#>   15  random  1  kappa -0.040000000 -0.010000000  0.01000000  0.014527363
#>   16 poor_er  1   rank  0.000000000  0.250000000  0.50000000  0.500000000
#>   17 poor_er  1  score  0.010392811  0.463080594  0.71360146  0.658913152
#>   18 poor_er  1  label -1.000000000 -1.000000000  0.00000000  0.000000000
#>   19 poor_er  1    err  0.245000000  0.285000000  0.32500000  0.352885572
#>   20 poor_er  1    acc  0.495000000  0.580000000  0.67500000  0.647114428
#>   21 poor_er  1     sp  0.000000000  0.470000000  0.75000000  0.647114428
#>   22 poor_er  1     sn  0.000000000  0.390000000  0.75000000  0.647114428
#>   23 poor_er  1   prec  0.000000000  0.593939394  0.68595041  0.665338459
#>   24 poor_er  1    mcc -0.070888121  0.257499727  0.41633320  0.340906972
#>   25 poor_er  1 fscore  0.000000000  0.520000000  0.69892473  0.598279528
#>   26 poor_er  1   bacc  0.495000000  0.580000000  0.67500000  0.647114428
#>   27 poor_er  1    npv  0.497382199  0.593333333  0.74757282  0.740421262
#>   28 poor_er  1   infm -0.010000000  0.160000000  0.35000000  0.294228856
#>   29 poor_er  1    mkd -0.502512563  0.381629162  0.46010549  0.405759721
#>   30 poor_er  1  kappa -0.010000000  0.160000000  0.35000000  0.294228856
#>   31 good_er  1   rank  0.000000000  0.250000000  0.50000000  0.500000000
#>   32 good_er  1  score  0.002756035  0.130615986  0.26873149  0.340532675
#>   33 good_er  1  label -1.000000000 -1.000000000  0.00000000  0.000000000
#>   34 good_er  1    err  0.290000000  0.315000000  0.35000000  0.370547264
#>   35 good_er  1    acc  0.495000000  0.580000000  0.65000000  0.629452736
#>   36 good_er  1     sp  0.000000000  0.380000000  0.70000000  0.629452736
#>   37 good_er  1     sn  0.000000000  0.430000000  0.70000000  0.629452736
#>   38 good_er  1   prec  0.497435897  0.586666667  0.69696970  0.718639747
#>   39 good_er  1    mcc -0.070888121  0.252046500  0.35682062  0.303953673
#>   40 good_er  1 fscore  0.000000000  0.569536424  0.66666667  0.591936257
#>   41 good_er  1   bacc  0.495000000  0.580000000  0.65000000  0.629452736
#>   42 good_er  1    npv  0.000000000  0.594936709  0.66379310  0.651360561
#>   43 good_er  1   infm -0.010000000  0.160000000  0.30000000  0.258905473
#>   44 good_er  1    mkd -0.502512563  0.346666667  0.39477680  0.370000308
#>   45 good_er  1  kappa -0.010000000  0.160000000  0.30000000  0.258905473
#>   46   excel  1   rank  0.000000000  0.250000000  0.50000000  0.500000000
#>   47   excel  1  score -2.783539413  0.002649425  1.44490925  1.538649677
#>   48   excel  1  label -1.000000000 -1.000000000  0.00000000  0.000000000
#>   49   excel  1    err  0.040000000  0.130000000  0.25000000  0.255223881
#>   50   excel  1    acc  0.500000000  0.625000000  0.75000000  0.744776119
#>   51   excel  1     sp  0.000000000  0.500000000  0.95000000  0.744776119
#>   52   excel  1     sn  0.000000000  0.500000000  0.95000000  0.744776119
#>   53   excel  1   prec  0.500000000  0.666666667  0.94897959  0.841857902
#>   54   excel  1    mcc  0.070888121  0.382260072  0.57735027  0.565438685
#>   55   excel  1 fscore  0.000000000  0.666666667  0.76335878  0.706333074
#>   56   excel  1   bacc  0.500000000  0.625000000  0.75000000  0.744776119
#>   57   excel  1    npv  0.500000000  0.666666667  0.94949495  0.842281664
#>   58   excel  1   infm  0.000000000  0.250000000  0.50000000  0.489552239
#>   59   excel  1    mkd  0.500000000  0.571428571  0.66666667  0.684139566
#>   60   excel  1  kappa  0.000000000  0.250000000  0.50000000  0.489552239
#>   61    perf  1   rank  0.000000000  0.250000000  0.50000000  0.500000000
#>   62    perf  1  score  0.000000000  0.000000000  0.50000000  0.500000000
#>   63    perf  1  label -1.000000000 -1.000000000  0.00000000  0.000000000
#>   64    perf  1    err  0.000000000  0.125000000  0.25000000  0.251243781
#>   65    perf  1    acc  0.500000000  0.625000000  0.75000000  0.748756219
#>   66    perf  1     sp  0.000000000  0.500000000  1.00000000  0.748756219
#>   67    perf  1     sn  0.000000000  0.500000000  1.00000000  0.748756219
#>   68    perf  1   prec  0.500000000  0.666666667  1.00000000  0.846096234
#>   69    perf  1    mcc  0.070888121  0.382260072  0.57735027  0.573525244
#>   70    perf  1 fscore  0.000000000  0.666666667  0.76335878  0.710427365
#>   71    perf  1   bacc  0.500000000  0.625000000  0.75000000  0.748756219
#>   72    perf  1    npv  0.500000000  0.666666667  1.00000000  0.846096234
#>   73    perf  1   infm  0.000000000  0.250000000  0.50000000  0.497512438
#>   74    perf  1    mkd  0.500000000  0.571428571  0.66666667  0.692192468
#>   75    perf  1  kappa  0.000000000  0.250000000  0.50000000  0.497512438
#>   76  random  2   rank  0.000000000  0.250000000  0.50000000  0.500000000
#>   77  random  2  score -2.703088861 -0.610472804  0.08936676  0.051044383
#>   78  random  2  label -1.000000000 -1.000000000  0.00000000  0.000000000
#>   79  random  2    err  0.465000000  0.480000000  0.48500000  0.486268657
#>   80  random  2    acc  0.480000000  0.510000000  0.51500000  0.513731343
#>   81  random  2     sp  0.000000000  0.270000000  0.49000000  0.513731343
#>   82  random  2     sn  0.000000000  0.270000000  0.49000000  0.513731343
#>   83  random  2   prec  0.000000000  0.505494505  0.51578947  0.514199976
#>   84  random  2    mcc -0.096076892  0.020667821  0.04113450  0.035048877
#>   85  random  2 fscore  0.000000000  0.360000000  0.50000000  0.464477550
#>   86  random  2   bacc  0.480000000  0.510000000  0.51500000  0.513731343
#>   87  random  2    npv  0.000000000  0.507575758  0.51388889  0.523460311
#>   88  random  2   infm -0.040000000  0.020000000  0.03000000  0.027462687
#>   89  random  2    mkd -0.502512563  0.021701389  0.04960317  0.037660287
#>   90  random  2  kappa -0.040000000  0.020000000  0.03000000  0.027462687
#>   91 poor_er  2   rank  0.000000000  0.250000000  0.50000000  0.500000000
#>   92 poor_er  2  score  0.021795799  0.443988471  0.74746965  0.654716332
#>   93 poor_er  2  label -1.000000000 -1.000000000  0.00000000  0.000000000
#>   94 poor_er  2    err  0.285000000  0.310000000  0.34500000  0.356965174
#>   95 poor_er  2    acc  0.500000000  0.610000000  0.65500000  0.643034826
#>   96 poor_er  2     sp  0.000000000  0.450000000  0.70000000  0.643034826
#>   97 poor_er  2     sn  0.000000000  0.390000000  0.70000000  0.643034826
#>   98 poor_er  2   prec  0.500000000  0.630136986  0.69811321  0.716095312
#>   99 poor_er  2    mcc  0.070888121  0.314485451  0.35634832  0.347610063
#>  100 poor_er  2 fscore  0.000000000  0.524137931  0.68783069  0.598715790
#>  101 poor_er  2   bacc  0.500000000  0.610000000  0.65500000  0.643034826
#>  102 poor_er  2    npv  0.500000000  0.597402597  0.70103093  0.737361853
#>  103 poor_er  2   infm  0.000000000  0.220000000  0.31000000  0.286069652
#>  104 poor_er  2    mkd  0.316628453  0.394776799  0.43956044  0.453457166
#>  105 poor_er  2  kappa  0.000000000  0.220000000  0.31000000  0.286069652
#>  106 good_er  2   rank  0.000000000  0.250000000  0.50000000  0.500000000
#>  107 good_er  2  score  0.003226189  0.115310392  0.30327978  0.383512926
#>  108 good_er  2  label -1.000000000 -1.000000000  0.00000000  0.000000000
#>  109 good_er  2    err  0.220000000  0.265000000  0.31500000  0.333333333
#>  110 good_er  2    acc  0.500000000  0.610000000  0.68500000  0.666666667
#>  111 good_er  2     sp  0.000000000  0.400000000  0.76000000  0.666666667
#>  112 good_er  2     sn  0.000000000  0.460000000  0.76000000  0.666666667
#>  113 good_er  2   prec  0.500000000  0.600000000  0.76470588  0.760162705
#>  114 good_er  2    mcc  0.070888121  0.318329379  0.41979189  0.394594979
#>  115 good_er  2 fscore  0.000000000  0.613333333  0.70930233  0.629207273
#>  116 good_er  2   bacc  0.500000000  0.610000000  0.68500000  0.666666667
#>  117 good_er  2    npv  0.500000000  0.640000000  0.76699029  0.736368039
#>  118 good_er  2   infm  0.000000000  0.220000000  0.37000000  0.333333333
#>  119 good_er  2    mkd  0.255102041  0.460105488  0.51251131  0.496530744
#>  120 good_er  2  kappa  0.000000000  0.220000000  0.37000000  0.333333333
#>  121   excel  2   rank  0.000000000  0.250000000  0.50000000  0.500000000
#>  122   excel  2  score -2.111172777 -0.238733589  1.41101723  1.473986530
#>  123   excel  2  label -1.000000000 -1.000000000  0.00000000  0.000000000
#>  124   excel  2    err  0.030000000  0.130000000  0.25000000  0.255771144
#>  125   excel  2    acc  0.500000000  0.625000000  0.75000000  0.744228856
#>  126   excel  2     sp  0.000000000  0.500000000  0.96000000  0.744228856
#>  127   excel  2     sn  0.000000000  0.500000000  0.96000000  0.744228856
#>  128   excel  2   prec  0.500000000  0.666666667  0.96000000  0.841758793
#>  129   excel  2    mcc  0.070888121  0.382260072  0.57735027  0.564286286
#>  130   excel  2 fscore  0.000000000  0.666666667  0.76335878  0.706012592
#>  131   excel  2   bacc  0.500000000  0.625000000  0.75000000  0.744228856
#>  132   excel  2    npv  0.500000000  0.666666667  0.95918367  0.841191374
#>  133   excel  2   infm  0.000000000  0.250000000  0.50000000  0.488457711
#>  134   excel  2    mkd  0.500000000  0.571428571  0.66666667  0.682950167
#>  135   excel  2  kappa  0.000000000  0.250000000  0.50000000  0.488457711
#>  136    perf  2   rank  0.000000000  0.250000000  0.50000000  0.500000000
#>  137    perf  2  score  0.000000000  0.000000000  0.50000000  0.500000000
#>  138    perf  2  label -1.000000000 -1.000000000  0.00000000  0.000000000
#>  139    perf  2    err  0.000000000  0.125000000  0.25000000  0.251243781
#>  140    perf  2    acc  0.500000000  0.625000000  0.75000000  0.748756219
#>  141    perf  2     sp  0.000000000  0.500000000  1.00000000  0.748756219
#>  142    perf  2     sn  0.000000000  0.500000000  1.00000000  0.748756219
#>  143    perf  2   prec  0.500000000  0.666666667  1.00000000  0.846096234
#>  144    perf  2    mcc  0.070888121  0.382260072  0.57735027  0.573525244
#>  145    perf  2 fscore  0.000000000  0.666666667  0.76335878  0.710427365
#>  146    perf  2   bacc  0.500000000  0.625000000  0.75000000  0.748756219
#>  147    perf  2    npv  0.500000000  0.666666667  1.00000000  0.846096234
#>  148    perf  2   infm  0.000000000  0.250000000  0.50000000  0.497512438
#>  149    perf  2    mkd  0.500000000  0.571428571  0.66666667  0.692192468
#>  150    perf  2  kappa  0.000000000  0.250000000  0.50000000  0.497512438
#>  151  random  3   rank  0.000000000  0.250000000  0.50000000  0.500000000
#>  152  random  3  score -2.561729710 -0.608765923  0.02182284  0.005821703
#>  153  random  3  label -1.000000000 -1.000000000  0.00000000  0.000000000
#>  154  random  3    err  0.415000000  0.465000000  0.47500000  0.475223881
#>  155  random  3    acc  0.480000000  0.505000000  0.52500000  0.524776119
#>  156  random  3     sp  0.000000000  0.250000000  0.52000000  0.524776119
#>  157  random  3     sn  0.000000000  0.300000000  0.52000000  0.524776119
#>  158  random  3   prec  0.487012987  0.502538071  0.53125000  0.554113797
#>  159  random  3    mcc -0.058621038  0.011073376  0.06059679  0.058068029
#>  160  random  3 fscore  0.000000000  0.402684564  0.52307692  0.480268724
#>  161  random  3   bacc  0.480000000  0.505000000  0.52500000  0.524776119
#>  162  random  3    npv  0.333333333  0.502538071  0.52040816  0.525381871
#>  163  random  3   infm -0.040000000  0.010000000  0.05000000  0.049552239
#>  164  random  3    mkd -0.171821306  0.013159626  0.07085737  0.079495668
#>  165  random  3  kappa -0.040000000  0.010000000  0.05000000  0.049552239
#>  166 poor_er  3   rank  0.000000000  0.250000000  0.50000000  0.500000000
#>  167 poor_er  3  score  0.004255299  0.471494469  0.70667536  0.643905726
#>  168 poor_er  3  label -1.000000000 -1.000000000  0.00000000  0.000000000
#>  169 poor_er  3    err  0.255000000  0.285000000  0.32500000  0.345223881
#>  170 poor_er  3    acc  0.500000000  0.595000000  0.67500000  0.654776119
#>  171 poor_er  3     sp  0.000000000  0.460000000  0.72000000  0.654776119
#>  172 poor_er  3     sn  0.000000000  0.420000000  0.72000000  0.654776119
#>  173 poor_er  3   prec  0.500000000  0.636986301  0.71962617  0.718381134
#>  174 poor_er  3    mcc  0.070888121  0.298770846  0.40150395  0.368522472
#>  175 poor_er  3 fscore  0.000000000  0.560509554  0.69930070  0.609137097
#>  176 poor_er  3   bacc  0.500000000  0.595000000  0.67500000  0.654776119
#>  177 poor_er  3    npv  0.500000000  0.611842105  0.71568627  0.750246306
#>  178 poor_er  3   infm  0.000000000  0.190000000  0.35000000  0.309552239
#>  179 poor_er  3    mkd  0.271739130  0.425810678  0.46919625  0.468627440
#>  180 poor_er  3  kappa  0.000000000  0.190000000  0.35000000  0.309552239
#>  181 good_er  3   rank  0.000000000  0.250000000  0.50000000  0.500000000
#>  182 good_er  3  score  0.003808949  0.147996319  0.32071300  0.377439220
#>  183 good_er  3  label -1.000000000 -1.000000000  0.00000000  0.000000000
#>  184 good_er  3    err  0.250000000  0.295000000  0.36000000  0.360945274
#>  185 good_er  3    acc  0.500000000  0.585000000  0.64000000  0.639054726
#>  186 good_er  3     sp  0.000000000  0.340000000  0.72000000  0.639054726
#>  187 good_er  3     sn  0.000000000  0.440000000  0.72000000  0.639054726
#>  188 good_er  3   prec  0.500000000  0.567901235  0.72277228  0.734894503
#>  189 good_er  3    mcc  0.070888121  0.236498084  0.34082253  0.330406916
#>  190 good_er  3 fscore  0.000000000  0.582781457  0.67741935  0.603968922
#>  191 good_er  3   bacc  0.500000000  0.585000000  0.64000000  0.639054726
#>  192 good_er  3    npv  0.500000000  0.625000000  0.69600000  0.685653281
#>  193 good_er  3   infm  0.000000000  0.170000000  0.28000000  0.278109453
#>  194 good_er  3    mkd  0.174520070  0.334376011  0.47042338  0.420547785
#>  195 good_er  3  kappa  0.000000000  0.170000000  0.28000000  0.278109453
#>  196   excel  3   rank  0.000000000  0.250000000  0.50000000  0.500000000
#>  197   excel  3  score -2.895491804 -0.297976233  1.19034266  1.293015560
#>  198   excel  3  label -1.000000000 -1.000000000  0.00000000  0.000000000
#>  199   excel  3    err  0.055000000  0.140000000  0.25000000  0.259850746
#>  200   excel  3    acc  0.500000000  0.625000000  0.75000000  0.740149254
#>  201   excel  3     sp  0.000000000  0.500000000  0.94000000  0.740149254
#>  202   excel  3     sn  0.000000000  0.500000000  0.94000000  0.740149254
#>  203   excel  3   prec  0.500000000  0.666666667  0.93877551  0.836438055
#>  204   excel  3    mcc  0.070888121  0.382260072  0.57735027  0.555910211
#>  205   excel  3 fscore  0.000000000  0.666666667  0.76045627  0.701376807
#>  206   excel  3   bacc  0.500000000  0.625000000  0.75000000  0.740149254
#>  207   excel  3    npv  0.500000000  0.666666667  0.94000000  0.838079984
#>  208   excel  3   infm  0.000000000  0.250000000  0.50000000  0.480298507
#>  209   excel  3    mkd  0.500000000  0.571428571  0.66666667  0.674518038
#>  210   excel  3  kappa  0.000000000  0.250000000  0.50000000  0.480298507
#>  211    perf  3   rank  0.000000000  0.250000000  0.50000000  0.500000000
#>  212    perf  3  score  0.000000000  0.000000000  0.50000000  0.500000000
#>  213    perf  3  label -1.000000000 -1.000000000  0.00000000  0.000000000
#>  214    perf  3    err  0.000000000  0.125000000  0.25000000  0.251243781
#>  215    perf  3    acc  0.500000000  0.625000000  0.75000000  0.748756219
#>  216    perf  3     sp  0.000000000  0.500000000  1.00000000  0.748756219
#>  217    perf  3     sn  0.000000000  0.500000000  1.00000000  0.748756219
#>  218    perf  3   prec  0.500000000  0.666666667  1.00000000  0.846096234
#>  219    perf  3    mcc  0.070888121  0.382260072  0.57735027  0.573525244
#>  220    perf  3 fscore  0.000000000  0.666666667  0.76335878  0.710427365
#>  221    perf  3   bacc  0.500000000  0.625000000  0.75000000  0.748756219
#>  222    perf  3    npv  0.500000000  0.666666667  1.00000000  0.846096234
#>  223    perf  3   infm  0.000000000  0.250000000  0.50000000  0.497512438
#>  224    perf  3    mkd  0.500000000  0.571428571  0.66666667  0.692192468
#>  225    perf  3  kappa  0.000000000  0.250000000  0.50000000  0.497512438
#>  226  random  4   rank  0.000000000  0.250000000  0.50000000  0.500000000
#>  227  random  4  score -2.930684562 -0.806432743 -0.14951549 -0.150401568
#>  228  random  4  label -1.000000000 -1.000000000  0.00000000  0.000000000
#>  229  random  4    err  0.465000000  0.490000000  0.50000000  0.507512438
#>  230  random  4    acc  0.435000000  0.475000000  0.50000000  0.492487562
#>  231  random  4     sp  0.000000000  0.190000000  0.50000000  0.492487562
#>  232  random  4     sn  0.000000000  0.250000000  0.50000000  0.492487562
#>  233  random  4   prec  0.000000000  0.480769231  0.50000000  0.499827588
#>  234  random  4    mcc -0.149130039 -0.071622791  0.00000000 -0.020241152
#>  235  random  4 fscore  0.000000000  0.324675325  0.50251256  0.446207541
#>  236  random  4   bacc  0.435000000  0.475000000  0.50000000  0.492487562
#>  237  random  4    npv  0.000000000  0.414634146  0.50000000  0.465782486
#>  238  random  4   infm -0.130000000 -0.050000000  0.00000000 -0.015024876
#>  239  random  4    mkd -0.502512563 -0.111111111  0.00000000 -0.034389926
#>  240  random  4  kappa -0.130000000 -0.050000000  0.00000000 -0.015024876
#>  241 poor_er  4   rank  0.000000000  0.250000000  0.50000000  0.500000000
#>  242 poor_er  4  score  0.011468529  0.428312451  0.69050299  0.631381913
#>  243 poor_er  4  label -1.000000000 -1.000000000  0.00000000  0.000000000
#>  244 poor_er  4    err  0.215000000  0.245000000  0.31000000  0.331144279
#>  245 poor_er  4    acc  0.500000000  0.580000000  0.69000000  0.668855721
#>  246 poor_er  4     sp  0.000000000  0.500000000  0.77000000  0.668855721
#>  247 poor_er  4     sn  0.000000000  0.390000000  0.77000000  0.668855721
#>  248 poor_er  4   prec  0.500000000  0.666666667  0.73913043  0.721151635
#>  249 poor_er  4    mcc  0.070888121  0.245624477  0.43643578  0.396356743
#>  250 poor_er  4 fscore  0.000000000  0.520000000  0.71684588  0.620218813
#>  251 poor_er  4   bacc  0.500000000  0.580000000  0.69000000  0.668855721
#>  252 poor_er  4    npv  0.500000000  0.593333333  0.77000000  0.772505614
#>  253 poor_er  4   infm  0.000000000  0.160000000  0.38000000  0.337711443
#>  254 poor_er  4    mkd  0.235294118  0.421052632  0.51813472  0.493657249
#>  255 poor_er  4  kappa  0.000000000  0.160000000  0.38000000  0.337711443
#>  256 good_er  4   rank  0.000000000  0.250000000  0.50000000  0.500000000
#>  257 good_er  4  score  0.011742578  0.143933860  0.29447852  0.382419382
#>  258 good_er  4  label -1.000000000 -1.000000000  0.00000000  0.000000000
#>  259 good_er  4    err  0.270000000  0.310000000  0.34500000  0.359552239
#>  260 good_er  4    acc  0.495000000  0.595000000  0.65500000  0.640447761
#>  261 good_er  4     sp  0.000000000  0.390000000  0.71000000  0.640447761
#>  262 good_er  4     sn  0.000000000  0.430000000  0.71000000  0.640447761
#>  263 good_er  4   prec  0.497487437  0.589403974  0.71134021  0.735712053
#>  264 good_er  4    mcc -0.070888121  0.284805626  0.37423409  0.333459949
#>  265 good_er  4 fscore  0.000000000  0.577181208  0.68794326  0.604114312
#>  266 good_er  4   bacc  0.495000000  0.595000000  0.65500000  0.640447761
#>  267 good_er  4    npv  0.000000000  0.613924051  0.69523810  0.675289177
#>  268 good_er  4   infm -0.010000000  0.190000000  0.31000000  0.280895522
#>  269 good_er  4    mkd -0.502512563  0.363825364  0.42072364  0.411001230
#>  270 good_er  4  kappa -0.010000000  0.190000000  0.31000000  0.280895522
#>  271   excel  4   rank  0.000000000  0.250000000  0.50000000  0.500000000
#>  272   excel  4  score -2.991157068  0.052879730  1.56272526  1.453973195
#>  273   excel  4  label -1.000000000 -1.000000000  0.00000000  0.000000000
#>  274   excel  4    err  0.040000000  0.125000000  0.25000000  0.255671642
#>  275   excel  4    acc  0.500000000  0.625000000  0.75000000  0.744328358
#>  276   excel  4     sp  0.000000000  0.500000000  0.94000000  0.744328358
#>  277   excel  4     sn  0.000000000  0.500000000  0.94000000  0.744328358
#>  278   excel  4   prec  0.500000000  0.666666667  0.94230769  0.841580692
#>  279   excel  4    mcc  0.070888121  0.382260072  0.57735027  0.564548131
#>  280   excel  4 fscore  0.000000000  0.666666667  0.76335878  0.705964300
#>  281   excel  4   bacc  0.500000000  0.625000000  0.75000000  0.744328358
#>  282   excel  4    npv  0.500000000  0.666666667  0.94059406  0.841691609
#>  283   excel  4   infm  0.000000000  0.250000000  0.50000000  0.488656716
#>  284   excel  4    mkd  0.500000000  0.571428571  0.66666667  0.683272302
#>  285   excel  4  kappa  0.000000000  0.250000000  0.50000000  0.488656716
#>  286    perf  4   rank  0.000000000  0.250000000  0.50000000  0.500000000
#>  287    perf  4  score  0.000000000  0.000000000  0.50000000  0.500000000
#>  288    perf  4  label -1.000000000 -1.000000000  0.00000000  0.000000000
#>  289    perf  4    err  0.000000000  0.125000000  0.25000000  0.251243781
#>  290    perf  4    acc  0.500000000  0.625000000  0.75000000  0.748756219
#>  291    perf  4     sp  0.000000000  0.500000000  1.00000000  0.748756219
#>  292    perf  4     sn  0.000000000  0.500000000  1.00000000  0.748756219
#>  293    perf  4   prec  0.500000000  0.666666667  1.00000000  0.846096234
#>  294    perf  4    mcc  0.070888121  0.382260072  0.57735027  0.573525244
#>  295    perf  4 fscore  0.000000000  0.666666667  0.76335878  0.710427365
#>  296    perf  4   bacc  0.500000000  0.625000000  0.75000000  0.748756219
#>  297    perf  4    npv  0.500000000  0.666666667  1.00000000  0.846096234
#>  298    perf  4   infm  0.000000000  0.250000000  0.50000000  0.497512438
#>  299    perf  4    mkd  0.500000000  0.571428571  0.66666667  0.692192468
#>  300    perf  4  kappa  0.000000000  0.250000000  0.50000000  0.497512438
#>         3rd Qu.       Max.
#>    1 0.75000000 1.00000000
#>    2 0.74648213 2.55038362
#>    3 1.00000000 1.00000000
#>    4 0.50500000 0.52000000
#>    5 0.51500000 0.54500000
#>    6 0.74000000 1.00000000
#>    7 0.78000000 1.00000000
#>    8 0.51851852 1.00000000
#>    9 0.04618722 0.11724208
#>   10 0.62151394 0.66666667
#>   11 0.51500000 0.54500000
#>   12 0.53030303 0.62162162
#>   13 0.03000000 0.09000000
#>   14 0.06417661 0.50251256
#>   15 0.03000000 0.09000000
#>   16 0.75000000 1.00000000
#>   17 0.88581020 0.99991656
#>   18 1.00000000 1.00000000
#>   19 0.42000000 0.50500000
#>   20 0.71500000 0.75500000
#>   21 0.89000000 1.00000000
#>   22 0.97000000 1.00000000
#>   23 0.75903614 0.80597015
#>   24 0.44919070 0.51022966
#>   25 0.74528302 0.77600000
#>   26 0.71500000 0.75500000
#>   27 0.91666667 1.00000000
#>   28 0.43000000 0.51000000
#>   29 0.50020008 0.58666667
#>   30 0.43000000 0.51000000
#>   31 0.75000000 1.00000000
#>   32 0.52984482 0.96247406
#>   33 1.00000000 1.00000000
#>   34 0.42000000 0.50500000
#>   35 0.68500000 0.71000000
#>   36 0.93000000 1.00000000
#>   37 0.88000000 1.00000000
#>   38 0.85106383 1.00000000
#>   39 0.39001950 0.42866070
#>   40 0.69950739 0.73636364
#>   41 0.68500000 0.71000000
#>   42 0.73529412 0.78181818
#>   43 0.37000000 0.42000000
#>   44 0.44923630 0.56497175
#>   45 0.37000000 0.42000000
#>   46 0.75000000 1.00000000
#>   47 3.10779267 5.55536221
#>   48 1.00000000 1.00000000
#>   49 0.37500000 0.50000000
#>   50 0.87000000 0.96000000
#>   51 1.00000000 1.00000000
#>   52 1.00000000 1.00000000
#>   53 1.00000000 1.00000000
#>   54 0.76431763 0.92166048
#>   55 0.87150838 0.96116505
#>   56 0.87000000 0.96000000
#>   57 1.00000000 1.00000000
#>   58 0.74000000 0.92000000
#>   59 0.78740157 0.92332397
#>   60 0.74000000 0.92000000
#>   61 0.75000000 1.00000000
#>   62 1.00000000 1.00000000
#>   63 1.00000000 1.00000000
#>   64 0.37500000 0.50000000
#>   65 0.87500000 1.00000000
#>   66 1.00000000 1.00000000
#>   67 1.00000000 1.00000000
#>   68 1.00000000 1.00000000
#>   69 0.77459667 1.00000000
#>   70 0.87640449 1.00000000
#>   71 0.87500000 1.00000000
#>   72 1.00000000 1.00000000
#>   73 0.75000000 1.00000000
#>   74 0.80000000 1.00000000
#>   75 0.75000000 1.00000000
#>   76 0.75000000 1.00000000
#>   77 0.82406543 2.42164433
#>   78 1.00000000 1.00000000
#>   79 0.49000000 0.52000000
#>   80 0.52000000 0.53500000
#>   81 0.77000000 1.00000000
#>   82 0.77000000 1.00000000
#>   83 0.52631579 0.66666667
#>   84 0.05735771 0.10482848
#>   85 0.61847390 0.66896552
#>   86 0.52000000 0.53500000
#>   87 0.54285714 0.71428571
#>   88 0.04000000 0.07000000
#>   89 0.07440476 0.22205774
#>   90 0.04000000 0.07000000
#>   91 0.75000000 1.00000000
#>   92 0.87264107 0.99865630
#>   93 1.00000000 1.00000000
#>   94 0.39000000 0.50000000
#>   95 0.69000000 0.71500000
#>   96 0.89000000 1.00000000
#>   97 0.95000000 1.00000000
#>   98 0.78571429 1.00000000
#>   99 0.41036087 0.48432210
#>  100 0.72463768 0.76377953
#>  101 0.69000000 0.71500000
#>  102 0.89795918 1.00000000
#>  103 0.38000000 0.43000000
#>  104 0.51322542 0.61728395
#>  105 0.38000000 0.43000000
#>  106 0.75000000 1.00000000
#>  107 0.59876932 0.99952181
#>  108 1.00000000 1.00000000
#>  109 0.39000000 0.50000000
#>  110 0.73500000 0.78000000
#>  111 0.96000000 1.00000000
#>  112 0.90000000 1.00000000
#>  113 0.92000000 1.00000000
#>  114 0.49638925 0.56929858
#>  115 0.74666667 0.77725118
#>  116 0.73500000 0.78000000
#>  117 0.80701754 1.00000000
#>  118 0.47000000 0.56000000
#>  119 0.53954468 0.58781362
#>  120 0.47000000 0.56000000
#>  121 0.75000000 1.00000000
#>  122 3.01778036 5.23190004
#>  123 1.00000000 1.00000000
#>  124 0.37500000 0.50000000
#>  125 0.87000000 0.97000000
#>  126 1.00000000 1.00000000
#>  127 1.00000000 1.00000000
#>  128 1.00000000 1.00000000
#>  129 0.76431763 0.94018806
#>  130 0.87005650 0.96969697
#>  131 0.87000000 0.97000000
#>  132 1.00000000 1.00000000
#>  133 0.74000000 0.94000000
#>  134 0.78740157 0.94037615
#>  135 0.74000000 0.94000000
#>  136 0.75000000 1.00000000
#>  137 1.00000000 1.00000000
#>  138 1.00000000 1.00000000
#>  139 0.37500000 0.50000000
#>  140 0.87500000 1.00000000
#>  141 1.00000000 1.00000000
#>  142 1.00000000 1.00000000
#>  143 1.00000000 1.00000000
#>  144 0.77459667 1.00000000
#>  145 0.87640449 1.00000000
#>  146 0.87500000 1.00000000
#>  147 1.00000000 1.00000000
#>  148 0.75000000 1.00000000
#>  149 0.80000000 1.00000000
#>  150 0.75000000 1.00000000
#>  151 0.75000000 1.00000000
#>  152 0.66324891 3.32620529
#>  153 1.00000000 1.00000000
#>  154 0.49500000 0.52000000
#>  155 0.53500000 0.58500000
#>  156 0.80000000 1.00000000
#>  157 0.75000000 1.00000000
#>  158 0.59740260 1.00000000
#>  159 0.09828045 0.17557525
#>  160 0.60483871 0.67114094
#>  161 0.53500000 0.58500000
#>  162 0.53932584 1.00000000
#>  163 0.07000000 0.17000000
#>  164 0.13706140 0.50505051
#>  165 0.07000000 0.17000000
#>  166 0.75000000 1.00000000
#>  167 0.86400182 0.99998174
#>  168 1.00000000 1.00000000
#>  169 0.40500000 0.50000000
#>  170 0.71500000 0.74500000
#>  171 0.92000000 1.00000000
#>  172 0.96000000 1.00000000
#>  173 0.78571429 1.00000000
#>  174 0.45398976 0.51538988
#>  175 0.74524715 0.77922078
#>  176 0.71500000 0.74500000
#>  177 0.92000000 1.00000000
#>  178 0.43000000 0.49000000
#>  179 0.52083333 0.59796968
#>  180 0.43000000 0.49000000
#>  181 0.75000000 1.00000000
#>  182 0.56944371 0.99190481
#>  183 1.00000000 1.00000000
#>  184 0.41500000 0.50000000
#>  185 0.70500000 0.75000000
#>  186 0.94000000 1.00000000
#>  187 0.84000000 1.00000000
#>  188 0.87755102 1.00000000
#>  189 0.44089168 0.51505353
#>  190 0.70270270 0.73684211
#>  191 0.70500000 0.75000000
#>  192 0.72549020 1.00000000
#>  193 0.41000000 0.50000000
#>  194 0.51260592 0.55741360
#>  195 0.41000000 0.50000000
#>  196 0.75000000 1.00000000
#>  197 2.87133272 4.83221389
#>  198 1.00000000 1.00000000
#>  199 0.37500000 0.50000000
#>  200 0.86000000 0.94500000
#>  201 1.00000000 1.00000000
#>  202 1.00000000 1.00000000
#>  203 1.00000000 1.00000000
#>  204 0.74426518 0.89040077
#>  205 0.86206897 0.94581281
#>  206 0.86000000 0.94500000
#>  207 1.00000000 1.00000000
#>  208 0.72000000 0.89000000
#>  209 0.76923077 0.89080172
#>  210 0.72000000 0.89000000
#>  211 0.75000000 1.00000000
#>  212 1.00000000 1.00000000
#>  213 1.00000000 1.00000000
#>  214 0.37500000 0.50000000
#>  215 0.87500000 1.00000000
#>  216 1.00000000 1.00000000
#>  217 1.00000000 1.00000000
#>  218 1.00000000 1.00000000
#>  219 0.77459667 1.00000000
#>  220 0.87640449 1.00000000
#>  221 0.87500000 1.00000000
#>  222 1.00000000 1.00000000
#>  223 0.75000000 1.00000000
#>  224 0.80000000 1.00000000
#>  225 0.75000000 1.00000000
#>  226 0.75000000 1.00000000
#>  227 0.58023697 2.78819233
#>  228 1.00000000 1.00000000
#>  229 0.52500000 0.56500000
#>  230 0.51000000 0.53500000
#>  231 0.75000000 1.00000000
#>  232 0.69000000 1.00000000
#>  233 0.51807229 0.75000000
#>  234 0.02841256 0.07372098
#>  235 0.55696203 0.66666667
#>  236 0.51000000 0.53500000
#>  237 0.50609756 0.66666667
#>  238 0.02000000 0.07000000
#>  239 0.03112356 0.25510204
#>  240 0.02000000 0.07000000
#>  241 0.75000000 1.00000000
#>  242 0.87714509 0.99556057
#>  243 1.00000000 1.00000000
#>  244 0.42000000 0.50000000
#>  245 0.75500000 0.78500000
#>  246 0.89000000 1.00000000
#>  247 1.00000000 1.00000000
#>  248 0.79629630 1.00000000
#>  249 0.54096591 0.60301363
#>  250 0.78431373 0.81327801
#>  251 0.75500000 0.78500000
#>  252 1.00000000 1.00000000
#>  253 0.51000000 0.57000000
#>  254 0.57142857 0.67114094
#>  255 0.51000000 0.57000000
#>  256 0.75000000 1.00000000
#>  257 0.60183137 0.99193627
#>  258 1.00000000 1.00000000
#>  259 0.40500000 0.50500000
#>  260 0.69000000 0.73000000
#>  261 0.93000000 1.00000000
#>  262 0.89000000 1.00000000
#>  263 0.86274510 1.00000000
#>  264 0.42135049 0.46475800
#>  265 0.70634921 0.72727273
#>  266 0.69000000 0.73000000
#>  267 0.73684211 0.84210526
#>  268 0.38000000 0.46000000
#>  269 0.48000000 0.60606061
#>  270 0.38000000 0.46000000
#>  271 0.75000000 1.00000000
#>  272 2.99851613 5.26560820
#>  273 1.00000000 1.00000000
#>  274 0.37500000 0.50000000
#>  275 0.87500000 0.96000000
#>  276 1.00000000 1.00000000
#>  277 1.00000000 1.00000000
#>  278 1.00000000 1.00000000
#>  279 0.77459667 0.92073688
#>  280 0.87640449 0.96078431
#>  281 0.87500000 0.96000000
#>  282 1.00000000 1.00000000
#>  283 0.75000000 0.92000000
#>  284 0.80000000 0.92147436
#>  285 0.75000000 0.92000000
#>  286 0.75000000 1.00000000
#>  287 1.00000000 1.00000000
#>  288 1.00000000 1.00000000
#>  289 0.37500000 0.50000000
#>  290 0.87500000 1.00000000
#>  291 1.00000000 1.00000000
#>  292 1.00000000 1.00000000
#>  293 1.00000000 1.00000000
#>  294 0.77459667 1.00000000
#>  295 0.87640449 1.00000000
#>  296 0.87500000 1.00000000
#>  297 1.00000000 1.00000000
#>  298 0.75000000 1.00000000
#>  299 0.80000000 1.00000000
#>  300 0.75000000 1.00000000
#> 
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1     random          1            100            100
#>    2    poor_er          1            100            100
#>    3    good_er          1            100            100
#>    4      excel          1            100            100
#>    5       perf          1            100            100
#>    6     random          2            100            100
#>    7    poor_er          2            100            100
#>    8    good_er          2            100            100
#>    9      excel          2            100            100
#>   10       perf          2            100            100
#>   11     random          3            100            100
#>   12    poor_er          3            100            100
#>   13    good_er          3            100            100
#>   14      excel          3            100            100
#>   15       perf          3            100            100
#>   16     random          4            100            100
#>   17    poor_er          4            100            100
#>   18    good_er          4            100            100
#>   19      excel          4            100            100
#>   20       perf          4            100            100
#> 


##################################################
### N-fold cross validation datasets
###

## Load test data
data(M2N50F5)

## Speficy nessesary columns to create mdat
cvdat <- mmdata(
  nfold_df = M2N50F5, score_cols = c(1, 2),
  lab_col = 3, fold_col = 4,
  modnames = c("m1", "m2"), dsids = 1:5
)

## Generate an mmcurve object that contains ROC and Precision-Recall curves
cvcurves <- evalmod(cvdat)
cvcurves
#> 
#>     === AUCs ===
#> 
#>      Model name Dataset ID Curve type       AUC Baseline
#>    1         m1          1        ROC 1.0000000      0.5
#>    2         m1          1        PRC 1.0000000      0.5
#>    3         m1          2        ROC 0.4166667      0.5
#>    4         m1          2        PRC 0.5164199      0.6
#>    5         m1          3        ROC 0.2000000      0.5
#>    6         m1          3        PRC 0.4891743      0.5
#>    7         m1          4        ROC 0.7916667      0.5
#>    8         m1          4        PRC 0.7728152      0.4
#>    9         m1          5        ROC 0.4400000      0.5
#>   10         m1          5        PRC 0.4266312      0.5
#>   11         m2          1        ROC 0.4000000      0.5
#>   12         m2          1        PRC 0.4247188      0.5
#>   13         m2          2        ROC 0.7083333      0.5
#>   14         m2          2        PRC 0.6568625      0.6
#>   15         m2          3        ROC 0.8400000      0.5
#>   16         m2          3        PRC 0.9057736      0.5
#>   17         m2          4        ROC 0.7916667      0.5
#>   18         m2          4        PRC 0.8527712      0.4
#>   19         m2          5        ROC 0.4000000      0.5
#>   20         m2          5        PRC 0.4247188      0.5
#> 
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1         m1          1              5              5
#>    2         m1          2              4              6
#>    3         m1          3              5              5
#>    4         m1          4              6              4
#>    5         m1          5              5              5
#>    6         m2          1              5              5
#>    7         m2          2              4              6
#>    8         m2          3              5              5
#>    9         m2          4              6              4
#>   10         m2          5              5              5
#> 

## Generate an mmpoints object that contains basic evaluation metrics
cvpoints <- evalmod(cvdat, mode = "basic")
cvpoints
#> 
#>     === Basic performance evaluation metrics ===
#> 
#>      ## Performance metrics
#>       rank:   normalized rank
#>       score:  score
#>       label:  label
#>       err:    error rate
#>       acc:    accuracy
#>       sp:     specificity
#>       sn:     sensitivity
#>       prec:   precision
#>       mcc:    Matthews correlation coefficient
#>       fscore: F-score
#>       bacc:   balanced accuracy
#>       npv:    negative predictive value
#>       infm:   informedness (Youden's J)
#>       mkd:    markedness
#>       kappa:  Cohen's kappa
#> 
#> 
#>      Model ID Metric        Min.     1st Qu.       Median         Mean
#>    1    m1  1   rank  0.00000000  0.25000000  0.500000000  0.500000000
#>    2    m1  1  score -1.57617327 -0.92376396 -0.002327284  0.113500523
#>    3    m1  1  label -1.00000000 -1.00000000  0.000000000  0.000000000
#>    4    m1  1    err  0.00000000  0.15000000  0.300000000  0.272727273
#>    5    m1  1    acc  0.50000000  0.60000000  0.700000000  0.727272727
#>    6    m1  1     sp  0.00000000  0.50000000  1.000000000  0.727272727
#>    7    m1  1     sn  0.00000000  0.50000000  1.000000000  0.727272727
#>    8    m1  1   prec  0.50000000  0.66964286  1.000000000  0.838924964
#>    9    m1  1    mcc  0.33333333  0.50000000  0.654653671  0.623218574
#>   10    m1  1 fscore  0.00000000  0.61904762  0.750000000  0.676023471
#>   11    m1  1   bacc  0.50000000  0.60000000  0.700000000  0.727272727
#>   12    m1  1    npv  0.50000000  0.66964286  1.000000000  0.838924964
#>   13    m1  1   infm  0.00000000  0.20000000  0.400000000  0.454545455
#>   14    m1  1    mkd  0.50000000  0.55555556  0.625000000  0.677849928
#>   15    m1  1  kappa  0.00000000  0.20000000  0.400000000  0.454545455
#>   16    m1  2   rank  0.00000000  0.25000000  0.500000000  0.500000000
#>   17    m1  2  score -1.84251337 -1.08765052 -0.450813882 -0.375652956
#>   18    m1  2  label -1.00000000 -1.00000000  1.000000000  0.200000000
#>   19    m1  2    err  0.40000000  0.45000000  0.500000000  0.536363636
#>   20    m1  2    acc  0.30000000  0.40000000  0.500000000  0.463636364
#>   21    m1  2     sp  0.00000000  0.25000000  0.500000000  0.454545455
#>   22    m1  2     sn  0.00000000  0.16666667  0.500000000  0.469696970
#>   23    m1  2   prec  0.00000000  0.41666667  0.555555556  0.450180375
#>   24    m1  2    mcc -0.40824829 -0.27216553 -0.102062073 -0.125094358
#>   25    m1  2 fscore  0.00000000  0.23611111  0.545454545  0.439152766
#>   26    m1  2   bacc  0.33333333  0.41666667  0.458333333  0.462121212
#>   27    m1  2    npv  0.00000000  0.30952381  0.333333333  0.314610390
#>   28    m1  2   infm -0.33333333 -0.16666667 -0.083333333 -0.075757576
#>   29    m1  2    mkd -0.66666667 -0.42222222 -0.166666667 -0.235209235
#>   30    m1  2  kappa -0.29629630 -0.17216117 -0.071428571 -0.068029503
#>   31    m1  3   rank  0.00000000  0.25000000  0.500000000  0.500000000
#>   32    m1  3  score -1.15587519 -0.16251342  0.389025335  0.414373732
#>   33    m1  3  label -1.00000000 -1.00000000  0.000000000  0.000000000
#>   34    m1  3    err  0.40000000  0.50000000  0.600000000  0.636363636
#>   35    m1  3    acc  0.10000000  0.25000000  0.400000000  0.363636364
#>   36    m1  3     sp  0.00000000  0.00000000  0.200000000  0.363636364
#>   37    m1  3     sn  0.00000000  0.20000000  0.200000000  0.363636364
#>   38    m1  3   prec  0.16666667  0.26785714  0.375000000  0.459559885
#>   39    m1  3    mcc -0.81649658 -0.60000000 -0.408248290 -0.355290715
#>   40    m1  3 fscore  0.00000000  0.21111111  0.285714286  0.318732278
#>   41    m1  3   bacc  0.10000000  0.25000000  0.400000000  0.363636364
#>   42    m1  3    npv  0.00000000  0.00000000  0.200000000  0.228860029
#>   43    m1  3   infm -0.80000000 -0.50000000 -0.200000000 -0.272727273
#>   44    m1  3    mkd -0.83333333 -0.61250000 -0.500000000 -0.311580087
#>   45    m1  3  kappa -0.80000000 -0.50000000 -0.200000000 -0.272727273
#>   46    m1  4   rank  0.00000000  0.25000000  0.500000000  0.500000000
#>   47    m1  4  score -1.54622519 -1.19762686 -0.400691531 -0.141998509
#>   48    m1  4  label -1.00000000 -1.00000000 -1.000000000 -0.200000000
#>   49    m1  4    err  0.20000000  0.30000000  0.400000000  0.372727273
#>   50    m1  4    acc  0.40000000  0.60000000  0.600000000  0.627272727
#>   51    m1  4     sp  0.00000000  0.41666667  0.666666667  0.606060606
#>   52    m1  4     sn  0.00000000  0.50000000  0.750000000  0.659090909
#>   53    m1  4   prec  0.40000000  0.50000000  0.571428571  0.652958153
#>   54    m1  4    mcc  0.16666667  0.27216553  0.408248290  0.379646701
#>   55    m1  4 fscore  0.00000000  0.53571429  0.600000000  0.544137681
#>   56    m1  4   bacc  0.50000000  0.58333333  0.625000000  0.632575758
#>   57    m1  4    npv  0.60000000  0.69047619  0.750000000  0.813419913
#>   58    m1  4   infm  0.00000000  0.16666667  0.250000000  0.265151515
#>   59    m1  4    mkd  0.16666667  0.39047619  0.444444444  0.466378066
#>   60    m1  4  kappa  0.00000000  0.15229885  0.285714286  0.258592780
#>   61    m1  5   rank  0.00000000  0.25000000  0.500000000  0.500000000
#>   62    m1  5  score -1.59441987 -0.50932491 -0.113562820  0.006988569
#>   63    m1  5  label -1.00000000 -1.00000000  0.000000000  0.000000000
#>   64    m1  5    err  0.40000000  0.50000000  0.500000000  0.527272727
#>   65    m1  5    acc  0.30000000  0.40000000  0.500000000  0.472727273
#>   66    m1  5     sp  0.00000000  0.30000000  0.400000000  0.472727273
#>   67    m1  5     sn  0.00000000  0.10000000  0.400000000  0.472727273
#>   68    m1  5   prec  0.00000000  0.16666667  0.500000000  0.350937951
#>   69    m1  5    mcc -0.50000000 -0.21821789  0.000000000 -0.077777778
#>   70    m1  5 fscore  0.00000000  0.12500000  0.444444444  0.391172968
#>   71    m1  5   bacc  0.30000000  0.40000000  0.500000000  0.472727273
#>   72    m1  5    npv  0.37500000  0.43650794  0.500000000  0.574062049
#>   73    m1  5   infm -0.40000000 -0.20000000  0.000000000 -0.054545455
#>   74    m1  5    mkd -0.62500000 -0.36904762  0.000000000 -0.075000000
#>   75    m1  5  kappa -0.40000000 -0.20000000  0.000000000 -0.054545455
#>   76    m2  1   rank  0.00000000  0.25000000  0.500000000  0.500000000
#>   77    m2  1  score -1.56663748 -0.93714170 -0.338617003 -0.081054576
#>   78    m2  1  label -1.00000000 -1.00000000  0.000000000  0.000000000
#>   79    m2  1    err  0.50000000  0.50000000  0.500000000  0.545454545
#>   80    m2  1    acc  0.40000000  0.40000000  0.500000000  0.454545455
#>   81    m2  1     sp  0.00000000  0.20000000  0.400000000  0.454545455
#>   82    m2  1     sn  0.00000000  0.20000000  0.400000000  0.454545455
#>   83    m2  1   prec  0.00000000  0.36666667  0.444444444  0.373304473
#>   84    m2  1    mcc -0.33333333 -0.21821789 -0.200000000 -0.144789161
#>   85    m2  1 fscore  0.00000000  0.26785714  0.444444444  0.389008466
#>   86    m2  1   bacc  0.40000000  0.40000000  0.500000000  0.454545455
#>   87    m2  1    npv  0.00000000  0.36666667  0.444444444  0.373304473
#>   88    m2  1   infm -0.20000000 -0.20000000  0.000000000 -0.090909091
#>   89    m2  1    mkd -0.55555556 -0.50000000 -0.238095238 -0.253391053
#>   90    m2  1  kappa -0.20000000 -0.20000000  0.000000000 -0.090909091
#>   91    m2  2   rank  0.00000000  0.25000000  0.500000000  0.500000000
#>   92    m2  2  score -0.94229545 -0.51338997  0.168580899 -0.003990330
#>   93    m2  2  label -1.00000000 -1.00000000  1.000000000  0.200000000
#>   94    m2  2    err  0.20000000  0.30000000  0.400000000  0.409090909
#>   95    m2  2    acc  0.30000000  0.45000000  0.600000000  0.590909091
#>   96    m2  2     sp  0.00000000  0.50000000  0.750000000  0.613636364
#>   97    m2  2     sn  0.00000000  0.25000000  0.666666667  0.575757576
#>   98    m2  2   prec  0.00000000  0.55000000  0.666666667  0.570995671
#>   99    m2  2    mcc -0.40824829  0.08908708  0.356348323  0.244147488
#>  100    m2  2 fscore  0.00000000  0.34722222  0.727272727  0.548311285
#>  101    m2  2   bacc  0.37500000  0.50000000  0.625000000  0.594696970
#>  102    m2  2    npv  0.33333333  0.41428571  0.600000000  0.641233766
#>  103    m2  2   infm -0.25000000  0.00000000  0.250000000  0.189393939
#>  104    m2  2    mkd -0.66666667 -0.01488095  0.380952381  0.212229437
#>  105    m2  2  kappa -0.20689655  0.00000000  0.230769231  0.198986039
#>  106    m2  3   rank  0.00000000  0.25000000  0.500000000  0.500000000
#>  107    m2  3  score -1.09627630 -0.40057827 -0.167608702  0.082174350
#>  108    m2  3  label -1.00000000 -1.00000000  0.000000000  0.000000000
#>  109    m2  3    err  0.10000000  0.25000000  0.400000000  0.345454545
#>  110    m2  3    acc  0.50000000  0.55000000  0.600000000  0.654545455
#>  111    m2  3     sp  0.00000000  0.30000000  0.800000000  0.654545455
#>  112    m2  3     sn  0.00000000  0.50000000  0.800000000  0.654545455
#>  113    m2  3   prec  0.50000000  0.56349206  0.800000000  0.781240981
#>  114    m2  3    mcc  0.00000000  0.33333333  0.408248290  0.429364789
#>  115    m2  3 fscore  0.00000000  0.59340659  0.666666667  0.612175199
#>  116    m2  3   bacc  0.50000000  0.55000000  0.600000000  0.654545455
#>  117    m2  3    npv  0.50000000  0.59027778  0.714285714  0.722258297
#>  118    m2  3   infm  0.00000000  0.10000000  0.200000000  0.309090909
#>  119    m2  3    mkd  0.00000000  0.45833333  0.555555556  0.503499278
#>  120    m2  3  kappa  0.00000000  0.10000000  0.200000000  0.309090909
#>  121    m2  4   rank  0.00000000  0.25000000  0.500000000  0.500000000
#>  122    m2  4  score -1.16547213 -0.35410360 -0.099106516  0.103674375
#>  123    m2  4  label -1.00000000 -1.00000000 -1.000000000 -0.200000000
#>  124    m2  4    err  0.10000000  0.25000000  0.400000000  0.372727273
#>  125    m2  4    acc  0.40000000  0.50000000  0.600000000  0.627272727
#>  126    m2  4     sp  0.00000000  0.25000000  0.666666667  0.606060606
#>  127    m2  4     sn  0.00000000  0.62500000  0.750000000  0.659090909
#>  128    m2  4   prec  0.37500000  0.43650794  0.600000000  0.681637807
#>  129    m2  4    mcc -0.10206207  0.25000000  0.408248290  0.369241846
#>  130    m2  4 fscore  0.00000000  0.52272727  0.600000000  0.561158538
#>  131    m2  4   bacc  0.45833333  0.52083333  0.625000000  0.632575758
#>  132    m2  4    npv  0.50000000  0.66666667  0.750000000  0.765800866
#>  133    m2  4   infm -0.08333333  0.04166667  0.250000000  0.265151515
#>  134    m2  4    mkd -0.12500000  0.32500000  0.444444444  0.447438672
#>  135    m2  4  kappa -0.07142857  0.03703704  0.230769231  0.269859693
#>  136    m2  5   rank  0.00000000  0.25000000  0.500000000  0.500000000
#>  137    m2  5  score -1.79081467 -1.14209817  0.032207934 -0.329061088
#>  138    m2  5  label -1.00000000 -1.00000000  0.000000000  0.000000000
#>  139    m2  5    err  0.50000000  0.50000000  0.500000000  0.545454545
#>  140    m2  5    acc  0.40000000  0.40000000  0.500000000  0.454545455
#>  141    m2  5     sp  0.00000000  0.20000000  0.400000000  0.454545455
#>  142    m2  5     sn  0.00000000  0.20000000  0.400000000  0.454545455
#>  143    m2  5   prec  0.00000000  0.36666667  0.444444444  0.373304473
#>  144    m2  5    mcc -0.33333333 -0.21821789 -0.200000000 -0.144789161
#>  145    m2  5 fscore  0.00000000  0.26785714  0.444444444  0.389008466
#>  146    m2  5   bacc  0.40000000  0.40000000  0.500000000  0.454545455
#>  147    m2  5    npv  0.00000000  0.36666667  0.444444444  0.373304473
#>  148    m2  5   infm -0.20000000 -0.20000000  0.000000000 -0.090909091
#>  149    m2  5    mkd -0.55555556 -0.50000000 -0.238095238 -0.253391053
#>  150    m2  5  kappa -0.20000000 -0.20000000  0.000000000 -0.090909091
#>          3rd Qu.      Max.
#>    1  0.75000000 1.0000000
#>    2  1.06708368 2.0606025
#>    3  1.00000000 1.0000000
#>    4  0.40000000 0.5000000
#>    5  0.85000000 1.0000000
#>    6  1.00000000 1.0000000
#>    7  1.00000000 1.0000000
#>    8  1.00000000 1.0000000
#>    9  0.81649658 1.0000000
#>   10  0.86111111 1.0000000
#>   11  0.85000000 1.0000000
#>   12  1.00000000 1.0000000
#>   13  0.70000000 1.0000000
#>   14  0.77380952 1.0000000
#>   15  0.70000000 1.0000000
#>   16  0.75000000 1.0000000
#>   17 -0.08830171 2.2002973
#>   18  1.00000000 1.0000000
#>   19  0.60000000 0.7000000
#>   20  0.55000000 0.6000000
#>   21  0.62500000 1.0000000
#>   22  0.75000000 1.0000000
#>   23  0.60000000 0.6666667
#>   24  0.00000000 0.1666667
#>   25  0.66666667 0.7500000
#>   26  0.50000000 0.5833333
#>   27  0.40000000 0.5000000
#>   28  0.00000000 0.1666667
#>   29 -0.04761905 0.1666667
#>   30  0.00000000 0.1666667
#>   31  0.75000000 1.0000000
#>   32  1.01753386 1.6870336
#>   33  1.00000000 1.0000000
#>   34  0.75000000 0.9000000
#>   35  0.50000000 0.6000000
#>   36  0.70000000 1.0000000
#>   37  0.50000000 1.0000000
#>   38  0.50000000 1.0000000
#>   39 -0.21821789 0.3333333
#>   40  0.39743590 0.6666667
#>   41  0.50000000 0.6000000
#>   42  0.46428571 0.5555556
#>   43  0.00000000 0.2000000
#>   44 -0.11904762 0.5555556
#>   45  0.00000000 0.2000000
#>   46  0.75000000 1.0000000
#>   47  1.00867637 1.5387145
#>   48  1.00000000 1.0000000
#>   49  0.40000000 0.6000000
#>   50  0.70000000 0.8000000
#>   51  0.91666667 1.0000000
#>   52  1.00000000 1.0000000
#>   53  0.83333333 1.0000000
#>   54  0.40824829 0.6123724
#>   55  0.66666667 0.7272727
#>   56  0.68750000 0.7500000
#>   57  1.00000000 1.0000000
#>   58  0.37500000 0.5000000
#>   59  0.58571429 0.7500000
#>   60  0.37391304 0.5454545
#>   61  0.75000000 1.0000000
#>   62  0.45270677 1.9237289
#>   63  1.00000000 1.0000000
#>   64  0.60000000 0.7000000
#>   65  0.50000000 0.6000000
#>   66  0.60000000 1.0000000
#>   67  0.80000000 1.0000000
#>   68  0.50000000 0.5714286
#>   69  0.00000000 0.3333333
#>   70  0.64102564 0.7142857
#>   71  0.50000000 0.6000000
#>   72  0.58333333 1.0000000
#>   73  0.00000000 0.2000000
#>   74  0.11904762 0.5555556
#>   75  0.00000000 0.2000000
#>   76  0.75000000 1.0000000
#>   77  0.43266892 2.8510651
#>   78  1.00000000 1.0000000
#>   79  0.60000000 0.6000000
#>   80  0.50000000 0.5000000
#>   81  0.70000000 1.0000000
#>   82  0.70000000 1.0000000
#>   83  0.50000000 0.5000000
#>   84  0.00000000 0.0000000
#>   85  0.55844156 0.6666667
#>   86  0.50000000 0.5000000
#>   87  0.50000000 0.5000000
#>   88  0.00000000 0.0000000
#>   89  0.00000000 0.0000000
#>   90  0.00000000 0.0000000
#>   91  0.75000000 1.0000000
#>   92  0.39797893 0.7890534
#>   93  1.00000000 1.0000000
#>   94  0.55000000 0.7000000
#>   95  0.70000000 0.8000000
#>   96  0.75000000 1.0000000
#>   97  0.91666667 1.0000000
#>   98  0.75000000 0.8333333
#>   99  0.40824829 0.6123724
#>  100  0.78461538 0.8571429
#>  101  0.68750000 0.7916667
#>  102  0.87500000 1.0000000
#>  103  0.37500000 0.5833333
#>  104  0.59166667 0.7500000
#>  105  0.37391304 0.5833333
#>  106  0.75000000 1.0000000
#>  107  0.35464019 2.0647833
#>  108  1.00000000 1.0000000
#>  109  0.45000000 0.5000000
#>  110  0.75000000 0.9000000
#>  111  1.00000000 1.0000000
#>  112  0.80000000 1.0000000
#>  113  1.00000000 1.0000000
#>  114  0.60000000 0.8164966
#>  115  0.73863636 0.8888889
#>  116  0.75000000 0.9000000
#>  117  0.81666667 1.0000000
#>  118  0.50000000 0.8000000
#>  119  0.61250000 0.8333333
#>  120  0.50000000 0.8000000
#>  121  0.75000000 1.0000000
#>  122  0.79072036 1.4431452
#>  123  1.00000000 1.0000000
#>  124  0.50000000 0.6000000
#>  125  0.75000000 0.9000000
#>  126  1.00000000 1.0000000
#>  127  0.75000000 1.0000000
#>  128  1.00000000 1.0000000
#>  129  0.58333333 0.8017837
#>  130  0.66666667 0.8571429
#>  131  0.72916667 0.8750000
#>  132  0.84523810 1.0000000
#>  133  0.45833333 0.7500000
#>  134  0.63333333 0.8571429
#>  135  0.47272727 0.7826087
#>  136  0.75000000 1.0000000
#>  137  0.31323748 0.7002948
#>  138  1.00000000 1.0000000
#>  139  0.60000000 0.6000000
#>  140  0.50000000 0.5000000
#>  141  0.70000000 1.0000000
#>  142  0.70000000 1.0000000
#>  143  0.50000000 0.5000000
#>  144  0.00000000 0.0000000
#>  145  0.55844156 0.6666667
#>  146  0.50000000 0.5000000
#>  147  0.50000000 0.5000000
#>  148  0.00000000 0.0000000
#>  149  0.00000000 0.0000000
#>  150  0.00000000 0.0000000
#> 
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1         m1          1              5              5
#>    2         m1          2              4              6
#>    3         m1          3              5              5
#>    4         m1          4              6              4
#>    5         m1          5              5              5
#>    6         m2          1              5              5
#>    7         m2          2              4              6
#>    8         m2          3              5              5
#>    9         m2          4              6              4
#>   10         m2          5              5              5
#> 

## Specify mmdata arguments from evalmod
cvcurves2 <- evalmod(
  nfold_df = M2N50F5, score_cols = c(1, 2),
  lab_col = 3, fold_col = 4,
  modnames = c("m1", "m2"), dsids = 1:5
)
cvcurves2
#> 
#>     === AUCs ===
#> 
#>      Model name Dataset ID Curve type       AUC Baseline
#>    1         m1          1        ROC 1.0000000      0.5
#>    2         m1          1        PRC 1.0000000      0.5
#>    3         m1          2        ROC 0.4166667      0.5
#>    4         m1          2        PRC 0.5164199      0.6
#>    5         m1          3        ROC 0.2000000      0.5
#>    6         m1          3        PRC 0.4891743      0.5
#>    7         m1          4        ROC 0.7916667      0.5
#>    8         m1          4        PRC 0.7728152      0.4
#>    9         m1          5        ROC 0.4400000      0.5
#>   10         m1          5        PRC 0.4266312      0.5
#>   11         m2          1        ROC 0.4000000      0.5
#>   12         m2          1        PRC 0.4247188      0.5
#>   13         m2          2        ROC 0.7083333      0.5
#>   14         m2          2        PRC 0.6568625      0.6
#>   15         m2          3        ROC 0.8400000      0.5
#>   16         m2          3        PRC 0.9057736      0.5
#>   17         m2          4        ROC 0.7916667      0.5
#>   18         m2          4        PRC 0.8527712      0.4
#>   19         m2          5        ROC 0.4000000      0.5
#>   20         m2          5        PRC 0.4247188      0.5
#> 
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1         m1          1              5              5
#>    2         m1          2              4              6
#>    3         m1          3              5              5
#>    4         m1          4              6              4
#>    5         m1          5              5              5
#>    6         m2          1              5              5
#>    7         m2          2              4              6
#>    8         m2          3              5              5
#>    9         m2          4              6              4
#>   10         m2          5              5              5
#> 


##################################################
### AUC with the U statistic
###

## mode = "aucroc" returns 'aucroc' S3 object
data(P10N10)

# 'aucroc' S3 object
uauc1 <- evalmod(
  scores = P10N10$scores, labels = P10N10$labels,
  mode = "aucroc"
)

# print 'aucroc'
uauc1
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1         m1          1             10             10
#> 
#> 
#>     === AUCs ===
#> 
#>      Model name Dataset ID  AUC  U
#>    1         m1          1 0.72 72
#> 

# as.data.frame 'aucroc'
as.data.frame(uauc1)

## It is 2-3 times faster than mode = "rocprc"
# A sample of 100,000
samp1 <- create_sim_samples(1, 50000, 50000)

# a function to test mode = "rocprc"
func_evalmod_rocprc <- function(samp) {
  curves <- evalmod(scores = samp$scores, labels = samp$labels)
  aucs <- auc(curves)
}

# a function to test mode = "aucroc"
func_evalmod_aucroc <- function(samp) {
  uaucs <- evalmod(
    scores = samp$scores, labels = samp$labels,
    mode = "aucroc"
  )
  as.data.frame(uaucs)
}

# Process time
system.time(res1 <- func_evalmod_rocprc(samp1))
#>    user  system elapsed 
#>   0.023   0.004   0.027 
system.time(res2 <- func_evalmod_aucroc(samp1))
#>    user  system elapsed 
#>   0.023   0.000   0.014 

# AUCs
res1
#>   modnames dsids curvetypes      aucs baselines
#> 1       m1     1        ROC 0.5017164       0.5
#> 2       m1     1        PRC 0.4997046       0.5
res2
#>   modnames dsids      aucs     ustats
#> 1       m1     1 0.5017164 1254290885


##################################################
### Multiclass evaluation
###

## Load a 3-class dataset with one score column per class
data(C3N150)

## Each class is evaluated against the rest
mccurves <- evalmod(scores = C3N150$scores, labels = C3N150$labels)
mccurves
#> 
#>     === AUCs ===
#> 
#>      Model name Dataset ID Curve type       AUC  Baseline
#>    1         c1          1        ROC 0.9732000 0.5000000
#>    2         c1          1        PRC 0.9558435 0.3333333
#>    3         c2          1        ROC 0.7758000 0.5000000
#>    4         c2          1        PRC 0.6550357 0.3333333
#>    5         c3          1        ROC 0.5336000 0.5000000
#>    6         c3          1        PRC 0.4162555 0.3333333
#> 
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID Class # of negatives # of positives
#>    1         c1          1    c1            100             50
#>    2         c2          1    c2            100             50
#>    3         c3          1    c3            100             50
#> 

## Per-class AUCs, plus their macro-average
auc(mccurves)
#>        modnames dsids curvetypes      aucs baselines
#> 1            c1     1        ROC 0.9732000 0.5000000
#> 2            c1     1        PRC 0.9558435 0.3333333
#> 3            c2     1        ROC 0.7758000 0.5000000
#> 4            c2     1        PRC 0.6550357 0.3333333
#> 5            c3     1        ROC 0.5336000 0.5000000
#> 6            c3     1        PRC 0.4162555 0.3333333
#> 7 macro-average     1        ROC 0.7608667 0.5000000
#> 8 macro-average     1        PRC 0.6757116 0.3333333
```
