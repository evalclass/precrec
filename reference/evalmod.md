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
  calculated. `x_bins` is effective only when `mode` is set to `rocprc`
  or `prcroc`.

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
#>      Model name Dataset ID Curve type       AUC
#>    1         m1          1        ROC 0.7200000
#>    2         m1          1        PRC 0.7397716
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
#>      Model name Dataset ID Curve type       AUC
#>    1     random          1        ROC 0.4732000
#>    2     random          1        PRC 0.4627220
#>    3    poor_er          1        ROC 0.7828000
#>    4    poor_er          1        PRC 0.7204096
#>    5    good_er          1        ROC 0.8503000
#>    6    good_er          1        PRC 0.8828175
#>    7      excel          1        ROC 0.9765000
#>    8      excel          1        PRC 0.9786819
#>    9       perf          1        ROC 1.0000000
#>   10       perf          1        PRC 1.0000000
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
#>        Model ID Metric          Min.     1st Qu.      Median        Mean
#>    1  random  1   rank  0.0000000000  0.25000000  0.50000000  0.50000000
#>    2  random  1  score -2.4302702453 -0.68435442 -0.14429547 -0.02051745
#>    3  random  1  label -1.0000000000 -1.00000000  0.00000000  0.00000000
#>    4  random  1    err  0.4550000000  0.49000000  0.50500000  0.51333333
#>    5  random  1    acc  0.4150000000  0.46500000  0.49500000  0.48666667
#>    6  random  1     sp  0.0000000000  0.26000000  0.49000000  0.48666667
#>    7  random  1     sn  0.0000000000  0.17000000  0.49000000  0.48666667
#>    8  random  1   prec  0.0000000000  0.40000000  0.49074074  0.45250732
#>    9  random  1    mcc -0.1925982422 -0.09805694 -0.02028185 -0.03230709
#>   10  random  1 fscore  0.0000000000  0.23611111  0.49000000  0.42994554
#>   11  random  1   bacc  0.4150000000  0.46500000  0.49500000  0.48666667
#>   12  random  1    npv  0.0000000000  0.47133758  0.49462366  0.49232788
#>   13  random  1   infm -0.1700000000 -0.07000000 -0.01000000 -0.02666667
#>   14  random  1    mkd -0.5076142132 -0.13805798 -0.03014772 -0.05516480
#>   15  random  1  kappa -0.1700000000 -0.07000000 -0.01000000 -0.02666667
#>   16 poor_er  1   rank  0.0000000000  0.25000000  0.50000000  0.50000000
#>   17 poor_er  1  score  0.0081844223  0.44444921  0.68683884  0.63452609
#>   18 poor_er  1  label -1.0000000000 -1.00000000  0.00000000  0.00000000
#>   19 poor_er  1    err  0.2650000000  0.30000000  0.34500000  0.35930348
#>   20 poor_er  1    acc  0.4950000000  0.59000000  0.65500000  0.64069652
#>   21 poor_er  1     sp  0.0000000000  0.43000000  0.70000000  0.64069652
#>   22 poor_er  1     sn  0.0000000000  0.36000000  0.70000000  0.64069652
#>   23 poor_er  1   prec  0.0000000000  0.62251656  0.70000000  0.67621890
#>   24 poor_er  1    mcc -0.0708881205  0.25234800  0.37282186  0.33310771
#>   25 poor_er  1 fscore  0.0000000000  0.48000000  0.68728522  0.59269030
#>   26 poor_er  1   bacc  0.4950000000  0.59000000  0.65500000  0.64069652
#>   27 poor_er  1    npv  0.4974874372  0.57333333  0.70476190  0.73497267
#>   28 poor_er  1   infm -0.0100000000  0.18000000  0.31000000  0.28139303
#>   29 poor_er  1    mkd -0.5025125628  0.34820096  0.41334812  0.41119156
#>   30 poor_er  1  kappa -0.0100000000  0.18000000  0.31000000  0.28139303
#>   31 good_er  1   rank  0.0000000000  0.25000000  0.50000000  0.50000000
#>   32 good_er  1  score  0.0007292628  0.08985390  0.22927981  0.33559392
#>   33 good_er  1  label -1.0000000000 -1.00000000  0.00000000  0.00000000
#>   34 good_er  1    err  0.2000000000  0.24000000  0.30500000  0.32572139
#>   35 good_er  1    acc  0.5000000000  0.59500000  0.69500000  0.67427861
#>   36 good_er  1     sp  0.0000000000  0.41000000  0.78000000  0.67427861
#>   37 good_er  1     sn  0.0000000000  0.49000000  0.78000000  0.67427861
#>   38 good_er  1   prec  0.5000000000  0.60666667  0.78000000  0.77566021
#>   39 good_er  1    mcc  0.0000000000  0.28553298  0.45903404  0.40722036
#>   40 good_er  1 fscore  0.0000000000  0.64900662  0.70370370  0.63959221
#>   41 good_er  1   bacc  0.5000000000  0.59500000  0.69500000  0.67427861
#>   42 good_er  1    npv  0.5000000000  0.65562914  0.76106195  0.72518543
#>   43 good_er  1   infm  0.0000000000  0.19000000  0.39000000  0.34855721
#>   44 good_er  1    mkd  0.0000000000  0.43125317  0.53475936  0.50084564
#>   45 good_er  1  kappa  0.0000000000  0.19000000  0.39000000  0.34855721
#>   46   excel  1   rank  0.0000000000  0.25000000  0.50000000  0.50000000
#>   47   excel  1  score -2.5815672343 -0.10759066  1.45351425  1.45300911
#>   48   excel  1  label -1.0000000000 -1.00000000  0.00000000  0.00000000
#>   49   excel  1    err  0.0500000000  0.14000000  0.25500000  0.26293532
#>   50   excel  1    acc  0.5000000000  0.62500000  0.74500000  0.73706468
#>   51   excel  1     sp  0.0000000000  0.49000000  0.94000000  0.73706468
#>   52   excel  1     sn  0.0000000000  0.50000000  0.94000000  0.73706468
#>   53   excel  1   prec  0.5000000000  0.66000000  0.93814433  0.83453616
#>   54   excel  1    mcc  0.0708881205  0.38226007  0.56965192  0.54916786
#>   55   excel  1 fscore  0.0000000000  0.66666667  0.75776398  0.69891812
#>   56   excel  1   bacc  0.5000000000  0.62500000  0.74500000  0.73706468
#>   57   excel  1    npv  0.5000000000  0.66666667  0.94000000  0.83270863
#>   58   excel  1   infm  0.0000000000  0.25000000  0.49000000  0.47412935
#>   59   excel  1    mkd  0.5000000000  0.57142857  0.65359477  0.66724478
#>   60   excel  1  kappa  0.0000000000  0.25000000  0.49000000  0.47412935
#>   61    perf  1   rank  0.0000000000  0.25000000  0.50000000  0.50000000
#>   62    perf  1  score  0.0000000000  0.00000000  0.50000000  0.50000000
#>   63    perf  1  label -1.0000000000 -1.00000000  0.00000000  0.00000000
#>   64    perf  1    err  0.0000000000  0.12500000  0.25000000  0.25124378
#>   65    perf  1    acc  0.5000000000  0.62500000  0.75000000  0.74875622
#>   66    perf  1     sp  0.0000000000  0.50000000  1.00000000  0.74875622
#>   67    perf  1     sn  0.0000000000  0.50000000  1.00000000  0.74875622
#>   68    perf  1   prec  0.5000000000  0.66666667  1.00000000  0.84609623
#>   69    perf  1    mcc  0.0708881205  0.38226007  0.57735027  0.57352524
#>   70    perf  1 fscore  0.0000000000  0.66666667  0.76335878  0.71042736
#>   71    perf  1   bacc  0.5000000000  0.62500000  0.75000000  0.74875622
#>   72    perf  1    npv  0.5000000000  0.66666667  1.00000000  0.84609623
#>   73    perf  1   infm  0.0000000000  0.25000000  0.50000000  0.49751244
#>   74    perf  1    mkd  0.5000000000  0.57142857  0.66666667  0.69219247
#>   75    perf  1  kappa  0.0000000000  0.25000000  0.50000000  0.49751244
#>         3rd Qu.      Max.
#>    1 0.75000000 1.0000000
#>    2 0.59949968 2.3807557
#>    3 1.00000000 1.0000000
#>    4 0.53500000 0.5850000
#>    5 0.51000000 0.5450000
#>    6 0.67000000 1.0000000
#>    7 0.76000000 1.0000000
#>    8 0.50694444 0.5266272
#>    9 0.02172186 0.1243421
#>   10 0.60800000 0.6666667
#>   11 0.51000000 0.5450000
#>   12 0.51612903 0.6451613
#>   13 0.02000000 0.0900000
#>   14 0.02380952 0.1717885
#>   15 0.02000000 0.0900000
#>   16 0.75000000 1.0000000
#>   17 0.87582970 0.9940102
#>   18 1.00000000 1.0000000
#>   19 0.41000000 0.5050000
#>   20 0.70000000 0.7350000
#>   21 0.86000000 1.0000000
#>   22 0.93000000 1.0000000
#>   23 0.74683544 0.8333333
#>   24 0.42289876 0.4943536
#>   25 0.73764259 0.7705628
#>   26 0.70000000 0.7350000
#>   27 0.86792453 1.0000000
#>   28 0.40000000 0.4700000
#>   29 0.50006758 0.5681818
#>   30 0.40000000 0.4700000
#>   31 0.75000000 1.0000000
#>   32 0.55648389 0.9953815
#>   33 1.00000000 1.0000000
#>   34 0.40500000 0.5000000
#>   35 0.76000000 0.8000000
#>   36 0.99000000 1.0000000
#>   37 0.91000000 1.0000000
#>   38 0.97560976 1.0000000
#>   39 0.54677686 0.6059679
#>   40 0.75862069 0.7884615
#>   41 0.76000000 0.8000000
#>   42 0.80851064 1.0000000
#>   43 0.52000000 0.6000000
#>   44 0.58666667 0.6400000
#>   45 0.52000000 0.6000000
#>   46 0.75000000 1.0000000
#>   47 2.82684698 5.5367984
#>   48 1.00000000 1.0000000
#>   49 0.37500000 0.5000000
#>   50 0.86000000 0.9500000
#>   51 1.00000000 1.0000000
#>   52 0.99000000 1.0000000
#>   53 1.00000000 1.0000000
#>   54 0.73988011 0.9001801
#>   55 0.85714286 0.9504950
#>   56 0.86000000 0.9500000
#>   57 0.98333333 1.0000000
#>   58 0.72000000 0.9000000
#>   59 0.76400679 0.9003601
#>   60 0.72000000 0.9000000
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
#>      Model name Dataset ID Curve type       AUC
#>    1    good_er          1        ROC 0.8072000
#>    2    good_er          1        PRC 0.8583414
#>    3    good_er          2        ROC 0.7783000
#>    4    good_er          2        PRC 0.8240326
#>    5    good_er          3        ROC 0.8512000
#>    6    good_er          3        PRC 0.8848769
#>    7    good_er          4        ROC 0.8358000
#>    8    good_er          4        PRC 0.8765968
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
#>    2 good_er  1  score  0.0007231320  0.1259463 0.2395143 0.3440506 0.5384651
#>    3 good_er  1  label -1.0000000000 -1.0000000 0.0000000 0.0000000 1.0000000
#>    4 good_er  1    err  0.2200000000  0.2800000 0.3350000 0.3471642 0.4200000
#>    5 good_er  1    acc  0.5000000000  0.5800000 0.6650000 0.6528358 0.7200000
#>    6 good_er  1     sp  0.0000000000  0.3700000 0.7100000 0.6528358 0.9900000
#>    7 good_er  1     sn  0.0000000000  0.4900000 0.7100000 0.6528358 0.8700000
#>    8 good_er  1   prec  0.5000000000  0.5800000 0.7070707 0.7576893 0.9787234
#>    9 good_er  1    mcc  0.0708881205  0.2592852 0.3604768 0.3645049 0.4836761
#>   10 good_er  1 fscore  0.0000000000  0.6533333 0.6956522 0.6200059 0.7106599
#>   11 good_er  1   bacc  0.5000000000  0.5800000 0.6650000 0.6528358 0.7200000
#>   12 good_er  1    npv  0.5000000000  0.6600000 0.7129630 0.7071454 0.7462687
#>   13 good_er  1   infm  0.0000000000  0.1600000 0.3300000 0.3056716 0.4400000
#>   14 good_er  1    mkd  0.2491694352  0.3591704 0.4738381 0.4648347 0.5730051
#>   15 good_er  1  kappa  0.0000000000  0.1600000 0.3300000 0.3056716 0.4400000
#>   16 good_er  2   rank  0.0000000000  0.2500000 0.5000000 0.5000000 0.7500000
#>   17 good_er  2  score  0.0030932471  0.1184568 0.2623920 0.3349419 0.4830234
#>   18 good_er  2  label -1.0000000000 -1.0000000 0.0000000 0.0000000 1.0000000
#>   19 good_er  2    err  0.2650000000  0.3000000 0.3450000 0.3615423 0.4200000
#>   20 good_er  2    acc  0.5000000000  0.5800000 0.6550000 0.6384577 0.7000000
#>   21 good_er  2     sp  0.0000000000  0.3700000 0.7000000 0.6384577 0.9500000
#>   22 good_er  2     sn  0.0000000000  0.4500000 0.7000000 0.6384577 0.8700000
#>   23 good_er  2   prec  0.5000000000  0.5800000 0.6969697 0.7376279 0.8979592
#>   24 good_er  2    mcc  0.0708881205  0.2488211 0.3354511 0.3327230 0.4425642
#>   25 good_er  2 fscore  0.0000000000  0.6000000 0.6827586 0.6037899 0.6945607
#>   26 good_er  2   bacc  0.5000000000  0.5800000 0.6550000 0.6384577 0.7000000
#>   27 good_er  2    npv  0.5000000000  0.6333333 0.6964286 0.6926172 0.7246377
#>   28 good_er  2   infm  0.0000000000  0.1600000 0.3100000 0.2769154 0.4000000
#>   29 good_er  2    mkd  0.2210433245  0.3306667 0.4432624 0.4302451 0.5213764
#>   30 good_er  2  kappa  0.0000000000  0.1600000 0.3100000 0.2769154 0.4000000
#>   31 good_er  3   rank  0.0000000000  0.2500000 0.5000000 0.5000000 0.7500000
#>   32 good_er  3  score  0.0017103979  0.1141798 0.2681277 0.3506082 0.5303036
#>   33 good_er  3  label -1.0000000000 -1.0000000 0.0000000 0.0000000 1.0000000
#>   34 good_er  3    err  0.2000000000  0.2450000 0.3000000 0.3252736 0.4050000
#>   35 good_er  3    acc  0.5000000000  0.5950000 0.7000000 0.6747264 0.7550000
#>   36 good_er  3     sp  0.0000000000  0.4200000 0.7700000 0.6747264 1.0000000
#>   37 good_er  3     sn  0.0000000000  0.5000000 0.7700000 0.6747264 0.9200000
#>   38 good_er  3   prec  0.5000000000  0.6133333 0.7700000 0.7766621 1.0000000
#>   39 good_er  3    mcc  0.0708881205  0.2987708 0.4466064 0.4099772 0.5469587
#>   40 good_er  3 fscore  0.0000000000  0.6622517 0.7094340 0.6401242 0.7555556
#>   41 good_er  3   bacc  0.5000000000  0.5950000 0.7000000 0.6747264 0.7550000
#>   42 good_er  3    npv  0.5000000000  0.6644295 0.7692308 0.7327139 0.8108108
#>   43 good_er  3   infm  0.0000000000  0.1900000 0.4000000 0.3494527 0.5100000
#>   44 good_er  3    mkd  0.2220577350  0.4672365 0.5218788 0.5093760 0.5847953
#>   45 good_er  3  kappa  0.0000000000  0.1900000 0.4000000 0.3494527 0.5100000
#>   46 good_er  4   rank  0.0000000000  0.2500000 0.5000000 0.5000000 0.7500000
#>   47 good_er  4  score  0.0008981645  0.1251022 0.2725321 0.3323335 0.4885612
#>   48 good_er  4  label -1.0000000000 -1.0000000 0.0000000 0.0000000 1.0000000
#>   49 good_er  4    err  0.2050000000  0.2450000 0.3100000 0.3329353 0.4200000
#>   50 good_er  4    acc  0.4950000000  0.5800000 0.6900000 0.6670647 0.7550000
#>   51 good_er  4     sp  0.0000000000  0.3900000 0.7600000 0.6670647 0.9900000
#>   52 good_er  4     sn  0.0000000000  0.4900000 0.7600000 0.6670647 0.8900000
#>   53 good_er  4   prec  0.4974874372  0.5933333 0.7600000 0.7706452 0.9777778
#>   54 good_er  4    mcc -0.0708881205  0.2592852 0.4375000 0.3897146 0.5301326
#>   55 good_er  4 fscore  0.0000000000  0.6533333 0.6959707 0.6335847 0.7487179
#>   56 good_er  4   bacc  0.4950000000  0.5800000 0.6900000 0.6670647 0.7550000
#>   57 good_er  4    npv  0.0000000000  0.6470588 0.7368421 0.6999448 0.7792208
#>   58 good_er  4   infm -0.0100000000  0.1600000 0.3800000 0.3341294 0.5100000
#>   59 good_er  4    mkd -0.5025125628  0.3816292 0.5162466 0.4705900 0.5828072
#>   60 good_er  4  kappa -0.0100000000  0.1600000 0.3800000 0.3341294 0.5100000
#>           Max.
#>    1 1.0000000
#>    2 0.9854039
#>    3 1.0000000
#>    4 0.5000000
#>    5 0.7800000
#>    6 1.0000000
#>    7 1.0000000
#>    8 1.0000000
#>    9 0.6091963
#>   10 0.7471264
#>   11 0.7800000
#>   12 1.0000000
#>   13 0.5600000
#>   14 0.6747638
#>   15 0.5600000
#>   16 1.0000000
#>   17 0.9809642
#>   18 1.0000000
#>   19 0.5000000
#>   20 0.7350000
#>   21 1.0000000
#>   22 1.0000000
#>   23 1.0000000
#>   24 0.5104175
#>   25 0.7115385
#>   26 0.7350000
#>   27 1.0000000
#>   28 0.4700000
#>   29 0.6097561
#>   30 0.4700000
#>   31 1.0000000
#>   32 0.9965829
#>   33 1.0000000
#>   34 0.5000000
#>   35 0.8000000
#>   36 1.0000000
#>   37 1.0000000
#>   38 1.0000000
#>   39 0.6059679
#>   40 0.7897436
#>   41 0.8000000
#>   42 1.0000000
#>   43 0.6000000
#>   44 0.6666667
#>   45 0.6000000
#>   46 1.0000000
#>   47 0.9883169
#>   48 1.0000000
#>   49 0.5050000
#>   50 0.7950000
#>   51 1.0000000
#>   52 1.0000000
#>   53 1.0000000
#>   54 0.6164928
#>   55 0.7788462
#>   56 0.7950000
#>   57 0.8333333
#>   58 0.5900000
#>   59 0.6546015
#>   60 0.5900000
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
#>      Model name Dataset ID Curve type       AUC
#>    1     random          1        ROC 0.5093000
#>    2     random          1        PRC 0.4994518
#>    3    poor_er          1        ROC 0.8331000
#>    4    poor_er          1        PRC 0.7840479
#>    5    good_er          1        ROC 0.8326000
#>    6    good_er          1        PRC 0.8704286
#>    7      excel          1        ROC 0.9849000
#>    8      excel          1        PRC 0.9847555
#>    9       perf          1        ROC 1.0000000
#>   10       perf          1        PRC 1.0000000
#>   11     random          2        ROC 0.5449000
#>   12     random          2        PRC 0.5593729
#>   13    poor_er          2        ROC 0.7995000
#>   14    poor_er          2        PRC 0.7575759
#>   15    good_er          2        ROC 0.7839000
#>   16    good_er          2        PRC 0.8335881
#>   17      excel          2        ROC 0.9773000
#>   18      excel          2        PRC 0.9769814
#>   19       perf          2        ROC 1.0000000
#>   20       perf          2        PRC 1.0000000
#>   21     random          3        ROC 0.5573000
#>   22     random          3        PRC 0.5469593
#>   23    poor_er          3        ROC 0.7887000
#>   24    poor_er          3        PRC 0.7102208
#>   25    good_er          3        ROC 0.7873000
#>   26    good_er          3        PRC 0.8183902
#>   27      excel          3        ROC 0.9916000
#>   28      excel          3        PRC 0.9914376
#>   29       perf          3        ROC 1.0000000
#>   30       perf          3        PRC 1.0000000
#>   31     random          4        ROC 0.5374000
#>   32     random          4        PRC 0.5062558
#>   33    poor_er          4        ROC 0.7695000
#>   34    poor_er          4        PRC 0.7048317
#>   35    good_er          4        ROC 0.8178000
#>   36    good_er          4        PRC 0.8454298
#>   37      excel          4        ROC 0.9895000
#>   38      excel          4        PRC 0.9905851
#>   39       perf          4        ROC 1.0000000
#>   40       perf          4        PRC 1.0000000
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
#>        Model ID Metric          Min.      1st Qu.      Median         Mean
#>    1  random  1   rank  0.0000000000  0.250000000  0.50000000  0.500000000
#>    2  random  1  score -3.2078866813 -0.745006382 -0.08349552 -0.068201326
#>    3  random  1  label -1.0000000000 -1.000000000  0.00000000  0.000000000
#>    4  random  1    err  0.4550000000  0.485000000  0.50000000  0.495373134
#>    5  random  1    acc  0.4750000000  0.490000000  0.50000000  0.504626866
#>    6  random  1     sp  0.0000000000  0.280000000  0.49000000  0.504626866
#>    7  random  1     sn  0.0000000000  0.230000000  0.49000000  0.504626866
#>    8  random  1   prec  0.0000000000  0.488372093  0.50000000  0.492301318
#>    9  random  1    mcc -0.1601281538 -0.030098007  0.00000000  0.005291133
#>   10  random  1 fscore  0.0000000000  0.306666667  0.48756219  0.453873273
#>   11  random  1   bacc  0.4750000000  0.490000000  0.50000000  0.504626866
#>   12  random  1    npv  0.0000000000  0.487179487  0.50000000  0.488234639
#>   13  random  1   infm -0.0500000000 -0.020000000  0.00000000  0.009253731
#>   14  random  1    mkd -0.5128205128 -0.036036036  0.00000000 -0.019464043
#>   15  random  1  kappa -0.0500000000 -0.020000000  0.00000000  0.009253731
#>   16 poor_er  1   rank  0.0000000000  0.250000000  0.50000000  0.500000000
#>   17 poor_er  1  score  0.0016994083  0.439499266  0.73482573  0.647463495
#>   18 poor_er  1  label -1.0000000000 -1.000000000  0.00000000  0.000000000
#>   19 poor_er  1    err  0.2150000000  0.250000000  0.32500000  0.334278607
#>   20 poor_er  1    acc  0.5000000000  0.595000000  0.67500000  0.665721393
#>   21 poor_er  1     sp  0.0000000000  0.490000000  0.76000000  0.665721393
#>   22 poor_er  1     sn  0.0000000000  0.370000000  0.76000000  0.665721393
#>   23 poor_er  1   prec  0.5000000000  0.660000000  0.74468085  0.722992872
#>   24 poor_er  1    mcc  0.0708881205  0.276261463  0.41181385  0.391458834
#>   25 poor_er  1 fscore  0.0000000000  0.493333333  0.70270270  0.617418507
#>   26 poor_er  1   bacc  0.5000000000  0.595000000  0.67500000  0.665721393
#>   27 poor_er  1    npv  0.5000000000  0.580000000  0.76086957  0.769278108
#>   28 poor_er  1   infm  0.0000000000  0.190000000  0.35000000  0.331442786
#>   29 poor_er  1    mkd  0.2304147465  0.390625000  0.51282051  0.492270979
#>   30 poor_er  1  kappa  0.0000000000  0.190000000  0.35000000  0.331442786
#>   31 good_er  1   rank  0.0000000000  0.250000000  0.50000000  0.500000000
#>   32 good_er  1  score  0.0014233844  0.100355713  0.26570912  0.359191600
#>   33 good_er  1  label -1.0000000000 -1.000000000  0.00000000  0.000000000
#>   34 good_er  1    err  0.2150000000  0.245000000  0.31500000  0.334527363
#>   35 good_er  1    acc  0.5000000000  0.575000000  0.68500000  0.665472637
#>   36 good_er  1     sp  0.0000000000  0.380000000  0.76000000  0.665472637
#>   37 good_er  1     sn  0.0000000000  0.470000000  0.76000000  0.665472637
#>   38 good_er  1   prec  0.5000000000  0.588235294  0.76000000  0.767407576
#>   39 good_er  1    mcc  0.0586210382  0.247327782  0.42533941  0.388461288
#>   40 good_er  1 fscore  0.0000000000  0.626666667  0.69620253  0.631407594
#>   41 good_er  1   bacc  0.5000000000  0.575000000  0.68500000  0.665472637
#>   42 good_er  1    npv  0.5000000000  0.646666667  0.74193548  0.716169772
#>   43 good_er  1   infm  0.0000000000  0.150000000  0.37000000  0.330945274
#>   44 good_er  1    mkd  0.1718213058  0.396396396  0.51416473  0.483577348
#>   45 good_er  1  kappa  0.0000000000  0.150000000  0.37000000  0.330945274
#>   46   excel  1   rank  0.0000000000  0.250000000  0.50000000  0.500000000
#>   47   excel  1  score -2.1323541001 -0.241640534  1.58876914  1.412004455
#>   48   excel  1  label -1.0000000000 -1.000000000  0.00000000  0.000000000
#>   49   excel  1    err  0.0600000000  0.130000000  0.25000000  0.258756219
#>   50   excel  1    acc  0.5000000000  0.625000000  0.75000000  0.741243781
#>   51   excel  1     sp  0.0000000000  0.500000000  0.92000000  0.741243781
#>   52   excel  1     sn  0.0000000000  0.500000000  0.92000000  0.741243781
#>   53   excel  1   prec  0.5000000000  0.666666667  0.91752577  0.838198774
#>   54   excel  1    mcc  0.0708881205  0.382260072  0.57735027  0.558279460
#>   55   excel  1 fscore  0.0000000000  0.666666667  0.76335878  0.702744202
#>   56   excel  1   bacc  0.5000000000  0.625000000  0.75000000  0.741243781
#>   57   excel  1    npv  0.5000000000  0.666666667  0.91919192  0.838829191
#>   58   excel  1   infm  0.0000000000  0.250000000  0.50000000  0.482487562
#>   59   excel  1    mkd  0.5000000000  0.571428571  0.66666667  0.677027965
#>   60   excel  1  kappa  0.0000000000  0.250000000  0.50000000  0.482487562
#>   61    perf  1   rank  0.0000000000  0.250000000  0.50000000  0.500000000
#>   62    perf  1  score  0.0000000000  0.000000000  0.50000000  0.500000000
#>   63    perf  1  label -1.0000000000 -1.000000000  0.00000000  0.000000000
#>   64    perf  1    err  0.0000000000  0.125000000  0.25000000  0.251243781
#>   65    perf  1    acc  0.5000000000  0.625000000  0.75000000  0.748756219
#>   66    perf  1     sp  0.0000000000  0.500000000  1.00000000  0.748756219
#>   67    perf  1     sn  0.0000000000  0.500000000  1.00000000  0.748756219
#>   68    perf  1   prec  0.5000000000  0.666666667  1.00000000  0.846096234
#>   69    perf  1    mcc  0.0708881205  0.382260072  0.57735027  0.573525244
#>   70    perf  1 fscore  0.0000000000  0.666666667  0.76335878  0.710427365
#>   71    perf  1   bacc  0.5000000000  0.625000000  0.75000000  0.748756219
#>   72    perf  1    npv  0.5000000000  0.666666667  1.00000000  0.846096234
#>   73    perf  1   infm  0.0000000000  0.250000000  0.50000000  0.497512438
#>   74    perf  1    mkd  0.5000000000  0.571428571  0.66666667  0.692192468
#>   75    perf  1  kappa  0.0000000000  0.250000000  0.50000000  0.497512438
#>   76  random  2   rank  0.0000000000  0.250000000  0.50000000  0.500000000
#>   77  random  2  score -2.5933324230 -0.536055925  0.21499503  0.144060395
#>   78  random  2  label -1.0000000000 -1.000000000  0.00000000  0.000000000
#>   79  random  2    err  0.4400000000  0.465000000  0.48000000  0.477661692
#>   80  random  2    acc  0.4900000000  0.510000000  0.52000000  0.522338308
#>   81  random  2     sp  0.0000000000  0.260000000  0.54000000  0.522338308
#>   82  random  2     sn  0.0000000000  0.280000000  0.54000000  0.522338308
#>   83  random  2   prec  0.4946808511  0.509677419  0.52542373  0.552895928
#>   84  random  2    mcc -0.0458831468  0.032025631  0.05296717  0.056214892
#>   85  random  2 fscore  0.0000000000  0.368421053  0.53465347  0.476134668
#>   86  random  2   bacc  0.4900000000  0.510000000  0.52000000  0.522338308
#>   87  random  2    npv  0.4000000000  0.513661202  0.52272727  0.531596938
#>   88  random  2   infm -0.0200000000  0.020000000  0.04000000  0.044676617
#>   89  random  2    mkd -0.1052631579  0.036805300  0.06757670  0.084492866
#>   90  random  2  kappa -0.0200000000  0.020000000  0.04000000  0.044676617
#>   91 poor_er  2   rank  0.0000000000  0.250000000  0.50000000  0.500000000
#>   92 poor_er  2  score  0.0114088112  0.452871191  0.70197834  0.638110129
#>   93 poor_er  2  label -1.0000000000 -1.000000000  0.00000000  0.000000000
#>   94 poor_er  2    err  0.2650000000  0.290000000  0.32500000  0.350995025
#>   95 poor_er  2    acc  0.5000000000  0.600000000  0.67500000  0.649004975
#>   96 poor_er  2     sp  0.0000000000  0.460000000  0.69000000  0.649004975
#>   97 poor_er  2     sn  0.0000000000  0.400000000  0.69000000  0.649004975
#>   98 poor_er  2   prec  0.5000000000  0.635761589  0.68800000  0.705264689
#>   99 poor_er  2    mcc  0.0320256308  0.295524265  0.39001950  0.354820520
#>  100 poor_er  2 fscore  0.0000000000  0.533333333  0.69565217  0.603376855
#>  101 poor_er  2   bacc  0.5000000000  0.600000000  0.67500000  0.649004975
#>  102 poor_er  2    npv  0.5000000000  0.600000000  0.69902913  0.743563877
#>  103 poor_er  2   infm  0.0000000000  0.200000000  0.35000000  0.298009950
#>  104 poor_er  2    mkd  0.1025641026  0.391802291  0.44567063  0.448828566
#>  105 poor_er  2  kappa  0.0000000000  0.200000000  0.35000000  0.298009950
#>  106 good_er  2   rank  0.0000000000  0.250000000  0.50000000  0.500000000
#>  107 good_er  2  score  0.0007582405  0.097405018  0.29725873  0.349562840
#>  108 good_er  2  label -1.0000000000 -1.000000000  0.00000000  0.000000000
#>  109 good_er  2    err  0.2400000000  0.285000000  0.35500000  0.358756219
#>  110 good_er  2    acc  0.5000000000  0.575000000  0.64500000  0.641243781
#>  111 good_er  2     sp  0.0000000000  0.360000000  0.72000000  0.641243781
#>  112 good_er  2     sn  0.0000000000  0.460000000  0.72000000  0.641243781
#>  113 good_er  2   prec  0.5000000000  0.573333333  0.71717172  0.742941518
#>  114 good_er  2    mcc  0.0708881205  0.234620000  0.33752637  0.339309526
#>  115 good_er  2 fscore  0.0000000000  0.613333333  0.68041237  0.607630446
#>  116 good_er  2   bacc  0.5000000000  0.575000000  0.64500000  0.641243781
#>  117 good_er  2    npv  0.5000000000  0.640000000  0.70000000  0.695604443
#>  118 good_er  2   infm  0.0000000000  0.150000000  0.29000000  0.282487562
#>  119 good_er  2    mkd  0.2300966406  0.334376011  0.46082949  0.438545961
#>  120 good_er  2  kappa  0.0000000000  0.150000000  0.29000000  0.282487562
#>  121   excel  2   rank  0.0000000000  0.250000000  0.50000000  0.500000000
#>  122   excel  2  score -2.0707660653  0.204467610  1.50602451  1.495055564
#>  123   excel  2  label -1.0000000000 -1.000000000  0.00000000  0.000000000
#>  124   excel  2    err  0.0700000000  0.140000000  0.25000000  0.262537313
#>  125   excel  2    acc  0.5000000000  0.625000000  0.75000000  0.737462687
#>  126   excel  2     sp  0.0000000000  0.500000000  0.91000000  0.737462687
#>  127   excel  2     sn  0.0000000000  0.500000000  0.91000000  0.737462687
#>  128   excel  2   prec  0.5000000000  0.666666667  0.91262136  0.834040695
#>  129   excel  2    mcc  0.0708881205  0.382260072  0.57735027  0.550472637
#>  130   excel  2 fscore  0.0000000000  0.666666667  0.76335878  0.698829318
#>  131   excel  2   bacc  0.5000000000  0.625000000  0.75000000  0.737462687
#>  132   excel  2    npv  0.5000000000  0.666666667  0.91000000  0.835085070
#>  133   excel  2   infm  0.0000000000  0.250000000  0.50000000  0.474925373
#>  134   excel  2    mkd  0.5000000000  0.571428571  0.66666667  0.669125765
#>  135   excel  2  kappa  0.0000000000  0.250000000  0.50000000  0.474925373
#>  136    perf  2   rank  0.0000000000  0.250000000  0.50000000  0.500000000
#>  137    perf  2  score  0.0000000000  0.000000000  0.50000000  0.500000000
#>  138    perf  2  label -1.0000000000 -1.000000000  0.00000000  0.000000000
#>  139    perf  2    err  0.0000000000  0.125000000  0.25000000  0.251243781
#>  140    perf  2    acc  0.5000000000  0.625000000  0.75000000  0.748756219
#>  141    perf  2     sp  0.0000000000  0.500000000  1.00000000  0.748756219
#>  142    perf  2     sn  0.0000000000  0.500000000  1.00000000  0.748756219
#>  143    perf  2   prec  0.5000000000  0.666666667  1.00000000  0.846096234
#>  144    perf  2    mcc  0.0708881205  0.382260072  0.57735027  0.573525244
#>  145    perf  2 fscore  0.0000000000  0.666666667  0.76335878  0.710427365
#>  146    perf  2   bacc  0.5000000000  0.625000000  0.75000000  0.748756219
#>  147    perf  2    npv  0.5000000000  0.666666667  1.00000000  0.846096234
#>  148    perf  2   infm  0.0000000000  0.250000000  0.50000000  0.497512438
#>  149    perf  2    mkd  0.5000000000  0.571428571  0.66666667  0.692192468
#>  150    perf  2  kappa  0.0000000000  0.250000000  0.50000000  0.497512438
#>  151  random  3   rank  0.0000000000  0.250000000  0.50000000  0.500000000
#>  152  random  3  score -2.3699971030 -0.731625197 -0.04981453 -0.004743491
#>  153  random  3  label -1.0000000000 -1.000000000  0.00000000  0.000000000
#>  154  random  3    err  0.4350000000  0.455000000  0.47000000  0.471492537
#>  155  random  3    acc  0.4800000000  0.510000000  0.53000000  0.528507463
#>  156  random  3     sp  0.0000000000  0.290000000  0.54000000  0.528507463
#>  157  random  3     sn  0.0000000000  0.280000000  0.54000000  0.528507463
#>  158  random  3   prec  0.2500000000  0.518987342  0.54255319  0.543639953
#>  159  random  3    mcc -0.1234035105  0.039335096  0.07372098  0.061633597
#>  160  random  3 fscore  0.0000000000  0.373333333  0.54000000  0.480636999
#>  161  random  3   bacc  0.4800000000  0.510000000  0.53000000  0.528507463
#>  162  random  3    npv  0.0000000000  0.511494253  0.52898551  0.517169839
#>  163  random  3   infm -0.0400000000  0.020000000  0.06000000  0.057014925
#>  164  random  3    mkd -0.5076142132  0.054436581  0.08741259  0.060809792
#>  165  random  3  kappa -0.0400000000  0.020000000  0.06000000  0.057014925
#>  166 poor_er  3   rank  0.0000000000  0.250000000  0.50000000  0.500000000
#>  167 poor_er  3  score  0.0103928109  0.452581488  0.69741357  0.650680542
#>  168 poor_er  3  label -1.0000000000 -1.000000000  0.00000000  0.000000000
#>  169 poor_er  3    err  0.2600000000  0.290000000  0.32500000  0.356368159
#>  170 poor_er  3    acc  0.4900000000  0.580000000  0.67500000  0.643631841
#>  171 poor_er  3     sp  0.0000000000  0.460000000  0.73000000  0.643631841
#>  172 poor_er  3     sn  0.0000000000  0.390000000  0.73000000  0.643631841
#>  173 poor_er  3   prec  0.2500000000  0.600000000  0.68852459  0.671107111
#>  174 poor_er  3    mcc -0.0714285714  0.245076427  0.39498278  0.332649794
#>  175 poor_er  3 fscore  0.0000000000  0.516556291  0.68817204  0.595008980
#>  176 poor_er  3   bacc  0.4900000000  0.580000000  0.67500000  0.643631841
#>  177 poor_er  3    npv  0.4948453608  0.590604027  0.73195876  0.731952363
#>  178 poor_er  3   infm -0.0200000000  0.160000000  0.35000000  0.287263682
#>  179 poor_er  3    mkd -0.2551020408  0.372224361  0.45004500  0.403059474
#>  180 poor_er  3  kappa -0.0200000000  0.160000000  0.35000000  0.287263682
#>  181 good_er  3   rank  0.0000000000  0.250000000  0.50000000  0.500000000
#>  182 good_er  3  score  0.0027560347  0.138868458  0.26768307  0.345942512
#>  183 good_er  3  label -1.0000000000 -1.000000000  0.00000000  0.000000000
#>  184 good_er  3    err  0.2600000000  0.290000000  0.34500000  0.357064677
#>  185 good_er  3    acc  0.4950000000  0.585000000  0.65500000  0.642935323
#>  186 good_er  3     sp  0.0000000000  0.390000000  0.74000000  0.642935323
#>  187 good_er  3     sn  0.0000000000  0.440000000  0.74000000  0.642935323
#>  188 good_er  3   prec  0.4974874372  0.593333333  0.73404255  0.736512185
#>  189 good_er  3    mcc -0.0708881205  0.272381401  0.36579273  0.333812369
#>  190 good_er  3 fscore  0.0000000000  0.586666667  0.67441860  0.606723709
#>  191 good_er  3   bacc  0.4950000000  0.585000000  0.65500000  0.642935323
#>  192 good_er  3    npv  0.0000000000  0.600000000  0.69166667  0.666991303
#>  193 good_er  3   infm -0.0100000000  0.170000000  0.31000000  0.285870647
#>  194 good_er  3    mkd -0.5025125628  0.364254492  0.45866667  0.403503487
#>  195 good_er  3  kappa -0.0100000000  0.170000000  0.31000000  0.285870647
#>  196   excel  3   rank  0.0000000000  0.250000000  0.50000000  0.500000000
#>  197   excel  3  score -2.7835394135  0.007141627  1.36967840  1.555159604
#>  198   excel  3  label -1.0000000000 -1.000000000  0.00000000  0.000000000
#>  199   excel  3    err  0.0450000000  0.130000000  0.25000000  0.255422886
#>  200   excel  3    acc  0.5000000000  0.625000000  0.75000000  0.744577114
#>  201   excel  3     sp  0.0000000000  0.500000000  0.94000000  0.744577114
#>  202   excel  3     sn  0.0000000000  0.500000000  0.94000000  0.744577114
#>  203   excel  3   prec  0.5000000000  0.666666667  0.94174757  0.841702970
#>  204   excel  3    mcc  0.0708881205  0.382260072  0.57735027  0.565035359
#>  205   excel  3 fscore  0.0000000000  0.666666667  0.76335878  0.706156630
#>  206   excel  3   bacc  0.5000000000  0.625000000  0.75000000  0.744577114
#>  207   excel  3    npv  0.5000000000  0.666666667  0.94059406  0.842035840
#>  208   excel  3   infm  0.0000000000  0.250000000  0.50000000  0.489154229
#>  209   excel  3    mkd  0.5000000000  0.571428571  0.66666667  0.683738810
#>  210   excel  3  kappa  0.0000000000  0.250000000  0.50000000  0.489154229
#>  211    perf  3   rank  0.0000000000  0.250000000  0.50000000  0.500000000
#>  212    perf  3  score  0.0000000000  0.000000000  0.50000000  0.500000000
#>  213    perf  3  label -1.0000000000 -1.000000000  0.00000000  0.000000000
#>  214    perf  3    err  0.0000000000  0.125000000  0.25000000  0.251243781
#>  215    perf  3    acc  0.5000000000  0.625000000  0.75000000  0.748756219
#>  216    perf  3     sp  0.0000000000  0.500000000  1.00000000  0.748756219
#>  217    perf  3     sn  0.0000000000  0.500000000  1.00000000  0.748756219
#>  218    perf  3   prec  0.5000000000  0.666666667  1.00000000  0.846096234
#>  219    perf  3    mcc  0.0708881205  0.382260072  0.57735027  0.573525244
#>  220    perf  3 fscore  0.0000000000  0.666666667  0.76335878  0.710427365
#>  221    perf  3   bacc  0.5000000000  0.625000000  0.75000000  0.748756219
#>  222    perf  3    npv  0.5000000000  0.666666667  1.00000000  0.846096234
#>  223    perf  3   infm  0.0000000000  0.250000000  0.50000000  0.497512438
#>  224    perf  3    mkd  0.5000000000  0.571428571  0.66666667  0.692192468
#>  225    perf  3  kappa  0.0000000000  0.250000000  0.50000000  0.497512438
#>  226  random  4   rank  0.0000000000  0.250000000  0.50000000  0.500000000
#>  227  random  4  score -2.7030888609 -0.677835284  0.01911689  0.015022404
#>  228  random  4  label -1.0000000000 -1.000000000  0.00000000  0.000000000
#>  229  random  4    err  0.4350000000  0.470000000  0.48000000  0.481393035
#>  230  random  4    acc  0.4750000000  0.500000000  0.52000000  0.518606965
#>  231  random  4     sp  0.0000000000  0.290000000  0.50000000  0.518606965
#>  232  random  4     sn  0.0000000000  0.280000000  0.50000000  0.518606965
#>  233  random  4   prec  0.0000000000  0.500000000  0.51376147  0.498705223
#>  234  random  4    mcc -0.1172420764  0.000000000  0.05192861  0.040610765
#>  235  random  4 fscore  0.0000000000  0.370860927  0.50000000  0.466468638
#>  236  random  4   bacc  0.4750000000  0.500000000  0.52000000  0.518606965
#>  237  random  4    npv  0.0000000000  0.500000000  0.52272727  0.535556713
#>  238  random  4   infm -0.0500000000  0.000000000  0.04000000  0.037213930
#>  239  random  4    mkd -0.5050505051  0.000000000  0.06134217  0.034261936
#>  240  random  4  kappa -0.0500000000  0.000000000  0.04000000  0.037213930
#>  241 poor_er  4   rank  0.0000000000  0.250000000  0.50000000  0.500000000
#>  242 poor_er  4  score  0.0217957990  0.482956029  0.76442341  0.672093282
#>  243 poor_er  4  label -1.0000000000 -1.000000000  0.00000000  0.000000000
#>  244 poor_er  4    err  0.2850000000  0.315000000  0.34500000  0.365920398
#>  245 poor_er  4    acc  0.4950000000  0.580000000  0.65500000  0.634079602
#>  246 poor_er  4     sp  0.0000000000  0.430000000  0.71000000  0.634079602
#>  247 poor_er  4     sn  0.0000000000  0.390000000  0.71000000  0.634079602
#>  248 poor_er  4   prec  0.0000000000  0.616438356  0.68181818  0.665447966
#>  249 poor_er  4    mcc -0.0708881205  0.259285197  0.34197056  0.318769679
#>  250 poor_er  4 fscore  0.0000000000  0.520000000  0.68728522  0.585575128
#>  251 poor_er  4   bacc  0.4950000000  0.580000000  0.65500000  0.634079602
#>  252 poor_er  4    npv  0.4974874372  0.593333333  0.70192308  0.729741550
#>  253 poor_er  4   infm -0.0100000000  0.160000000  0.31000000  0.268159204
#>  254 poor_er  4    mkd -0.5025125628  0.339820822  0.39477680  0.395189516
#>  255 poor_er  4  kappa -0.0100000000  0.160000000  0.31000000  0.268159204
#>  256 good_er  4   rank  0.0000000000  0.250000000  0.50000000  0.500000000
#>  257 good_er  4  score  0.0032261889  0.114682040  0.26892995  0.370935690
#>  258 good_er  4  label -1.0000000000 -1.000000000  0.00000000  0.000000000
#>  259 good_er  4    err  0.2200000000  0.275000000  0.33500000  0.341890547
#>  260 good_er  4    acc  0.5000000000  0.600000000  0.66500000  0.658109453
#>  261 good_er  4     sp  0.0000000000  0.380000000  0.76000000  0.658109453
#>  262 good_er  4     sn  0.0000000000  0.460000000  0.76000000  0.658109453
#>  263 good_er  4   prec  0.5000000000  0.589403974  0.76237624  0.753685895
#>  264 good_er  4    mcc  0.0708881205  0.287256886  0.37673632  0.374678806
#>  265 good_er  4 fscore  0.0000000000  0.610389610  0.70072993  0.621885113
#>  266 good_er  4   bacc  0.5000000000  0.600000000  0.66500000  0.658109453
#>  267 good_er  4    npv  0.5000000000  0.636986301  0.75000000  0.719508424
#>  268 good_er  4   infm  0.0000000000  0.200000000  0.33000000  0.316218905
#>  269 good_er  4    mkd  0.2551020408  0.400839855  0.50322061  0.473194319
#>  270 good_er  4  kappa  0.0000000000  0.200000000  0.33000000  0.316218905
#>  271   excel  4   rank  0.0000000000  0.250000000  0.50000000  0.500000000
#>  272   excel  4  score -2.1111727772 -0.197869053  1.38326763  1.490064631
#>  273   excel  4  label -1.0000000000 -1.000000000  0.00000000  0.000000000
#>  274   excel  4    err  0.0450000000  0.130000000  0.25000000  0.256467662
#>  275   excel  4    acc  0.5000000000  0.625000000  0.75000000  0.743532338
#>  276   excel  4     sp  0.0000000000  0.500000000  0.94000000  0.743532338
#>  277   excel  4     sn  0.0000000000  0.500000000  0.94000000  0.743532338
#>  278   excel  4   prec  0.5000000000  0.666666667  0.94000000  0.841064084
#>  279   excel  4    mcc  0.0708881205  0.382260072  0.57735027  0.562872449
#>  280   excel  4 fscore  0.0000000000  0.666666667  0.76335878  0.705317989
#>  281   excel  4   bacc  0.5000000000  0.625000000  0.75000000  0.743532338
#>  282   excel  4    npv  0.5000000000  0.666666667  0.94117647  0.840479137
#>  283   excel  4   infm  0.0000000000  0.250000000  0.50000000  0.487064677
#>  284   excel  4    mkd  0.5000000000  0.571428571  0.66666667  0.681543220
#>  285   excel  4  kappa  0.0000000000  0.250000000  0.50000000  0.487064677
#>  286    perf  4   rank  0.0000000000  0.250000000  0.50000000  0.500000000
#>  287    perf  4  score  0.0000000000  0.000000000  0.50000000  0.500000000
#>  288    perf  4  label -1.0000000000 -1.000000000  0.00000000  0.000000000
#>  289    perf  4    err  0.0000000000  0.125000000  0.25000000  0.251243781
#>  290    perf  4    acc  0.5000000000  0.625000000  0.75000000  0.748756219
#>  291    perf  4     sp  0.0000000000  0.500000000  1.00000000  0.748756219
#>  292    perf  4     sn  0.0000000000  0.500000000  1.00000000  0.748756219
#>  293    perf  4   prec  0.5000000000  0.666666667  1.00000000  0.846096234
#>  294    perf  4    mcc  0.0708881205  0.382260072  0.57735027  0.573525244
#>  295    perf  4 fscore  0.0000000000  0.666666667  0.76335878  0.710427365
#>  296    perf  4   bacc  0.5000000000  0.625000000  0.75000000  0.748756219
#>  297    perf  4    npv  0.5000000000  0.666666667  1.00000000  0.846096234
#>  298    perf  4   infm  0.0000000000  0.250000000  0.50000000  0.497512438
#>  299    perf  4    mkd  0.5000000000  0.571428571  0.66666667  0.692192468
#>  300    perf  4  kappa  0.0000000000  0.250000000  0.50000000  0.497512438
#>         3rd Qu.      Max.
#>    1 0.75000000 1.0000000
#>    2 0.58400337 2.4350957
#>    3 1.00000000 1.0000000
#>    4 0.51000000 0.5250000
#>    5 0.51500000 0.5450000
#>    6 0.73000000 1.0000000
#>    7 0.78000000 1.0000000
#>    8 0.52671756 0.6000000
#>    9 0.04267125 0.1019638
#>   10 0.61832061 0.6666667
#>   11 0.51500000 0.5450000
#>   12 0.51428571 0.5849057
#>   13 0.03000000 0.0900000
#>   14 0.05726284 0.1155179
#>   15 0.03000000 0.0900000
#>   16 0.75000000 1.0000000
#>   17 0.89471313 0.9997019
#>   18 1.00000000 1.0000000
#>   19 0.40500000 0.5000000
#>   20 0.75000000 0.7850000
#>   21 0.87000000 1.0000000
#>   22 0.99000000 1.0000000
#>   23 0.77777778 1.0000000
#>   24 0.52130528 0.6030136
#>   25 0.76777251 0.8132780
#>   26 0.75000000 0.7850000
#>   27 0.97826087 1.0000000
#>   28 0.50000000 0.5700000
#>   29 0.58072009 0.6611372
#>   30 0.50000000 0.5700000
#>   31 0.75000000 1.0000000
#>   32 0.60373924 0.9839133
#>   33 1.00000000 1.0000000
#>   34 0.42500000 0.5000000
#>   35 0.75500000 0.7850000
#>   36 0.97000000 1.0000000
#>   37 0.88000000 1.0000000
#>   38 0.94736842 1.0000000
#>   39 0.51876659 0.5995352
#>   40 0.74336283 0.7720930
#>   41 0.75500000 0.7850000
#>   42 0.78409091 1.0000000
#>   43 0.51000000 0.5700000
#>   44 0.57471264 0.6428571
#>   45 0.51000000 0.5700000
#>   46 0.75000000 1.0000000
#>   47 2.88662362 5.1511163
#>   48 1.00000000 1.0000000
#>   49 0.37500000 0.5000000
#>   50 0.87000000 0.9400000
#>   51 1.00000000 1.0000000
#>   52 1.00000000 1.0000000
#>   53 1.00000000 1.0000000
#>   54 0.76431763 0.8828296
#>   55 0.87150838 0.9423077
#>   56 0.87000000 0.9400000
#>   57 1.00000000 1.0000000
#>   58 0.74000000 0.8800000
#>   59 0.78740157 0.8856683
#>   60 0.74000000 0.8800000
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
#>   76 0.75000000 1.0000000
#>   77 0.79419412 2.9229714
#>   78 1.00000000 1.0000000
#>   79 0.49000000 0.5100000
#>   80 0.53500000 0.5600000
#>   81 0.78000000 1.0000000
#>   82 0.76000000 1.0000000
#>   83 0.56250000 1.0000000
#>   84 0.08227832 0.1481759
#>   85 0.61044177 0.6689189
#>   86 0.53500000 0.5600000
#>   87 0.53508772 1.0000000
#>   88 0.07000000 0.1200000
#>   89 0.11089828 0.5050505
#>   90 0.07000000 0.1200000
#>   91 0.75000000 1.0000000
#>   92 0.87495471 0.9999074
#>   93 1.00000000 1.0000000
#>   94 0.40000000 0.5000000
#>   95 0.71000000 0.7350000
#>   96 0.90000000 1.0000000
#>   97 0.96000000 1.0000000
#>   98 0.79411765 1.0000000
#>   99 0.44155506 0.4871603
#>  100 0.73684211 0.7692308
#>  101 0.71000000 0.7350000
#>  102 0.91111111 1.0000000
#>  103 0.42000000 0.4700000
#>  104 0.51282051 0.5917160
#>  105 0.42000000 0.4700000
#>  106 0.75000000 1.0000000
#>  107 0.54997159 0.9976283
#>  108 1.00000000 1.0000000
#>  109 0.42500000 0.5000000
#>  110 0.71500000 0.7600000
#>  111 0.96000000 1.0000000
#>  112 0.86000000 1.0000000
#>  113 0.91666667 1.0000000
#>  114 0.45265855 0.5385205
#>  115 0.69750890 0.7252747
#>  116 0.71500000 0.7600000
#>  117 0.71276596 1.0000000
#>  118 0.43000000 0.5200000
#>  119 0.54209536 0.6097561
#>  120 0.43000000 0.5200000
#>  121 0.75000000 1.0000000
#>  122 2.89118889 4.9741119
#>  123 1.00000000 1.0000000
#>  124 0.37500000 0.5000000
#>  125 0.86000000 0.9300000
#>  126 1.00000000 1.0000000
#>  127 1.00000000 1.0000000
#>  128 1.00000000 1.0000000
#>  129 0.74426518 0.8615522
#>  130 0.86206897 0.9320388
#>  131 0.86000000 0.9300000
#>  132 1.00000000 1.0000000
#>  133 0.72000000 0.8600000
#>  134 0.76923077 0.8631072
#>  135 0.72000000 0.8600000
#>  136 0.75000000 1.0000000
#>  137 1.00000000 1.0000000
#>  138 1.00000000 1.0000000
#>  139 0.37500000 0.5000000
#>  140 0.87500000 1.0000000
#>  141 1.00000000 1.0000000
#>  142 1.00000000 1.0000000
#>  143 1.00000000 1.0000000
#>  144 0.77459667 1.0000000
#>  145 0.87640449 1.0000000
#>  146 0.87500000 1.0000000
#>  147 1.00000000 1.0000000
#>  148 0.75000000 1.0000000
#>  149 0.80000000 1.0000000
#>  150 0.75000000 1.0000000
#>  151 0.75000000 1.0000000
#>  152 0.68076089 2.5503836
#>  153 1.00000000 1.0000000
#>  154 0.49000000 0.5200000
#>  155 0.54500000 0.5650000
#>  156 0.78000000 1.0000000
#>  157 0.79000000 1.0000000
#>  158 0.55882353 1.0000000
#>  159 0.09984333 0.1324120
#>  160 0.63035019 0.6666667
#>  161 0.54500000 0.5650000
#>  162 0.55844156 0.6000000
#>  163 0.09000000 0.1300000
#>  164 0.10989011 0.5025126
#>  165 0.09000000 0.1300000
#>  166 0.75000000 1.0000000
#>  167 0.88870103 0.9997137
#>  168 1.00000000 1.0000000
#>  169 0.42000000 0.5100000
#>  170 0.71000000 0.7400000
#>  171 0.89000000 1.0000000
#>  172 0.96000000 1.0000000
#>  173 0.75641026 1.0000000
#>  174 0.45019321 0.5015568
#>  175 0.74178404 0.7741935
#>  176 0.71000000 0.7400000
#>  177 0.89285714 1.0000000
#>  178 0.42000000 0.4800000
#>  179 0.48019208 0.5717256
#>  180 0.42000000 0.4800000
#>  181 0.75000000 1.0000000
#>  182 0.53401674 0.9667090
#>  183 1.00000000 1.0000000
#>  184 0.41500000 0.5050000
#>  185 0.71000000 0.7400000
#>  186 0.94000000 1.0000000
#>  187 0.89000000 1.0000000
#>  188 0.88235294 1.0000000
#>  189 0.44558895 0.4807194
#>  190 0.71351351 0.7441860
#>  191 0.71000000 0.7400000
#>  192 0.75000000 0.7948718
#>  193 0.42000000 0.4800000
#>  194 0.49820415 0.5649718
#>  195 0.42000000 0.4800000
#>  196 0.75000000 1.0000000
#>  197 3.15321039 5.5553622
#>  198 1.00000000 1.0000000
#>  199 0.37500000 0.5000000
#>  200 0.87000000 0.9550000
#>  201 1.00000000 1.0000000
#>  202 1.00000000 1.0000000
#>  203 1.00000000 1.0000000
#>  204 0.76431763 0.9104098
#>  205 0.87150838 0.9556650
#>  206 0.87000000 0.9550000
#>  207 1.00000000 1.0000000
#>  208 0.74000000 0.9100000
#>  209 0.78740157 0.9108197
#>  210 0.74000000 0.9100000
#>  211 0.75000000 1.0000000
#>  212 1.00000000 1.0000000
#>  213 1.00000000 1.0000000
#>  214 0.37500000 0.5000000
#>  215 0.87500000 1.0000000
#>  216 1.00000000 1.0000000
#>  217 1.00000000 1.0000000
#>  218 1.00000000 1.0000000
#>  219 0.77459667 1.0000000
#>  220 0.87640449 1.0000000
#>  221 0.87500000 1.0000000
#>  222 1.00000000 1.0000000
#>  223 0.75000000 1.0000000
#>  224 0.80000000 1.0000000
#>  225 0.75000000 1.0000000
#>  226 0.75000000 1.0000000
#>  227 0.81938227 2.5521379
#>  228 1.00000000 1.0000000
#>  229 0.50000000 0.5250000
#>  230 0.53000000 0.5650000
#>  231 0.78000000 1.0000000
#>  232 0.79000000 1.0000000
#>  233 0.53846154 0.5652174
#>  234 0.07367405 0.1377146
#>  235 0.62698413 0.6736111
#>  236 0.53000000 0.5650000
#>  237 0.56250000 0.7777778
#>  238 0.06000000 0.1300000
#>  239 0.08723466 0.2908668
#>  240 0.06000000 0.1300000
#>  241 0.75000000 1.0000000
#>  242 0.89269577 0.9995218
#>  243 1.00000000 1.0000000
#>  244 0.42000000 0.5050000
#>  245 0.68500000 0.7150000
#>  246 0.89000000 1.0000000
#>  247 0.93000000 1.0000000
#>  248 0.72727273 0.8333333
#>  249 0.40720320 0.4525696
#>  250 0.72566372 0.7548638
#>  251 0.68500000 0.7150000
#>  252 0.86000000 1.0000000
#>  253 0.37000000 0.4300000
#>  254 0.48000000 0.6024096
#>  255 0.37000000 0.4300000
#>  256 0.75000000 1.0000000
#>  257 0.57557367 0.9911639
#>  258 1.00000000 1.0000000
#>  259 0.40000000 0.5000000
#>  260 0.72500000 0.7800000
#>  261 0.96000000 1.0000000
#>  262 0.88000000 1.0000000
#>  263 0.91836735 1.0000000
#>  264 0.48349284 0.5740647
#>  265 0.72807018 0.7676768
#>  266 0.72500000 0.7800000
#>  267 0.77173913 1.0000000
#>  268 0.45000000 0.5600000
#>  269 0.53475936 0.5928854
#>  270 0.45000000 0.5600000
#>  271 0.75000000 1.0000000
#>  272 3.00602457 5.2319000
#>  273 1.00000000 1.0000000
#>  274 0.37500000 0.5000000
#>  275 0.87000000 0.9550000
#>  276 1.00000000 1.0000000
#>  277 1.00000000 1.0000000
#>  278 1.00000000 1.0000000
#>  279 0.76431763 0.9104098
#>  280 0.87005650 0.9543147
#>  281 0.87000000 0.9550000
#>  282 1.00000000 1.0000000
#>  283 0.74000000 0.9100000
#>  284 0.78740157 0.9108197
#>  285 0.74000000 0.9100000
#>  286 0.75000000 1.0000000
#>  287 1.00000000 1.0000000
#>  288 1.00000000 1.0000000
#>  289 0.37500000 0.5000000
#>  290 0.87500000 1.0000000
#>  291 1.00000000 1.0000000
#>  292 1.00000000 1.0000000
#>  293 1.00000000 1.0000000
#>  294 0.77459667 1.0000000
#>  295 0.87640449 1.0000000
#>  296 0.87500000 1.0000000
#>  297 1.00000000 1.0000000
#>  298 0.75000000 1.0000000
#>  299 0.80000000 1.0000000
#>  300 0.75000000 1.0000000
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
#>      Model name Dataset ID Curve type       AUC
#>    1         m1          1        ROC 1.0000000
#>    2         m1          1        PRC 1.0000000
#>    3         m1          2        ROC 0.4166667
#>    4         m1          2        PRC 0.5164199
#>    5         m1          3        ROC 0.2000000
#>    6         m1          3        PRC 0.4891743
#>    7         m1          4        ROC 0.7916667
#>    8         m1          4        PRC 0.7728152
#>    9         m1          5        ROC 0.4400000
#>   10         m1          5        PRC 0.4266312
#>   11         m2          1        ROC 0.4000000
#>   12         m2          1        PRC 0.4247188
#>   13         m2          2        ROC 0.7083333
#>   14         m2          2        PRC 0.6568625
#>   15         m2          3        ROC 0.8400000
#>   16         m2          3        PRC 0.9057736
#>   17         m2          4        ROC 0.7916667
#>   18         m2          4        PRC 0.8527712
#>   19         m2          5        ROC 0.4000000
#>   20         m2          5        PRC 0.4247188
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
#>      Model name Dataset ID Curve type       AUC
#>    1         m1          1        ROC 1.0000000
#>    2         m1          1        PRC 1.0000000
#>    3         m1          2        ROC 0.4166667
#>    4         m1          2        PRC 0.5164199
#>    5         m1          3        ROC 0.2000000
#>    6         m1          3        PRC 0.4891743
#>    7         m1          4        ROC 0.7916667
#>    8         m1          4        PRC 0.7728152
#>    9         m1          5        ROC 0.4400000
#>   10         m1          5        PRC 0.4266312
#>   11         m2          1        ROC 0.4000000
#>   12         m2          1        PRC 0.4247188
#>   13         m2          2        ROC 0.7083333
#>   14         m2          2        PRC 0.6568625
#>   15         m2          3        ROC 0.8400000
#>   16         m2          3        PRC 0.9057736
#>   17         m2          4        ROC 0.7916667
#>   18         m2          4        PRC 0.8527712
#>   19         m2          5        ROC 0.4000000
#>   20         m2          5        PRC 0.4247188
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
#>   0.029   0.005   0.034 
system.time(res2 <- func_evalmod_aucroc(samp1))
#>    user  system elapsed 
#>   0.019   0.000   0.012 

# AUCs
res1
#>   modnames dsids curvetypes      aucs
#> 1       m1     1        ROC 0.5003782
#> 2       m1     1        PRC 0.4988955
res2
#>   modnames dsids      aucs     ustats
#> 1       m1     1 0.5003782 1250945430


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
#>      Model name Dataset ID Curve type       AUC
#>    1         c1          1        ROC 0.9732000
#>    2         c1          1        PRC 0.9558435
#>    3         c2          1        ROC 0.7758000
#>    4         c2          1        PRC 0.6550357
#>    5         c3          1        ROC 0.5336000
#>    6         c3          1        PRC 0.4162555
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
#>        modnames dsids curvetypes      aucs
#> 1            c1     1        ROC 0.9732000
#> 2            c1     1        PRC 0.9558435
#> 3            c2     1        ROC 0.7758000
#> 4            c2     1        PRC 0.6550357
#> 5            c3     1        ROC 0.5336000
#> 6            c3     1        PRC 0.4162555
#> 7 macro-average     1        ROC 0.7608667
#> 8 macro-average     1        PRC 0.6757116
```
