# Evaluate models and calculate performance evaluation measures

The `evalmod` function calculates ROC and Precision-Recall curves for
specified prediction scores and binary labels. It also calculate several
basic performance evaluation measures, such as accuracy, error rate, and
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

  A string that specifies the types of evaluation measures that the
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

  :   Warn, and return `NA` for the measures that are undefined

  ROC and precision-recall curves are undefined for such a dataset, so
  `on_single_class` is effective only when `mode` is set to `rocprc`,
  `prcroc`, or `aucroc`. `mode = "basic"` always warns and calculates
  what it can, because accuracy and error rate are still defined.

- metrics:

  A character vector that names the basic evaluation measures to
  calculate in addition to the default set, or the string `"all"` for
  every measure `precrec` knows. The default `NULL` is the fourteen
  measures `evalmod` has always returned: `score`, `label`, `error`,
  `accuracy`, `specificity`, `sensitivity`, `precision`, `mcc`,
  `fscore`, `balanced_accuracy`, `npv`, `informedness`, `markedness` and
  `kappa`.

  The measures that can be added are `fpr`, `fnr`,
  `false_discovery_rate`, `false_omission_rate`,
  `predicted_positive_rate`, `predicted_negative_rate`, `lift`, `odds`,
  `mi`, `chisq` and `cost`. They are the measures `ROCR` provides that
  `precrec` did not, and each of them also answers to the identifier
  `ROCR` uses for it - `fall`, `miss`, `pcfall`, `pcmiss`, `rpp`, `rnp`
  and `mutual_information` - and to its standard abbreviation where it
  has one.

  They are not calculated by default because each is another vector the
  size of the dataset, and because `plot` and `autoplot` draw one panel
  per measure the object holds. A measure that was not asked for cannot
  be plotted; `metrics` is effective only when `mode` is set to `basic`.

- cost_fp:

  A numeric value for the cost of a false positive, used by the `cost`
  measure. `cost` is not normalized, following `ROCR`: it is
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
evaluation measures. The number of models and the number of datasets can
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
    evaluation measures; error rate, accuracy, specificity, sensitivity,
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

## Generate an sspoints object that contains basic evaluation measures
sspoints <- evalmod(
  mode = "basic", scores = P10N10$scores,
  labels = P10N10$labels
)
sspoints
#> 
#>     === Basic performance evaluation measures ===
#> 
#>      ## Performance measures (Meas.)
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
#>      Model ID  Meas.       Min.    1st Qu.     Median       Mean    3rd Qu.
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
#>    1     random          1        ROC 0.4613000
#>    2     random          1        PRC 0.4718958
#>    3    poor_er          1        ROC 0.7796000
#>    4    poor_er          1        PRC 0.7088592
#>    5    good_er          1        ROC 0.7364000
#>    6    good_er          1        PRC 0.7861169
#>    7      excel          1        ROC 0.9852000
#>    8      excel          1        PRC 0.9865822
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

## Generate an mspoints object that contains basic evaluation measures
mspoints <- evalmod(mdat, mode = "basic")
mspoints
#> 
#>     === Basic performance evaluation measures ===
#> 
#>      ## Performance measures (Meas.)
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
#>        Model ID  Meas.         Min.     1st Qu.      Median         Mean
#>    1  random  1   rank  0.000000000  0.25000000  0.50000000  0.500000000
#>    2  random  1  score -2.625232982 -0.76743379 -0.12512491 -0.162316745
#>    3  random  1  label -1.000000000 -1.00000000  0.00000000  0.000000000
#>    4  random  1    err  0.455000000  0.50500000  0.52000000  0.519253731
#>    5  random  1    acc  0.425000000  0.46000000  0.48000000  0.480746269
#>    6  random  1     sp  0.000000000  0.23000000  0.46000000  0.480746269
#>    7  random  1     sn  0.000000000  0.19000000  0.46000000  0.480746269
#>    8  random  1   prec  0.333333333  0.42857143  0.46268657  0.466389684
#>    9  random  1    mcc -0.169939626 -0.09069832 -0.05114036 -0.035084735
#>   10  random  1 fscore  0.000000000  0.25333333  0.45771144  0.424904954
#>   11  random  1   bacc  0.425000000  0.46000000  0.48000000  0.480746269
#>   12  random  1    npv  0.442307692  0.46666667  0.47674419  0.524041966
#>   13  random  1   infm -0.150000000 -0.08000000 -0.04000000 -0.038507463
#>   14  random  1    mkd -0.192529842 -0.10256410 -0.06305170 -0.009568351
#>   15  random  1  kappa -0.150000000 -0.08000000 -0.04000000 -0.038507463
#>   16 poor_er  1   rank  0.000000000  0.25000000  0.50000000  0.500000000
#>   17 poor_er  1  score  0.011450813  0.47464434  0.70177878  0.642896467
#>   18 poor_er  1  label -1.000000000 -1.00000000  0.00000000  0.000000000
#>   19 poor_er  1    err  0.290000000  0.31000000  0.32500000  0.360895522
#>   20 poor_er  1    acc  0.490000000  0.59000000  0.67500000  0.639104478
#>   21 poor_er  1     sp  0.000000000  0.43000000  0.71000000  0.639104478
#>   22 poor_er  1     sn  0.000000000  0.39000000  0.71000000  0.639104478
#>   23 poor_er  1   prec  0.000000000  0.62000000  0.66964286  0.665626286
#>   24 poor_er  1    mcc -0.100503782  0.27356200  0.38276591  0.328566131
#>   25 poor_er  1 fscore  0.000000000  0.51655629  0.68728522  0.591288219
#>   26 poor_er  1   bacc  0.490000000  0.59000000  0.67500000  0.639104478
#>   27 poor_er  1    npv  0.494949495  0.59210526  0.70526316  0.734620516
#>   28 poor_er  1   infm -0.020000000  0.18000000  0.35000000  0.278208955
#>   29 poor_er  1    mkd -0.505050505  0.37202381  0.41004100  0.400246802
#>   30 poor_er  1  kappa -0.020000000  0.18000000  0.35000000  0.278208955
#>   31 good_er  1   rank  0.000000000  0.25000000  0.50000000  0.500000000
#>   32 good_er  1  score  0.002605057  0.07422731  0.23868407  0.310583495
#>   33 good_er  1  label -1.000000000 -1.00000000  0.00000000  0.000000000
#>   34 good_er  1    err  0.290000000  0.34000000  0.36500000  0.382388060
#>   35 good_er  1    acc  0.500000000  0.57500000  0.63500000  0.617611940
#>   36 good_er  1     sp  0.000000000  0.36000000  0.67000000  0.617611940
#>   37 good_er  1     sn  0.000000000  0.44000000  0.67000000  0.617611940
#>   38 good_er  1   prec  0.500000000  0.57333333  0.66336634  0.712573333
#>   39 good_er  1    mcc  0.000000000  0.21716721  0.29538063  0.284896479
#>   40 good_er  1 fscore  0.000000000  0.58666667  0.66331658  0.581905091
#>   41 good_er  1   bacc  0.500000000  0.57500000  0.63500000  0.617611940
#>   42 good_er  1    npv  0.500000000  0.61666667  0.66019417  0.657780751
#>   43 good_er  1   infm  0.000000000  0.15000000  0.27000000  0.235223881
#>   44 good_er  1    mkd  0.000000000  0.29069767  0.32051282  0.370354083
#>   45 good_er  1  kappa  0.000000000  0.15000000  0.27000000  0.235223881
#>   46   excel  1   rank  0.000000000  0.25000000  0.50000000  0.500000000
#>   47   excel  1  score -2.472072577 -0.29064226  1.41853356  1.421932466
#>   48   excel  1  label -1.000000000 -1.00000000  0.00000000  0.000000000
#>   49   excel  1    err  0.060000000  0.13500000  0.25000000  0.258606965
#>   50   excel  1    acc  0.500000000  0.62500000  0.75000000  0.741393035
#>   51   excel  1     sp  0.000000000  0.50000000  0.93000000  0.741393035
#>   52   excel  1     sn  0.000000000  0.50000000  0.93000000  0.741393035
#>   53   excel  1   prec  0.500000000  0.66666667  0.93137255  0.838920518
#>   54   excel  1    mcc  0.070888121  0.38226007  0.57735027  0.558509644
#>   55   excel  1 fscore  0.000000000  0.66666667  0.76335878  0.703185485
#>   56   excel  1   bacc  0.500000000  0.62500000  0.75000000  0.741393035
#>   57   excel  1    npv  0.500000000  0.66666667  0.93069307  0.838261377
#>   58   excel  1   infm  0.000000000  0.25000000  0.50000000  0.482786070
#>   59   excel  1    mkd  0.500000000  0.57142857  0.66666667  0.677181896
#>   60   excel  1  kappa  0.000000000  0.25000000  0.50000000  0.482786070
#>   61    perf  1   rank  0.000000000  0.25000000  0.50000000  0.500000000
#>   62    perf  1  score  0.000000000  0.00000000  0.50000000  0.500000000
#>   63    perf  1  label -1.000000000 -1.00000000  0.00000000  0.000000000
#>   64    perf  1    err  0.000000000  0.12500000  0.25000000  0.251243781
#>   65    perf  1    acc  0.500000000  0.62500000  0.75000000  0.748756219
#>   66    perf  1     sp  0.000000000  0.50000000  1.00000000  0.748756219
#>   67    perf  1     sn  0.000000000  0.50000000  1.00000000  0.748756219
#>   68    perf  1   prec  0.500000000  0.66666667  1.00000000  0.846096234
#>   69    perf  1    mcc  0.070888121  0.38226007  0.57735027  0.573525244
#>   70    perf  1 fscore  0.000000000  0.66666667  0.76335878  0.710427365
#>   71    perf  1   bacc  0.500000000  0.62500000  0.75000000  0.748756219
#>   72    perf  1    npv  0.500000000  0.66666667  1.00000000  0.846096234
#>   73    perf  1   infm  0.000000000  0.25000000  0.50000000  0.497512438
#>   74    perf  1    mkd  0.500000000  0.57142857  0.66666667  0.692192468
#>   75    perf  1  kappa  0.000000000  0.25000000  0.50000000  0.497512438
#>          3rd Qu.      Max.
#>    1  0.75000000 1.0000000
#>    2  0.43118237 2.8909175
#>    3  1.00000000 1.0000000
#>    4  0.54000000 0.5750000
#>    5  0.49500000 0.5450000
#>    6  0.69000000 1.0000000
#>    7  0.73000000 1.0000000
#>    8  0.49689441 1.0000000
#>    9 -0.01250263 0.2170724
#>   10  0.58634538 0.6872852
#>   11  0.49500000 0.5450000
#>   12  0.49740933 1.0000000
#>   13 -0.01000000 0.0900000
#>   14 -0.01390627 0.5235602
#>   15 -0.01000000 0.0900000
#>   16  0.75000000 1.0000000
#>   17  0.87113793 0.9996329
#>   18  1.00000000 1.0000000
#>   19  0.41000000 0.5100000
#>   20  0.69000000 0.7100000
#>   21  0.89000000 1.0000000
#>   22  0.93000000 1.0000000
#>   23  0.75675676 0.8125000
#>   24  0.41011540 0.4752485
#>   25  0.72992701 0.7637795
#>   26  0.69000000 0.7100000
#>   27  0.86538462 1.0000000
#>   28  0.38000000 0.4200000
#>   29  0.49376299 0.5988024
#>   30  0.38000000 0.4200000
#>   31  0.75000000 1.0000000
#>   32  0.44978463 0.9968168
#>   33  1.00000000 1.0000000
#>   34  0.42500000 0.5000000
#>   35  0.66000000 0.7100000
#>   36  0.94000000 1.0000000
#>   37  0.86000000 1.0000000
#>   38  0.88235294 1.0000000
#>   39  0.35286410 0.4730162
#>   40  0.67619048 0.6935484
#>   41  0.66000000 0.7100000
#>   42  0.69696970 1.0000000
#>   43  0.32000000 0.4200000
#>   44  0.50179211 0.5813953
#>   45  0.32000000 0.4200000
#>   46  0.75000000 1.0000000
#>   47  2.98753715 5.7917107
#>   48  1.00000000 1.0000000
#>   49  0.37500000 0.5000000
#>   50  0.86500000 0.9400000
#>   51  1.00000000 1.0000000
#>   52  1.00000000 1.0000000
#>   53  1.00000000 1.0000000
#>   54  0.75604925 0.8801761
#>   55  0.86842105 0.9405941
#>   56  0.86500000 0.9400000
#>   57  1.00000000 1.0000000
#>   58  0.73000000 0.8800000
#>   59  0.77866667 0.8803521
#>   60  0.73000000 0.8800000
#>   61  0.75000000 1.0000000
#>   62  1.00000000 1.0000000
#>   63  1.00000000 1.0000000
#>   64  0.37500000 0.5000000
#>   65  0.87500000 1.0000000
#>   66  1.00000000 1.0000000
#>   67  1.00000000 1.0000000
#>   68  1.00000000 1.0000000
#>   69  0.77459667 1.0000000
#>   70  0.87640449 1.0000000
#>   71  0.87500000 1.0000000
#>   72  1.00000000 1.0000000
#>   73  0.75000000 1.0000000
#>   74  0.80000000 1.0000000
#>   75  0.75000000 1.0000000
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
#>    1    good_er          1        ROC 0.8273000
#>    2    good_er          1        PRC 0.8615398
#>    3    good_er          2        ROC 0.8211000
#>    4    good_er          2        PRC 0.8477956
#>    5    good_er          3        ROC 0.7880000
#>    6    good_er          3        PRC 0.8272355
#>    7    good_er          4        ROC 0.8078000
#>    8    good_er          4        PRC 0.8451917
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

## Generate an smpoints object that contains basic evaluation measures
smpoints <- evalmod(mdat, mode = "basic")
smpoints
#> 
#>     === Basic performance evaluation measures ===
#> 
#>      ## Performance measures (Meas.)
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
#>        Model ID  Meas.          Min.     1st Qu.    Median      Mean   3rd Qu.
#>    1 good_er  1   rank  0.0000000000  0.25000000 0.5000000 0.5000000 0.7500000
#>    2 good_er  1  score  0.0009256685  0.09590163 0.2438201 0.3368616 0.5058031
#>    3 good_er  1  label -1.0000000000 -1.00000000 0.0000000 0.0000000 1.0000000
#>    4 good_er  1    err  0.2450000000  0.26500000 0.3050000 0.3371642 0.4050000
#>    5 good_er  1    acc  0.5000000000  0.59500000 0.6950000 0.6628358 0.7350000
#>    6 good_er  1     sp  0.0000000000  0.41000000 0.7500000 0.6628358 0.9800000
#>    7 good_er  1     sn  0.0000000000  0.48000000 0.7500000 0.6628358 0.9100000
#>    8 good_er  1   prec  0.5000000000  0.60666667 0.7473684 0.7625079 0.9600000
#>    9 good_er  1    mcc  0.0708881205  0.27747917 0.4253394 0.3857951 0.4907128
#>   10 good_er  1 fscore  0.0000000000  0.64000000 0.6996198 0.6274156 0.7317073
#>   11 good_er  1   bacc  0.5000000000  0.59500000 0.6950000 0.6628358 0.7350000
#>   12 good_er  1    npv  0.5000000000  0.65306122 0.7474747 0.7230258 0.8030303
#>   13 good_er  1   infm  0.0000000000  0.19000000 0.3900000 0.3256716 0.4700000
#>   14 good_er  1    mkd  0.2551020408  0.42900043 0.4900490 0.4855337 0.5425347
#>   15 good_er  1  kappa  0.0000000000  0.19000000 0.3900000 0.3256716 0.4700000
#>   16 good_er  2   rank  0.0000000000  0.25000000 0.5000000 0.5000000 0.7500000
#>   17 good_er  2  score  0.0035396582  0.11678363 0.2818819 0.3359957 0.5011444
#>   18 good_er  2  label -1.0000000000 -1.00000000 0.0000000 0.0000000 1.0000000
#>   19 good_er  2    err  0.2500000000  0.27000000 0.3300000 0.3402488 0.3900000
#>   20 good_er  2    acc  0.5000000000  0.61000000 0.6700000 0.6597512 0.7300000
#>   21 good_er  2     sp  0.0000000000  0.39000000 0.7400000 0.6597512 0.9800000
#>   22 good_er  2     sn  0.0000000000  0.48000000 0.7400000 0.6597512 0.8900000
#>   23 good_er  2   prec  0.5000000000  0.59615385 0.7403846 0.7552153 0.9523810
#>   24 good_er  2    mcc  0.0411345035  0.32226437 0.3958333 0.3807775 0.4800877
#>   25 good_er  2 fscore  0.0000000000  0.64000000 0.7078652 0.6229962 0.7235772
#>   26 good_er  2   bacc  0.5000000000  0.61000000 0.6700000 0.6597512 0.7300000
#>   27 good_er  2    npv  0.5000000000  0.65333333 0.7373737 0.7270756 0.7843137
#>   28 good_er  2   infm  0.0000000000  0.22000000 0.3400000 0.3195025 0.4600000
#>   29 good_er  2    mkd  0.1692047377  0.44485351 0.4901961 0.4822909 0.5257143
#>   30 good_er  2  kappa  0.0000000000  0.22000000 0.3400000 0.3195025 0.4600000
#>   31 good_er  3   rank  0.0000000000  0.25000000 0.5000000 0.5000000 0.7500000
#>   32 good_er  3  score  0.0003870733  0.11381581 0.2867032 0.3503578 0.5177063
#>   33 good_er  3  label -1.0000000000 -1.00000000 0.0000000 0.0000000 1.0000000
#>   34 good_er  3    err  0.2600000000  0.29500000 0.3350000 0.3567164 0.4150000
#>   35 good_er  3    acc  0.4950000000  0.58500000 0.6650000 0.6432836 0.7050000
#>   36 good_er  3     sp  0.0000000000  0.38000000 0.7100000 0.6432836 0.9600000
#>   37 good_er  3     sn  0.0000000000  0.46000000 0.7100000 0.6432836 0.8800000
#>   38 good_er  3   prec  0.4974619289  0.58940397 0.7113402 0.7407338 0.9074074
#>   39 good_er  3    mcc -0.0708881205  0.25132279 0.3801927 0.3380444 0.4330569
#>   40 good_er  3 fscore  0.0000000000  0.60927152 0.6836364 0.6079563 0.7076923
#>   41 good_er  3   bacc  0.4950000000  0.58500000 0.6650000 0.6432836 0.7050000
#>   42 good_er  3    npv  0.0000000000  0.61538462 0.6990291 0.6723969 0.7500000
#>   43 good_er  3   infm -0.0100000000  0.17000000 0.3300000 0.2865672 0.4100000
#>   44 good_er  3    mkd -0.5025125628  0.35842294 0.4222016 0.4131307 0.5154639
#>   45 good_er  3  kappa -0.0100000000  0.17000000 0.3300000 0.2865672 0.4100000
#>   46 good_er  4   rank  0.0000000000  0.25000000 0.5000000 0.5000000 0.7500000
#>   47 good_er  4  score  0.0021737290  0.10038831 0.3338746 0.3619188 0.5892401
#>   48 good_er  4  label -1.0000000000 -1.00000000 0.0000000 0.0000000 1.0000000
#>   49 good_er  4    err  0.2400000000  0.27000000 0.3350000 0.3468657 0.4150000
#>   50 good_er  4    acc  0.5000000000  0.58500000 0.6650000 0.6531343 0.7300000
#>   51 good_er  4     sp  0.0000000000  0.36000000 0.7500000 0.6531343 0.9800000
#>   52 good_er  4     sn  0.0000000000  0.48000000 0.7500000 0.6531343 0.8600000
#>   53 good_er  4   prec  0.5000000000  0.57615894 0.7450980 0.7520778 0.9500000
#>   54 good_er  4    mcc  0.0708881205  0.24691259 0.3834129 0.3634790 0.4904171
#>   55 good_er  4 fscore  0.0000000000  0.63157895 0.6909091 0.6184292 0.7181818
#>   56 good_er  4   bacc  0.5000000000  0.58500000 0.6650000 0.6531343 0.7300000
#>   57 good_er  4    npv  0.5000000000  0.64900662 0.7281553 0.7090432 0.7553191
#>   58 good_er  4   infm  0.0000000000  0.17000000 0.3300000 0.3062687 0.4600000
#>   59 good_er  4    mkd  0.2622377622  0.37878788 0.4924128 0.4611210 0.5347594
#>   60 good_er  4  kappa  0.0000000000  0.17000000 0.3300000 0.3062687 0.4600000
#>           Max.
#>    1 1.0000000
#>    2 0.9966341
#>    3 1.0000000
#>    4 0.5000000
#>    5 0.7550000
#>    6 1.0000000
#>    7 1.0000000
#>    8 1.0000000
#>    9 0.5464008
#>   10 0.7655502
#>   11 0.7550000
#>   12 1.0000000
#>   13 0.5100000
#>   14 0.6451613
#>   15 0.5100000
#>   16 1.0000000
#>   17 0.9975991
#>   18 1.0000000
#>   19 0.5000000
#>   20 0.7500000
#>   21 1.0000000
#>   22 1.0000000
#>   23 1.0000000
#>   24 0.5391624
#>   25 0.7549020
#>   26 0.7500000
#>   27 1.0000000
#>   28 0.5000000
#>   29 0.6185024
#>   30 0.5000000
#>   31 1.0000000
#>   32 0.9996034
#>   33 1.0000000
#>   34 0.5050000
#>   35 0.7400000
#>   36 1.0000000
#>   37 1.0000000
#>   38 1.0000000
#>   39 0.5039033
#>   40 0.7281106
#>   41 0.7400000
#>   42 0.7931034
#>   43 0.4800000
#>   44 0.5892658
#>   45 0.4800000
#>   46 1.0000000
#>   47 0.9924562
#>   48 1.0000000
#>   49 0.5000000
#>   50 0.7600000
#>   51 1.0000000
#>   52 1.0000000
#>   53 1.0000000
#>   54 0.5489586
#>   55 0.7562189
#>   56 0.7600000
#>   57 1.0000000
#>   58 0.5200000
#>   59 0.6133333
#>   60 0.5200000
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
#>    1     random          1        ROC 0.4698000
#>    2     random          1        PRC 0.4889832
#>    3    poor_er          1        ROC 0.8006000
#>    4    poor_er          1        PRC 0.7659710
#>    5    good_er          1        ROC 0.8003000
#>    6    good_er          1        PRC 0.8365382
#>    7      excel          1        ROC 0.9825000
#>    8      excel          1        PRC 0.9846561
#>    9       perf          1        ROC 1.0000000
#>   10       perf          1        PRC 1.0000000
#>   11     random          2        ROC 0.5034000
#>   12     random          2        PRC 0.4981629
#>   13    poor_er          2        ROC 0.7836000
#>   14    poor_er          2        PRC 0.7684940
#>   15    good_er          2        ROC 0.8108000
#>   16    good_er          2        PRC 0.8406739
#>   17      excel          2        ROC 0.9857000
#>   18      excel          2        PRC 0.9869817
#>   19       perf          2        ROC 1.0000000
#>   20       perf          2        PRC 1.0000000
#>   21     random          3        ROC 0.5503000
#>   22     random          3        PRC 0.5425962
#>   23    poor_er          3        ROC 0.8097000
#>   24    poor_er          3        PRC 0.7688925
#>   25    good_er          3        ROC 0.8066000
#>   26    good_er          3        PRC 0.8400369
#>   27      excel          3        ROC 0.9862000
#>   28      excel          3        PRC 0.9847615
#>   29       perf          3        ROC 1.0000000
#>   30       perf          3        PRC 1.0000000
#>   31     random          4        ROC 0.4962000
#>   32     random          4        PRC 0.5355406
#>   33    poor_er          4        ROC 0.7849000
#>   34    poor_er          4        PRC 0.7386785
#>   35    good_er          4        ROC 0.8202000
#>   36    good_er          4        PRC 0.8463619
#>   37      excel          4        ROC 0.9802000
#>   38      excel          4        PRC 0.9816984
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

## Generate an mmpoints object that contains basic evaluation measures
mmpoints <- evalmod(mdat, mode = "basic")
mmpoints
#> 
#>     === Basic performance evaluation measures ===
#> 
#>      ## Performance measures (Meas.)
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
#>        Model ID  Meas.          Min.     1st Qu.       Median          Mean
#>    1  random  1   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>    2  random  1  score -3.0385697604 -0.66505450  0.057564205 -0.0032225087
#>    3  random  1  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>    4  random  1    err  0.4750000000  0.50000000  0.520000000  0.5150248756
#>    5  random  1    acc  0.4450000000  0.47500000  0.480000000  0.4849751244
#>    6  random  1     sp  0.0000000000  0.22000000  0.470000000  0.4849751244
#>    7  random  1     sn  0.0000000000  0.23000000  0.470000000  0.4849751244
#>    8  random  1   prec  0.3333333333  0.46236559  0.480000000  0.4874532473
#>    9  random  1    mcc -0.1120409274 -0.06173634 -0.041424651 -0.0284866414
#>   10  random  1 fscore  0.0000000000  0.30666667  0.470000000  0.4339723461
#>   11  random  1   bacc  0.4450000000  0.47500000  0.480000000  0.4849751244
#>   12  random  1    npv  0.4255319149  0.46728972  0.485549133  0.5006075925
#>   13  random  1   infm -0.1100000000 -0.05000000 -0.040000000 -0.0300497512
#>   14  random  1    mkd -0.1692047377 -0.07168459 -0.046019328 -0.0119391602
#>   15  random  1  kappa -0.1100000000 -0.05000000 -0.040000000 -0.0300497512
#>   16 poor_er  1   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>   17 poor_er  1  score  0.0063572009  0.47349096  0.723741804  0.6575554870
#>   18 poor_er  1  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>   19 poor_er  1    err  0.2600000000  0.29000000  0.335000000  0.3504477612
#>   20 poor_er  1    acc  0.5000000000  0.59000000  0.665000000  0.6495522388
#>   21 poor_er  1     sp  0.0000000000  0.46000000  0.740000000  0.6495522388
#>   22 poor_er  1     sn  0.0000000000  0.39000000  0.740000000  0.6495522388
#>   23 poor_er  1   prec  0.5000000000  0.63380282  0.732673267  0.7103268206
#>   24 poor_er  1    mcc  0.0708881205  0.26561731  0.393534185  0.3570862767
#>   25 poor_er  1 fscore  0.0000000000  0.52000000  0.692041522  0.6029304206
#>   26 poor_er  1   bacc  0.5000000000  0.59000000  0.665000000  0.6495522388
#>   27 poor_er  1    npv  0.5000000000  0.59333333  0.734693878  0.7460340970
#>   28 poor_er  1   infm  0.0000000000  0.18000000  0.330000000  0.2991044776
#>   29 poor_er  1    mkd  0.2906976744  0.39277298  0.461389024  0.4563609176
#>   30 poor_er  1  kappa  0.0000000000  0.18000000  0.330000000  0.2991044776
#>   31 good_er  1   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>   32 good_er  1  score  0.0019544715  0.09464476  0.266800885  0.3325140075
#>   33 good_er  1  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>   34 good_er  1    err  0.2550000000  0.30000000  0.320000000  0.3505970149
#>   35 good_er  1    acc  0.5000000000  0.59500000  0.680000000  0.6494029851
#>   36 good_er  1     sp  0.0000000000  0.39000000  0.700000000  0.6494029851
#>   37 good_er  1     sn  0.0000000000  0.46000000  0.700000000  0.6494029851
#>   38 good_er  1   prec  0.5000000000  0.59459459  0.696969697  0.7467080156
#>   39 good_er  1    mcc  0.0708881205  0.28553298  0.389194875  0.3591459762
#>   40 good_er  1 fscore  0.0000000000  0.60927152  0.687719298  0.6132535773
#>   41 good_er  1   bacc  0.5000000000  0.59500000  0.680000000  0.6494029851
#>   42 good_er  1    npv  0.5000000000  0.63758389  0.693069307  0.7156984245
#>   43 good_er  1   infm  0.0000000000  0.19000000  0.360000000  0.2988059701
#>   44 good_er  1    mkd  0.3447650154  0.39097744  0.441361917  0.4624064401
#>   45 good_er  1  kappa  0.0000000000  0.19000000  0.360000000  0.2988059701
#>   46   excel  1   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>   47   excel  1  score -2.2999037945 -0.07517908  1.398519946  1.4432471963
#>   48   excel  1  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>   49   excel  1    err  0.0600000000  0.13000000  0.255000000  0.2599502488
#>   50   excel  1    acc  0.5000000000  0.62500000  0.745000000  0.7400497512
#>   51   excel  1     sp  0.0000000000  0.49000000  0.910000000  0.7400497512
#>   52   excel  1     sn  0.0000000000  0.50000000  0.910000000  0.7400497512
#>   53   excel  1   prec  0.5000000000  0.66225166  0.912621359  0.8377698064
#>   54   excel  1    mcc  0.0708881205  0.38226007  0.569651921  0.5556460798
#>   55   excel  1 fscore  0.0000000000  0.66666667  0.763358779  0.7019566075
#>   56   excel  1   bacc  0.5000000000  0.62500000  0.745000000  0.7400497512
#>   57   excel  1    npv  0.5000000000  0.66666667  0.910891089  0.8364102486
#>   58   excel  1   infm  0.0000000000  0.25000000  0.490000000  0.4800995025
#>   59   excel  1    mkd  0.5000000000  0.57142857  0.657894737  0.6741800550
#>   60   excel  1  kappa  0.0000000000  0.25000000  0.490000000  0.4800995025
#>   61    perf  1   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>   62    perf  1  score  0.0000000000  0.00000000  0.500000000  0.5000000000
#>   63    perf  1  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>   64    perf  1    err  0.0000000000  0.12500000  0.250000000  0.2512437811
#>   65    perf  1    acc  0.5000000000  0.62500000  0.750000000  0.7487562189
#>   66    perf  1     sp  0.0000000000  0.50000000  1.000000000  0.7487562189
#>   67    perf  1     sn  0.0000000000  0.50000000  1.000000000  0.7487562189
#>   68    perf  1   prec  0.5000000000  0.66666667  1.000000000  0.8460962341
#>   69    perf  1    mcc  0.0708881205  0.38226007  0.577350269  0.5735252435
#>   70    perf  1 fscore  0.0000000000  0.66666667  0.763358779  0.7104273649
#>   71    perf  1   bacc  0.5000000000  0.62500000  0.750000000  0.7487562189
#>   72    perf  1    npv  0.5000000000  0.66666667  1.000000000  0.8460962341
#>   73    perf  1   infm  0.0000000000  0.25000000  0.500000000  0.4975124378
#>   74    perf  1    mkd  0.5000000000  0.57142857  0.666666667  0.6921924681
#>   75    perf  1  kappa  0.0000000000  0.25000000  0.500000000  0.4975124378
#>   76  random  2   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>   77  random  2  score -2.1808823604 -0.55260870 -0.002567253  0.0717506381
#>   78  random  2  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>   79  random  2    err  0.4300000000  0.49000000  0.505000000  0.4983084577
#>   80  random  2    acc  0.4650000000  0.48500000  0.495000000  0.5016915423
#>   81  random  2     sp  0.0000000000  0.24000000  0.490000000  0.5016915423
#>   82  random  2     sn  0.0000000000  0.31000000  0.490000000  0.5016915423
#>   83  random  2   prec  0.0000000000  0.48245614  0.494252874  0.4894635718
#>   84  random  2    mcc -0.1205957675 -0.05095448 -0.020004001 -0.0005702774
#>   85  random  2 fscore  0.0000000000  0.41059603  0.494949495  0.4539690404
#>   86  random  2   bacc  0.4650000000  0.48500000  0.495000000  0.5016915423
#>   87  random  2    npv  0.0000000000  0.47169811  0.494505495  0.4901034564
#>   88  random  2   infm -0.0700000000 -0.03000000 -0.010000000  0.0033830846
#>   89  random  2    mkd -0.5050505051 -0.06422608 -0.020292208 -0.0204329718
#>   90  random  2  kappa -0.0700000000 -0.03000000 -0.010000000  0.0033830846
#>   91 poor_er  2   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>   92 poor_er  2  score  0.0011442418  0.46281111  0.723817175  0.6546701299
#>   93 poor_er  2  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>   94 poor_er  2    err  0.2900000000  0.31500000  0.330000000  0.3589054726
#>   95 poor_er  2    acc  0.5000000000  0.60000000  0.670000000  0.6410945274
#>   96 poor_er  2     sp  0.0000000000  0.46000000  0.670000000  0.6410945274
#>   97 poor_er  2     sn  0.0000000000  0.40000000  0.670000000  0.6410945274
#>   98 poor_er  2   prec  0.5000000000  0.63698630  0.672897196  0.7099988135
#>   99 poor_er  2    mcc  0.0708881205  0.30241917  0.360288346  0.3416028557
#>  100 poor_er  2 fscore  0.0000000000  0.53333333  0.673366834  0.5963945117
#>  101 poor_er  2   bacc  0.5000000000  0.60000000  0.670000000  0.6410945274
#>  102 poor_er  2    npv  0.5000000000  0.60000000  0.676190476  0.7339439314
#>  103 poor_er  2   infm  0.0000000000  0.20000000  0.340000000  0.2821890547
#>  104 poor_er  2    mkd  0.3300330033  0.38244767  0.425230333  0.4439427449
#>  105 poor_er  2  kappa  0.0000000000  0.20000000  0.340000000  0.2821890547
#>  106 good_er  2   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>  107 good_er  2  score  0.0083967150  0.11215977  0.339638552  0.3768638288
#>  108 good_er  2  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>  109 good_er  2    err  0.2400000000  0.28500000  0.325000000  0.3453731343
#>  110 good_er  2    acc  0.5000000000  0.60000000  0.675000000  0.6546268657
#>  111 good_er  2     sp  0.0000000000  0.41000000  0.730000000  0.6546268657
#>  112 good_er  2     sn  0.0000000000  0.47000000  0.730000000  0.6546268657
#>  113 good_er  2   prec  0.5000000000  0.60810811  0.727272727  0.7504424146
#>  114 good_er  2    mcc  0.0708881205  0.29019428  0.391924758  0.3672571613
#>  115 good_er  2 fscore  0.0000000000  0.62162162  0.693140794  0.6182402355
#>  116 good_er  2   bacc  0.5000000000  0.60000000  0.675000000  0.6546268657
#>  117 good_er  2    npv  0.5000000000  0.64383562  0.731182796  0.7145186936
#>  118 good_er  2   infm  0.0000000000  0.20000000  0.350000000  0.3092537313
#>  119 good_er  2    mkd  0.2220577350  0.41178334  0.480769231  0.4649611083
#>  120 good_er  2  kappa  0.0000000000  0.20000000  0.350000000  0.3092537313
#>  121   excel  2   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>  122   excel  2  score -3.5720617171  0.03688547  1.463896647  1.4038660084
#>  123   excel  2  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>  124   excel  2    err  0.0600000000  0.13500000  0.250000000  0.2583582090
#>  125   excel  2    acc  0.5000000000  0.62500000  0.750000000  0.7416417910
#>  126   excel  2     sp  0.0000000000  0.50000000  0.920000000  0.7416417910
#>  127   excel  2     sn  0.0000000000  0.50000000  0.920000000  0.7416417910
#>  128   excel  2   prec  0.5000000000  0.66666667  0.923076923  0.8391552741
#>  129   excel  2    mcc  0.0708881205  0.38226007  0.577350269  0.5590601659
#>  130   excel  2 fscore  0.0000000000  0.66666667  0.763358779  0.7034180234
#>  131   excel  2   bacc  0.5000000000  0.62500000  0.750000000  0.7416417910
#>  132   excel  2    npv  0.5000000000  0.66666667  0.920792079  0.8386217420
#>  133   excel  2   infm  0.0000000000  0.25000000  0.500000000  0.4832835821
#>  134   excel  2    mkd  0.5000000000  0.57142857  0.666666667  0.6777770161
#>  135   excel  2  kappa  0.0000000000  0.25000000  0.500000000  0.4832835821
#>  136    perf  2   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>  137    perf  2  score  0.0000000000  0.00000000  0.500000000  0.5000000000
#>  138    perf  2  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>  139    perf  2    err  0.0000000000  0.12500000  0.250000000  0.2512437811
#>  140    perf  2    acc  0.5000000000  0.62500000  0.750000000  0.7487562189
#>  141    perf  2     sp  0.0000000000  0.50000000  1.000000000  0.7487562189
#>  142    perf  2     sn  0.0000000000  0.50000000  1.000000000  0.7487562189
#>  143    perf  2   prec  0.5000000000  0.66666667  1.000000000  0.8460962341
#>  144    perf  2    mcc  0.0708881205  0.38226007  0.577350269  0.5735252435
#>  145    perf  2 fscore  0.0000000000  0.66666667  0.763358779  0.7104273649
#>  146    perf  2   bacc  0.5000000000  0.62500000  0.750000000  0.7487562189
#>  147    perf  2    npv  0.5000000000  0.66666667  1.000000000  0.8460962341
#>  148    perf  2   infm  0.0000000000  0.25000000  0.500000000  0.4975124378
#>  149    perf  2    mkd  0.5000000000  0.57142857  0.666666667  0.6921924681
#>  150    perf  2  kappa  0.0000000000  0.25000000  0.500000000  0.4975124378
#>  151  random  3   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>  152  random  3  score -3.6245118960 -0.61962623  0.035365241  0.0490521464
#>  153  random  3  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>  154  random  3    err  0.4300000000  0.45000000  0.480000000  0.4749751244
#>  155  random  3    acc  0.4850000000  0.50500000  0.520000000  0.5250248756
#>  156  random  3     sp  0.0000000000  0.27000000  0.550000000  0.5250248756
#>  157  random  3     sn  0.0000000000  0.29000000  0.550000000  0.5250248756
#>  158  random  3   prec  0.2500000000  0.50261780  0.528925620  0.5392324946
#>  159  random  3    mcc -0.0714285714  0.01227805  0.046188022  0.0544612984
#>  160  random  3 fscore  0.0000000000  0.38666667  0.555555556  0.4781834656
#>  161  random  3   bacc  0.4850000000  0.50500000  0.520000000  0.5250248756
#>  162  random  3    npv  0.0000000000  0.50261780  0.524590164  0.5199986065
#>  163  random  3   infm -0.0300000000  0.01000000  0.040000000  0.0500497512
#>  164  random  3    mkd -0.5025125628  0.01481262  0.061199510  0.0592311010
#>  165  random  3  kappa -0.0300000000  0.01000000  0.040000000  0.0500497512
#>  166 poor_er  3   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>  167 poor_er  3  score  0.0186685207  0.46536186  0.696133738  0.6416807653
#>  168 poor_er  3  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>  169 poor_er  3    err  0.2350000000  0.28500000  0.340000000  0.3459203980
#>  170 poor_er  3    acc  0.5000000000  0.59500000  0.660000000  0.6540796020
#>  171 poor_er  3     sp  0.0000000000  0.45000000  0.720000000  0.6540796020
#>  172 poor_er  3     sn  0.0000000000  0.41000000  0.720000000  0.6540796020
#>  173 poor_er  3   prec  0.5000000000  0.63333333  0.724137931  0.7128833908
#>  174 poor_er  3    mcc  0.0708881205  0.29877085  0.382917848  0.3662738605
#>  175 poor_er  3 fscore  0.0000000000  0.54666667  0.689655172  0.6075207773
#>  176 poor_er  3   bacc  0.5000000000  0.59500000  0.660000000  0.6540796020
#>  177 poor_er  3    npv  0.5000000000  0.60666667  0.720000000  0.7510625217
#>  178 poor_er  3   infm  0.0000000000  0.19000000  0.320000000  0.3081592040
#>  179 poor_er  3    mkd  0.2551020408  0.39932745  0.451127820  0.4639459126
#>  180 poor_er  3  kappa  0.0000000000  0.19000000  0.320000000  0.3081592040
#>  181 good_er  3   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>  182 good_er  3  score  0.0002239683  0.12733396  0.250207858  0.3233809979
#>  183 good_er  3  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>  184 good_er  3    err  0.2400000000  0.28500000  0.325000000  0.3474626866
#>  185 good_er  3    acc  0.5000000000  0.59000000  0.675000000  0.6525373134
#>  186 good_er  3     sp  0.0000000000  0.38000000  0.730000000  0.6525373134
#>  187 good_er  3     sn  0.0000000000  0.46000000  0.730000000  0.6525373134
#>  188 good_er  3   prec  0.5000000000  0.58666667  0.729166667  0.7494676288
#>  189 good_er  3    mcc  0.0708881205  0.25920125  0.382805469  0.3640749897
#>  190 good_er  3 fscore  0.0000000000  0.60927152  0.692579505  0.6169208435
#>  191 good_er  3   bacc  0.5000000000  0.59000000  0.675000000  0.6525373134
#>  192 good_er  3    npv  0.5000000000  0.63758389  0.722222222  0.7159282057
#>  193 good_er  3   infm  0.0000000000  0.18000000  0.350000000  0.3050746269
#>  194 good_er  3    mkd  0.2834868887  0.39787798  0.475756656  0.4653958345
#>  195 good_er  3  kappa  0.0000000000  0.18000000  0.350000000  0.3050746269
#>  196   excel  3   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>  197   excel  3  score -1.6232730521 -0.02764254  1.517342521  1.5004948803
#>  198   excel  3  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>  199   excel  3    err  0.0500000000  0.13500000  0.250000000  0.2581094527
#>  200   excel  3    acc  0.5000000000  0.62500000  0.750000000  0.7418905473
#>  201   excel  3     sp  0.0000000000  0.50000000  0.930000000  0.7418905473
#>  202   excel  3     sn  0.0000000000  0.50000000  0.930000000  0.7418905473
#>  203   excel  3   prec  0.5000000000  0.66666667  0.932038835  0.8382946868
#>  204   excel  3    mcc  0.0708881205  0.38226007  0.577350269  0.5594325489
#>  205   excel  3 fscore  0.0000000000  0.66666667  0.760456274  0.7031840887
#>  206   excel  3   bacc  0.5000000000  0.62500000  0.750000000  0.7418905473
#>  207   excel  3    npv  0.5000000000  0.66666667  0.932692308  0.8397097868
#>  208   excel  3   infm  0.0000000000  0.25000000  0.500000000  0.4837810945
#>  209   excel  3    mkd  0.5000000000  0.57142857  0.662251656  0.6780044736
#>  210   excel  3  kappa  0.0000000000  0.25000000  0.500000000  0.4837810945
#>  211    perf  3   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>  212    perf  3  score  0.0000000000  0.00000000  0.500000000  0.5000000000
#>  213    perf  3  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>  214    perf  3    err  0.0000000000  0.12500000  0.250000000  0.2512437811
#>  215    perf  3    acc  0.5000000000  0.62500000  0.750000000  0.7487562189
#>  216    perf  3     sp  0.0000000000  0.50000000  1.000000000  0.7487562189
#>  217    perf  3     sn  0.0000000000  0.50000000  1.000000000  0.7487562189
#>  218    perf  3   prec  0.5000000000  0.66666667  1.000000000  0.8460962341
#>  219    perf  3    mcc  0.0708881205  0.38226007  0.577350269  0.5735252435
#>  220    perf  3 fscore  0.0000000000  0.66666667  0.763358779  0.7104273649
#>  221    perf  3   bacc  0.5000000000  0.62500000  0.750000000  0.7487562189
#>  222    perf  3    npv  0.5000000000  0.66666667  1.000000000  0.8460962341
#>  223    perf  3   infm  0.0000000000  0.25000000  0.500000000  0.4975124378
#>  224    perf  3    mkd  0.5000000000  0.57142857  0.666666667  0.6921924681
#>  225    perf  3  kappa  0.0000000000  0.25000000  0.500000000  0.4975124378
#>  226  random  4   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>  227  random  4  score -2.7048139123 -0.68637147 -0.014199461 -0.0061517084
#>  228  random  4  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>  229  random  4    err  0.4750000000  0.49000000  0.505000000  0.5018905473
#>  230  random  4    acc  0.4750000000  0.48500000  0.495000000  0.4981094527
#>  231  random  4     sp  0.0000000000  0.23000000  0.490000000  0.4981094527
#>  232  random  4     sn  0.0000000000  0.26000000  0.490000000  0.4981094527
#>  233  random  4   prec  0.4687500000  0.49107143  0.496774194  0.5274055213
#>  234  random  4    mcc -0.1428571429 -0.03079966 -0.011076296 -0.0031980351
#>  235  random  4 fscore  0.0000000000  0.34210526  0.489583333  0.4513323482
#>  236  random  4   bacc  0.4750000000  0.48500000  0.495000000  0.4981094527
#>  237  random  4    npv  0.0000000000  0.47916667  0.492537313  0.4712522010
#>  238  random  4   infm -0.0500000000 -0.03000000 -0.010000000 -0.0037810945
#>  239  random  4    mkd -0.5102040816 -0.03387534 -0.012268433 -0.0013422776
#>  240  random  4  kappa -0.0500000000 -0.03000000 -0.010000000 -0.0037810945
#>  241 poor_er  4   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>  242 poor_er  4  score  0.0059061577  0.48643478  0.696801578  0.6499215100
#>  243 poor_er  4  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>  244 poor_er  4    err  0.2500000000  0.30000000  0.345000000  0.3582587065
#>  245 poor_er  4    acc  0.5000000000  0.58000000  0.655000000  0.6417412935
#>  246 poor_er  4     sp  0.0000000000  0.41000000  0.740000000  0.6417412935
#>  247 poor_er  4     sn  0.0000000000  0.38000000  0.740000000  0.6417412935
#>  248 poor_er  4   prec  0.5000000000  0.60839161  0.727272727  0.6927666934
#>  249 poor_er  4    mcc  0.0411345035  0.25132279  0.361808177  0.3346890466
#>  250 poor_er  4 fscore  0.0000000000  0.51006711  0.701754386  0.5955620739
#>  251 poor_er  4   bacc  0.5000000000  0.58000000  0.655000000  0.6417412935
#>  252 poor_er  4    npv  0.5000000000  0.58940397  0.742574257  0.7295150558
#>  253 poor_er  4   infm  0.0000000000  0.16000000  0.310000000  0.2834825871
#>  254 poor_er  4    mkd  0.1692047377  0.37264094  0.437062937  0.4222817492
#>  255 poor_er  4  kappa  0.0000000000  0.16000000  0.310000000  0.2834825871
#>  256 good_er  4   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>  257 good_er  4  score  0.0014192720  0.09999452  0.273171522  0.3398996594
#>  258 good_er  4  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>  259 good_er  4    err  0.2400000000  0.27500000  0.320000000  0.3406965174
#>  260 good_er  4    acc  0.4950000000  0.60500000  0.680000000  0.6593034826
#>  261 good_er  4     sp  0.0000000000  0.42000000  0.730000000  0.6593034826
#>  262 good_er  4     sn  0.0000000000  0.46000000  0.730000000  0.6593034826
#>  263 good_er  4   prec  0.4974619289  0.61333333  0.727272727  0.7545238130
#>  264 good_er  4    mcc -0.0708881205  0.31861207  0.422200331  0.3753975565
#>  265 good_er  4 fscore  0.0000000000  0.61333333  0.705882353  0.6224938859
#>  266 good_er  4   bacc  0.4950000000  0.60500000  0.680000000  0.6593034826
#>  267 good_er  4    npv  0.0000000000  0.61635220  0.718446602  0.7027802531
#>  268 good_er  4   infm -0.0100000000  0.21000000  0.360000000  0.3186069652
#>  269 good_er  4    mkd -0.5025125628  0.43739192  0.463811887  0.4573040661
#>  270 good_er  4  kappa -0.0100000000  0.21000000  0.360000000  0.3186069652
#>  271   excel  4   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>  272   excel  4  score -2.2016953324  0.12495678  1.275541833  1.4058527536
#>  273   excel  4  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>  274   excel  4    err  0.0800000000  0.13500000  0.250000000  0.2610945274
#>  275   excel  4    acc  0.5000000000  0.62500000  0.750000000  0.7389054726
#>  276   excel  4     sp  0.0000000000  0.50000000  0.910000000  0.7389054726
#>  277   excel  4     sn  0.0000000000  0.50000000  0.910000000  0.7389054726
#>  278   excel  4   prec  0.5000000000  0.66666667  0.909090909  0.8363311924
#>  279   excel  4    mcc  0.0708881205  0.38226007  0.577350269  0.5535080918
#>  280   excel  4 fscore  0.0000000000  0.66666667  0.763358779  0.7006467208
#>  281   excel  4   bacc  0.5000000000  0.62500000  0.750000000  0.7389054726
#>  282   excel  4    npv  0.5000000000  0.66666667  0.909090909  0.8359246368
#>  283   excel  4   infm  0.0000000000  0.25000000  0.500000000  0.4778109453
#>  284   excel  4    mkd  0.5000000000  0.57142857  0.666666667  0.6722558291
#>  285   excel  4  kappa  0.0000000000  0.25000000  0.500000000  0.4778109453
#>  286    perf  4   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>  287    perf  4  score  0.0000000000  0.00000000  0.500000000  0.5000000000
#>  288    perf  4  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>  289    perf  4    err  0.0000000000  0.12500000  0.250000000  0.2512437811
#>  290    perf  4    acc  0.5000000000  0.62500000  0.750000000  0.7487562189
#>  291    perf  4     sp  0.0000000000  0.50000000  1.000000000  0.7487562189
#>  292    perf  4     sn  0.0000000000  0.50000000  1.000000000  0.7487562189
#>  293    perf  4   prec  0.5000000000  0.66666667  1.000000000  0.8460962341
#>  294    perf  4    mcc  0.0708881205  0.38226007  0.577350269  0.5735252435
#>  295    perf  4 fscore  0.0000000000  0.66666667  0.763358779  0.7104273649
#>  296    perf  4   bacc  0.5000000000  0.62500000  0.750000000  0.7487562189
#>  297    perf  4    npv  0.5000000000  0.66666667  1.000000000  0.8460962341
#>  298    perf  4   infm  0.0000000000  0.25000000  0.500000000  0.4975124378
#>  299    perf  4    mkd  0.5000000000  0.57142857  0.666666667  0.6921924681
#>  300    perf  4  kappa  0.0000000000  0.25000000  0.500000000  0.4975124378
#>         3rd Qu.      Max.
#>    1 0.75000000 1.0000000
#>    2 0.70383112 2.9137159
#>    3 1.00000000 1.0000000
#>    4 0.52500000 0.5550000
#>    5 0.50000000 0.5250000
#>    6 0.73000000 1.0000000
#>    7 0.72000000 1.0000000
#>    8 0.50000000 1.0000000
#>    9 0.00000000 0.1005038
#>   10 0.57707510 0.6711409
#>   11 0.50000000 0.5250000
#>   12 0.50000000 1.0000000
#>   13 0.00000000 0.0500000
#>   14 0.00000000 0.5050505
#>   15 0.00000000 0.0500000
#>   16 0.75000000 1.0000000
#>   17 0.91004596 0.9985957
#>   18 1.00000000 1.0000000
#>   19 0.41000000 0.5000000
#>   20 0.71000000 0.7400000
#>   21 0.89000000 1.0000000
#>   22 0.96000000 1.0000000
#>   23 0.77083333 1.0000000
#>   24 0.45354924 0.5031767
#>   25 0.74436090 0.7739130
#>   26 0.71000000 0.7400000
#>   27 0.91489362 1.0000000
#>   28 0.42000000 0.4800000
#>   29 0.51546392 0.5952381
#>   30 0.42000000 0.4800000
#>   31 0.75000000 1.0000000
#>   32 0.54258792 0.9892889
#>   33 1.00000000 1.0000000
#>   34 0.40500000 0.5000000
#>   35 0.70000000 0.7450000
#>   36 0.96000000 1.0000000
#>   37 0.89000000 1.0000000
#>   38 0.91304348 1.0000000
#>   39 0.43696789 0.5089004
#>   40 0.70588235 0.7466667
#>   41 0.70000000 0.7450000
#>   42 0.78787879 1.0000000
#>   43 0.40000000 0.4900000
#>   44 0.52852982 0.6134969
#>   45 0.40000000 0.4900000
#>   46 0.75000000 1.0000000
#>   47 2.81743566 5.4504118
#>   48 1.00000000 1.0000000
#>   49 0.37500000 0.5000000
#>   50 0.87000000 0.9400000
#>   51 1.00000000 1.0000000
#>   52 0.99000000 1.0000000
#>   53 1.00000000 1.0000000
#>   54 0.76227922 0.8844333
#>   55 0.86516854 0.9368421
#>   56 0.87000000 0.9400000
#>   57 0.98750000 1.0000000
#>   58 0.74000000 0.8800000
#>   59 0.78522920 0.8888889
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
#>   77 0.73469464 2.7604293
#>   78 1.00000000 1.0000000
#>   79 0.51500000 0.5350000
#>   80 0.51000000 0.5700000
#>   81 0.81000000 1.0000000
#>   82 0.74000000 1.0000000
#>   83 0.51162791 0.6304348
#>   84 0.03964718 0.1559024
#>   85 0.58730159 0.6689420
#>   86 0.51000000 0.5700000
#>   87 0.52032520 0.7142857
#>   88 0.02000000 0.1400000
#>   89 0.04974299 0.2220577
#>   90 0.02000000 0.1400000
#>   91 0.75000000 1.0000000
#>   92 0.88196813 0.9996147
#>   93 1.00000000 1.0000000
#>   94 0.40000000 0.5000000
#>   95 0.68500000 0.7100000
#>   96 0.90000000 1.0000000
#>   97 0.96000000 1.0000000
#>   98 0.80392157 1.0000000
#>   99 0.40091700 0.4917076
#>  100 0.72262774 0.7698413
#>  101 0.68500000 0.7100000
#>  102 0.91836735 1.0000000
#>  103 0.37000000 0.4200000
#>  104 0.51282051 0.5827506
#>  105 0.37000000 0.4200000
#>  106 0.75000000 1.0000000
#>  107 0.59775645 0.9717329
#>  108 1.00000000 1.0000000
#>  109 0.40000000 0.5000000
#>  110 0.71500000 0.7600000
#>  111 0.97000000 1.0000000
#>  112 0.91000000 1.0000000
#>  113 0.93333333 1.0000000
#>  114 0.46282255 0.5226197
#>  115 0.72636816 0.7473684
#>  116 0.71500000 0.7600000
#>  117 0.80000000 1.0000000
#>  118 0.43000000 0.5200000
#>  119 0.52012664 0.6030702
#>  120 0.43000000 0.5200000
#>  121 0.75000000 1.0000000
#>  122 2.81980197 5.7493668
#>  123 1.00000000 1.0000000
#>  124 0.37500000 0.5000000
#>  125 0.86500000 0.9400000
#>  126 1.00000000 1.0000000
#>  127 1.00000000 1.0000000
#>  128 1.00000000 1.0000000
#>  129 0.75413386 0.8807048
#>  130 0.86956522 0.9411765
#>  131 0.86500000 0.9400000
#>  132 1.00000000 1.0000000
#>  133 0.73000000 0.8800000
#>  134 0.77763766 0.8814103
#>  135 0.73000000 0.8800000
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
#>  152 0.73628310 3.1319450
#>  153 1.00000000 1.0000000
#>  154 0.49500000 0.5150000
#>  155 0.55000000 0.5700000
#>  156 0.79000000 1.0000000
#>  157 0.77000000 1.0000000
#>  158 0.56976744 1.0000000
#>  159 0.11000550 0.1527525
#>  160 0.61176471 0.6666667
#>  161 0.55000000 0.5700000
#>  162 0.54676259 0.6666667
#>  163 0.10000000 0.1400000
#>  164 0.12004802 0.5025126
#>  165 0.10000000 0.1400000
#>  166 0.75000000 1.0000000
#>  167 0.88174590 0.9987441
#>  168 1.00000000 1.0000000
#>  169 0.40500000 0.5000000
#>  170 0.71500000 0.7650000
#>  171 0.91000000 1.0000000
#>  172 0.95000000 1.0000000
#>  173 0.78571429 1.0000000
#>  174 0.45038310 0.5446004
#>  175 0.74716981 0.7896996
#>  176 0.71500000 0.7650000
#>  177 0.90196078 1.0000000
#>  178 0.43000000 0.5300000
#>  179 0.53763441 0.5988024
#>  180 0.43000000 0.5300000
#>  181 0.75000000 1.0000000
#>  182 0.50651386 0.9992257
#>  183 1.00000000 1.0000000
#>  184 0.41000000 0.5000000
#>  185 0.71500000 0.7600000
#>  186 0.96000000 1.0000000
#>  187 0.88000000 1.0000000
#>  188 0.91489362 1.0000000
#>  189 0.46915820 0.5405484
#>  190 0.71861472 0.7389163
#>  191 0.71500000 0.7600000
#>  192 0.76119403 1.0000000
#>  193 0.43000000 0.5200000
#>  194 0.53475936 0.6027728
#>  195 0.43000000 0.5200000
#>  196 0.75000000 1.0000000
#>  197 2.97201532 4.8355899
#>  198 1.00000000 1.0000000
#>  199 0.37500000 0.5000000
#>  200 0.86500000 0.9500000
#>  201 1.00000000 1.0000000
#>  202 1.00000000 1.0000000
#>  203 1.00000000 1.0000000
#>  204 0.75837159 0.9007209
#>  205 0.86666667 0.9489796
#>  206 0.86500000 0.9500000
#>  207 1.00000000 1.0000000
#>  208 0.73000000 0.9000000
#>  209 0.78125000 0.9014423
#>  210 0.73000000 0.9000000
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
#>  227 0.65239610 2.4673076
#>  228 1.00000000 1.0000000
#>  229 0.51500000 0.5250000
#>  230 0.51000000 0.5250000
#>  231 0.76000000 1.0000000
#>  232 0.73000000 1.0000000
#>  233 0.51851852 1.0000000
#>  234 0.02155255 0.1601282
#>  235 0.58775510 0.6666667
#>  236 0.51000000 0.5250000
#>  237 0.50609756 0.5166667
#>  238 0.02000000 0.0500000
#>  239 0.02660282 0.5128205
#>  240 0.02000000 0.0500000
#>  241 0.75000000 1.0000000
#>  242 0.88883935 0.9987629
#>  243 1.00000000 1.0000000
#>  244 0.42000000 0.5000000
#>  245 0.70000000 0.7500000
#>  246 0.88000000 1.0000000
#>  247 0.91000000 1.0000000
#>  248 0.76344086 1.0000000
#>  249 0.42032279 0.5001000
#>  250 0.72800000 0.7592593
#>  251 0.70000000 0.7500000
#>  252 0.83018868 1.0000000
#>  253 0.40000000 0.5000000
#>  254 0.48076923 0.5649718
#>  255 0.40000000 0.5000000
#>  256 0.75000000 1.0000000
#>  257 0.54580444 0.9880928
#>  258 1.00000000 1.0000000
#>  259 0.39500000 0.5050000
#>  260 0.72500000 0.7600000
#>  261 0.96000000 1.0000000
#>  262 0.92000000 1.0000000
#>  263 0.90909091 1.0000000
#>  264 0.46083024 0.5385205
#>  265 0.73076923 0.7522936
#>  266 0.72500000 0.7600000
#>  267 0.80000000 0.8800000
#>  268 0.45000000 0.5200000
#>  269 0.53475936 0.5847953
#>  270 0.45000000 0.5200000
#>  271 0.75000000 1.0000000
#>  272 2.61587430 6.1800547
#>  273 1.00000000 1.0000000
#>  274 0.37500000 0.5000000
#>  275 0.86500000 0.9200000
#>  276 1.00000000 1.0000000
#>  277 1.00000000 1.0000000
#>  278 1.00000000 1.0000000
#>  279 0.76021848 0.8442318
#>  280 0.86842105 0.9183673
#>  281 0.86500000 0.9200000
#>  282 1.00000000 1.0000000
#>  283 0.73000000 0.8400000
#>  284 0.78522920 0.8491049
#>  285 0.73000000 0.8400000
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

## Generate an mmpoints object that contains basic evaluation measures
cvpoints <- evalmod(cvdat, mode = "basic")
cvpoints
#> 
#>     === Basic performance evaluation measures ===
#> 
#>      ## Performance measures (Meas.)
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
#>      Model ID  Meas.        Min.     1st Qu.       Median         Mean
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
#>   0.031   0.005   0.036 
system.time(res2 <- func_evalmod_aucroc(samp1))
#>    user  system elapsed 
#>   0.022   0.000   0.014 

# AUCs
res1
#>   modnames dsids curvetypes      aucs
#> 1       m1     1        ROC 0.4997636
#> 2       m1     1        PRC 0.4989021
res2
#>   modnames dsids      aucs     ustats
#> 1       m1     1 0.4997636 1249409114


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
