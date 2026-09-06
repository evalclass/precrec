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
  `mi`, `chisq`, `cost` and `sar`. They are the measures `ROCR` provides
  that `precrec` did not, and each of them also answers to the
  identifier `ROCR` uses for it - `fall`, `miss`, `pcfall`, `pcmiss`,
  `rpp`, `rnp` and `mutual_information` - and to its standard
  abbreviation where it has one.

  `roc_dist` and `sedi` can be added on the same footing. `roc_dist` is
  the distance from `(1 - specificity, sensitivity)` to the perfect
  corner of ROC space, and is the one measure here that is better when
  it is smaller. `sedi` is the symmetric extremal dependence index, a
  skill score built to stay informative when the positive class is rare.

  `jaccard`, `positive_likelihood_ratio` and `negative_likelihood_ratio`
  come from `scikit-learn`. `jaccard` is the Jaccard index, also called
  the critical success index: `TP / (TP + FP + FN)`, the confusion
  matrix with its true negative corner left out, which is the same
  omission `precision` and `sensitivity` make. The two likelihood ratios
  are `sensitivity / fpr` and `fnr / specificity`; `odds`, the
  diagnostic odds ratio, is their quotient.

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
#>    1     random          1        ROC 0.4827000
#>    2     random          1        PRC 0.5045877
#>    3    poor_er          1        ROC 0.8688000
#>    4    poor_er          1        PRC 0.8420661
#>    5    good_er          1        ROC 0.7817000
#>    6    good_er          1        PRC 0.8076452
#>    7      excel          1        ROC 0.9801000
#>    8      excel          1        PRC 0.9800466
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
#>        Model ID  Meas.          Min.     1st Qu.     Median        Mean
#>    1  random  1   rank  0.0000000000  0.25000000  0.5000000  0.50000000
#>    2  random  1  score -2.8200342587 -0.55073147  0.1039170  0.03265130
#>    3  random  1  label -1.0000000000 -1.00000000  0.0000000  0.00000000
#>    4  random  1    err  0.4800000000  0.49500000  0.5050000  0.50860697
#>    5  random  1    acc  0.4550000000  0.48000000  0.4950000  0.49139303
#>    6  random  1     sp  0.0000000000  0.23000000  0.4900000  0.49139303
#>    7  random  1     sn  0.0000000000  0.25000000  0.4900000  0.49139303
#>    8  random  1   prec  0.3333333333  0.48502994  0.4921875  0.50375604
#>    9  random  1    mcc -0.1613587437 -0.05600023 -0.0200040 -0.02613107
#>   10  random  1 fscore  0.0000000000  0.32679739  0.4950495  0.44429904
#>   11  random  1   bacc  0.4550000000  0.48000000  0.4950000  0.49139303
#>   12  random  1    npv  0.0000000000  0.45283019  0.4936709  0.45077962
#>   13  random  1   infm -0.0900000000 -0.04000000 -0.0100000 -0.01721393
#>   14  random  1    mkd -0.5102040816 -0.06953136 -0.0200080 -0.04546433
#>   15  random  1  kappa -0.0900000000 -0.04000000 -0.0100000 -0.01721393
#>   16 poor_er  1   rank  0.0000000000  0.25000000  0.5000000  0.50000000
#>   17 poor_er  1  score  0.0299690408  0.38545888  0.7130317  0.63590847
#>   18 poor_er  1  label -1.0000000000 -1.00000000  0.0000000  0.00000000
#>   19 poor_er  1    err  0.1850000000  0.24500000  0.3000000  0.31651741
#>   20 poor_er  1    acc  0.5000000000  0.61500000  0.7000000  0.68348259
#>   21 poor_er  1     sp  0.0000000000  0.48000000  0.7800000  0.68348259
#>   22 poor_er  1     sn  0.0000000000  0.45000000  0.7800000  0.68348259
#>   23 poor_er  1   prec  0.5000000000  0.65333333  0.7757009  0.75822006
#>   24 poor_er  1    mcc  0.0708881205  0.33843904  0.4669351  0.43286533
#>   25 poor_er  1 fscore  0.0000000000  0.59602649  0.7168459  0.63996505
#>   26 poor_er  1   bacc  0.5000000000  0.61500000  0.7000000  0.68348259
#>   27 poor_er  1    npv  0.5000000000  0.63013699  0.7777778  0.78209272
#>   28 poor_er  1   infm  0.0000000000  0.23000000  0.4000000  0.36696517
#>   29 poor_er  1    mkd  0.3076923077  0.50179211  0.5254309  0.54031278
#>   30 poor_er  1  kappa  0.0000000000  0.23000000  0.4000000  0.36696517
#>   31 good_er  1   rank  0.0000000000  0.25000000  0.5000000  0.50000000
#>   32 good_er  1  score  0.0003745525  0.13064732  0.2509382  0.34603080
#>   33 good_er  1  label -1.0000000000 -1.00000000  0.0000000  0.00000000
#>   34 good_er  1    err  0.2900000000  0.32000000  0.3350000  0.35985075
#>   35 good_er  1    acc  0.5000000000  0.60500000  0.6650000  0.64014925
#>   36 good_er  1     sp  0.0000000000  0.40000000  0.6700000  0.64014925
#>   37 good_er  1     sn  0.0000000000  0.44000000  0.6700000  0.64014925
#>   38 good_er  1   prec  0.5000000000  0.60000000  0.6700000  0.72985498
#>   39 good_er  1    mcc  0.0708881205  0.31294750  0.3512086  0.34091221
#>   40 good_er  1 fscore  0.0000000000  0.58503401  0.6688963  0.60174822
#>   41 good_er  1   bacc  0.5000000000  0.60500000  0.6650000  0.64014925
#>   42 good_er  1    npv  0.5000000000  0.62658228  0.6732673  0.71556904
#>   43 good_er  1   infm  0.0000000000  0.21000000  0.3300000  0.28029851
#>   44 good_er  1    mkd  0.3171355499  0.38919684  0.4448535  0.44542402
#>   45 good_er  1  kappa  0.0000000000  0.21000000  0.3300000  0.28029851
#>   46   excel  1   rank  0.0000000000  0.25000000  0.5000000  0.50000000
#>   47   excel  1  score -3.3551544513  0.35188881  1.4806172  1.50135386
#>   48   excel  1  label -1.0000000000 -1.00000000  0.0000000  0.00000000
#>   49   excel  1    err  0.0650000000  0.14000000  0.2500000  0.26114428
#>   50   excel  1    acc  0.5000000000  0.62500000  0.7500000  0.73885572
#>   51   excel  1     sp  0.0000000000  0.50000000  0.9200000  0.73885572
#>   52   excel  1     sn  0.0000000000  0.50000000  0.9200000  0.73885572
#>   53   excel  1   prec  0.5000000000  0.66666667  0.9207921  0.83564365
#>   54   excel  1    mcc  0.0708881205  0.38226007  0.5773503  0.55332057
#>   55   excel  1 fscore  0.0000000000  0.66666667  0.7633588  0.70031441
#>   56   excel  1   bacc  0.5000000000  0.62500000  0.7500000  0.73885572
#>   57   excel  1    npv  0.5000000000  0.66666667  0.9215686  0.83633658
#>   58   excel  1   infm  0.0000000000  0.25000000  0.5000000  0.47771144
#>   59   excel  1    mkd  0.5000000000  0.57142857  0.6666667  0.67198024
#>   60   excel  1  kappa  0.0000000000  0.25000000  0.5000000  0.47771144
#>   61    perf  1   rank  0.0000000000  0.25000000  0.5000000  0.50000000
#>   62    perf  1  score  0.0000000000  0.00000000  0.5000000  0.50000000
#>   63    perf  1  label -1.0000000000 -1.00000000  0.0000000  0.00000000
#>   64    perf  1    err  0.0000000000  0.12500000  0.2500000  0.25124378
#>   65    perf  1    acc  0.5000000000  0.62500000  0.7500000  0.74875622
#>   66    perf  1     sp  0.0000000000  0.50000000  1.0000000  0.74875622
#>   67    perf  1     sn  0.0000000000  0.50000000  1.0000000  0.74875622
#>   68    perf  1   prec  0.5000000000  0.66666667  1.0000000  0.84609623
#>   69    perf  1    mcc  0.0708881205  0.38226007  0.5773503  0.57352524
#>   70    perf  1 fscore  0.0000000000  0.66666667  0.7633588  0.71042736
#>   71    perf  1   bacc  0.5000000000  0.62500000  0.7500000  0.74875622
#>   72    perf  1    npv  0.5000000000  0.66666667  1.0000000  0.84609623
#>   73    perf  1   infm  0.0000000000  0.25000000  0.5000000  0.49751244
#>   74    perf  1    mkd  0.5000000000  0.57142857  0.6666667  0.69219247
#>   75    perf  1  kappa  0.0000000000  0.25000000  0.5000000  0.49751244
#>         3rd Qu.       Max.
#>    1 0.75000000 1.00000000
#>    2 0.68441697 2.32649442
#>    3 1.00000000 1.00000000
#>    4 0.52000000 0.54500000
#>    5 0.50500000 0.52000000
#>    6 0.75000000 1.00000000
#>    7 0.73000000 1.00000000
#>    8 0.50666667 1.00000000
#>    9 0.01025675 0.07235746
#>   10 0.58064516 0.66666667
#>   11 0.50500000 0.52000000
#>   12 0.50306748 0.51807229
#>   13 0.01000000 0.04000000
#>   14 0.01066667 0.50251256
#>   15 0.01000000 0.04000000
#>   16 0.75000000 1.00000000
#>   17 0.88049755 0.99806839
#>   18 1.00000000 1.00000000
#>   19 0.38500000 0.50000000
#>   20 0.75500000 0.81500000
#>   21 0.95000000 1.00000000
#>   22 0.98000000 1.00000000
#>   23 0.86111111 1.00000000
#>   24 0.53963524 0.63539194
#>   25 0.78087649 0.82727273
#>   26 0.75500000 0.81500000
#>   27 0.96000000 1.00000000
#>   28 0.51000000 0.63000000
#>   29 0.60148602 0.64583333
#>   30 0.51000000 0.63000000
#>   31 0.75000000 1.00000000
#>   32 0.51202716 0.99942064
#>   33 1.00000000 1.00000000
#>   34 0.39500000 0.50000000
#>   35 0.68000000 0.71000000
#>   36 0.94000000 1.00000000
#>   37 0.90000000 1.00000000
#>   38 0.88000000 1.00000000
#>   39 0.38868988 0.49102862
#>   40 0.70802920 0.72950820
#>   41 0.68000000 0.71000000
#>   42 0.80357143 1.00000000
#>   43 0.36000000 0.42000000
#>   44 0.50545358 0.60277275
#>   45 0.36000000 0.42000000
#>   46 0.75000000 1.00000000
#>   47 2.74061967 5.68705714
#>   48 1.00000000 1.00000000
#>   49 0.37500000 0.50000000
#>   50 0.86000000 0.93500000
#>   51 1.00000000 1.00000000
#>   52 1.00000000 1.00000000
#>   53 1.00000000 1.00000000
#>   54 0.74782186 0.87108954
#>   55 0.86666667 0.93333333
#>   56 0.86000000 0.93500000
#>   57 1.00000000 1.00000000
#>   58 0.72000000 0.87000000
#>   59 0.77220077 0.87218045
#>   60 0.72000000 0.87000000
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
#>    1    good_er          1        ROC 0.8309000
#>    2    good_er          1        PRC 0.8518567
#>    3    good_er          2        ROC 0.8416000
#>    4    good_er          2        PRC 0.8766286
#>    5    good_er          3        ROC 0.7692000
#>    6    good_er          3        PRC 0.8189129
#>    7    good_er          4        ROC 0.7669000
#>    8    good_er          4        PRC 0.8167236
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
#>        Model ID  Meas.          Min.    1st Qu.    Median      Mean   3rd Qu.
#>    1 good_er  1   rank  0.0000000000  0.2500000 0.5000000 0.5000000 0.7500000
#>    2 good_er  1  score  0.0005527905  0.1254571 0.2897201 0.3575120 0.5451061
#>    3 good_er  1  label -1.0000000000 -1.0000000 0.0000000 0.0000000 1.0000000
#>    4 good_er  1    err  0.2200000000  0.2550000 0.3350000 0.3353731 0.4050000
#>    5 good_er  1    acc  0.5000000000  0.5950000 0.6650000 0.6646269 0.7450000
#>    6 good_er  1     sp  0.0000000000  0.4100000 0.7800000 0.6646269 0.9400000
#>    7 good_er  1     sn  0.0000000000  0.4400000 0.7800000 0.6646269 0.9100000
#>    8 good_er  1   prec  0.5000000000  0.6040268 0.7755102 0.7586114 0.8888889
#>    9 good_er  1    mcc  0.0708881205  0.2855330 0.4078502 0.3875567 0.5062977
#>   10 good_er  1 fscore  0.0000000000  0.5866667 0.6988848 0.6276122 0.7435897
#>   11 good_er  1   bacc  0.5000000000  0.5950000 0.6650000 0.6646269 0.7450000
#>   12 good_er  1    npv  0.5000000000  0.6266667 0.7722772 0.7263583 0.8076923
#>   13 good_er  1   infm  0.0000000000  0.1900000 0.3300000 0.3292537 0.4900000
#>   14 good_er  1    mkd  0.2551020408  0.4137931 0.5102041 0.4849697 0.5420954
#>   15 good_er  1  kappa  0.0000000000  0.1900000 0.3300000 0.3292537 0.4900000
#>   16 good_er  2   rank  0.0000000000  0.2500000 0.5000000 0.5000000 0.7500000
#>   17 good_er  2  score  0.0017857724  0.0898539 0.2402709 0.3330578 0.5136449
#>   18 good_er  2  label -1.0000000000 -1.0000000 0.0000000 0.0000000 1.0000000
#>   19 good_er  2    err  0.2100000000  0.2450000 0.3100000 0.3300498 0.4100000
#>   20 good_er  2    acc  0.4950000000  0.5900000 0.6900000 0.6699502 0.7550000
#>   21 good_er  2     sp  0.0000000000  0.4100000 0.7700000 0.6699502 0.9800000
#>   22 good_er  2     sn  0.0000000000  0.4800000 0.7700000 0.6699502 0.9100000
#>   23 good_er  2   prec  0.4974619289  0.6066667 0.7700000 0.7716198 0.9574468
#>   24 good_er  2    mcc -0.0708881205  0.2746758 0.4525696 0.3941844 0.5307005
#>   25 good_er  2 fscore  0.0000000000  0.6357616 0.6992481 0.6356556 0.7575758
#>   26 good_er  2   bacc  0.4950000000  0.5900000 0.6900000 0.6699502 0.7550000
#>   27 good_er  2    npv  0.0000000000  0.6265823 0.7333333 0.6988217 0.7941176
#>   28 good_er  2   infm -0.0100000000  0.1800000 0.3800000 0.3399005 0.5100000
#>   29 good_er  2    mkd -0.5025125628  0.4137931 0.5300530 0.4704415 0.5797986
#>   30 good_er  2  kappa -0.0100000000  0.1800000 0.3800000 0.3399005 0.5100000
#>   31 good_er  3   rank  0.0000000000  0.2500000 0.5000000 0.5000000 0.7500000
#>   32 good_er  3  score  0.0007231320  0.1320840 0.2428511 0.3360475 0.5035083
#>   33 good_er  3  label -1.0000000000 -1.0000000 0.0000000 0.0000000 1.0000000
#>   34 good_er  3    err  0.2550000000  0.3100000 0.3500000 0.3660697 0.4300000
#>   35 good_er  3    acc  0.5000000000  0.5700000 0.6500000 0.6339303 0.6900000
#>   36 good_er  3     sp  0.0000000000  0.3700000 0.6800000 0.6339303 0.9700000
#>   37 good_er  3     sn  0.0000000000  0.4700000 0.6800000 0.6339303 0.8700000
#>   38 good_er  3   prec  0.5000000000  0.5763889 0.6767677 0.7336876 0.9347826
#>   39 good_er  3    mcc  0.0708881205  0.2229814 0.3239199 0.3219825 0.4162597
#>   40 good_er  3 fscore  0.0000000000  0.6266667 0.6767677 0.5998677 0.6829268
#>   41 good_er  3   bacc  0.5000000000  0.5700000 0.6500000 0.6339303 0.6900000
#>   42 good_er  3    npv  0.5000000000  0.6466667 0.6785714 0.6832068 0.7179487
#>   43 good_er  3   infm  0.0000000000  0.1400000 0.3000000 0.2678607 0.3800000
#>   44 good_er  3    mkd  0.2057142857  0.3198604 0.3947768 0.4168944 0.5319149
#>   45 good_er  3  kappa  0.0000000000  0.1400000 0.3000000 0.2678607 0.3800000
#>   46 good_er  4   rank  0.0000000000  0.2500000 0.5000000 0.5000000 0.7500000
#>   47 good_er  4  score  0.0008682671  0.1144044 0.2663099 0.3359253 0.4903323
#>   48 good_er  4  label -1.0000000000 -1.0000000 0.0000000 0.0000000 1.0000000
#>   49 good_er  4    err  0.2800000000  0.3050000 0.3650000 0.3672139 0.4200000
#>   50 good_er  4    acc  0.4950000000  0.5800000 0.6350000 0.6327861 0.6950000
#>   51 good_er  4     sp  0.0000000000  0.3600000 0.6900000 0.6327861 0.9500000
#>   52 good_er  4     sn  0.0000000000  0.4500000 0.6900000 0.6327861 0.8600000
#>   53 good_er  4   prec  0.4974874372  0.5733333 0.6900000 0.7323105 0.9038462
#>   54 good_er  4    mcc -0.0708881205  0.2413256 0.3061862 0.3191171 0.4256480
#>   55 good_er  4 fscore  0.0000000000  0.6081081 0.6781116 0.5983535 0.6867925
#>   56 good_er  4   bacc  0.4950000000  0.5800000 0.6350000 0.6327861 0.6950000
#>   57 good_er  4    npv  0.0000000000  0.6333333 0.6829268 0.6694099 0.7118644
#>   58 good_er  4   infm -0.0100000000  0.1600000 0.2700000 0.2655721 0.3900000
#>   59 good_er  4    mkd -0.5025125628  0.3038194 0.4025765 0.4017204 0.5181347
#>   60 good_er  4  kappa -0.0100000000  0.1600000 0.2700000 0.2655721 0.3900000
#>           Max.
#>    1 1.0000000
#>    2 0.9988285
#>    3 1.0000000
#>    4 0.5000000
#>    5 0.7800000
#>    6 1.0000000
#>    7 1.0000000
#>    8 1.0000000
#>    9 0.5618006
#>   10 0.7843137
#>   11 0.7800000
#>   12 1.0000000
#>   13 0.5600000
#>   14 0.5744485
#>   15 0.5600000
#>   16 1.0000000
#>   17 0.9953815
#>   18 1.0000000
#>   19 0.5050000
#>   20 0.7900000
#>   21 1.0000000
#>   22 1.0000000
#>   23 1.0000000
#>   24 0.5955947
#>   25 0.7826087
#>   26 0.7900000
#>   27 0.8285714
#>   28 0.5800000
#>   29 0.6250000
#>   30 0.5800000
#>   31 1.0000000
#>   32 0.9854039
#>   33 1.0000000
#>   34 0.5000000
#>   35 0.7450000
#>   36 1.0000000
#>   37 1.0000000
#>   38 1.0000000
#>   39 0.5324775
#>   40 0.7099567
#>   41 0.7450000
#>   42 1.0000000
#>   43 0.4900000
#>   44 0.6032602
#>   45 0.4900000
#>   46 1.0000000
#>   47 0.9998552
#>   48 1.0000000
#>   49 0.5050000
#>   50 0.7200000
#>   51 1.0000000
#>   52 1.0000000
#>   53 1.0000000
#>   54 0.4917076
#>   55 0.7005076
#>   56 0.7200000
#>   57 0.9285714
#>   58 0.4400000
#>   59 0.5988024
#>   60 0.4400000
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
#>    1     random          1        ROC 0.5678000
#>    2     random          1        PRC 0.5240844
#>    3    poor_er          1        ROC 0.8299000
#>    4    poor_er          1        PRC 0.8106666
#>    5    good_er          1        ROC 0.8190000
#>    6    good_er          1        PRC 0.8665520
#>    7      excel          1        ROC 0.9913000
#>    8      excel          1        PRC 0.9920591
#>    9       perf          1        ROC 1.0000000
#>   10       perf          1        PRC 1.0000000
#>   11     random          2        ROC 0.4719000
#>   12     random          2        PRC 0.4980368
#>   13    poor_er          2        ROC 0.7879000
#>   14    poor_er          2        PRC 0.7619653
#>   15    good_er          2        ROC 0.8377000
#>   16    good_er          2        PRC 0.8747640
#>   17      excel          2        ROC 0.9858000
#>   18      excel          2        PRC 0.9834684
#>   19       perf          2        ROC 1.0000000
#>   20       perf          2        PRC 1.0000000
#>   21     random          3        ROC 0.5254000
#>   22     random          3        PRC 0.4996127
#>   23    poor_er          3        ROC 0.8214000
#>   24    poor_er          3        PRC 0.7731334
#>   25    good_er          3        ROC 0.8176000
#>   26    good_er          3        PRC 0.8500151
#>   27      excel          3        ROC 0.9850000
#>   28      excel          3        PRC 0.9853962
#>   29       perf          3        ROC 1.0000000
#>   30       perf          3        PRC 1.0000000
#>   31     random          4        ROC 0.5168000
#>   32     random          4        PRC 0.5245181
#>   33    poor_er          4        ROC 0.7782000
#>   34    poor_er          4        PRC 0.7291026
#>   35    good_er          4        ROC 0.7819000
#>   36    good_er          4        PRC 0.8283109
#>   37      excel          4        ROC 0.9806000
#>   38      excel          4        PRC 0.9807111
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
#>    2  random  1  score -3.1654053854 -0.58975462  0.008348982  0.0078300686
#>    3  random  1  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>    4  random  1    err  0.4050000000  0.45000000  0.465000000  0.4662686567
#>    5  random  1    acc  0.4850000000  0.52000000  0.535000000  0.5337313433
#>    6  random  1     sp  0.0000000000  0.33000000  0.530000000  0.5337313433
#>    7  random  1     sn  0.0000000000  0.28000000  0.530000000  0.5337313433
#>    8  random  1   prec  0.0000000000  0.51648352  0.535031847  0.5171274437
#>    9  random  1    mcc -0.1005037815  0.04841590  0.079305159  0.0795460615
#>   10  random  1 fscore  0.0000000000  0.37333333  0.530000000  0.4804759895
#>   11  random  1   bacc  0.4850000000  0.52000000  0.535000000  0.5337313433
#>   12  random  1    npv  0.0000000000  0.51655629  0.530000000  0.5697337529
#>   13  random  1   infm -0.0300000000  0.04000000  0.070000000  0.0674626866
#>   14  random  1    mkd -0.5050505051  0.05000500  0.084697911  0.0868611966
#>   15  random  1  kappa -0.0300000000  0.04000000  0.070000000  0.0674626866
#>   16 poor_er  1   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>   17 poor_er  1  score  0.0019574498  0.49987752  0.760255502  0.6716741309
#>   18 poor_er  1  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>   19 poor_er  1    err  0.2150000000  0.27000000  0.330000000  0.3358706468
#>   20 poor_er  1    acc  0.5000000000  0.61500000  0.670000000  0.6641293532
#>   21 poor_er  1     sp  0.0000000000  0.48000000  0.730000000  0.6641293532
#>   22 poor_er  1     sn  0.0000000000  0.41000000  0.730000000  0.6641293532
#>   23 poor_er  1   prec  0.5000000000  0.65100671  0.729166667  0.7375302616
#>   24 poor_er  1    mcc  0.0708881205  0.32905341  0.387835876  0.3930361980
#>   25 poor_er  1 fscore  0.0000000000  0.54666667  0.689655172  0.6189511566
#>   26 poor_er  1   bacc  0.5000000000  0.61500000  0.670000000  0.6641293532
#>   27 poor_er  1    npv  0.5000000000  0.60666667  0.730000000  0.7642722465
#>   28 poor_er  1   infm  0.0000000000  0.23000000  0.340000000  0.3282587065
#>   29 poor_er  1    mkd  0.3392130258  0.43706294  0.507305195  0.5018025081
#>   30 poor_er  1  kappa  0.0000000000  0.23000000  0.340000000  0.3282587065
#>   31 good_er  1   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>   32 good_er  1  score  0.0042922271  0.10597487  0.249923627  0.3387038313
#>   33 good_er  1  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>   34 good_er  1    err  0.2200000000  0.25500000  0.320000000  0.3412935323
#>   35 good_er  1    acc  0.4950000000  0.58000000  0.680000000  0.6587064677
#>   36 good_er  1     sp  0.0000000000  0.38000000  0.750000000  0.6587064677
#>   37 good_er  1     sn  0.0000000000  0.50000000  0.750000000  0.6587064677
#>   38 good_er  1   prec  0.4973821990  0.58666667  0.752475248  0.7635817871
#>   39 good_er  1    mcc -0.0241191535  0.23754989  0.411813845  0.3697101761
#>   40 good_er  1 fscore  0.0000000000  0.65517241  0.687022901  0.6260901970
#>   41 good_er  1   bacc  0.4950000000  0.58000000  0.680000000  0.6587064677
#>   42 good_er  1    npv  0.4444444444  0.62893082  0.709677419  0.6920062719
#>   43 good_er  1   infm -0.0100000000  0.16000000  0.360000000  0.3174129353
#>   44 good_er  1    mkd -0.0581733566  0.38871473  0.505050505  0.4555880590
#>   45 good_er  1  kappa -0.0100000000  0.16000000  0.360000000  0.3174129353
#>   46   excel  1   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>   47   excel  1  score -2.9825331637 -0.11921578  1.457976635  1.4203342391
#>   48   excel  1  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>   49   excel  1    err  0.0300000000  0.13000000  0.250000000  0.2555721393
#>   50   excel  1    acc  0.5000000000  0.62500000  0.750000000  0.7444278607
#>   51   excel  1     sp  0.0000000000  0.50000000  0.970000000  0.7444278607
#>   52   excel  1     sn  0.0000000000  0.50000000  0.970000000  0.7444278607
#>   53   excel  1   prec  0.5000000000  0.66666667  0.969387755  0.8418558051
#>   54   excel  1    mcc  0.0708881205  0.38226007  0.577350269  0.5646430017
#>   55   excel  1 fscore  0.0000000000  0.66666667  0.763358779  0.7061687974
#>   56   excel  1   bacc  0.5000000000  0.62500000  0.750000000  0.7444278607
#>   57   excel  1    npv  0.5000000000  0.66666667  0.969696970  0.8413995748
#>   58   excel  1   infm  0.0000000000  0.25000000  0.500000000  0.4888557214
#>   59   excel  1    mkd  0.5000000000  0.57142857  0.666666667  0.6832553799
#>   60   excel  1  kappa  0.0000000000  0.25000000  0.500000000  0.4888557214
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
#>   77  random  2  score -3.2606467067 -0.54716372  0.096607745  0.0970309833
#>   78  random  2  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>   79  random  2    err  0.4750000000  0.49500000  0.510000000  0.5139800995
#>   80  random  2    acc  0.4300000000  0.47000000  0.490000000  0.4860199005
#>   81  random  2     sp  0.0000000000  0.26000000  0.470000000  0.4860199005
#>   82  random  2     sn  0.0000000000  0.21000000  0.470000000  0.4860199005
#>   83  random  2   prec  0.4000000000  0.45744681  0.486486486  0.4929426286
#>   84  random  2    mcc -0.1418271572 -0.07012990 -0.030183164 -0.0319662382
#>   85  random  2 fscore  0.0000000000  0.28378378  0.467661692  0.4345840498
#>   86  random  2   bacc  0.4300000000  0.47000000  0.490000000  0.4860199005
#>   87  random  2    npv  0.2500000000  0.46400000  0.491525424  0.4793282172
#>   88  random  2   infm -0.1400000000 -0.06000000 -0.020000000 -0.0279601990
#>   89  random  2    mkd -0.2551020408 -0.08469791 -0.041528239 -0.0277291542
#>   90  random  2  kappa -0.1400000000 -0.06000000 -0.020000000 -0.0279601990
#>   91 poor_er  2   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>   92 poor_er  2  score  0.0192075598  0.44510428  0.749008547  0.6567613187
#>   93 poor_er  2  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>   94 poor_er  2    err  0.2800000000  0.30500000  0.345000000  0.3567661692
#>   95 poor_er  2    acc  0.5000000000  0.60500000  0.655000000  0.6432338308
#>   96 poor_er  2     sp  0.0000000000  0.44000000  0.700000000  0.6432338308
#>   97 poor_er  2     sn  0.0000000000  0.40000000  0.700000000  0.6432338308
#>   98 poor_er  2   prec  0.5000000000  0.63157895  0.695238095  0.7068611082
#>   99 poor_er  2    mcc  0.0708881205  0.31086702  0.355617784  0.3456871716
#>  100 poor_er  2 fscore  0.0000000000  0.53691275  0.682593857  0.5977382819
#>  101 poor_er  2   bacc  0.5000000000  0.60500000  0.655000000  0.6432338308
#>  102 poor_er  2    npv  0.5000000000  0.60135135  0.702970297  0.7392395415
#>  103 poor_er  2   infm  0.0000000000  0.21000000  0.310000000  0.2864676617
#>  104 poor_er  2    mkd  0.3076923077  0.38888889  0.429565990  0.4461006497
#>  105 poor_er  2  kappa  0.0000000000  0.21000000  0.310000000  0.2864676617
#>  106 good_er  2   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>  107 good_er  2  score  0.0008981645  0.11986767  0.282729457  0.3354048473
#>  108 good_er  2  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>  109 good_er  2    err  0.1950000000  0.25000000  0.315000000  0.3319900498
#>  110 good_er  2    acc  0.4950000000  0.58500000  0.685000000  0.6680099502
#>  111 good_er  2     sp  0.0000000000  0.40000000  0.760000000  0.6680099502
#>  112 good_er  2     sn  0.0000000000  0.48000000  0.760000000  0.6680099502
#>  113 good_er  2   prec  0.4974874372  0.60264901  0.760000000  0.7701271339
#>  114 good_er  2    mcc -0.0708881205  0.27238140  0.428410224  0.3921974502
#>  115 good_er  2 fscore  0.0000000000  0.64000000  0.699386503  0.6339542820
#>  116 good_er  2   bacc  0.4950000000  0.58500000  0.685000000  0.6680099502
#>  117 good_er  2    npv  0.0000000000  0.64052288  0.747572816  0.7041701653
#>  118 good_er  2   infm -0.0100000000  0.17000000  0.370000000  0.3360199005
#>  119 good_er  2    mkd -0.5025125628  0.40794841  0.518134715  0.4742972992
#>  120 good_er  2  kappa -0.0100000000  0.17000000  0.370000000  0.3360199005
#>  121   excel  2   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>  122   excel  2  score -3.2078866813 -0.15240843  1.699977893  1.4486631563
#>  123   excel  2  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>  124   excel  2    err  0.0550000000  0.13000000  0.255000000  0.2583084577
#>  125   excel  2    acc  0.5000000000  0.62500000  0.745000000  0.7416915423
#>  126   excel  2     sp  0.0000000000  0.50000000  0.930000000  0.7416915423
#>  127   excel  2     sn  0.0000000000  0.49000000  0.930000000  0.7416915423
#>  128   excel  2   prec  0.5000000000  0.66666667  0.927835052  0.8376364559
#>  129   excel  2    mcc  0.0708881205  0.38226007  0.569651921  0.5588659736
#>  130   excel  2 fscore  0.0000000000  0.65333333  0.760456274  0.7028535462
#>  131   excel  2   bacc  0.5000000000  0.62500000  0.745000000  0.7416915423
#>  132   excel  2    npv  0.5000000000  0.66000000  0.928571429  0.8396112536
#>  133   excel  2   infm  0.0000000000  0.25000000  0.490000000  0.4833830846
#>  134   excel  2    mkd  0.5000000000  0.57142857  0.653594771  0.6772477095
#>  135   excel  2  kappa  0.0000000000  0.25000000  0.490000000  0.4833830846
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
#>  152  random  3  score -2.6846332535 -0.74103171 -0.019840582  0.0003724096
#>  153  random  3  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>  154  random  3    err  0.4350000000  0.47500000  0.490000000  0.4873631841
#>  155  random  3    acc  0.4700000000  0.49500000  0.510000000  0.5126368159
#>  156  random  3     sp  0.0000000000  0.31000000  0.490000000  0.5126368159
#>  157  random  3     sn  0.0000000000  0.26000000  0.490000000  0.5126368159
#>  158  random  3   prec  0.0000000000  0.49473684  0.508379888  0.4913873983
#>  159  random  3    mcc -0.1428571429 -0.01025178  0.024119154  0.0272086265
#>  160  random  3 fscore  0.0000000000  0.34666667  0.492462312  0.4595020071
#>  161  random  3   bacc  0.4700000000  0.49500000  0.510000000  0.5126368159
#>  162  random  3    npv  0.0000000000  0.49685535  0.510344828  0.5246322698
#>  163  random  3   infm -0.0600000000 -0.01000000  0.020000000  0.0252736318
#>  164  random  3    mkd -0.5102040816 -0.01078632  0.032144005  0.0160196682
#>  165  random  3  kappa -0.0600000000 -0.01000000  0.020000000  0.0252736318
#>  166 poor_er  3   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>  167 poor_er  3  score  0.0016994083  0.44550795  0.722484558  0.6463469552
#>  168 poor_er  3  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>  169 poor_er  3    err  0.2250000000  0.26500000  0.335000000  0.3400995025
#>  170 poor_er  3    acc  0.5000000000  0.59500000  0.665000000  0.6599004975
#>  171 poor_er  3     sp  0.0000000000  0.48000000  0.730000000  0.6599004975
#>  172 poor_er  3     sn  0.0000000000  0.38000000  0.730000000  0.6599004975
#>  173 poor_er  3   prec  0.5000000000  0.65333333  0.732142857  0.7157424148
#>  174 poor_er  3    mcc  0.0708881205  0.28381430  0.398348238  0.3779613795
#>  175 poor_er  3 fscore  0.0000000000  0.50666667  0.701754386  0.6117613559
#>  176 poor_er  3   bacc  0.5000000000  0.59500000  0.665000000  0.6599004975
#>  177 poor_er  3    npv  0.5000000000  0.58666667  0.733333333  0.7605262021
#>  178 poor_er  3   infm  0.0000000000  0.19000000  0.330000000  0.3198009950
#>  179 poor_er  3    mkd  0.2038043478  0.38853813  0.494001411  0.4762686170
#>  180 poor_er  3  kappa  0.0000000000  0.19000000  0.330000000  0.3198009950
#>  181 good_er  3   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>  182 good_er  3  score  0.0014233844  0.10737586  0.281686501  0.3554744610
#>  183 good_er  3  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>  184 good_er  3    err  0.2450000000  0.26500000  0.325000000  0.3419900498
#>  185 good_er  3    acc  0.5000000000  0.58500000  0.675000000  0.6580099502
#>  186 good_er  3     sp  0.0000000000  0.39000000  0.750000000  0.6580099502
#>  187 good_er  3     sn  0.0000000000  0.46000000  0.750000000  0.6580099502
#>  188 good_er  3   prec  0.5000000000  0.59333333  0.750000000  0.7558439068
#>  189 good_er  3    mcc  0.0708881205  0.25972454  0.417586537  0.3745910390
#>  190 good_er  3 fscore  0.0000000000  0.61333333  0.691176471  0.6223121173
#>  191 good_er  3   bacc  0.5000000000  0.58500000  0.675000000  0.6580099502
#>  192 good_er  3    npv  0.5000000000  0.64000000  0.745098039  0.7178513891
#>  193 good_er  3   infm  0.0000000000  0.17000000  0.350000000  0.3160199005
#>  194 good_er  3    mkd  0.2834868887  0.41379310  0.492412823  0.4736952959
#>  195 good_er  3  kappa  0.0000000000  0.17000000  0.350000000  0.3160199005
#>  196   excel  3   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>  197   excel  3  score -2.1323541001 -0.14455152  1.716453572  1.4991040901
#>  198   excel  3  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>  199   excel  3    err  0.0700000000  0.13000000  0.250000000  0.2587064677
#>  200   excel  3    acc  0.5000000000  0.62500000  0.750000000  0.7412935323
#>  201   excel  3     sp  0.0000000000  0.50000000  0.920000000  0.7412935323
#>  202   excel  3     sn  0.0000000000  0.50000000  0.920000000  0.7412935323
#>  203   excel  3   prec  0.5000000000  0.66666667  0.920000000  0.8384627075
#>  204   excel  3    mcc  0.0708881205  0.38226007  0.577350269  0.5583851630
#>  205   excel  3 fscore  0.0000000000  0.66666667  0.763358779  0.7028977623
#>  206   excel  3   bacc  0.5000000000  0.62500000  0.750000000  0.7412935323
#>  207   excel  3    npv  0.5000000000  0.66666667  0.918367347  0.8386753149
#>  208   excel  3   infm  0.0000000000  0.25000000  0.500000000  0.4825870647
#>  209   excel  3    mkd  0.5000000000  0.57142857  0.666666667  0.6771380224
#>  210   excel  3  kappa  0.0000000000  0.25000000  0.500000000  0.4825870647
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
#>  227  random  4  score -2.5933324230 -0.62212527  0.150844909  0.0836118865
#>  228  random  4  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>  229  random  4    err  0.4500000000  0.47000000  0.490000000  0.4916417910
#>  230  random  4    acc  0.4550000000  0.49000000  0.510000000  0.5083582090
#>  231  random  4     sp  0.0000000000  0.24000000  0.510000000  0.5083582090
#>  232  random  4     sn  0.0000000000  0.27000000  0.510000000  0.5083582090
#>  233  random  4   prec  0.2857142857  0.49333333  0.508474576  0.5218835949
#>  234  random  4    mcc -0.1973855085 -0.02674812  0.020016019  0.0113873403
#>  235  random  4 fscore  0.0000000000  0.36734694  0.517412935  0.4627223284
#>  236  random  4   bacc  0.4550000000  0.49000000  0.510000000  0.5083582090
#>  237  random  4    npv  0.0000000000  0.48648649  0.508771930  0.4723683559
#>  238  random  4   infm -0.0900000000 -0.02000000  0.020000000  0.0167164179
#>  239  random  4    mkd -0.5050505051 -0.03921569  0.020072260 -0.0057480493
#>  240  random  4  kappa -0.0900000000 -0.02000000  0.020000000  0.0167164179
#>  241 poor_er  4   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>  242 poor_er  4  score  0.0114088112  0.48913826  0.723865944  0.6557810605
#>  243 poor_er  4  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>  244 poor_er  4    err  0.2500000000  0.31000000  0.345000000  0.3615920398
#>  245 poor_er  4    acc  0.5000000000  0.58500000  0.655000000  0.6384079602
#>  246 poor_er  4     sp  0.0000000000  0.45000000  0.690000000  0.6384079602
#>  247 poor_er  4     sn  0.0000000000  0.37000000  0.690000000  0.6384079602
#>  248 poor_er  4   prec  0.5000000000  0.63576159  0.687022901  0.6867660399
#>  249 poor_er  4    mcc  0.0320256308  0.24936953  0.360476772  0.3308233590
#>  250 poor_er  4 fscore  0.0000000000  0.49664430  0.680203046  0.5902696242
#>  251 poor_er  4   bacc  0.5000000000  0.58500000  0.655000000  0.6384079602
#>  252 poor_er  4    npv  0.5000000000  0.58108108  0.693069307  0.7350164407
#>  253 poor_er  4   infm  0.0000000000  0.17000000  0.310000000  0.2768159204
#>  254 poor_er  4    mkd  0.1025641026  0.35063114  0.385551948  0.4217824806
#>  255 poor_er  4  kappa  0.0000000000  0.17000000  0.310000000  0.2768159204
#>  256 good_er  4   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>  257 good_er  4  score  0.0007582405  0.10587415  0.297907895  0.3570225576
#>  258 good_er  4  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>  259 good_er  4    err  0.2400000000  0.29500000  0.365000000  0.3597512438
#>  260 good_er  4    acc  0.5000000000  0.58000000  0.635000000  0.6402487562
#>  261 good_er  4     sp  0.0000000000  0.37000000  0.690000000  0.6402487562
#>  262 good_er  4     sn  0.0000000000  0.46000000  0.690000000  0.6402487562
#>  263 good_er  4   prec  0.5000000000  0.57664234  0.696078431  0.7400124637
#>  264 good_er  4    mcc  0.0708881205  0.24124895  0.309897047  0.3382940085
#>  265 good_er  4 fscore  0.0000000000  0.61744966  0.683453237  0.6061241082
#>  266 good_er  4   bacc  0.5000000000  0.58000000  0.635000000  0.6402487562
#>  267 good_er  4    npv  0.5000000000  0.64238411  0.693069307  0.6992808186
#>  268 good_er  4   infm  0.0000000000  0.16000000  0.270000000  0.2804975124
#>  269 good_er  4    mkd  0.2297794118  0.31933186  0.487602448  0.4392932822
#>  270 good_er  4  kappa  0.0000000000  0.16000000  0.270000000  0.2804975124
#>  271   excel  4   rank  0.0000000000  0.25000000  0.500000000  0.5000000000
#>  272   excel  4  score -2.3699971030  0.16953255  1.524408552  1.5001273007
#>  273   excel  4  label -1.0000000000 -1.00000000  0.000000000  0.0000000000
#>  274   excel  4    err  0.0600000000  0.14000000  0.250000000  0.2608955224
#>  275   excel  4    acc  0.5000000000  0.62500000  0.750000000  0.7391044776
#>  276   excel  4     sp  0.0000000000  0.50000000  0.920000000  0.7391044776
#>  277   excel  4     sn  0.0000000000  0.50000000  0.920000000  0.7391044776
#>  278   excel  4   prec  0.5000000000  0.66666667  0.923076923  0.8359827238
#>  279   excel  4    mcc  0.0708881205  0.38226007  0.577350269  0.5538418044
#>  280   excel  4 fscore  0.0000000000  0.66666667  0.763358779  0.7005990135
#>  281   excel  4   bacc  0.5000000000  0.62500000  0.750000000  0.7391044776
#>  282   excel  4    npv  0.5000000000  0.66666667  0.920792079  0.8365334898
#>  283   excel  4   infm  0.0000000000  0.25000000  0.500000000  0.4782089552
#>  284   excel  4    mkd  0.5000000000  0.57142857  0.666666667  0.6725162135
#>  285   excel  4  kappa  0.0000000000  0.25000000  0.500000000  0.4782089552
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
#>    2 0.64373225 3.1229390
#>    3 1.00000000 1.0000000
#>    4 0.48000000 0.5150000
#>    5 0.55000000 0.5950000
#>    6 0.78000000 1.0000000
#>    7 0.83000000 1.0000000
#>    8 0.54966887 0.5813953
#>    9 0.12974484 0.2083138
#>   10 0.66115702 0.6804124
#>   11 0.55000000 0.5950000
#>   12 0.64516129 0.8888889
#>   13 0.10000000 0.1900000
#>   14 0.18315018 0.4072135
#>   15 0.10000000 0.1900000
#>   16 0.75000000 1.0000000
#>   17 0.90591710 0.9968292
#>   18 1.00000000 1.0000000
#>   19 0.38500000 0.5000000
#>   20 0.73000000 0.7850000
#>   21 0.91000000 1.0000000
#>   22 0.98000000 1.0000000
#>   23 0.82692308 1.0000000
#>   24 0.49969981 0.5995352
#>   25 0.76153846 0.8138528
#>   26 0.73000000 0.7850000
#>   27 0.95833333 1.0000000
#>   28 0.46000000 0.5700000
#>   29 0.56818182 0.6372379
#>   30 0.46000000 0.5700000
#>   31 0.75000000 1.0000000
#>   32 0.52818064 0.9965829
#>   33 1.00000000 1.0000000
#>   34 0.42000000 0.5050000
#>   35 0.74500000 0.7800000
#>   36 1.00000000 1.0000000
#>   37 0.88000000 1.0000000
#>   38 1.00000000 1.0000000
#>   39 0.52208695 0.5773503
#>   40 0.73640167 0.7692308
#>   41 0.74500000 0.7800000
#>   42 0.75675676 1.0000000
#>   43 0.49000000 0.5600000
#>   44 0.56179775 0.6666667
#>   45 0.49000000 0.5600000
#>   46 0.75000000 1.0000000
#>   47 2.87322333 5.5806975
#>   48 1.00000000 1.0000000
#>   49 0.37500000 0.5000000
#>   50 0.87000000 0.9700000
#>   51 1.00000000 1.0000000
#>   52 1.00000000 1.0000000
#>   53 1.00000000 1.0000000
#>   54 0.76431763 0.9401881
#>   55 0.86842105 0.9702970
#>   56 0.87000000 0.9700000
#>   57 1.00000000 1.0000000
#>   58 0.74000000 0.9400000
#>   59 0.78740157 0.9403762
#>   60 0.74000000 0.9400000
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
#>   77 0.73844141 2.9847024
#>   78 1.00000000 1.0000000
#>   79 0.53000000 0.5700000
#>   80 0.50500000 0.5250000
#>   81 0.71000000 1.0000000
#>   82 0.76000000 1.0000000
#>   83 0.50292398 1.0000000
#>   84 0.01023051 0.1234035
#>   85 0.60629921 0.6688963
#>   86 0.50500000 0.5250000
#>   87 0.50259067 1.0000000
#>   88 0.01000000 0.0500000
#>   89 0.01106317 0.5076142
#>   90 0.01000000 0.0500000
#>   91 0.75000000 1.0000000
#>   92 0.91147743 0.9998158
#>   93 1.00000000 1.0000000
#>   94 0.39500000 0.5000000
#>   95 0.69500000 0.7200000
#>   96 0.90000000 1.0000000
#>   97 0.94000000 1.0000000
#>   98 0.80000000 1.0000000
#>   99 0.41443025 0.5078008
#>  100 0.73504274 0.7734375
#>  101 0.69500000 0.7200000
#>  102 0.88235294 1.0000000
#>  103 0.39000000 0.4400000
#>  104 0.50735667 0.6289308
#>  105 0.39000000 0.4400000
#>  106 0.75000000 1.0000000
#>  107 0.49760630 0.9883169
#>  108 1.00000000 1.0000000
#>  109 0.41500000 0.5050000
#>  110 0.75000000 0.8050000
#>  111 0.98000000 1.0000000
#>  112 0.90000000 1.0000000
#>  113 0.95833333 1.0000000
#>  114 0.52946904 0.6300053
#>  115 0.74747475 0.7788462
#>  116 0.75000000 0.8050000
#>  117 0.78409091 0.8571429
#>  118 0.50000000 0.6100000
#>  119 0.58280719 0.6506667
#>  120 0.50000000 0.6100000
#>  121 0.75000000 1.0000000
#>  122 2.92338811 5.0077595
#>  123 1.00000000 1.0000000
#>  124 0.37500000 0.5000000
#>  125 0.87000000 0.9450000
#>  126 0.99000000 1.0000000
#>  127 1.00000000 1.0000000
#>  128 0.98765432 1.0000000
#>  129 0.76431763 0.8921885
#>  130 0.87150838 0.9468599
#>  131 0.87000000 0.9450000
#>  132 1.00000000 1.0000000
#>  133 0.74000000 0.8900000
#>  134 0.78740157 0.8943825
#>  135 0.74000000 0.8900000
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
#>  152 0.64195103 2.5323981
#>  153 1.00000000 1.0000000
#>  154 0.50500000 0.5300000
#>  155 0.52500000 0.5650000
#>  156 0.76000000 1.0000000
#>  157 0.81000000 1.0000000
#>  158 0.52727273 0.5555556
#>  159 0.06861916 0.1491300
#>  160 0.63281250 0.6689895
#>  161 0.52500000 0.5650000
#>  162 0.56250000 0.7142857
#>  163 0.05000000 0.1300000
#>  164 0.08912656 0.2220577
#>  165 0.05000000 0.1300000
#>  166 0.75000000 1.0000000
#>  167 0.89334235 0.9997019
#>  168 1.00000000 1.0000000
#>  169 0.40500000 0.5000000
#>  170 0.73500000 0.7750000
#>  171 0.88000000 1.0000000
#>  172 0.98000000 1.0000000
#>  173 0.77142857 1.0000000
#>  174 0.49082113 0.5920142
#>  175 0.76153846 0.8101266
#>  176 0.73500000 0.7750000
#>  177 0.95833333 1.0000000
#>  178 0.47000000 0.5500000
#>  179 0.55555556 0.6372379
#>  180 0.47000000 0.5500000
#>  181 0.75000000 1.0000000
#>  182 0.58777409 0.9747818
#>  183 1.00000000 1.0000000
#>  184 0.41500000 0.5000000
#>  185 0.73500000 0.7550000
#>  186 0.96000000 1.0000000
#>  187 0.89000000 1.0000000
#>  188 0.92000000 1.0000000
#>  189 0.48487426 0.5345225
#>  190 0.72573840 0.7671233
#>  191 0.73500000 0.7550000
#>  192 0.78313253 1.0000000
#>  193 0.47000000 0.5100000
#>  194 0.53763441 0.5952381
#>  195 0.47000000 0.5100000
#>  196 0.75000000 1.0000000
#>  197 2.88662362 5.1511163
#>  198 1.00000000 1.0000000
#>  199 0.37500000 0.5000000
#>  200 0.87000000 0.9300000
#>  201 1.00000000 1.0000000
#>  202 1.00000000 1.0000000
#>  203 1.00000000 1.0000000
#>  204 0.76431763 0.8627653
#>  205 0.87150838 0.9295775
#>  206 0.87000000 0.9300000
#>  207 1.00000000 1.0000000
#>  208 0.74000000 0.8600000
#>  209 0.78740157 0.8655395
#>  210 0.74000000 0.8600000
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
#>  227 0.76938285 2.9229714
#>  228 1.00000000 1.0000000
#>  229 0.51000000 0.5450000
#>  230 0.53000000 0.5500000
#>  231 0.77000000 1.0000000
#>  232 0.74000000 1.0000000
#>  233 0.55000000 1.0000000
#>  234 0.06462471 0.1063349
#>  235 0.59349593 0.6666667
#>  236 0.53000000 0.5500000
#>  237 0.52023121 0.5373134
#>  238 0.06000000 0.1000000
#>  239 0.07322942 0.5025126
#>  240 0.06000000 0.1000000
#>  241 0.75000000 1.0000000
#>  242 0.89033449 0.9999074
#>  243 1.00000000 1.0000000
#>  244 0.41500000 0.5000000
#>  245 0.69000000 0.7500000
#>  246 0.87000000 1.0000000
#>  247 0.95000000 1.0000000
#>  248 0.74074074 1.0000000
#>  249 0.39911048 0.5316743
#>  250 0.72463768 0.7863248
#>  251 0.69000000 0.7500000
#>  252 0.90196078 1.0000000
#>  253 0.38000000 0.5000000
#>  254 0.52083333 0.5928854
#>  255 0.38000000 0.5000000
#>  256 0.75000000 1.0000000
#>  257 0.56484372 0.9941320
#>  258 1.00000000 1.0000000
#>  259 0.42000000 0.5000000
#>  260 0.70500000 0.7600000
#>  261 0.96000000 1.0000000
#>  262 0.87000000 1.0000000
#>  263 0.92857143 1.0000000
#>  264 0.45226834 0.5356557
#>  265 0.69523810 0.7272727
#>  266 0.70500000 0.7600000
#>  267 0.72972973 1.0000000
#>  268 0.41000000 0.5200000
#>  269 0.53763441 0.6073174
#>  270 0.41000000 0.5200000
#>  271 0.75000000 1.0000000
#>  272 3.04988349 4.9741119
#>  273 1.00000000 1.0000000
#>  274 0.37500000 0.5000000
#>  275 0.86000000 0.9400000
#>  276 1.00000000 1.0000000
#>  277 1.00000000 1.0000000
#>  278 1.00000000 1.0000000
#>  279 0.74782186 0.8807048
#>  280 0.86206897 0.9411765
#>  281 0.86000000 0.9400000
#>  282 1.00000000 1.0000000
#>  283 0.72000000 0.8800000
#>  284 0.77077394 0.8814103
#>  285 0.72000000 0.8800000
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
#>   0.031   0.006   0.038 
system.time(res2 <- func_evalmod_aucroc(samp1))
#>    user  system elapsed 
#>   0.020   0.001   0.013 

# AUCs
res1
#>   modnames dsids curvetypes      aucs
#> 1       m1     1        ROC 0.4999385
#> 2       m1     1        PRC 0.4989090
res2
#>   modnames dsids      aucs     ustats
#> 1       m1     1 0.4999385 1249846248


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
