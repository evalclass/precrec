# Print the summary of a precrec object

The `print` function prints a summary of an `S3` object created by
[`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md),
[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md),
[`metric_curve()`](https://evalclass.github.io/precrec/reference/metric_curve.md)
or
[`classification_report()`](https://evalclass.github.io/precrec/reference/classification_report.md).
It is called for its side effect, and is what the console shows when one
of those objects is evaluated at the prompt.

## Usage

``` r
# S3 method for class 'mdat'
print(x, ...)

# S3 method for class 'curve_info'
print(x, ...)

# S3 method for class 'beval_info'
print(x, ...)

# S3 method for class 'aucroc'
print(x, ...)

# S3 method for class 'xycurve_info'
print(x, ...)

# S3 method for class 'classification_report'
print(x, digits = 2, ...)
```

## Arguments

- x:

  An `S3` object created by
  [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md),
  [`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md),
  [`metric_curve()`](https://evalclass.github.io/precrec/reference/metric_curve.md)
  or
  [`classification_report()`](https://evalclass.github.io/precrec/reference/classification_report.md).
  The `print` function takes one of the following `S3` objects.

  |  |  |
  |----|----|
  | **`S3` object** | **Created by** |
  | `mdat` | [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md) |
  | `curve_info` | [`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md) |
  | `beval_info` | `evalmod(mode = "basic")` |
  | `aucroc` | `evalmod(mode = "aucroc")` |
  | `xycurve_info` | [`metric_curve()`](https://evalclass.github.io/precrec/reference/metric_curve.md) |
  | `classification_report` | [`classification_report()`](https://evalclass.github.io/precrec/reference/classification_report.md) |

  Every object but a `classification_report` includes a summary of the
  input data - the model names, the dataset IDs and the class counts.
  Alongside it, a curve object reports its AUCs beside the baseline each
  one is read against - `0.5` for a ROC curve and the proportion of
  positives for a precision-recall curve, see
  [`auc()`](https://evalclass.github.io/precrec/reference/auc.md) - and
  its partial AUCs as well when it came from
  [`part()`](https://evalclass.github.io/precrec/reference/part.md); a
  basic-metric object reports what each metric abbreviation means and a
  five-number summary of every metric; an `aucroc` object reports the
  AUCs beside the U statistics they came from; and a
  [`metric_curve()`](https://evalclass.github.io/precrec/reference/metric_curve.md)
  object names the metric pair and counts the points on it. A
  `classification_report` prints its own table of per-class precision,
  recall and F-score.

  The curve and point objects carry a second class naming how many
  models and test datasets they hold, such as `sscurves` or `mmpoints`,
  but all of them print through `curve_info` or `beval_info`. See the
  **Value** section of
  [`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md).

- ...:

  Not used by these methods.

- digits:

  The number of digits after the decimal point, between `0` and `20`.
  Used by the `classification_report` method only.

## Value

The `print` function returns `x` invisibly.

## See also

[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
and
[`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
for creating the objects,
[`as.data.frame()`](https://evalclass.github.io/precrec/reference/as.data.frame.md)
for the same results as a data frame, and
[`auc()`](https://evalclass.github.io/precrec/reference/auc.md) for the
AUCs alone.

## Examples

``` r

##################################################
### Input data
###

## Load a dataset with 10 positives and 10 negatives
data(P10N10)

mdat <- mmdata(P10N10$scores, P10N10$labels)
mdat
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1         m1          1             10             10
#> 


##################################################
### ROC and Precision-Recall curves
###

curves <- evalmod(mdat)
curves
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

## Partial curves also report the partial AUCs
part(curves, xlim = c(0, 0.25))
#> 
#>     === AUCs ===
#> 
#>      Model name Dataset ID Curve type       AUC Baseline
#>    1         m1          1        ROC 0.7200000      0.5
#>    2         m1          1        PRC 0.7397716      0.5
#> 
#> 
#>     === partial AUCs ===
#> 
#>      Model name Dataset ID Curve type      pAUC Standardized
#>    1         m1          1        ROC 0.1006250    0.4025000
#>    2         m1          1        PRC 0.2345849    0.9383396
#> 
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1         m1          1             10             10
#> 


##################################################
### Basic evaluation metrics
###

points <- evalmod(mdat, mode = "basic")
points
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
### AUC with the U statistic
###

evalmod(mdat, mode = "aucroc")
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


##################################################
### One metric against another
###

metric_curve(mdat)
#> 
#>     === Sensitivity vs FPR ===
#> 
#>      A registered pair: this is the ROC curve, and is
#>      calculated by the same code as evalmod(mode = "rocprc").
#> 
#>      Model name Dataset ID # of points
#>    1         m1          1        1017
#> 
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1         m1          1             10             10
#> 


##################################################
### Per-class precision, recall and F-score
###

classification_report(mdat, at = 12)
#> 
#>               precision    recall  f1-score   support
#> 
#>      negative      0.62      0.50      0.56        10
#>      positive      0.58      0.70      0.64        10
#> 
#>      accuracy                          0.60        20
#>     macro avg      0.60      0.60      0.60        20
#>  weighted avg      0.60      0.60      0.60        20
#> 
```
