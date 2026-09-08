# Comparison with other tools

`precrec` overlaps with several other packages. This page is about which
to reach for, and about the one place where they disagree on a number
rather than on an interface.

| Tool | What it is for | Against `precrec` |
|----|----|----|
| `ROCR` (R) | Cutoff metrics and their plots | `precrec` covers its metrics and accepts its names; see below |
| `pROC` (R) | ROC analysis and inference | Analytic DeLong intervals and `roc.test()`; no precision-recall curves |
| `PRROC` (R) | Precision-recall curves | Also integrates them properly, and takes weighted or soft labels, which `precrec` does not |
| `yardstick` (R) | Metrics inside `tidymodels` | Tibble in, tibble out, and fits `tune` and `workflows`; `precrec` reached name parity in 0.17.0 |
| `scikit-learn` (Python) | General model evaluation | The reference in Python; [`classification_report()`](https://evalclass.github.io/precrec/reference/classification_report.md) mirrors its report |
| `imbalanced-learn` (Python) | Metrics for imbalanced data | Has the geometric mean and an index-balanced accuracy that `precrec` does not |

## Where the numbers differ

There are two ways to turn precision-recall points into an area, and
they are not two ways of computing the same thing.

The **step** estimator joins the points with horizontal steps - what
`scikit-learn`’s `average_precision_score` reports, and what `precrec`
provides as
[`average_precision()`](https://evalclass.github.io/precrec/reference/average_precision.md).
The **interpolated** area is what
[`auc()`](https://evalclass.github.io/precrec/reference/auc.md) returns
for the PRC, taken along the non-linear interpolation between achievable
points. On balanced data they nearly agree; as the positives get rarer
they do not:

``` r

library(precrec)

compare <- function(np, nn) {
  set.seed(1)
  scores <- c(rnorm(np, 1.2), rnorm(nn, 0))
  labels <- rep(c(1, 0), c(np, nn))
  curves <- evalmod(scores = scores, labels = labels)
  areas <- auc(curves)

  data.frame(
    positives = sprintf("%.0f%%", 100 * np / (np + nn)),
    interpolated = areas$aucs[areas$curvetypes == "PRC"],
    step = average_precision(curves)$aps
  )
}

res <- do.call(rbind, list(
  compare(500, 500), compare(100, 900),
  compare(20, 980)
))
res$overstated <- sprintf("%+.1f%%", 100 * (res$step / res$interpolated - 1))

knitr::kable(res, digits = 4)
```

| positives | interpolated |   step | overstated |
|:----------|-------------:|-------:|:-----------|
| 50%       |       0.8005 | 0.8009 | +0.0%      |
| 10%       |       0.3672 | 0.3741 | +1.9%      |
| 2%        |       0.1291 | 0.1467 | +13.6%     |

At two percent positives the step estimator reads more than a tenth too
high. That is the case `precrec` was written for, and it is why the
interpolated area is what
[`auc()`](https://evalclass.github.io/precrec/reference/auc.md) reports.
Joining the raw points with straight lines is a third answer again, and
the one the package exists to argue against.

## ROCR names work here

The ROCR metrics were checked against ROCR itself over balanced,
imbalanced and tied data as they were added, and its names are accepted
as aliases:

``` r

points <- evalmod(
  scores = P10N10$scores, labels = P10N10$labels,
  mode = "basic", metrics = c("fall", "miss", "rpp")
)

unique(as.data.frame(points)$type)
#>  [1] score                   label                   error                  
#>  [4] accuracy                specificity             sensitivity            
#>  [7] precision               mcc                     fscore                 
#> [10] balanced_accuracy       npv                     informedness           
#> [13] markedness              kappa                   fpr                    
#> [16] fnr                     predicted_positive_rate
#> 17 Levels: score label error accuracy specificity sensitivity precision ... predicted_positive_rate
```

`fall`, `miss` and `rpp` come back as `fpr`, `fnr` and
`predicted_positive_rate`. Three ROCR metrics are deliberately absent -
the ROC convex hull, the expected-cost curve and the calibration error -
each being a curve in a space of its own rather than a column of the
metric table.

## When to use something else

Reach for `pROC` if you need ROC inference and nothing else: its DeLong
intervals are analytic where
[`auc_boot()`](https://evalclass.github.io/precrec/articles/metrics-uncertainty.md)
resamples, and it has more ways to compare two curves. Reach for `PRROC`
if your labels are weighted or soft. Reach for `yardstick` if the
surrounding code is `tidymodels`, and for `scikit-learn` if it is
Python.

Reach for `precrec` when the precision-recall curve is the point, when
the data are imbalanced enough for the table above to matter, or when
you want one call to give you the curves, the per-cutoff metrics and the
areas together.

## Next

- [Metrics
  overview](https://evalclass.github.io/precrec/articles/metrics-overview.md)
- [AUC and other curve
  summaries](https://evalclass.github.io/precrec/articles/metrics-auc.md)
