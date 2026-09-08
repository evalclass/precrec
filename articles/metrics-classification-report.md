# Classification report

Every other page here reports a metric over all of its cutoffs, or a
number that summarizes a whole curve. This one reports a single
operating point, in the layout `scikit-learn`’s `classification_report`
prints.

``` r

library(precrec)

mdat <- mmdata(C3N150$scores, C3N150$labels)

classification_report(mdat, at = 0.5)
#> 
#>               precision    recall  f1-score   support
#> 
#>            c1      0.65      0.98      0.78        50
#>            c2      0.56      0.78      0.65        50
#>            c3      0.35      0.38      0.37        50
#> 
#>     micro avg      0.54      0.71      0.61       150
#>     macro avg      0.52      0.71      0.60       150
#>  weighted avg      0.52      0.71      0.60       150
```

## `at` has no default

`scikit-learn` reports on `y_pred`, so by the time it is called someone
has already decided where to cut. `precrec` holds scores and evaluates
every cutoff, so the report has to be told which one to use - and the
answer is not a detail. There is also no threshold that means the same
thing on every score scale `precrec` accepts: `0.5` is the middle of a
probability but says nothing about a log-odds or an SVM margin.

Pass one number for every class, or one per class:

``` r

classification_report(mdat, at = c(c1 = 0.4, c2 = 0.5, c3 = 0.6))
#> 
#>               precision    recall  f1-score   support
#> 
#>            c1      0.64      0.98      0.77        50
#>            c2      0.56      0.78      0.65        50
#>            c3      0.35      0.34      0.34        50
#> 
#>     micro avg      0.54      0.70      0.61       150
#>     macro avg      0.51      0.70      0.59       150
#>  weighted avg      0.51      0.70      0.59       150
```

## Why there is no accuracy row

A multi-class dataset is evaluated one-vs-rest, and each class is
thresholded on its own. Nothing makes those decisions agree, so an
observation can clear the bar for no class at all, or for several:

``` r

table(n_classes = rowSums(C3N150$scores >= 0.5))
#> n_classes
#>  0  1  2  3 
#> 20 71 49 10
```

With observations in no class and in three, there is no single-label
accuracy to report. This is the case `scikit-learn` documents, and it
prints `micro avg` in exactly this situation: precision, recall and
F-score recomputed from the true positives, false positives and false
negatives pooled over the classes, rather than averaged from the
per-class rates.

A binary problem is different. One threshold puts every observation on
exactly one side, so accuracy is defined and that is the row you get:

``` r

classification_report(
  scores = P10N10$scores, labels = P10N10$labels,
  at = 12
)
#> 
#>               precision    recall  f1-score   support
#> 
#>      negative      0.62      0.50      0.56        10
#>      positive      0.58      0.70      0.64        10
#> 
#>      accuracy                          0.60        20
#>     macro avg      0.60      0.60      0.60        20
#>  weighted avg      0.60      0.60      0.60        20
```

Both classes get a row, as in `scikit-learn`. The negative row is the
confusion matrix turned around, so its precision is the negative
predictive value and its recall the specificity.

## The three averages

`macro avg` weights every class equally and `weighted avg` weights each
by its support - the same pair
[`auc()`](https://evalclass.github.io/precrec/articles/metrics-auc.md)
offers as `macro_weight`. Use macro when the rare classes are the
interesting ones, weighted when you want the average observation.

`micro avg` is neither: it pools the counts first and computes the
metrics once. When the predictions happen to be single-label the three
collapse to the same number, and that number is the accuracy - which is
why `scikit-learn` prints one row or the other and never both.

## Undefined cells

A class nothing was predicted into has no precision, and a class with no
observations has no recall. `zero_division` decides what goes in the
cell: `0` by default, as in `scikit-learn`, or `NA` for the convention
the per-cutoff metrics follow. A score of `NA` is never a positive
prediction, matching the `na_worst = TRUE` default of the rest of the
package.

## What it is not

One point on the curves, chosen by you. It cannot tell you whether a
different threshold would have done better, and a threshold picked by
looking at the same data it is scored on will flatter the model. For the
threshold-free view, which is what `precrec` exists for, see [AUC and
other curve
summaries](https://evalclass.github.io/precrec/articles/metrics-auc.md).

## Next

- [Metrics
  overview](https://evalclass.github.io/precrec/articles/metrics-overview.md)
- [AUC and other curve
  summaries](https://evalclass.github.io/precrec/articles/metrics-auc.md)
