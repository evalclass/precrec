# Classification report

`classification_report` builds the per-class table of precision, recall
and F-score that `scikit-learn`'s `classification_report` prints,
together with the summary rows that go under it.

## Usage

``` r
classification_report(
  mdat,
  scores = NULL,
  labels = NULL,
  at = NULL,
  zero_division = 0,
  ...
)
```

## Arguments

- mdat:

  An `mdata` object created by
  [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md).
  It can be omitted when `scores` and `labels` are given.

- scores:

  A numeric vector, matrix, array, data frame, or list of scores. See
  [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
  for the accepted shapes.

- labels:

  A numeric, character, logical, or factor vector of observed labels, or
  a list of such vectors.

- at:

  The operating point, as a score threshold. An observation is predicted
  positive for a class when its score for that class is greater than or
  equal to the threshold. It has no default and must be given; see the
  note below on why. Either

  - a single number, used for every class, or

  - one number per class, named after the classes or given in their
    order.

  A score of `NA` is never predicted positive, matching the default
  `na_worst = TRUE` of the rest of the package.

- zero_division:

  The value reported when a precision, recall or F-score divides by
  zero - a class nothing was predicted into, or a class with no
  observations. `0` by default, as in `scikit-learn`. Use `NA` for the
  convention the per-cutoff metrics of
  [`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
  follow.

- ...:

  Further arguments passed to
  [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
  when `scores` and `labels` are given instead of `mdat`.

## Value

A data frame with one row per class and one row per summary, and the
columns

|             |                                          |
|-------------|------------------------------------------|
| `modnames`  | Model name                               |
| `dsids`     | Dataset ID                               |
| `class`     | Class name, or the name of a summary row |
| `precision` | Predicted positives that are positive    |
| `recall`    | Positives that are predicted positive    |
| `fscore`    | Harmonic mean of the two                 |
| `support`   | Observations of the class                |

The object also has the class `classification_report`, which only
affects how it prints; it is a data frame in every other respect.

## The summary rows

`macro avg` is the unweighted mean over the classes and `weighted avg`
the mean weighted by support. The third row depends on whether the
predictions assign each observation to exactly one class, which is the
rule `scikit-learn` documents:

- A binary problem at a threshold predicts each observation into exactly
  one of the two classes, so the row is `accuracy`.

- A multi-class problem is evaluated by one-vs-rest, and each class is
  thresholded on its own, so an observation can fall into no class or
  into several. There is then no single-label accuracy, and the row is
  `micro avg`: precision, recall and F-score recomputed from the true
  positives, false positives and false negatives pooled over the
  classes.

## Why `at` has no default

`scikit-learn` reports on `y_pred`, so the caller has already chosen an
operating point before the function is called. `precrec` holds scores
and evaluates every cutoff, so the report has to be told which one to
use, and the answer changes the table: on the three-class `C3N150` the
F-score of one class moves from 0.37 to 0.51 between two reasonable
choices. Scores in `precrec` are on whatever scale the classifier
produced, so there is no threshold that is meaningful for all of them -
`0.5` says nothing about a log-odds or an SVM margin. The threshold is
therefore always the caller's.

## See also

[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
for the same metrics at every cutoff,
[`auc()`](https://evalclass.github.io/precrec/reference/auc.md) for the
threshold-free summaries and their macro averages, and
[`prob_metrics()`](https://evalclass.github.io/precrec/reference/prob_metrics.md)
for the probability-based losses.

## Examples

``` r

## Multi-class: one row per class, then micro, macro and weighted averages
data(C3N150)
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
#> 

## A threshold per class, named or in class order
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
#> 

## Binary: both classes, and an accuracy row rather than a micro average
data(P10N10)
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
#> 

## It is a data frame, so the usual accessors work
report <- classification_report(mdat, at = 0.5)
report[report$class == "macro avg", ]
#> 
#>               precision    recall  f1-score   support
#> 
#> 
#>     macro avg      0.52      0.71      0.60       150
#> 
```
