# Reconstruct per-instance data from a table of curve points

The `format_points` function takes a data frame of performance values
calculated at a set of thresholds - true and false positive rates, or
recall and precision - and reconstructs per-instance scores and labels
that reproduce those points exactly. The result is a list for
[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
and
[`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md).

## Usage

``` r
format_points(
  points_df,
  threshold_col,
  tpr_col = NULL,
  fpr_col = NULL,
  rec_col = NULL,
  prec_col = NULL,
  np = NULL,
  nn = NULL,
  mod_col = NULL,
  dsid_col = NULL
)
```

## Arguments

- points_df:

  A data frame with one row per threshold, holding a threshold column,
  two rate columns, and optionally model and dataset columns.

- threshold_col:

  A number/string that specifies the threshold column of `points_df`.

- tpr_col:

  A number/string that specifies the true positive rate column of
  `points_df`.

- fpr_col:

  A number/string that specifies the false positive rate column of
  `points_df`.

- rec_col:

  A number/string that specifies the recall column of `points_df`.
  Recall and the true positive rate are the same quantity; supply one
  pair of columns, either `tpr_col`/`fpr_col` or `rec_col`/`prec_col`.

- prec_col:

  A number/string that specifies the precision column of `points_df`.

- np:

  The number of positives. A single number, a numeric vector with one
  element per group, or a number/string that specifies a column of
  `points_df`.

- nn:

  The number of negatives, in the same three forms as `np`.

- mod_col:

  A number/string that specifies the model column of `points_df`. `NULL`
  treats the whole table as one model.

- dsid_col:

  A number/string that specifies the dataset column of `points_df`.
  `NULL` treats the whole table as one dataset.

## Value

The `format_points` function returns a list that contains scores,
labels, model names and dataset IDs.

## Details

A table of rates is a set of supporting points on a curve, and the
instances between two adjacent points are missing. `format_points` puts
them back: the rows between two thresholds become instances that share a
score, and precrec spreads the true and false positives of a tied run
evenly over the cutoffs inside it. That even spread is the non-linear
interpolation of Davis and Goadrich (2006), so the reconstructed curve
is the one that interpolation prescribes.

The interpolation is exact only where a gap holds a single instance.
Over the wider gaps a threshold table leaves, it assumes the positives
and negatives inside a gap alternate at a constant rate, and the area
under the resulting curve is an estimate whose error the table gives no
way to bound. Per-instance scores and labels remain the input to prefer
wherever they exist.

`np` and `nn` are required. Recall and precision determine the class
skew but not the totals, and true and false positive rates determine
neither, so the counts cannot be recovered without them. They also place
the chance-level baseline of the precision-recall curve, and supply the
instances below the lowest threshold when the table stops short of
predicting everything positive.

Reconstructed scores are threshold values, not per-instance predictions.
Everything that reads the *ranking* is exact - the curves, the areas,
the basic metrics - but
[`prob_metrics()`](https://evalclass.github.io/precrec/reference/prob_metrics.md)
reads the values themselves, and a Brier score or log loss taken from
reconstructed data means nothing.

The direction of the threshold column is inferred from the rates. Both
conventions work - a threshold that keeps fewer instances as it rises,
and one that keeps more - and the reconstructed scores are negated for
the second so that a larger score always means a more likely positive.

## References

Davis J, Goadrich M (2006) The relationship between precision-recall and
ROC curves. *Proceedings of the 23rd International Conference on Machine
Learning*, 233-240.
[doi:10.1145/1143844.1143874](https://doi.org/10.1145/1143844.1143874)

## See also

[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
for calculation evaluation metrics.
[`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
for formatting input data.
[`format_nfold()`](https://evalclass.github.io/precrec/reference/format_nfold.md)
for cross validation data frames.

## Examples

``` r

##################################################
### A single model, true and false positive rates
###

roc_df <- data.frame(
  threshold = c(0.9, 0.7, 0.5, 0.3, 0.1),
  tpr = c(0.2, 0.5, 0.7, 0.9, 1.0),
  fpr = c(0.02, 0.10, 0.25, 0.55, 1.0)
)

pts1 <- format_points(roc_df,
  threshold_col = "threshold",
  tpr_col = "tpr", fpr_col = "fpr",
  np = 50, nn = 100
)
#> Reconstructed 150 instances from 5 points in 1 group.
#> ℹ Between two points the curve assumes a constant class skew. Areas read off it
#>   are estimates.

evalmod(mmdata(pts1$scores, pts1$labels))
#> 
#>     === AUCs ===
#> 
#>      Model name Dataset ID Curve type       AUC  Baseline
#>    1         m1          1        ROC 0.7875000 0.5000000
#>    2         m1          1        PRC 0.6595687 0.3333333
#> 
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1         m1          1            100             50
#> 


##################################################
### Two models, recall and precision
###

prc_df <- data.frame(
  model = rep(c("m1", "m2"), each = 4),
  threshold = rep(c(0.8, 0.6, 0.4, 0.2), 2),
  recall = c(0.3, 0.6, 0.8, 1.0, 0.2, 0.4, 0.7, 1.0),
  precision = c(0.9, 0.8, 0.6, 0.4, 0.7, 0.6, 0.5, 0.4)
)

pts2 <- format_points(prc_df,
  threshold_col = "threshold",
  rec_col = "recall", prec_col = "precision",
  mod_col = "model", np = 40, nn = 60
)
#> Reconstructed 200 instances from 8 points in 2 groups.
#> ℹ Between two points the curve assumes a constant class skew. Areas read off it
#>   are estimates.
#> ℹ Rates were rounded to whole counts, moving one by at most 0.0071.

evalmod(mmdata(pts2$scores, pts2$labels, modnames = pts2$modnames))
#> 
#>     === AUCs ===
#> 
#>      Model name Dataset ID Curve type       AUC Baseline
#>    1         m1          1        ROC 0.8000000      0.5
#>    2         m1          1        PRC 0.7631726      0.4
#>    3         m2          1        ROC 0.6541667      0.5
#>    4         m2          1        PRC 0.5662187      0.4
#> 
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1         m1          1             60             40
#>    2         m2          1             60             40
#> 
```
