# Prepare your data

For a single model on a single test set you do not need any of this -
pass `scores` and `labels` straight to
[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md).
This page is for everything else.

``` r

library(precrec)
```

## Four helpers

| Function | What it does |
|----|----|
| [`join_scores()`](https://evalclass.github.io/precrec/reference/join_scores.md) | Collect the scores of several models into one list |
| [`join_labels()`](https://evalclass.github.io/precrec/reference/join_labels.md) | Collect the labels of several test sets into one list |
| [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md) | Turn those lists into the input [`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md) expects |
| [`create_sim_samples()`](https://evalclass.github.io/precrec/reference/create_sim_samples.md) | Make simulated data, for trying things out |

## Joining scores and labels

[`join_scores()`](https://evalclass.github.io/precrec/reference/join_scores.md)
accepts vectors, matrices and data frames, in any mixture, and returns a
list with one element per model.

``` r

s1 <- c(1, 2, 3, 4)
s2 <- c(5, 6, 7, 8)

scores <- join_scores(s1, s2)
```

[`join_labels()`](https://evalclass.github.io/precrec/reference/join_labels.md)
does the same for observed labels.

``` r

l1 <- c(1, 0, 1, 1)
l2 <- c(1, 0, 1, 0)

labels_same <- join_labels(l1, l1)
labels_diff <- join_labels(l1, l2)
```

Use the same label vector twice when two models were tested on the
*same* data, and two different ones when they were tested on different
data. That distinction is what tells `precrec` whether it is looking at
several models or several test sets.

## Building the input

[`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
puts them together. Two identifiers decide how the result is read:
`modnames` names the models, `dsids` numbers the test sets.

``` r

# Two models, one test set
mdat1 <- mmdata(scores, labels_same, modnames = c("mod1", "mod2"))

# One model, two test sets
mdat2 <- mmdata(scores, labels_diff, dsids = c(1, 2))
```

Leave them out and `precrec` uses sensible defaults. Set them when the
default guess is not what you meant.

## Simulated data

[`create_sim_samples()`](https://evalclass.github.io/precrec/reference/create_sim_samples.md)
generates scores at a chosen quality level, which is handy for
experiments and for every example on this site.

| Level     | Meaning               |
|-----------|-----------------------|
| `random`  | No better than chance |
| `poor_er` | Poor early retrieval  |
| `good_er` | Good early retrieval  |
| `excel`   | Excellent             |
| `perf`    | Perfect               |
| `all`     | All five at once      |

``` r

# 10 test sets, 100 positives and 100 negatives, two quality levels
samps <- create_sim_samples(10, 100, 100, c("poor_er", "good_er"))

simdat <- mmdata(samps[["scores"]], samps[["labels"]],
  modnames = samps[["modnames"]], dsids = samps[["dsids"]]
)
```

## Missing and tied scores

`NA` scores are ranked last by default; `na_worst = FALSE` ranks them
first. Tied scores share a rank by default (`ties_method = "equiv"`);
`"first"` keeps the input order and `"random"` shuffles them.

## Which direction the score runs

`precrec` ranks the highest score first, so a score has to be larger for
instances that are more likely to be positive. Plenty of quantities run
the other way - a p-value from Fisher’s exact test, a distance to a
cluster center, an error term - and for those, negate the score:

``` r

pvals <- c(0.001, 0.02, 0.3, 0.4, 0.8, 0.9)
obs <- c(1, 1, 0, 1, 0, 0)

evalmod(scores = -pvals, labels = obs)
#> 
#>     === AUCs ===
#> 
#>      Model name Dataset ID Curve type       AUC Baseline
#>    1         m1          1        ROC 0.8888889      0.5
#>    2         m1          1        PRC 0.9041060      0.5
#> 
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1         m1          1              3              3
```

There is no argument for this. Negation is the whole of it, and it
changes nothing else: the curves, the areas and the basic metrics all
read the ranking, and reversing the score reverses the ranking exactly.

The one thing negation does not survive is
[`prob_metrics()`](https://evalclass.github.io/precrec/reference/prob_metrics.md).
The Brier score and the log loss read the *values* of the scores rather
than their order, so they need genuine probabilities on 0 to 1 -
`-pvals` is rejected. Convert rather than negate when you want those:
`1 - pvals` ranks the same way and stays in range, though whether it is
calibrated is a separate question.

## When all you have is a table

Sometimes the per-instance scores are gone and what survives is a table
of performance values at a handful of thresholds - true and false
positive rates, or recall and precision.
[`format_points()`](https://evalclass.github.io/precrec/reference/format_points.md)
reconstructs instances that reproduce those points, so the table can
reach
[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
after all.

``` r

roc_tbl <- data.frame(
  threshold = c(0.9, 0.7, 0.5, 0.3, 0.1),
  tpr = c(0.2, 0.5, 0.7, 0.9, 1.0),
  fpr = c(0.02, 0.10, 0.25, 0.55, 1.00)
)

pts <- format_points(roc_tbl,
  threshold_col = "threshold",
  tpr_col = "tpr", fpr_col = "fpr",
  np = 50, nn = 100
)
#> Reconstructed 150 instances from 5 points in 1 group.
#> ℹ Between two points the curve assumes a constant class skew. Areas read off it
#>   are estimates.

evalmod(mmdata(pts$scores, pts$labels))
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
```

Pass `rec_col` and `prec_col` instead when the table holds recall and
precision, and `mod_col` and `dsid_col` when it holds several models or
test sets stacked in one data frame.

`np` and `nn` are not optional. Rates carry no totals: a true positive
rate of 0.4 is 20 positives out of 50 and 200 out of 500, and precision
and recall fix the ratio of the two classes but not their size. The
totals are also what places the chance-level baseline of the
precision-recall curve, which is the line most worth having on it.

### What the reconstruction assumes

The rows between two thresholds become instances that share a score, and
`precrec` spreads the true and false positives of a tied run evenly over
the cutoffs inside it. That even spread is the non-linear interpolation
of Davis and Goadrich, so nothing is being approximated that `precrec`
would not already do - a table with one row per distinct score
round-trips exactly, back to the original curve and the original area.

What a coarse table costs is the assumption underneath. The
interpolation is exact where a gap holds a single instance; over a gap
of many it takes the positives and negatives inside to alternate at a
constant rate, and they generally do not. An area read off fifteen
points is an estimate whose error the table gives no way to bound, in
either direction. Per-instance scores and labels remain the input to
prefer wherever they still exist.

One thing does not survive at all. The reconstructed scores are
threshold values, so everything that reads the *ranking* is exact, but
[`prob_metrics()`](https://evalclass.github.io/precrec/reference/prob_metrics.md)
reads the values themselves - a Brier score taken from reconstructed
data means nothing.

## Next

- [Compare several
  models](https://evalclass.github.io/precrec/articles/howto-multiple-models.md)
- [Average over several test
  sets](https://evalclass.github.io/precrec/articles/howto-multiple-test-sets.md)
- [Evaluate cross-validation
  folds](https://evalclass.github.io/precrec/articles/howto-cross-validation.md)
