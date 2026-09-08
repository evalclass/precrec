# Use precrec with tidymodels

`precrec` needs no adapter. `tune::collect_predictions()` returns a fold
column, a truth column and one `.pred_<class>` column per class, which
is the shape
[`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
already takes through `nfold_df`.

``` r

library(precrec)
```

## Set the positive class

This is the one thing to get right. `yardstick` treats the **first**
factor level as the event; `precrec` treats the **last** as the positive
class. Pair `yardstick`’s default score column with `precrec`’s default
and the AUC comes out inverted, with no warning.

``` r

lv <- c("bad", "good")
truth <- factor(c("bad", "bad", "good", "good", "good"), levels = lv)
scores <- c(0.1, 0.2, 0.8, 0.9, 0.7)

c(
  default = auc(evalmod(mmdata(scores, truth)))$aucs[1],
  first = auc(evalmod(mmdata(scores, truth, posclass = "bad")))$aucs[1]
)
#> default   first 
#>       1       0
```

Feed `.pred_<last level>` and leave `posclass` alone, or feed
`.pred_<first level>` and name it in `posclass`. Pointed at the same
class the two agree exactly: on a 4-fold `logistic_reg()` fit of
`two_class_dat`, `yardstick`’s `roc_auc()` and
[`auc()`](https://evalclass.github.io/precrec/reference/auc.md) both
return 0.885564, and the pairing above returns 0.114436.

## One model over folds

`collect_predictions()` gives `id` for the fold and `.row` for the
instance.

``` r

preds <- wf |>
  fit_resamples(folds, control = control_resamples(save_pred = TRUE)) |>
  collect_predictions()
```

The chunks that need `tidymodels` are not evaluated, so the site builds
without it. The rest of the page runs against a frame carrying the
columns `collect_predictions()` returns.

``` r

set.seed(1)
y <- sample(rep(c("good", "bad"), each = 40))
preds <- data.frame(
  id = rep(paste0("Fold", 1:4), each = 20), .row = 1:80,
  .pred_good = ifelse(y == "good", runif(80, 0.35, 1), runif(80, 0, 0.65)),
  truth = factor(y, levels = c("bad", "good"))
)
```

``` r

curves <- evalmod(
  nfold_df = preds, score_cols = ".pred_good",
  lab_col = "truth", fold_col = "id",
  modnames = "rf", dsids = 1:4
)

knitr::kable(auc_ci(curves))
```

| modnames | curvetypes |      mean |     error | lower_bound | upper_bound |   n |
|:---------|:-----------|----------:|----------:|------------:|------------:|----:|
| rf       | ROC        | 0.9664583 | 0.0255685 |   0.9408898 |   0.9920268 |   4 |
| rf       | PRC        | 0.9640375 | 0.0295294 |   0.9345081 |   0.9935669 |   4 |

## Several models

`collect_predictions()` stacks models by `wflow_id` or `.config`, so
widen first and name one score column per model. On a `workflow_set` it
averages over the folds unless you ask it not to, and then there is no
`id` column to widen on - so pass `summarize = FALSE`.

``` r

wide <- workflow_map(wset, "fit_resamples",
  resamples = folds,
  control = control_resamples(save_pred = TRUE)
) |>
  collect_predictions(summarize = FALSE) |>
  tidyr::pivot_wider(
    id_cols = c(id, .row, truth),
    names_from = wflow_id, values_from = .pred_good
  )

evalmod(
  nfold_df = as.data.frame(wide), score_cols = c("rec_rf", "rec_glm"),
  lab_col = "truth", fold_col = "id",
  modnames = c("rec_rf", "rec_glm"), dsids = 1:4
)
```

## More than two classes

Hand the `.pred_` columns over as a matrix. Classes are matched by
column name, so drop the prefix. Select them by level rather than by
pattern - `.pred_class` also starts with `.pred_`, and it is a factor,
so a loose match turns the whole matrix into characters.

``` r

sc <- as.matrix(preds[paste0(".pred_", levels(preds$truth))])
colnames(sc) <- levels(preds$truth)

evalmod(mmdata(sc, preds$truth))
```

`precrec` adds a `macro-average` row of its own, which is the number
`yardstick`’s `roc_auc(estimator = "macro")` reports.

One-vs-rest cannot be combined with `nfold_df`, so pool the folds for a
multiclass run, or loop over them yourself.

## The report

`.pred_class` is the hard prediction `parsnip` derives at a 0.5
threshold, and
[`classification_report()`](https://evalclass.github.io/precrec/reference/classification_report.md)
takes that threshold directly.

``` r

classification_report(mmdata(preds$.pred_good, preds$truth), at = 0.5)
#> 
#>               precision    recall  f1-score   support
#> 
#>      negative      0.86      0.93      0.89        40
#>      positive      0.92      0.85      0.88        40
#> 
#>      accuracy                          0.89        80
#>     macro avg      0.89      0.89      0.89        80
#>  weighted avg      0.89      0.89      0.89        80
```

## Next

- [Evaluate cross-validation
  folds](https://evalclass.github.io/precrec/articles/howto-cross-validation.md)
- [How precrec compares with other
  tools](https://evalclass.github.io/precrec/articles/metrics-other-tools.md)
