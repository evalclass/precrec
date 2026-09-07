# Calculate the Brier score, the RMSE and the log loss of predicted probabilities

The `prob_metrics` function calculates three probability-based
evaluation metrics - the Brier score, its square root the root mean
squared error, and the log loss - for prediction scores that are
probabilities. Unlike ROC and Precision-Recall curves, these metrics
depend on the values of the scores rather than on their ranks, so the
scores must lie in the range \[0, 1\].

## Usage

``` r
prob_metrics(
  mdat,
  scores = NULL,
  labels = NULL,
  eps = 1e-15,
  metrics = NULL,
  ...
)
```

## Arguments

- mdat:

  An `S3` object created by the
  [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
  function. It contains formatted scores and labels. The `prob_metrics`
  function ignores `scores` and `labels` when `mdat` is specified. These
  arguments are internally passed to the
  [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
  function when `mdat` is unspecified. In that case, both `scores` and
  `labels` must be at least specified.

- scores:

  A numeric dataset of predicted probabilities. It can be a vector, a
  matrix, an array, a data frame, or a list.

- labels:

  A numeric, character, logical, or factor dataset of observed labels.
  It can be a vector, a matrix, an array, a data frame, or a list.

- eps:

  A numeric value used to clamp the scores away from `0` and `1` before
  the log loss is calculated. A single confident and wrong prediction
  would otherwise make the log loss infinite.

- metrics:

  A character vector of additional metrics to calculate. The three
  metrics above are always returned; `"d2_brier"` and `"d2_logloss"` are
  returned as well when they are named here, and `"all"` asks for every
  metric the function knows.

  A D2 score rescales a loss against the loss of the null model, the one
  that predicts the observed prevalence for every case and ignores the
  scores:

  `D2 = 1 - loss(model) / loss(null)`

  It is `1` for a perfect model and `0` for one that does no better than
  the prevalence, and it is negative for a model that does worse - which
  is a real result rather than an error, so it is not clipped. A dataset
  holding a single class has a null loss of `0` and so a D2 score of
  `NA`.

- ...:

  These additional arguments are passed to
  [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
  for data preparation.

## Value

The `prob_metrics` function returns a data frame with the columns
`modnames`, `dsids`, `metrics`, and `values`. `metrics` is one of
"brier", "rmse" or "logloss", so each model and dataset combination
takes up three rows - plus one row for each metric named in `metrics`.

## See also

[`prob_metrics_ci()`](https://evalclass.github.io/precrec/reference/prob_metrics_ci.md)
for the CIs of these metrics over multiple datasets.
[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
for generating `S3` objects with performance evaluation metrics.
[`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
for formatting input data.

## Examples

``` r

##################################################
### Single model & single test dataset
###

## Predicted probabilities of 10 positives and 10 negatives
set.seed(1)
scores <- c(runif(10, 0.4, 1), runif(10, 0, 0.6))
labels <- c(rep(1, 10), rep(0, 10))

## Brier score and log loss
prob_metrics(scores = scores, labels = labels)


##################################################
### Multiple models & multiple test datasets
###

## The "poor_er" and "good_er" samples are drawn from beta distributions,
## so their scores can be read as probabilities
samps <- create_sim_samples(4, 100, 100, c("poor_er", "good_er"))
mdat <- mmdata(samps[["scores"]], samps[["labels"]],
  modnames = samps[["modnames"]],
  dsids = samps[["dsids"]]
)

pm <- prob_metrics(mdat)

## Show the Brier scores only
subset(pm, metrics == "brier")
#>    modnames dsids metrics    values
#> 1   poor_er     1   brier 0.2056728
#> 4   good_er     1   brier 0.1895236
#> 7   poor_er     2   brier 0.1729602
#> 10  good_er     2   brier 0.1749330
#> 13  poor_er     3   brier 0.2229980
#> 16  good_er     3   brier 0.2054167
#> 19  poor_er     4   brier 0.1781563
#> 22  good_er     4   brier 0.2131959

## The D2 scores, which say how much of each loss the model explains
subset(
  prob_metrics(mdat, metrics = c("d2_brier", "d2_logloss")),
  dsids == 1
)
#>    modnames dsids    metrics     values
#> 1   poor_er     1      brier 0.20567281
#> 2   poor_er     1       rmse 0.45351165
#> 3   poor_er     1    logloss 0.65079815
#> 4   poor_er     1   d2_brier 0.17730875
#> 5   poor_er     1 d2_logloss 0.06109674
#> 6   good_er     1      brier 0.18952365
#> 7   good_er     1       rmse 0.43534314
#> 8   good_er     1    logloss 0.64008714
#> 9   good_er     1   d2_brier 0.24190542
#> 10  good_er     1 d2_logloss 0.07654946
```
