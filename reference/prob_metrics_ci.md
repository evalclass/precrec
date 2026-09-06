# Calculate CIs of the Brier score and the log loss

The `prob_metrics_ci` function calculates the confidence intervals of
the Brier score and the log loss when multiple test datasets are
specified.

## Usage

``` r
prob_metrics_ci(
  mdat,
  scores = NULL,
  labels = NULL,
  eps = 1e-15,
  alpha = 0.05,
  dtype = "normal",
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

- alpha:

  A numeric value of the significant level (default: 0.05)

- dtype:

  A string to specify the distribution used for CI calculation.

  |                  |                     |
  |------------------|---------------------|
  | **dtype**        | **distribution**    |
  | normal (default) | Normal distribution |
  | z                | Normal distribution |
  | t                | t-distribution      |

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

The `prob_metrics_ci` function returns a data frame with the columns
`modnames`, `metrics`, `mean`, `error`, `lower_bound`, `upper_bound`,
and `n`.

## See also

[`prob_metrics()`](https://evalclass.github.io/precrec/reference/prob_metrics.md)
for the per-dataset metrics themselves.
[`auc_ci()`](https://evalclass.github.io/precrec/reference/auc_ci.md)
for the equivalent calculation on AUC scores.

## Examples

``` r

##################################################
### Single model & multiple test datasets
###

## Create sample datasets with 100 positives and 100 negatives
samps <- create_sim_samples(4, 100, 100, "good_er")
mdat <- mmdata(samps[["scores"]], samps[["labels"]],
  modnames = samps[["modnames"]],
  dsids = samps[["dsids"]]
)

## Calculate the CIs
prob_metrics_ci(mdat)
```
