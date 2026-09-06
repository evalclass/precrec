# Probability-based metrics

Everything else in `precrec` reads the *order* of the scores. These
three read their *values*, and ask a different question: not “does the
classifier rank well” but “are its probabilities honest”.

``` r

library(precrec)

# Scores drawn from beta distributions, so they are probabilities
samps <- create_sim_samples(4, 100, 100, c("poor_er", "good_er"))

mdat <- mmdata(samps[["scores"]], samps[["labels"]],
  modnames = samps[["modnames"]], dsids = samps[["dsids"]]
)
```

The scores must be probabilities between 0 and 1. A model that ranks
perfectly but always predicts 0.6 for positives scores badly here, and
that is the point.

## The three metrics

| Metric | What it is | Range | Best |
|----|----|----|----|
| `brier` | Mean squared difference between probability and outcome | 0 to 1 | 0 |
| `rmse` | Square root of the Brier score | 0 to 1 | 0 |
| `logloss` | Mean negative log likelihood | 0 upward | 0 |

The Brier score and the RMSE are the same information on two scales. Log
loss punishes confident mistakes much harder: a probability of 0.01 on
something that turns out positive costs far more than a probability of
0.4 does.

``` r

knitr::kable(head(prob_metrics(mdat)))
```

| modnames | dsids | metrics |    values |
|:---------|------:|:--------|----------:|
| poor_er  |     1 | brier   | 0.1882575 |
| poor_er  |     1 | rmse    | 0.4338865 |
| poor_er  |     1 | logloss | 0.5835319 |
| good_er  |     1 | brier   | 0.1832143 |
| good_er  |     1 | rmse    | 0.4280354 |
| good_er  |     1 | logloss | 0.5718492 |

One row per model, dataset and metric.

## D2: how much of the loss the model explains

A Brier score of 0.12 is hard to read on its own. Whether it is good
depends on the prevalence: on a dataset that is 1% positive, predicting
0.01 for every case and thinking about nothing else already scores
0.0099.

A D2 score answers that by dividing the loss by the loss of exactly that
null model, the one that predicts the observed prevalence and ignores
the scores:

| Metric       | Formula                   | Range   | Best |
|--------------|---------------------------|---------|------|
| `d2_brier`   | 1 - brier / (p x (1 - p)) | up to 1 | 1    |
| `d2_logloss` | 1 - logloss / entropy(p)  | up to 1 | 1    |

where p is the observed prevalence, so the denominator in each row is
what the null model itself scores.

It reads like R-squared. `1` is a perfect model, `0` is one that does no
better than knowing the prevalence, and a negative value is a model that
does worse than that - which happens, and is not clipped away.

``` r

knitr::kable(head(prob_metrics(mdat, metrics = "all"), 5))
```

| modnames | dsids | metrics    |    values |
|:---------|------:|:-----------|----------:|
| poor_er  |     1 | brier      | 0.1882575 |
| poor_er  |     1 | rmse       | 0.4338865 |
| poor_er  |     1 | logloss    | 0.5835319 |
| poor_er  |     1 | d2_brier   | 0.2469701 |
| poor_er  |     1 | d2_logloss | 0.1581414 |

`metrics =` is how they are asked for, either by name or with `"all"`.
The three metrics above are always returned, so a call that did not ask
for the D2 scores gets exactly what it always got.

## Confidence intervals

[`prob_metrics_ci()`](https://evalclass.github.io/precrec/reference/prob_metrics_ci.md)
summarizes those per-dataset values into an interval for each model and
metric.

``` r

knitr::kable(prob_metrics_ci(mdat))
```

| modnames | metrics |      mean |     error | lower_bound | upper_bound |   n |
|:---------|:--------|----------:|----------:|------------:|------------:|----:|
| poor_er  | brier   | 0.2010725 | 0.0230592 |   0.1780133 |   0.2241317 |   4 |
| poor_er  | rmse    | 0.4478294 | 0.0258360 |   0.4219934 |   0.4736654 |   4 |
| poor_er  | logloss | 0.6248149 | 0.0621295 |   0.5626854 |   0.6869444 |   4 |
| good_er  | brier   | 0.1907915 | 0.0084085 |   0.1823830 |   0.1992000 |   4 |
| good_er  | rmse    | 0.4367152 | 0.0095584 |   0.4271568 |   0.4462736 |   4 |
| good_er  | logloss | 0.6003447 | 0.0258281 |   0.5745166 |   0.6261727 |   4 |

Like
[`auc_ci()`](https://evalclass.github.io/precrec/reference/auc_ci.md),
it takes `alpha` for the level and `dtype` for the normal or t
distribution.

## When to use them

Use them when a downstream decision consumes the probability itself - an
expected-cost calculation, a threshold set from a risk budget, a model
whose output feeds another model. Then calibration matters as much as
ranking.

Use ROC and precision-recall curves when the decision is a ranking or a
cutoff. A well-ranked, badly calibrated model is fine for those, and
these metrics will tell you it is bad.

Reporting both is reasonable, and they answer different questions.

## Also available per cutoff

`sar` averages accuracy, the ROC area and one minus the RMSE into a
single per-cutoff measure. It is on the [ranking and
cost](https://evalclass.github.io/precrec/articles/measures-ranking-cost.md)
page, and it needs probabilities for the same reason these do.

## Next

- [AUC and other curve
  summaries](https://evalclass.github.io/precrec/articles/measures-auc.md)
- [Measures
  overview](https://evalclass.github.io/precrec/articles/measures-overview.md)
