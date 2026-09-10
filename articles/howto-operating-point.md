# Choose an operating point

Almost everything else here is threshold-free. A curve is every cutoff
at once and an AUC summarizes all of them, which is what makes them fair
ways to compare models. Shipping one means giving that up: something has
to turn a score into a decision, and that something is a threshold.

[`best_cutoff()`](https://evalclass.github.io/precrec/reference/best_cutoff.md)
picks it.

``` r

library(precrec)

set.seed(1)
scores <- c(rnorm(20, 1.2), rnorm(380, 0))
labels <- rep(c(1, 0), c(20, 380))
mdat <- mmdata(scores, labels)

best_cutoff(mdat)
#>   modname dsid       metric     value rank normalized_rank    score label error
#> 1      m1    1 informedness 0.5789474   60            0.15 1.155066     1  0.13
#>   accuracy specificity sensitivity precision       mcc fscore balanced_accuracy
#> 1     0.87   0.8789474         0.7 0.2333333 0.3533709   0.35         0.7894737
#>         npv informedness markedness     kappa
#> 1 0.9823529    0.5789474  0.2156863 0.2972973
```

## What comes back is a row, not a number

The threshold alone would not tell you whether to accept it. What comes
back is a row of
[`metric_table()`](https://evalclass.github.io/precrec/articles/howto-results-as-data.md),
so every other metric is there to be read at the same cutoff:

``` r

picked <- best_cutoff(mdat, metric = "mcc")

knitr::kable(
  picked[, c(
    "metric", "value", "rank", "score",
    "sensitivity", "specificity", "precision"
  )],
  digits = 3
)
```

| metric | value | rank | score | sensitivity | specificity | precision |
|:-------|------:|-----:|------:|------------:|------------:|----------:|
| mcc    | 0.376 |   35 |  1.53 |        0.55 |       0.937 |     0.314 |

`rank` is how many of the 400 instances that threshold calls positive,
and `score` is the cutoff itself - an observed score, with the rule
being `score >=` it. Reading the precision beside the criterion is the
point of returning the whole row: a cutoff can optimize what you asked
for and still be one you would not deploy.

## The criteria come in two groups

``` r

criteria <- c("youden", "topleft", "fscore", "mcc")
shown <- c("metric", "rank", "sensitivity", "specificity", "precision")

knitr::kable(
  do.call(rbind, lapply(criteria, function(m) {
    best_cutoff(mdat, metric = m)[, shown]
  })),
  row.names = FALSE, digits = 3
)
```

| metric       | rank | sensitivity | specificity | precision |
|:-------------|-----:|------------:|------------:|----------:|
| informedness |   60 |        0.70 |       0.879 |     0.233 |
| roc_dist     |   87 |        0.75 |       0.811 |     0.172 |
| fscore       |   35 |        0.55 |       0.937 |     0.314 |
| mcc          |   35 |        0.55 |       0.937 |     0.314 |

Youden’s J and the closest point to the top left corner are computed
from sensitivity and specificity alone. Both of those are conditioned on
the true class, so neither knows how rare the positives are, and both
are free to buy sensitivity with false positives that cost them nothing
in the number being maximized. `fscore` and `mcc` are computed from
precision as well, so the prevalence is in what they optimize.

|                      |                               |
|----------------------|-------------------------------|
| **Prevalence-blind** | `"youden"`, `"topleft"`       |
| **Prevalence-aware** | `"fscore"`, `"mcc"`, `"cost"` |

The default is `"youden"`, because that is what
`pROC::coords(x, "best")` does and most people arrive expecting it. On
imbalanced data it is the wrong default, and [Balanced and imbalanced
data](https://evalclass.github.io/precrec/articles/howto-imbalanced-data.html#so-does-the-threshold)
shows the two groups landing nowhere near each other on the same model.

Any metric
[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
calculates can be named, not only these five. The direction is a
property of the metric rather than an argument - `error`, `cost`,
`roc_dist`, the two error rates and the negative likelihood ratio are
minimized and the rest are maximized - so `metric = "accuracy"` means
the cutoff that maximizes accuracy without your having to say so.

Two of them are traps rather than criteria. `sensitivity` is largest
when everything is called positive and `specificity` when nothing is;
both are the correct answer to the question asked, and no warning says
that it was probably not the question meant.

## Saying what the mistakes cost

`"mcc"` is a reasonable default when you have nothing better to say.
When you do have something better to say, say it: `"cost"` minimizes
`cost_fp * FP + cost_fn * FN`, and the ratio between the two is the only
part that matters.

``` r

costly <- lapply(c(1, 5, 20), function(fn) {
  cbind(
    cost_fn = fn,
    best_cutoff(
      mdat,
      metric = "cost", cost_fp = 1, cost_fn = fn
    )[, c("rank", "score", "sensitivity", "precision")]
  )
})

knitr::kable(do.call(rbind, costly), row.names = FALSE, digits = 3)
```

| cost_fn | rank | score | sensitivity | precision |
|--------:|-----:|------:|------------:|----------:|
|       1 |    2 | 2.712 |        0.10 |     1.000 |
|       5 |   35 | 1.530 |        0.55 |     0.314 |
|      20 |  163 | 0.364 |        0.95 |     0.117 |

A missed positive costing twenty false alarms moves the threshold a long
way down, and it should. This is the criterion to reach for when the two
mistakes are genuinely different, because it is the only one that lets
you write down how different.

The first row is the warning that comes with it. Equal costs make the
total cost the error count, so minimizing it is maximizing accuracy -
and where positives are five percent of the data, calling almost nothing
positive is very nearly the most accurate thing a classifier can do.
`cost_fp = cost_fn` is not a neutral choice on imbalanced data. It is a
strong claim, and usually not the one intended.

## When scores are tied

A run of tied scores gets one row per instance in
[`metric_table()`](https://evalclass.github.io/precrec/reference/metric_table.md),
and by default those rows split the run’s positives and negatives
between them. That is what the curves need, and it is not what a
threshold does: no threshold can separate two instances that carry the
same score.

[`best_cutoff()`](https://evalclass.github.io/precrec/reference/best_cutoff.md)
takes care of this. Rows sharing a `score` are one threshold seen
several times rather than several cutoffs, so the `rank` reported is the
last of the run - the number that `score >=` that value actually calls
positive.

``` r

binned <- round(scores)
tied <- best_cutoff(mmdata(binned, labels), metric = "mcc")

c(rank = tied$rank, applied = sum(binned >= tied$score))
#>    rank applied 
#>      37      37
```

`basic_ties = "hold"` gives every cutoff in a run the counts of the
whole run, which is the policy `pROC` and `ROCR` use. Pass it when you
want [the table
underneath](https://evalclass.github.io/precrec/articles/howto-from-proc-rocr.html#tied-scores-produce-extra-rows)
to line up with theirs row for row. It rarely changes which cutoff comes
back, because the two policies agree at the edges of a run and that is
where the optimum tends to sit.

## Several models, several test sets

There is one row per model per test dataset, and nothing is averaged. A
cutoff belongs to the data it was read off.

``` r

folds <- create_sim_samples(5, 20, 380, "good_er")
fdat <- mmdata(folds[["scores"]], folds[["labels"]],
  modnames = folds[["modnames"]], dsids = folds[["dsids"]]
)

knitr::kable(
  best_cutoff(fdat, metric = "mcc")[, c(
    "dsid", "value", "rank", "score", "precision"
  )],
  row.names = FALSE, digits = 3
)
```

| dsid | value | rank | score | precision |
|:-----|------:|-----:|------:|----------:|
| 1    | 0.293 |   17 | 0.578 |     0.353 |
| 2    | 0.582 |    7 | 0.716 |     1.000 |
| 3    | 0.429 |    9 | 0.661 |     0.667 |
| 4    | 0.518 |   14 | 0.666 |     0.643 |
| 5    | 0.593 |   11 | 0.671 |     0.818 |

Five test sets from the same generator, and the cutoffs do not agree.
That spread is the sampling noise in the choice, and it is worth looking
at before quoting any one of the numbers: on a small or imbalanced test
set it is usually larger than people expect. Averaging the `score`
column is not obviously the right summary either - across models the
scores are not even on a common scale - so the `rank`, or the value of
the criterion, tends to travel better.

## The number you quote will be optimistic

A threshold chosen on the same data the model is scored on is flattered
by however much the criterion was free to chase, in exactly the way a
model selected on its test set is. The fix is the ordinary one - choose
the cutoff on data the model has not seen, and report the metrics at
that fixed cutoff on data neither the model nor the cutoff has seen.

`precrec` does not do this for you. Nothing here resamples or
cross-validates the choice, and
[`best_cutoff()`](https://evalclass.github.io/precrec/reference/best_cutoff.md)
will happily hand back the optimum of whatever you give it. With
[cross-validation
folds](https://evalclass.github.io/precrec/articles/howto-cross-validation.md)
already in hand, choosing on some folds and reporting on the rest is a
few lines of ordinary R.

## Next

- [Balanced and imbalanced
  data](https://evalclass.github.io/precrec/articles/howto-imbalanced-data.md) -
  why the criterion matters more than it looks
- [Classification
  report](https://evalclass.github.io/precrec/articles/metrics-classification-report.md) -
  the metrics at a threshold you have already chosen
- [Get the numbers
  out](https://evalclass.github.io/precrec/articles/howto-results-as-data.md) -
  the table this takes a row of
