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

## Next

- [Compare several
  models](https://evalclass.github.io/precrec/articles/howto-multiple-models.md)
- [Average over several test
  sets](https://evalclass.github.io/precrec/articles/howto-multiple-test-sets.md)
- [Evaluate cross-validation
  folds](https://evalclass.github.io/precrec/articles/howto-cross-validation.md)
