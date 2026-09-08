# Reformat input data for performance evaluation calculation

The `mmdata` function takes predicted scores and labels and returns an
`mdat` object. The
[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
function takes an `mdat` object as input data to calculate evaluation
metrics.

## Usage

``` r
mmdata(
  scores,
  labels,
  modnames = NULL,
  dsids = NULL,
  posclass = NULL,
  na_worst = TRUE,
  ties_method = "equiv",
  expd_first = NULL,
  mode = "rocprc",
  multiclass = NULL,
  nfold_df = NULL,
  score_cols = NULL,
  lab_col = NULL,
  fold_col = NULL,
  ...
)
```

## Arguments

- scores:

  A numeric dataset of predicted scores. It can be a vector, a matrix,
  an array, a data frame, or a list. The
  [`join_scores()`](https://evalclass.github.io/precrec/reference/join_scores.md)
  function can be useful to make scores with multiple datasets.

- labels:

  A numeric, character, logical, or factor dataset of observed labels.
  It can be a vector, a matrix, an array, a data frame, or a list. The
  [`join_labels()`](https://evalclass.github.io/precrec/reference/join_labels.md)
  function can be useful to make labels with multiple datasets.

- modnames:

  A character vector for the names of the models. The `evalmod` function
  automatically generates default names as "m1", "m2", "m3", and so on
  when it is `NULL`.

- dsids:

  A numeric vector for test dataset IDs. The `evalmod` function
  automatically generates the default ID as `1` when it is `NULL`.

- posclass:

  A scalar value to specify the label of positives in `labels`. It must
  be the same data type as `labels`. For example, `posclass = -1`
  changes the positive label from `1` to `-1` when `labels` contains `1`
  and `-1`. The positive label will be automatically detected when
  `posclass` is `NULL`.

- na_worst:

  A Boolean value for controlling the treatment of NAs in `scores`.

  TRUE

  :   All NAs are treated as the worst scores

  FALSE

  :   All NAs are treated as the best scores

- ties_method:

  A string for controlling ties in `scores`.

  "equiv"

  :   Ties are equivalently ranked

  "first"

  :   Ties are ranked in an increasing order as appeared

  "random"

  :   Ties are ranked in random order

- expd_first:

  A string to indicate which of the two variables - model names or test
  dataset IDs should be expanded first when they are automatically
  generated.

  "modnames"

  :   Model names are expanded first. For example, The `mmdata` function
      generates `modnames` as `c("m1", "m2")` and `dsids` as `c(1, 1)`
      when two vectors are passed as input, and `modnames` and `dsids`
      are unspecified.

  "dsids"

  :   Test dataset IDs are expanded first. For example, The `mmdata`
      function generates `modnames` as `c("m1", "m1")` and `dsids` as
      `c(1, 2)` when two vectors are passed as input, and `modnames` and
      `dsids` are unspecified.

- mode:

  A string that specifies the types of evaluation metrics that the
  `evalmod` function calculates.

  "rocprc"

  :   ROC and Precision-Recall curves

  "prcroc"

  :   Same as above

  "basic"

  :   Normalized ranks vs. accuracy, error rate, specificity,
      sensitivity, precision, Matthews correlation coefficient, and
      F-score.

  "aucroc"

  :   Fast AUC(ROC) calculation with the U statistic

- multiclass:

  A string that specifies how a dataset with more than two classes is
  evaluated.

  "none"

  :   Binary evaluation. Labels with more than two classes are an error,
      as they have always been.

  "ovr"

  :   One-vs-rest. Each class becomes its own binary problem - that
      class against all the others - and the `K` decompositions are
      carried on the model axis, so that they behave like `K` models
      evaluated on one dataset.

  `multiclass` is detected from the input when it is `NULL`: "ovr" when
  `labels` hold more than two classes and `scores` is a matrix or a data
  frame with one column per class, "none" otherwise. `scores` columns
  are matched to classes by their column names when those name the
  classes, and by position otherwise.

  `posclass` is ignored in one-vs-rest mode, and `multiclass` cannot be
  combined with `nfold_df`.

  Note that each one-vs-rest decomposition has its own class balance, so
  the baseline of a precision-recall curve differs from class to class.
  The plots leave the baseline out for that reason.

- nfold_df:

  A data frame that contains at least one score column, label and fold
  columns.

- score_cols:

  A character/numeric vector that specifies score columns of `nfold_df`.

- lab_col:

  A number/string that specifies the label column of `nfold_df`.

- fold_col:

  A number/string that specifies the fold column of `nfold_df`.

- ...:

  Not used by this method.

## Value

The `mmdata` function returns an `mdat` object that contains formatted
labels and score ranks. The object can be used as input data for the
[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
function.

## See also

[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
for calculation evaluation metrics.
[`join_scores()`](https://evalclass.github.io/precrec/reference/join_scores.md)
and
[`join_labels()`](https://evalclass.github.io/precrec/reference/join_labels.md)
for formatting scores and labels with multiple datasets.
[`format_nfold()`](https://evalclass.github.io/precrec/reference/format_nfold.md)
for creating n-fold cross validation dataset from data frame.

## Examples

``` r

##################################################
### Single model & single test dataset
###

## Load a dataset with 10 positives and 10 negatives
data(P10N10)

## Generate mdat object
ssmdat1 <- mmdata(P10N10$scores, P10N10$labels)
ssmdat1
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1         m1          1             10             10
#> 
ssmdat2 <- mmdata(1:8, sample(c(0, 1), 8, replace = TRUE))
ssmdat2
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1         m1          1              4              4
#> 


##################################################
### Multiple models & single test dataset
###

## Create sample datasets with 100 positives and 100 negatives
samps <- create_sim_samples(1, 100, 100, "all")

## Multiple models & single test dataset
msmdat1 <- mmdata(samps[["scores"]], samps[["labels"]],
  modnames = samps[["modnames"]]
)
msmdat1
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1     random          1            100            100
#>    2    poor_er          1            100            100
#>    3    good_er          1            100            100
#>    4      excel          1            100            100
#>    5       perf          1            100            100
#> 

## Use join_scores and join_labels
s1 <- c(1, 2, 3, 4)
s2 <- c(5, 6, 7, 8)
scores <- join_scores(s1, s2)

l1 <- c(1, 0, 1, 1)
l2 <- c(1, 0, 1, 1)
labels <- join_labels(l1, l2)

msmdat2 <- mmdata(scores, labels, modnames = c("ms1", "ms2"))
msmdat2
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1        ms1          1              1              3
#>    2        ms2          1              1              3
#> 


##################################################
### Single model & multiple test datasets
###

## Create sample datasets with 100 positives and 100 negatives
samps <- create_sim_samples(10, 100, 100, "good_er")

## Single model & multiple test datasets
smmdat <- mmdata(samps[["scores"]], samps[["labels"]],
  modnames = samps[["modnames"]],
  dsids = samps[["dsids"]]
)
smmdat
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1    good_er          1            100            100
#>    2    good_er          2            100            100
#>    3    good_er          3            100            100
#>    4    good_er          4            100            100
#>    5    good_er          5            100            100
#>    6    good_er          6            100            100
#>    7    good_er          7            100            100
#>    8    good_er          8            100            100
#>    9    good_er          9            100            100
#>   10    good_er         10            100            100
#> 


##################################################
### Multiple models & multiple test datasets
###

## Create sample datasets with 100 positives and 100 negatives
samps <- create_sim_samples(10, 100, 100, "all")

## Multiple models & multiple test datasets
mmmdat <- mmdata(samps[["scores"]], samps[["labels"]],
  modnames = samps[["modnames"]],
  dsids = samps[["dsids"]]
)
mmmdat
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1     random          1            100            100
#>    2    poor_er          1            100            100
#>    3    good_er          1            100            100
#>    4      excel          1            100            100
#>    5       perf          1            100            100
#>    6     random          2            100            100
#>    7    poor_er          2            100            100
#>    8    good_er          2            100            100
#>    9      excel          2            100            100
#>   10       perf          2            100            100
#>   11     random          3            100            100
#>   12    poor_er          3            100            100
#>   13    good_er          3            100            100
#>   14      excel          3            100            100
#>   15       perf          3            100            100
#>   16     random          4            100            100
#>   17    poor_er          4            100            100
#>   18    good_er          4            100            100
#>   19      excel          4            100            100
#>   20       perf          4            100            100
#>   21     random          5            100            100
#>   22    poor_er          5            100            100
#>   23    good_er          5            100            100
#>   24      excel          5            100            100
#>   25       perf          5            100            100
#>   26     random          6            100            100
#>   27    poor_er          6            100            100
#>   28    good_er          6            100            100
#>   29      excel          6            100            100
#>   30       perf          6            100            100
#>   31     random          7            100            100
#>   32    poor_er          7            100            100
#>   33    good_er          7            100            100
#>   34      excel          7            100            100
#>   35       perf          7            100            100
#>   36     random          8            100            100
#>   37    poor_er          8            100            100
#>   38    good_er          8            100            100
#>   39      excel          8            100            100
#>   40       perf          8            100            100
#>   41     random          9            100            100
#>   42    poor_er          9            100            100
#>   43    good_er          9            100            100
#>   44      excel          9            100            100
#>   45       perf          9            100            100
#>   46     random         10            100            100
#>   47    poor_er         10            100            100
#>   48    good_er         10            100            100
#>   49      excel         10            100            100
#>   50       perf         10            100            100
#> 


##################################################
### N-fold cross validation datasets
###

## Load test data
data(M2N50F5)
head(M2N50F5)
#>       score1     score2 label fold
#> 1  2.0606025  1.0689227   pos    1
#> 2  0.3066092  0.1745491   pos    3
#> 3  1.5597733 -1.5666375   pos    1
#> 4 -0.6044989  1.1572727   pos    3
#> 5 -0.2229031  0.6070042   pos    5
#> 6 -0.7679551 -1.7908147   pos    5

## Speficy nessesary columns to create mdat
cvdat1 <- mmdata(
  nfold_df = M2N50F5, score_cols = c(1, 2),
  lab_col = 3, fold_col = 4,
  modnames = c("m1", "m2"), dsids = 1:5
)
cvdat1
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1         m1          1              5              5
#>    2         m1          2              4              6
#>    3         m1          3              5              5
#>    4         m1          4              6              4
#>    5         m1          5              5              5
#>    6         m2          1              5              5
#>    7         m2          2              4              6
#>    8         m2          3              5              5
#>    9         m2          4              6              4
#>   10         m2          5              5              5
#> 

## Use column names
cvdat2 <- mmdata(
  nfold_df = M2N50F5, score_cols = c("score1", "score2"),
  lab_col = "label", fold_col = "fold",
  modnames = c("m1", "m2"), dsids = 1:5
)
cvdat2
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID # of negatives # of positives
#>    1         m1          1              5              5
#>    2         m1          2              4              6
#>    3         m1          3              5              5
#>    4         m1          4              6              4
#>    5         m1          5              5              5
#>    6         m2          1              5              5
#>    7         m2          2              4              6
#>    8         m2          3              5              5
#>    9         m2          4              6              4
#>   10         m2          5              5              5
#> 


##################################################
### Multiclass dataset
###

## Load a 3-class dataset with one score column per class
data(C3N150)

## One-vs-rest decomposition, detected from the input
mcmdat <- mmdata(C3N150$scores, C3N150$labels)
mcmdat
#> 
#>     === Input data ===
#> 
#>      Model name Dataset ID Class # of negatives # of positives
#>    1         c1          1    c1            100             50
#>    2         c2          1    c2            100             50
#>    3         c3          1    c3            100             50
#> 
```
