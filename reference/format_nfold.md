# Create n-fold cross validation dataset from data frame

The `format_nfold` function takes a data frame with scores, label, and
n-fold columns and convert it to a list for
[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
and
[`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md).

## Usage

``` r
format_nfold(nfold_df, score_cols, lab_col, fold_col)
```

## Arguments

- nfold_df:

  A data frame that contains at least one score column, label and fold
  columns.

- score_cols:

  A character/numeric vector that specifies score columns of `nfold_df`.

- lab_col:

  A number/string that specifies the label column of `nfold_df`.

- fold_col:

  A number/string that specifies the fold column of `nfold_df`.

## Value

The `format_nfold` function returns a list that contains multiple scores
and labels.

## See also

[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
for calculation evaluation measures.
[`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
for formatting input data.
[`join_scores()`](https://evalclass.github.io/precrec/reference/join_scores.md)
and
[`join_labels()`](https://evalclass.github.io/precrec/reference/join_labels.md)
for formatting scores and labels with multiple datasets.

## Examples

``` r

##################################################
### Convert dataframe with 2 models and 5-fold datasets
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

## Convert with format_nfold
nfold_list1 <- format_nfold(
  nfold_df = M2N50F5, score_cols = c(1, 2),
  lab_col = 3, fold_col = 4
)

## Show the list structure
str(nfold_list1)
#> List of 2
#>  $ scores:List of 10
#>   ..$ : num [1:10] 2.061 1.56 0.215 1.264 0.477 ...
#>   ..$ : num [1:10] -1.843 -0.386 -0.156 -1.134 -0.515 ...
#>   ..$ : num [1:10] 0.307 -0.604 -0.319 -1.156 1.687 ...
#>   ..$ : num [1:10] 1.539 1.231 -1.101 -0.371 0.843 ...
#>   ..$ : num [1:10] -0.223 -0.768 0.464 0.42 -0.34 ...
#>   ..$ : num [1:10] 1.0689 -1.5666 -1.0737 0.0649 -0.3662 ...
#>   ..$ : num [1:10] 0.3945 0.6563 -0.0559 0.3991 0.3931 ...
#>   ..$ : num [1:10] 0.175 1.157 -0.864 2.065 0.415 ...
#>   ..$ : num [1:10] 1.443 0.876 0.826 -0.771 -1.165 ...
#>   ..$ : num [1:10] 0.607 -1.7908 0.2527 -1.3882 -0.0227 ...
#>  $ labels:List of 10
#>   ..$ : num [1:10] 2 2 2 2 2 1 1 1 1 1
#>   ..$ : num [1:10] 2 2 2 2 2 2 1 1 1 1
#>   ..$ : num [1:10] 2 2 2 2 2 1 1 1 1 1
#>   ..$ : num [1:10] 2 2 2 2 1 1 1 1 1 1
#>   ..$ : num [1:10] 2 2 2 2 2 1 1 1 1 1
#>   ..$ : num [1:10] 2 2 2 2 2 1 1 1 1 1
#>   ..$ : num [1:10] 2 2 2 2 2 2 1 1 1 1
#>   ..$ : num [1:10] 2 2 2 2 2 1 1 1 1 1
#>   ..$ : num [1:10] 2 2 2 2 1 1 1 1 1 1
#>   ..$ : num [1:10] 2 2 2 2 2 1 1 1 1 1
str(nfold_list1$scores)
#> List of 10
#>  $ : num [1:10] 2.061 1.56 0.215 1.264 0.477 ...
#>  $ : num [1:10] -1.843 -0.386 -0.156 -1.134 -0.515 ...
#>  $ : num [1:10] 0.307 -0.604 -0.319 -1.156 1.687 ...
#>  $ : num [1:10] 1.539 1.231 -1.101 -0.371 0.843 ...
#>  $ : num [1:10] -0.223 -0.768 0.464 0.42 -0.34 ...
#>  $ : num [1:10] 1.0689 -1.5666 -1.0737 0.0649 -0.3662 ...
#>  $ : num [1:10] 0.3945 0.6563 -0.0559 0.3991 0.3931 ...
#>  $ : num [1:10] 0.175 1.157 -0.864 2.065 0.415 ...
#>  $ : num [1:10] 1.443 0.876 0.826 -0.771 -1.165 ...
#>  $ : num [1:10] 0.607 -1.7908 0.2527 -1.3882 -0.0227 ...
str(nfold_list1$labels)
#> List of 10
#>  $ : num [1:10] 2 2 2 2 2 1 1 1 1 1
#>  $ : num [1:10] 2 2 2 2 2 2 1 1 1 1
#>  $ : num [1:10] 2 2 2 2 2 1 1 1 1 1
#>  $ : num [1:10] 2 2 2 2 1 1 1 1 1 1
#>  $ : num [1:10] 2 2 2 2 2 1 1 1 1 1
#>  $ : num [1:10] 2 2 2 2 2 1 1 1 1 1
#>  $ : num [1:10] 2 2 2 2 2 2 1 1 1 1
#>  $ : num [1:10] 2 2 2 2 2 1 1 1 1 1
#>  $ : num [1:10] 2 2 2 2 1 1 1 1 1 1
#>  $ : num [1:10] 2 2 2 2 2 1 1 1 1 1


##################################################
### Speficy a single score column
###

## Convert with format_nfold
nfold_list2 <- format_nfold(
  nfold_df = M2N50F5, score_cols = 1,
  lab_col = 3, fold_col = 4
)

## Show the list structure
str(nfold_list2)
#> List of 2
#>  $ scores:List of 5
#>   ..$ : num [1:10] 2.061 1.56 0.215 1.264 0.477 ...
#>   ..$ : num [1:10] -1.843 -0.386 -0.156 -1.134 -0.515 ...
#>   ..$ : num [1:10] 0.307 -0.604 -0.319 -1.156 1.687 ...
#>   ..$ : num [1:10] 1.539 1.231 -1.101 -0.371 0.843 ...
#>   ..$ : num [1:10] -0.223 -0.768 0.464 0.42 -0.34 ...
#>  $ labels:List of 5
#>   ..$ : num [1:10] 2 2 2 2 2 1 1 1 1 1
#>   ..$ : num [1:10] 2 2 2 2 2 2 1 1 1 1
#>   ..$ : num [1:10] 2 2 2 2 2 1 1 1 1 1
#>   ..$ : num [1:10] 2 2 2 2 1 1 1 1 1 1
#>   ..$ : num [1:10] 2 2 2 2 2 1 1 1 1 1
str(nfold_list2$scores)
#> List of 5
#>  $ : num [1:10] 2.061 1.56 0.215 1.264 0.477 ...
#>  $ : num [1:10] -1.843 -0.386 -0.156 -1.134 -0.515 ...
#>  $ : num [1:10] 0.307 -0.604 -0.319 -1.156 1.687 ...
#>  $ : num [1:10] 1.539 1.231 -1.101 -0.371 0.843 ...
#>  $ : num [1:10] -0.223 -0.768 0.464 0.42 -0.34 ...
str(nfold_list2$labels)
#> List of 5
#>  $ : num [1:10] 2 2 2 2 2 1 1 1 1 1
#>  $ : num [1:10] 2 2 2 2 2 2 1 1 1 1
#>  $ : num [1:10] 2 2 2 2 2 1 1 1 1 1
#>  $ : num [1:10] 2 2 2 2 1 1 1 1 1 1
#>  $ : num [1:10] 2 2 2 2 2 1 1 1 1 1


##################################################
### Use column names
###

## Convert with format_nfold
nfold_list3 <- format_nfold(
  nfold_df = M2N50F5,
  score_cols = c("score1", "score2"),
  lab_col = "label", fold_col = "fold"
)

## Show the list structure
str(nfold_list3)
#> List of 2
#>  $ scores:List of 10
#>   ..$ : num [1:10] 2.061 1.56 0.215 1.264 0.477 ...
#>   ..$ : num [1:10] -1.843 -0.386 -0.156 -1.134 -0.515 ...
#>   ..$ : num [1:10] 0.307 -0.604 -0.319 -1.156 1.687 ...
#>   ..$ : num [1:10] 1.539 1.231 -1.101 -0.371 0.843 ...
#>   ..$ : num [1:10] -0.223 -0.768 0.464 0.42 -0.34 ...
#>   ..$ : num [1:10] 1.0689 -1.5666 -1.0737 0.0649 -0.3662 ...
#>   ..$ : num [1:10] 0.3945 0.6563 -0.0559 0.3991 0.3931 ...
#>   ..$ : num [1:10] 0.175 1.157 -0.864 2.065 0.415 ...
#>   ..$ : num [1:10] 1.443 0.876 0.826 -0.771 -1.165 ...
#>   ..$ : num [1:10] 0.607 -1.7908 0.2527 -1.3882 -0.0227 ...
#>  $ labels:List of 10
#>   ..$ : num [1:10] 2 2 2 2 2 1 1 1 1 1
#>   ..$ : num [1:10] 2 2 2 2 2 2 1 1 1 1
#>   ..$ : num [1:10] 2 2 2 2 2 1 1 1 1 1
#>   ..$ : num [1:10] 2 2 2 2 1 1 1 1 1 1
#>   ..$ : num [1:10] 2 2 2 2 2 1 1 1 1 1
#>   ..$ : num [1:10] 2 2 2 2 2 1 1 1 1 1
#>   ..$ : num [1:10] 2 2 2 2 2 2 1 1 1 1
#>   ..$ : num [1:10] 2 2 2 2 2 1 1 1 1 1
#>   ..$ : num [1:10] 2 2 2 2 1 1 1 1 1 1
#>   ..$ : num [1:10] 2 2 2 2 2 1 1 1 1 1
str(nfold_list3$scores)
#> List of 10
#>  $ : num [1:10] 2.061 1.56 0.215 1.264 0.477 ...
#>  $ : num [1:10] -1.843 -0.386 -0.156 -1.134 -0.515 ...
#>  $ : num [1:10] 0.307 -0.604 -0.319 -1.156 1.687 ...
#>  $ : num [1:10] 1.539 1.231 -1.101 -0.371 0.843 ...
#>  $ : num [1:10] -0.223 -0.768 0.464 0.42 -0.34 ...
#>  $ : num [1:10] 1.0689 -1.5666 -1.0737 0.0649 -0.3662 ...
#>  $ : num [1:10] 0.3945 0.6563 -0.0559 0.3991 0.3931 ...
#>  $ : num [1:10] 0.175 1.157 -0.864 2.065 0.415 ...
#>  $ : num [1:10] 1.443 0.876 0.826 -0.771 -1.165 ...
#>  $ : num [1:10] 0.607 -1.7908 0.2527 -1.3882 -0.0227 ...
str(nfold_list3$labels)
#> List of 10
#>  $ : num [1:10] 2 2 2 2 2 1 1 1 1 1
#>  $ : num [1:10] 2 2 2 2 2 2 1 1 1 1
#>  $ : num [1:10] 2 2 2 2 2 1 1 1 1 1
#>  $ : num [1:10] 2 2 2 2 1 1 1 1 1 1
#>  $ : num [1:10] 2 2 2 2 2 1 1 1 1 1
#>  $ : num [1:10] 2 2 2 2 2 1 1 1 1 1
#>  $ : num [1:10] 2 2 2 2 2 2 1 1 1 1
#>  $ : num [1:10] 2 2 2 2 2 1 1 1 1 1
#>  $ : num [1:10] 2 2 2 2 1 1 1 1 1 1
#>  $ : num [1:10] 2 2 2 2 2 1 1 1 1 1
```
