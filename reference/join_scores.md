# Join scores of multiple models into a list

The `join_scores` function takes predicted scores from multiple models
and converts them to a list.

## Usage

``` r
join_scores(..., byrow = FALSE, chklen = TRUE)
```

## Arguments

- ...:

  Multiple datasets. They can be vectors, arrays, matrices, data frames,
  and lists.

- byrow:

  A Boolean value to specify whether row vectors are used for matrix,
  data frame, and array.

- chklen:

  A Boolean value to specify whether all list items must be the same
  lengths.

## Value

The `join_scores` function returns a list that contains all combined
score data.

## See also

[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
for calculation evaluation metrics.
[`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
for formatting input data.
[`join_labels()`](https://evalclass.github.io/precrec/reference/join_labels.md)
for formatting labels with multiple datasets.

## Examples

``` r

##################################################
### Add three numeric vectors
###
s1 <- c(1, 2, 3, 4)
s2 <- c(5, 6, 7, 8)
s3 <- c(2, 4, 6, 8)
scores1 <- join_scores(s1, s2, s3)

## Show the list structure
str(scores1)
#> List of 3
#>  $ : num [1:4] 1 2 3 4
#>  $ : num [1:4] 5 6 7 8
#>  $ : num [1:4] 2 4 6 8


##################################################
### Add a matrix and a numeric vector
###
a1 <- matrix(seq(8), 4, 2)
scores2 <- join_scores(a1, s3)

## Show the list structure
str(scores2)
#> List of 3
#>  $ : int [1:4] 1 2 3 4
#>  $ : int [1:4] 5 6 7 8
#>  $ : num [1:4] 2 4 6 8


##################################################
### Use byrow
###
a2 <- matrix(seq(8), 2, 4, byrow = TRUE)
scores3 <- join_scores(a2, s3, byrow = TRUE)

## Show the list structure
str(scores3)
#> List of 3
#>  $ : int [1:4] 1 2 3 4
#>  $ : int [1:4] 5 6 7 8
#>  $ : num [1:4] 2 4 6 8


##################################################
### Use chklen
###
s4 <- c(1, 2, 3)
s5 <- c(5, 6, 7, 8)
scores4 <- join_scores(s4, s5, chklen = FALSE)

## Show the list structure
str(scores4)
#> List of 2
#>  $ : num [1:3] 1 2 3
#>  $ : num [1:4] 5 6 7 8
```
