# Join observed labels of multiple test datasets into a list

`join_labels` takes observed labels and converts them to a list.

## Usage

``` r
join_labels(..., byrow = FALSE, chklen = TRUE)
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

The `join_labels` function returns a list that contains all combined
label data.

## See also

[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
for calculation evaluation measures.
[`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
for formatting input data.
[`join_scores()`](https://evalclass.github.io/precrec/reference/join_scores.md)
for formatting scores with multiple datasets.

## Examples

``` r

##################################################
### Add three numeric vectors
###
l1 <- c(1, 0, 1, 1)
l2 <- c(1, 1, 0, 0)
l3 <- c(0, 1, 0, 1)
labels1 <- join_labels(l1, l2, l3)

## Show the list structure
str(labels1)
#> List of 3
#>  $ : num [1:4] 1 0 1 1
#>  $ : num [1:4] 1 1 0 0
#>  $ : num [1:4] 0 1 0 1


##################################################
### Add a matrix and a numeric vector
###
a1 <- matrix(rep(c(1, 0), 4), 4, 2)
labels2 <- join_labels(a1, l3)

## Show the list structure
str(labels2)
#> List of 3
#>  $ : num [1:4] 1 0 1 0
#>  $ : num [1:4] 1 0 1 0
#>  $ : num [1:4] 0 1 0 1


##################################################
### Use byrow
###
a2 <- matrix(rep(c(1, 0), 4), 2, 4, byrow = TRUE)
labels3 <- join_labels(a2, l3, byrow = TRUE)

## Show the list structure
str(labels3)
#> List of 3
#>  $ : num [1:4] 1 0 1 0
#>  $ : num [1:4] 1 0 1 0
#>  $ : num [1:4] 0 1 0 1


##################################################
### Use chklen
###
l4 <- c(-1, 0, -1)
l5 <- c(0, -1)
labels4 <- join_labels(l4, l5, chklen = FALSE)

## Show the list structure
str(labels4)
#> List of 2
#>  $ : num [1:3] -1 0 -1
#>  $ : num [1:2] 0 -1
```
