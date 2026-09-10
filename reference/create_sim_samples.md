# Create random samples for simulations

The `create_sim_samples` function generates random samples with
different performance levels.

## Usage

``` r
create_sim_samples(n_repeat, np, nn, score_names = "random")
```

## Arguments

- n_repeat:

  The number of iterations to make samples.

- np:

  The number of positives in a sample.

- nn:

  The number of negatives in a sample.

- score_names:

  A character vector for the names of the following performance levels.

  "random"

  :   Random

  "poor_er"

  :   Poor early retrieval

  "good_er"

  :   Good early retrieval

  "excel"

  :   Excellent

  "perf"

  :   Perfect

  "all"

  :   All of the above

## Value

The `create_sim_samples` function returns a list with the following
items.

- scores: a list of numeric vectors

- labels: an integer vector

- modnames: a character vector of the model names

- dsids: a character vector of the dataset IDs

## See also

[`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
for formatting input data.
[`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
for calculation evaluation metrics.

## Examples

``` r

##################################################
### Create a set of samples with 10 positives and 10 negatives
### for the random performance level
###
samps1 <- create_sim_samples(1, 10, 10, "random")

## Show the list structure
str(samps1)
#> List of 4
#>  $ scores  :List of 1
#>   ..$ :List of 1
#>   .. ..$ : num [1:20] 0.2985 0.0636 0.4066 -0.2822 1.534 ...
#>  $ labels  : num [1:20] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ modnames: chr "random"
#>  $ dsids   : int 1


##################################################
### Create two sets of samples with 10 positives and 20 negatives
### for the random and the poor early retrieval performance levels
###
samps2 <- create_sim_samples(2, 10, 20, c("random", "poor_er"))

## Show the list structure
str(samps2)
#> List of 4
#>  $ scores  :List of 2
#>   ..$ :List of 2
#>   .. ..$ : num [1:30] 0.995 -0.53 -0.14 -1.954 1.226 ...
#>   .. ..$ : num [1:30] 0.854 0.981 0.79 0.764 0.94 ...
#>   ..$ :List of 2
#>   .. ..$ : num [1:30] 0.75622 1.04398 -0.03706 -0.00101 -0.61282 ...
#>   .. ..$ : num [1:30] 0.897 0.517 0.851 0.636 0.748 ...
#>  $ labels  : num [1:30] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ modnames: chr [1:4] "random" "poor_er" "random" "poor_er"
#>  $ dsids   : int [1:4] 1 1 2 2


##################################################
### Create 3 sets of samples with 5 positives and 5 negatives
### for all 5 levels
###
samps3 <- create_sim_samples(3, 5, 5, "all")

## Show the list structure
str(samps3)
#> List of 4
#>  $ scores  :List of 3
#>   ..$ :List of 5
#>   .. ..$ : num [1:10] 1.48 -0.348 0.167 -0.729 1.345 ...
#>   .. ..$ : num [1:10] 0.363 0.771 0.715 0.662 0.639 ...
#>   .. ..$ : num [1:10] 0.185 0.257 0.396 0.824 0.356 ...
#>   .. ..$ : num [1:10] 2.93 1.79 4.45 3.25 3.76 ...
#>   .. ..$ : num [1:10] 1 1 1 1 1 0 0 0 0 0
#>   ..$ :List of 5
#>   .. ..$ : num [1:10] 1.269 1.348 -0.174 -0.374 -0.758 ...
#>   .. ..$ : num [1:10] 0.475 0.654 0.491 0.594 0.807 ...
#>   .. ..$ : num [1:10] 0.9517 0.7369 0.0151 0.826 0.9134 ...
#>   .. ..$ : num [1:10] 2.08 1.29 2 2.19 4.56 ...
#>   .. ..$ : num [1:10] 1 1 1 1 1 0 0 0 0 0
#>   ..$ :List of 5
#>   .. ..$ : num [1:10] -0.9849 1.2172 -1.3595 -0.0165 0.5207 ...
#>   .. ..$ : num [1:10] 0.692 0.804 0.721 0.461 0.713 ...
#>   .. ..$ : num [1:10] 0.0487 0.7394 0.7452 0.6935 0.0906 ...
#>   .. ..$ : num [1:10] 2.29 1.33 2.6 2.04 3.65 ...
#>   .. ..$ : num [1:10] 1 1 1 1 1 0 0 0 0 0
#>  $ labels  : num [1:10] 1 1 1 1 1 0 0 0 0 0
#>  $ modnames: chr [1:15] "random" "poor_er" "good_er" "excel" ...
#>  $ dsids   : int [1:15] 1 1 1 1 1 2 2 2 2 2 ...
```
