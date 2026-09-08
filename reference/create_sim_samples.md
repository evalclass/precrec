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
#>   .. ..$ : num [1:20] -0.102 -1.393 0.179 -0.904 -0.786 ...
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
#>   .. ..$ : num [1:30] 0.00684 1.01706 -2.38566 -0.4368 0.2238 ...
#>   .. ..$ : num [1:30] 0.974 0.971 0.887 0.854 0.735 ...
#>   ..$ :List of 2
#>   .. ..$ : num [1:30] -0.204 0.453 -0.814 -1.12 0.62 ...
#>   .. ..$ : num [1:30] 0.541 0.958 0.894 0.861 0.89 ...
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
#>   .. ..$ : num [1:10] -0.782 -2.273 -0.441 1.434 0.315 ...
#>   .. ..$ : num [1:10] 0.754 0.783 0.817 0.773 0.805 ...
#>   .. ..$ : num [1:10] 0.6451 0.3523 0.9775 0.1324 0.0533 ...
#>   .. ..$ : num [1:10] 3.83 2.15 3.66 4.6 2.65 ...
#>   .. ..$ : num [1:10] 1 1 1 1 1 0 0 0 0 0
#>   ..$ :List of 5
#>   .. ..$ : num [1:10] -1.01 1.798 -0.449 0.533 0.301 ...
#>   .. ..$ : num [1:10] 0.965 0.921 0.824 0.905 0.37 ...
#>   .. ..$ : num [1:10] 0.0189 0.5185 0.4495 0.8216 0.9576 ...
#>   .. ..$ : num [1:10] 3.32 2.65 2.21 2.18 3.37 ...
#>   .. ..$ : num [1:10] 1 1 1 1 1 0 0 0 0 0
#>   ..$ :List of 5
#>   .. ..$ : num [1:10] 2.364 0.904 1.659 -1.155 0.795 ...
#>   .. ..$ : num [1:10] 0.538 0.76 0.914 0.826 0.878 ...
#>   .. ..$ : num [1:10] 0.0208 0.1253 0.2542 0.0158 0.2213 ...
#>   .. ..$ : num [1:10] 2.7 1.84 1.44 3.1 4.33 ...
#>   .. ..$ : num [1:10] 1 1 1 1 1 0 0 0 0 0
#>  $ labels  : num [1:10] 1 1 1 1 1 0 0 0 0 0
#>  $ modnames: chr [1:15] "random" "poor_er" "good_er" "excel" ...
#>  $ dsids   : int [1:15] 1 1 1 1 1 2 2 2 2 2 ...
```
