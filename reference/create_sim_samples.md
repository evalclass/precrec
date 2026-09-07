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
#>   .. ..$ : num [1:20] -1.412 -0.714 -0.359 -2.083 0.92 ...
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
#>   .. ..$ : num [1:30] 0.205 -0.322 0.384 -1.835 -0.437 ...
#>   .. ..$ : num [1:30] 0.875 0.81 0.654 0.924 0.851 ...
#>   ..$ :List of 2
#>   .. ..$ : num [1:30] 1.762 -0.373 0.872 0.625 0.332 ...
#>   .. ..$ : num [1:30] 0.746 0.939 0.886 0.815 0.793 ...
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
#>   .. ..$ : num [1:10] 0.0223 0.5434 0.6012 0.7872 -1.1012 ...
#>   .. ..$ : num [1:10] 0.937 0.646 0.968 0.613 0.983 ...
#>   .. ..$ : num [1:10] 0.45 0.286 0.648 0.335 0.312 ...
#>   .. ..$ : num [1:10] 3.37 3.12 3.45 2.1 1.63 ...
#>   .. ..$ : num [1:10] 1 1 1 1 1 0 0 0 0 0
#>   ..$ :List of 5
#>   .. ..$ : num [1:10] 0.285 -0.714 1.171 1.08 0.575 ...
#>   .. ..$ : num [1:10] 0.973 0.921 0.883 0.895 0.897 ...
#>   .. ..$ : num [1:10] 0.654 0.048 0.639 0.688 0.812 ...
#>   .. ..$ : num [1:10] 1.08 1.92 4.05 3.07 2.55 ...
#>   .. ..$ : num [1:10] 1 1 1 1 1 0 0 0 0 0
#>   ..$ :List of 5
#>   .. ..$ : num [1:10] -0.677 -1.849 1.158 0.291 0.171 ...
#>   .. ..$ : num [1:10] 0.965 0.95 0.889 0.903 0.771 ...
#>   .. ..$ : num [1:10] 0.56 0.958 0.783 0.636 0.873 ...
#>   .. ..$ : num [1:10] 3.49 4.3 3.67 2.59 2.86 ...
#>   .. ..$ : num [1:10] 1 1 1 1 1 0 0 0 0 0
#>  $ labels  : num [1:10] 1 1 1 1 1 0 0 0 0 0
#>  $ modnames: chr [1:15] "random" "poor_er" "good_er" "excel" ...
#>  $ dsids   : int [1:15] 1 1 1 1 1 2 2 2 2 2 ...
```
