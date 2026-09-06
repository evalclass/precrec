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
for calculation evaluation measures.

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
#>   .. ..$ : num [1:20] 0.593 1.58 -0.514 0.509 0.387 ...
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
#>   .. ..$ : num [1:30] 0.688 -1.977 1.346 0.582 1.301 ...
#>   .. ..$ : num [1:30] 0.905 0.844 0.92 0.971 0.984 ...
#>   ..$ :List of 2
#>   .. ..$ : num [1:30] 0.669 -0.391 0.442 -0.54 -1.662 ...
#>   .. ..$ : num [1:30] 0.976 0.653 0.976 0.77 0.957 ...
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
#>   .. ..$ : num [1:10] 0.853 0.976 -0.937 -0.032 -1.159 ...
#>   .. ..$ : num [1:10] 0.937 0.831 0.984 0.836 0.965 ...
#>   .. ..$ : num [1:10] 0.0319 0.6405 0.5341 0.4024 0.611 ...
#>   .. ..$ : num [1:10] 3.43 5.34 1.9 2.57 4.19 ...
#>   .. ..$ : num [1:10] 1 1 1 1 1 0 0 0 0 0
#>   ..$ :List of 5
#>   .. ..$ : num [1:10] 2.611 0.105 1.48 0.138 -0.31 ...
#>   .. ..$ : num [1:10] 0.608 0.999 0.96 0.621 0.808 ...
#>   .. ..$ : num [1:10] 0.481 0.951 0.916 0.284 0.183 ...
#>   .. ..$ : num [1:10] 5.38 4.3 1.05 3.87 1.44 ...
#>   .. ..$ : num [1:10] 1 1 1 1 1 0 0 0 0 0
#>   ..$ :List of 5
#>   .. ..$ : num [1:10] -0.358 1.326 1.601 -1.046 -0.377 ...
#>   .. ..$ : num [1:10] 0.671 0.928 0.99 0.627 0.915 ...
#>   .. ..$ : num [1:10] 0.951 0.92 0.277 0.796 0.905 ...
#>   .. ..$ : num [1:10] 1.968 2.289 0.756 4.719 3.884 ...
#>   .. ..$ : num [1:10] 1 1 1 1 1 0 0 0 0 0
#>  $ labels  : num [1:10] 1 1 1 1 1 0 0 0 0 0
#>  $ modnames: chr [1:15] "random" "poor_er" "good_er" "excel" ...
#>  $ dsids   : int [1:15] 1 1 1 1 1 2 2 2 2 2 ...
```
