# precrec 0.15.0

* Require R >= 4.1 (was R >= 3.2.1)

* Migrate the unit tests to testthat edition 3, and run test files in parallel

* Remove the deprecated `context()` calls from the unit tests

* Convert the roxygen2 documentation to markdown. Links to functions now
  render with parentheses, such as `evalmod()`, and links to other packages
  are qualified, such as `gridExtra::arrangeGrob()`. The rendered help pages
  are otherwise unchanged.

* Replace `assertthat` with `cli` and `rlang` for argument validation.
  `assertthat` is no longer a dependency. Errors raised by argument checks now
  carry condition classes, so they can be caught by class rather than by
  message: `precrec_error_invalid_<argument>`, `precrec_error_invalid_arg` and
  `precrec_error`. The wording of these messages has changed, but the same
  inputs are accepted and rejected as before.

* Add the package website to `URL` in `DESCRIPTION`, and rebuild the pkgdown
  site with the Bootstrap 5 template.

* Add `inst/WORDLIST` and a spell-check test, plus a committed `.lintr`
  configuration.

* Fix the ranking of `NA` scores when `na_worst = TRUE`. The sentinel value
  used for `NA` was `DBL_MIN`, the smallest *positive* double, so `NA`s
  outranked every negative score instead of being ranked last. Results change
  only when the scores contain both `NA`s and negative values: `evalmod()`
  now returns the same curves and AUCs for such input as it does when a
  constant is added to every score. `na_worst = FALSE` was never affected.

* Calculate the standard errors of averaged curves and averaged points with
  Welford's algorithm instead of `E[x^2] - E[x]^2`. The previous formula lost
  precision through catastrophic cancellation and clamped the resulting
  negative variances to zero. Confidence bands from `evalmod(calc_avg = TRUE)`
  change at around the 1e-8 level.

# precrec 0.14.5

* Restructure unit tests for svg comparisons with vdiff

* Reformat signatures of S3 methods

# precrec 0.14.4

* Update unit tests to avoid is.atomic(NULL) issue

* Update argument names of S3 functions to keep them consistent

# precrec 0.14.3

* Use new R setup configurations of GitHub actions

# precrec 0.14.2

* Update the signature of the fortify generic function 

# precrec 0.14.0

* Use tidy aesthetics with aes() and sym() instead of aes_string()

# precrec 0.13.1

* Use patchwork to combine multiple plots instead of using grid and gridExtra

* Test ggplot results with vdiffr

# precrec 0.12.9

* Fix another case of incorrect assignment of dsid_modnames

# precrec 0.12.8

* Fix incorrect dsid_modnames when data.frame is created by fortify()

# precrec 0.12.7

* Use STRICT_R_HEADER in Rcpp source files
* Skip several unit tests on CRAN submission

# precrec 0.12.5

* Update citation

# precrec 0.12.4

* Transfer the GitHub repository to evalclass/precrec

# precrec 0.12.2

* Skip ggplot2 unit tests
* Change default branch to main

# precrec 0.12.1

* Replace std::random_shuffle with a new function 

# precrec 0.12

* Improve error messages when a data set includes only one class  
* Improve code quality using the results from lintr and CodeFactor.io

# precrec 0.11.2

* format_nfold function returns labels as integer even given as factor  

# precrec 0.11.1

* Update test cases to treat c(factor) as factor since c(factor) does not return integer anymore 

# precrec 0.11

* Add auc\_ci function for CI calculation of AUC scores

# precrec 0.10.1

* Remove src/Makevars to keep .so file unstripped

# precrec 0.10

* Fix Rcpp header for STRICT_R_HEADERS
* Strip symbols of .so file

# precrec 0.9.1

* Fix a bug with as.data.frame when multiple datasets given

* Add format_nfold function to convert a dataframe with n-fold data to a list

# precrec 0.8

* Add 'aucroc' mode for fast AUC (ROC)

* Change how to treat 'show_cb' and 'raw_curves' options

# precrec 0.7.1

* Add precrec_init.c to avoid CRAN warnings

# precrec 0.7

* Add reduce_points option to autoplot

# precrec 0.6.2

* Fix mdat print

# precrec 0.6.1

* Improve GitHub pages

# precrec 0.6

* Improve as.data.frame with Rcpp

* Create GitHub pages with pkgdown

# precrec 0.5.2

* Update README

# precrec 0.5

* Add partial AUCs

# precrec 0.4

* Add new measures
    * Matthews correlation coefficient
    * F-score
    
* New generic function
    * as.data.frame works on precrec S3 objects

# precrec 0.3

* Improved the testing environment
    * unit tests
    * codecov
    
* Improved several documents


# precrec 0.2

* Improved several documents
    * several help files (.Rd)
    * package vignette
    * README

# precrec 0.1

* The first release version of `precrec`

* The package offers five functions
    * evalmod
    * mmdata
    * join\_scores
    * join\_labels
    * create\_sim\_samples
