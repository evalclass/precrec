## Version 0.24.0

An update of the published package `precrec` (0.14.5 -> 0.24.0). The versions
in between were prepared but not submitted, so this release carries nineteen
sets of changes: multiclass evaluation by one-vs-rest decomposition, the
evaluation metrics of `ROCR` and `scikit-learn` as opt-in additions,
operating-point selection, confidence intervals for AUCs, the chance level
reported beside every area, faster C++ hot paths, and a number of bug fixes.
It adds one dependency, `checkmate`, for the argument checks. `NEWS.md` has
the full list.

## Test environments

- local Ubuntu 24.04, R release
- win-builder, R devel
- GitHub Actions
    - macOS-latest (release)
    - windows-latest (release)
    - ubuntu-latest (devel, release, oldrel-1)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

We checked 9 reverse dependencies (5 from CRAN + 4 from Bioconductor),
comparing R CMD check results across the CRAN and development versions of this
package. We saw 0 new problems. 3 packages (`aggreCAT`, `explainer`,
`inferCSN`) failed before installation, because their own dependencies could
not be installed in the check library; neither the failures nor their causes
involve this package.
