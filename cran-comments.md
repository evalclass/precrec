## Version 0.15.0

An update of the published package `precrec` (0.14.5 -> 0.15.0).

- Support datasets with more than two classes by one-vs-rest decomposition
- Add confusion-matrix measures, F-beta, and probability-based metrics
- Handle single-class datasets with a warning instead of an error
- Fix the ranking of `NA` scores and the panel titles of `autoplot()`
- Raise the R dependency to >= 4.1, and modernize the tests and internals

`NEWS.md` has the full list.

## Test environments

- local Ubuntu 22.04 and macOS, R release
- win-builder, R devel
- GitHub Actions
    - macOS-latest (release)
    - windows-latest (release)
    - ubuntu-latest (devel, release, oldrel-1)

## R CMD check results

0 errors | 0 warnings | 1 note

- **NOTE** on Linux: sub-directories of 1Mb or more, because `precrec.so` is
  over 4 MB.
