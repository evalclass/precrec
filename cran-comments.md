## Version 0.14.5

This is a submission for updating the already published package - precrec. In this version I have:

- Restructured unit tests for svg comparisons with vdiff

- Reformatted signatures of S3 methods, and

- Updated the version.

    -   0.14.4 -> 0.14.5

## Test environments

-   local Ubuntu 22.04.1, R 4.5.0

-   local MacBook Pro Ventura 13.7.6, R 4.5.0

-   win-builder, R Under development (unstable) (2025-05-13 r88200 ucrt)

-   GitHub Actions

    -   macOS-latest (release)
    -   windows-latest (release)
    -   ubuntu-latest (devel)
    -   ubuntu-latest (release)
    -   ubuntu-latest (oldrel-1)

## R CMD check results

-   **NOTE** from **ALL Ubuntu** test environments.

    -   sub-directories of 1Mb or more (it occurs because the size of `precrec.so` is over 4 MB)
