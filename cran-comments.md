## Version 0.14.2

This is a submission for updating the already published package - precrec. In this version I have:

- Updated the signature of the fortify generic funtion to suppress CRAN warnings, and

- Updated the version.

    -   0.14.1 -> 0.14.2

## Test environments

-   local Ubuntu 22.04.1, R 4.2.2

-   win-builder, R Under development (unstable) (2023-01-04 r83561 ucrt)

-   Windows Server 2012 R2 x64 (on AppVeyor), R 4.2.2 Patched (2023-01-06 r83581 ucrt)

-   GitHub Actions

    -   macOS-latest (release)
    -   windows-latest (release)
    -   ubuntu-latest (devel)
    -   ubuntu-latest (release)
    -   ubuntu-latest (oldrel-1)

## R CMD check results

-   **NOTE** from **ALL Ubuntu** test environments.

    -   sub-directories of 1Mb or more (it occurs because the size of `precrec.so` is over 4 MB)
