## Version 0.14.4

This is a submission for updating the already published package - precrec. In this version I have:

- Updated unit tests to avoid is.atomic(NULL) issue,

- Updated argument names of S3 functions to keep them consistent, and

- Updated the version.

    -   0.14.2 -> 0.14.4

## Test environments

-   local Ubuntu 22.04.1, R 4.3.1

-   win-builder, R Under development (unstable) (2023-10-10 r85312 ucrt)

-   Windows Server 2012 R2 x64 (on AppVeyor), R 4.3.1 Patched (2023-10-10 r85312 ucrt)

-   GitHub Actions

    -   macOS-latest (release)
    -   windows-latest (release)
    -   ubuntu-latest (devel)
    -   ubuntu-latest (release)
    -   ubuntu-latest (oldrel-1)

## R CMD check results

-   **NOTE** from **ALL Ubuntu** test environments.

    -   sub-directories of 1Mb or more (it occurs because the size of `precrec.so` is over 4 MB)
