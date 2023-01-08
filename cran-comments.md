## Version 0.13.0

This is a submission for updating the already published package - precrec. In this version I have:

-   Used tidy aesthetics with aes() and sym() instead of aes_string(),

-   Included the vdiffr package to test ggplot2 results,

-   Used patchwork to combine multiple plots instead of using grid and gridExtra, and

-   Updated the version.

    -   0.12.9 -> 0.13.0

## Test environments

-   local Ubuntu 20.04, R 4.2.1

-   win-builder, R Under development (unstable) (2022-03-09 r81867 ucrt)

-   Windows Server 2012 R2 x64 (on AppVeyor), R 4.2.2 Patched (2023-01-06 r83581 ucrt)

-   GitHub Actions

    -   macOS-latest (release)
    -   windows-latest (release)
    -   ubuntu-latest (devel)
    -   ubuntu-latest (release)
    -   ubuntu-latest (oldrel-1)

## R CMD check results

-   **NOTE** from **ALL Ubuntu** test environments.

    -   sub-directories of 1Mb or more (it occurs because the size of `precrec.so` is over 5 MB)
