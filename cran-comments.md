## Version 0.12.9

This is a submission for updating the already published package - precrec. In this version I have:

-   Fix another case of incorrect assignment of dsid_modnames, and

-   Updated the version.

    -   0.12.8 -> 0.12.9

## Test environments

-   local Ubuntu 20.04, R 4.1.2

-   local macOS Big Sur, R 4.1.2

-   win-builder, R Under development (unstable) (2022-03-09 r81867 ucrt)

-   Windows Server 2012 R2 x64 (on AppVeyor), R 4.1.2 Patched (2022-02-25 r81819)

-   GitHub Actions

    -   macOS-latest (release)
    -   windows-latest (release)
    -   ubuntu-latest (devel)
    -   ubuntu-latest (release)
    -   ubuntu-latest (oldrel-1)

## R CMD check results

-   **NOTE** from **ALL Ubuntu** test environments.

    -   sub-directories of 1Mb or more (it occurs because the size of `precrec.so` is over 5 MB)
