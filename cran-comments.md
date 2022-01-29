## Version 0.12.8
This is a submission for updating the already published package - precrec.
In this version I have:

* Fixed incorrect labels in data.frame when fortify() is used with mmcurves 

* Stopped using Travis and started using GitHub Actions for CI, and

* Updated the version.
    * 0.12.7 -> 0.12.8
    
## Test environments
* local Ubuntu 20.04, R 4.1.2
* local macOS Big Sur, R 4.1.2
* win-builder, R Under development (unstable) (2022-01-27 r81578 ucrt)
* Ubuntu 16.04.6 LTS (on travis-ci), R 4.0.2 & R Under development (2021-05-26 r80393)
* Windows Server 2012 R2 x64 (on AppVeyor), R 4.1.0 Patched (2022-01-24 r81578)

## R CMD check results
* **NOTE** from **local Ubuntu** and **travis-ci**.
    * sub-directories of 1Mb or more (it occurs because the size of `precrec.so` is over 5 MB)
