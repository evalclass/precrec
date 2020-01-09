## Version 0.11
This is a submission for updating the already published package - precrec.
In this version I have:

* Add CI calculation for AUC scores

* Updated the version.
    * 0.10.1 -> 0.11
    
## Test environments
* local macOS High Sierra, R 3.6.2
* local Ubuntu 18.04.2 LTS, R 3.6.2
* win-builder, R Under development (unstable)
* Ubuntu 14.04.5 LTS (on travis-ci), R 3.5.3 & R Under development (2019-04-12 r76385)
* Windows Server 2012 R2 x64 (on AppVeyor), 3.5.3 Patched

## R CMD check results
* One **NOTE** in the package size test on **local Ubuntu 18.04.2 LTS, R 3.6.2**.
    * It happens because the size of unstripped `precrec.so` is over 3 MB.    
