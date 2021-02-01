## Version 0.12.1
This is a submission for updating the already published package - precrec.
In this version I have:

* Replace std::random_shuffle with a new function

* Updated the version.
    * 0.12.0 -> 0.12.1
    
## Test environments
* local macOS High Sierra, R 4.0.3
* win-builder, R Under development (unstable) (2021-01-31 r79912)
* Ubuntu 16.04.6 LTS (on travis-ci), R 4.0.3 & R Under development (2020-12-25 r79685)
* Windows Server 2012 R2 x64 (on AppVeyor), R 4.0.3 Patched (2020-12-26 r79698)

## R CMD check results
* **NOTE** from **travis-ci**.
    * sub-directories of 1Mb or more (it occures because the size of unstripped `precrec.so` is over 3 MB)
