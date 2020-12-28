## Version 0.12.0
This is a submission for updating the already published package - precrec.
In this version I have:

* Improved error messages when a data set includes only one class  

* Improve code quality using the results from lintr and CodeFactor.io

* Updated the version.
    * 0.11.2 -> 0.12.0
    
## Test environments
* local macOS High Sierra, R 4.0.3
* win-builder, R Under development (unstable) (2020-12-19 r79650)
* Ubuntu 16.04.6 LTS (on travis-ci), R 4.0.3 & R Under development (2020-12-25 r79685)
* Windows Server 2012 R2 x64 (on AppVeyor), R 4.0.3 Patched (2020-12-26 r79698)

## R CMD check results
* **NOTE** from **travis-ci**.
    * sub-directories of 1Mb or more (it occures because the size of unstripped `precrec.so` is over 3 MB)

* **NOTE** from various test environments.
    * checking for future file timestamps (unable to verify current time) 
