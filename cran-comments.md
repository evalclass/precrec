## Version 0.3.2
This is a submission for updating the already published package - precrec.
In this version I have:

* Updated unit tests for testthat 0.11.0.9000.
    * It also works with the current testthat 0.11.0

* Updated the version.
    * 0.3.1 -> 0.3.2
    
## Test environments
* local OS X Yosemite, R 3.2.4 Revised
* local CentOS 6.7, R 3.2.3
* local Windows 10, R 3.2.4 Revised
* win-builder (devel and release)
* Ubuntu 12.04.5 LTS (on travis-ci), R 3.2.4 Revised

## R CMD check results
* There were no ERRORs or WARNINGs.

* One **NOTE** from the package size test on **local CentOS 6.7, R 3.2.3**.
  
    It happens because the the size of the package c++ library `precrec.so` was 3.8 MB. 
    It is almost 1 MB in other test environments.
