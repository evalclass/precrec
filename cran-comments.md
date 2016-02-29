## Version 0.3
This is a submission for updating the already published package - precrec.
In this version I have:

* Updated unit tests for gridExtra 2.2.0.

* Updated several documents.

* Updated the version.
    
    * 0.2.0 -> 0.3.1
    
## Test environments
* local OS X Yosemite, R 3.2.3
* local CentOS 6.7, R 3.2.3
* local Windows 10, R 3.2.3
* win-builder (devel and release)
* Ubuntu 12.04.5 LTS (on travis-ci), R 3.2.3

## R CMD check results
* There were no ERRORs or WARNINGs.

* One **NOTE** from the package size test on **local CentOS 6.7, R 3.2.3**.
  
    It happened because the the size of the package c++ library `precrec.so` was 3.8 MB. 
    It is almost 1 MB in other test environments.
