## Version 0.9.1
This is a submission for updating the already published package - precrec.
In this version I have:

* Fixed a bug in as.data.frame for precrec objects

* Added cross validation functions

* Updated the version.
    * 0.8 -> 0.9.1
    
## Test environments
* local CentOS 6.7, R 3.3.3
* local OS X Yosemite, R 3.4.1
* local Windows 10, R 3.4.1
* win-builder, R Under development (unstable)
* Ubuntu 12.04.5 LTS (on travis-ci), R 3.4.1 & R Under development 
* Windows Server 2012 R2 x64 (on AppVeyor), 3.4.1 Patched

## R CMD check results
* There were no ERRORs or WARNINGs.

* One **NOTE** in the package size test on **local CentOS 6.7, R 3.3.3** and **Ubuntu 12.04.5 LTS**.
  
    * It happens because the the size of the package c++ library `precrec.so` was 5-8 MB.     

* One **NOTE** from the "checking compiled code"" test on **AppVeyor**.

    * It is good practice to register native routines and to disable symbol search.
