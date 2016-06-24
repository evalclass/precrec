## Version 0.4.0
This is a submission for updating the already published package - precrec.
In this version I have:

* Added new basic measures
    * Matthews correlation coefficient
    * F-score
    * Scores & labels are also treated as basic measures 

* Updated the version.
    * 0.3.2 -> 0.4.0
    
## Test environments
* local OS X Yosemite, R 3.3.0
* local CentOS 6.7, R 3.3.0
* win-builder, Under development (unstable)
* Ubuntu 12.04.5 LTS (on travis-ci), R 3.3.0
* Windows Server 2012 R2 x64 (on AppVeyor), R 3.3.0 Patched

## R CMD check results
* There were no ERRORs or WARNINGs.

* One **NOTE** from the package size test on **local CentOS 6.7, R 3.3.0**.
  
    It happens because the the size of the package c++ library `precrec.so` was 3.9 MB. 
    It is smaller than 1 MB when tested in other environments.
