## Version 0.11.1
This is a submission for updating the already published package - precrec.
In this version I have:

* Update test cases to treat c(factor) as factor since c(factor) does not return interger anymore 

* Updated the version.
    * 0.11 -> 0.11.1
    
## Test environments
* local macOS High Sierra, R 4.0.0
* local Ubuntu 18.04.2 LTS, R 4.0.0
* win-builder, R Under development (unstable) (2020-01-03 r77630)
* Ubuntu 16.04.6 LTS (on travis-ci), R 3.6.2 & R Under development (2020-01-09 r77641)
* Windows Server 2012 R2 x64 (on AppVeyor), R 3.6.2 Patched

## R CMD check results
* One **NOTE** in the package size test on **local Ubuntu** and **Travis CI**.
    * It occures because the size of unstripped `precrec.so` is over 3 MB.    
