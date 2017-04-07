## Version 0.7.1
This is a submission for updating the already published package - precrec.
In this version I have:

* Reduce supporting points to make plotting speed faster for ggplot 

* Updated the version.
    * 0.6.2 -> 0.7.1
    
## Test environments
* local OS X Yosemite, R 3.3.3
* local CentOS 6.7, R 3.3.3
* local Windows 10, R 3.3.3
* win-builder, R Under development (unstable)
* Ubuntu 12.04.5 LTS (on travis-ci), R 3.3.3 & R Under development 
* Windows Server 2012 R2 x64 (on AppVeyor), 3.3.3 Patched

## R CMD check results
* There were no ERRORs or WARNINGs.

* One **NOTE** from the package size test on **local CentOS 6.7, R 3.3.3**.
  
    * It happens because the the size of the package c++ library `precrec.so` was 5.6 MB.     
          

* One **NOTE** from the package size test on **travis-ci, R 3.3.3**.
  
    * installed size is 7.3Mb    
         sub-directories of 1Mb or more: data 1.5MB, libs 4.9MB   
        
  
* One **NOTE** on **win-builder**.

    * Possibly mis-spelled words in DESCRIPTION:
        ROC (3:48, 10:78)  
  

* One **NOTE** from the "checking compiled code"" test on **AppVeyor**.

    * Warning in read\_symbols\_from_dll(so, rarch) :
       this requires 'objdump.exe' to be on the PATH   
