## Version 0.8
This is a submission for updating the already published package - precrec.
In this version I have:

* Add 'aucroc' mode for fast AUC (ROC)

* Change how to treat 'show_cb' and 'raw_curves' options

* Updated the version.
    * 0.7.1 -> 0.8
    
## Test environments
* local OS X Yosemite, R 3.4.0
* local CentOS 6.7, R 3.3.0
* local Windows 10, R 3.4.0
* win-builder, R Under development (unstable)
* Ubuntu 12.04.5 LTS (on travis-ci), R 3.4.0 & R Under development 
* Windows Server 2012 R2 x64 (on AppVeyor), 3.4.0 Patched

## R CMD check results
* There were no ERRORs or WARNINGs.

* One **NOTE** in the package size test on **local CentOS 6.7, R 3.3.3** and **Ubuntu 12.04.5 LTS**.
  
    * It happens because the the size of the package c++ library `precrec.so` was 5-8 MB.     
        
  
* One **NOTE** on **win-builder**.

    * Possibly mis-spelled words in DESCRIPTION:
       ROC (3:48, 10:78) 


* One **NOTE** from the "checking compiled code"" test on **AppVeyor**.

    * It is good practice to register native routines and to disable symbol search.
