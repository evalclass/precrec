## Version 0.6.2
This is a submission for updating the already published package - precrec.
In this version I have:

* Fix column names of S3 print function for mdat 

* Updated the version.
    * 0.6.1 -> 0.6.2
    
## Test environments
* local OS X Yosemite, R 3.3.2
* local CentOS 6.7, R 3.3.2
* local Windows 10, R 3.3.2
* win-builder, Under development (unstable)
* Ubuntu 12.04.5 LTS (on travis-ci), R 3.3.1
* Windows Server 2012 R2 x64 (on AppVeyor), 3.3.2 Patched

## R CMD check results
* There were no ERRORs or WARNINGs.

* One **NOTE** from the package size test on **local CentOS 6.7, R 3.3.1**.
  
    It happens because the the size of the package c++ library `precrec.so` was 5.4 MB. 
    It is smaller than 1 MB when tested in other environments.

* One **NOTE** from the CRAN incoming feasibility test on **win-builder**.

    * Possibly mis-spelled words in DESCRIPTION:
        ROC (3:48, 10:78)

    * Found the following (possibly) invalid URLs:  
        URL: https://doi.org/10.1093/bioinformatics/btw570  
          From: README.md  
          Status: 400  
          Message: Bad Request

    * Found the following (possibly) invalid DOIs:  
        DOI: 10.1093/bioinformatics/btw570  
          From: inst/CITATION  
          Status: Bad Request  
          Message: 400 
      
    **ROC is spelt correctly. Both URL and DOI are correct.**
