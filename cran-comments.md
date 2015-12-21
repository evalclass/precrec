## Version 0.2
This is a submission for updating the already published package - precrec.
In this version I have:

* Removed the links of ggplot2 from help files.

    * \\link[ggplot2]{ggplot2} -> \\pkg{ggplot2}

* Updated several documents.

* Updated the version.
    
    * 0.1.1 -> 0.2.0
    
## Test environments
* local OS X Yosemite, R 3.2.3
* local CentOS 6.7, R 3.2.3
* local Windows 10, R 3.2.3
* win-builder (devel and release)
* ubuntu (on travis-ci), R 3.2.3

## R CMD check results
* There were no ERRORs or WARNINGs.

* One **NOTE** from the package size test on **local CentOS 6.7, R 3.2.3**.
  
    It happened because the the size of the package c++ library `precrec.so` was 3.8 MB. 
    It is almost 1 MB in other test environments.
  
    We couldn't figure out the reason why only `precrec.so` on **local CentOS** is larger 
    than the other test environments. 
  
    Also, the previous version passed the same test on **local CentOS** with **R 3.2.1**
    even though we used the same cpp files.
