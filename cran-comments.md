## Version 0.11.2
This is a submission for updating the already published package - precrec.
In this version I have:

* Fixed format_nfold function to return lables as interger even given as factor  

* Updated the version.
    * 0.11.1 -> 0.11.2
    
## Test environments
* local macOS High Sierra, R 4.0.0
* local Ubuntu 18.04.2 LTS, R 4.0.0
* win-builder, R Under development (unstable) (2020-05-26 r78577)
* Ubuntu 16.04.6 LTS (on travis-ci), R 4.4.0 & R Under development (2020-05-15 r78470)
* Windows Server 2012 R2 x64 (on AppVeyor), R 4.0.0 Patched

## R CMD check results
* One **NOTE** from **local Ubuntu**.
    * It occures because the size of unstripped `precrec.so` is over 3 MB.    
* One **NOTE** from **win-builder, R Under development (unstable)**.
    * Non-standard file/directory found at top level: "'revdep'". I don't have 'revdep' folder at the top level. 
