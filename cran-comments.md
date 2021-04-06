## Version 0.12.5
This is a submission for updating the already published package - precrec.
In this version I have:

* Transferred the GitHub repository to an organization GitHub account,

* Updated citation,

* Added a flag to skip ggplot unit tests to reduce the total unit test time, and

* Updated the version.
    * 0.12.1 -> 0.12.5
    
## Test environments
* local Ubuntu 20.04 High Sierra, R 4.0.5
* local macOS High Sierra, R 4.0.4
* win-builder, R Under development (unstable) (2021-04-02 r80141)
* Ubuntu 16.04.6 LTS (on travis-ci), R 4.0.2 & R Under development (2021-04-02 r80141)
* Windows Server 2012 R2 x64 (on AppVeyor), R 4.0.5 Patched (2021-03-31 r80143)

## R CMD check results
* **NOTE** from **local Ubuntu** and **travis-ci**.
    * sub-directories of 1Mb or more (it occurs because the size of `precrec.so` is over 5 MB)
