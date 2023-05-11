## Test environments
* local Windows 10, R version 4.2.2 (2022-10-31 ucrt)
* local MacOS Ventura 13.3.1, R version 4.3.0 (2023-04-21)
* win-builder R version 4.3.0 RC (2023-04-18 r84287 ucrt)
* Rhub: Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Rhub: Windows Server 2022, R-devel, 64 bit

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

# nestedLogit 0.2.1

This is a patch release, correcting minor problems noted by a CRAN reviewer

* Reset all `par()` and `options()` calls so as to not alter user's workspace. 
* Now document all return values. 
* Added a reference to DESCRIPTION. It is to a book (with an ISBN), so no doi:, url, is appropriate.

