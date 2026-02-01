## Test environments
* local Windows 10, R version 4.5.2 (2025-10-31 ucrt)
* win-builder R Under development (unstable) (2026-01-31 r89365 ucrt)
* Rhub: Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Rhub: Windows Server 2022, R-devel, 64 bit

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

> devtools::revdep()
[1] "ggeffects"   "insight"     "parameters"  "performance"

## revdepcheck results

We checked 4 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages


## nestedLogit 0.3.4

This is a minor release, improving documentation 

* add link to `pkgdown` documentation in DESCRIPTION
* edit description of nested dichotomies in README to include examples and relations with other models

## nestedLogit 0.3.3

* Fixed documentation error in GSS.R
* `createDichotomies` methods now explicitly internal

