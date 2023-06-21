## Test environments
* local Windows 10, R version 4.2.3 (2023-03-15 ucrt)
* local MacOS Ventura 13.3.1, R version 4.3.0 (2023-04-21)
* win-builder R Under development (unstable) (2023-06-20 r84585 ucrt)
* Rhub: Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Rhub: Windows Server 2022, R-devel, 64 bit

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

> devtools::revdep()
[1] "ggeffects"  "insight"    "parameters"

## revdepcheck results

We checked 3 reverse dependencies (0 from CRAN + 3 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages


## nestedLogit 0.3.2

This is a modest enhancement to the package, adding effect displays and smoothing some infelicities in the previously released version.

* new `Effect` method for `"nestedLogit"` objects to create effect displays.
* add Effect example to vignette

# nestedLogit 0.3.1

* correct buglet in `as.data.frame` method when `newdata` has one column
* `predict.nestedLogit()` now includes the `newdata` data frame as an additional component (`.data`) in its result. Consequently, the `newdata` argument is no longer required in the `as.data.frame` method.

