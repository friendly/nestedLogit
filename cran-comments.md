## Test environments
* local Windows 10, Windows 11 R version 4.5.2 (2025-10-31 ucrt)
* win-builder R Under development (unstable) (2026-05-26 r90080 ucrt)
* R-hub: Ubuntu Linux 24.04.3 LTS, R-devel, GCC
* R-hub: macOS 15.7.4 Sequoia, R-devel, arm64


## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

> devtools::revdep()
[1] "ggeffects"   "insight"     "parameters"  "performance"

## revdepcheck results
> revdepcheck::revdep_check(num_workers=4)

We checked 4 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages


## nestedLogit 0.4.2

This is a moderate enhancement comprising new methods for display of nested logit models and RSQ-like measures.

* added an `equatiomatic::extract_eq()` method for "nestedLogit" objects
* added a vignette ("latex-equations") illustrating use of LaTeX equations in Rmarkdown / Quarto documents for these models and various options available.
* fixed subtle LaTeX bug in `extract_eq()` when dichotomy names contain "_"
* added a `submodel=` arg to avoid `$submodel` indexing
* Fixed DOIs in RSQ.R now that CRAN is pickier about their format
* Fixed documentation nits from Roxygen 8.0.0

## nestedLogit 0.4.1

* Added `RSQ()` function to compute pseudo-R² measures (McFadden, Cox-Snell, Nagelkerke, and others) for each dichotomy sub-model and for the combined polytomous model
* Added examples of `RSQ()` in the nestedLogit and other-examples vignettes
* Added methods `RSQ.multinom()` and `RSQ.polr()`

