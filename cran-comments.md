## Test environments
* local Windows 10, R version 4.2.2 (2022-10-31 ucrt)
* local MacOS Ventura 13.3.1, R version 4.3.0 (2023-04-21)
* win-builder R Under development (unstable) (2023-05-27 r84465 ucrt)
* Rhub: Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Rhub: Windows Server 2022, R-devel, 64 bit

## R CMD check results

0 errors | 0 warnings | 0 note

## Reverse dependencies

> devtools::revdep()
[1] "insight"    "parameters"

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages


# nestedLogit 0.3.0

This is a major enhancement to the package, adding computations of standard errors and confidence intervals to predicted probabilities and logits.

* The `predict()` method now computes standard errors for probabilities and logits using the delta method. These can be obtained for either the response probabilities (and equivalent logits) or for the predicted log odds of the individual dichotomies.
* A `confint()` method for the result of `predict()` generates the corresponding confidence intervals.
* `as.data.frame()` methods for predicted values, either for the nested logit model or for the separate dichotomies converts these to a data frame in long format, handy for using `ggplot()`.
* Added a `confint()` method for predicted probabilities and logits
* The `plot()` method for `"nestedLogit"` objects now plots confidence intervals for predicted probabilities.
* A new vignette, "standard-errors", describes the mathematics behind the standard error calculations.
* An old vignette on plotting methods was completely re-written using the new `predict()` methods and focusing exclusively on `ggplot2().
* Added an example of `lobstr::tree()` to print nested lists
* Now use `fig.show="hold"` to keep `par(op)` with the code.


