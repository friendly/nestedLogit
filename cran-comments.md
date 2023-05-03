## Test environments
* local Windows 10, R version 4.2.2 (2022-10-31 ucrt)
* win-builder R version 4.3.0 RC (2023-04-18 r84287 ucrt)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

# nestedLogit 0.2.0

This version has now advanced from experimental to stable with the recent enhancements, below. Now suitable for CRAN.

* Now allow dichotomies to be specified by a nested (recursive) of binary splits of the categories [suggestion of Achim Zeileis]
* The model object is now of class "nestedLogit" for uniformity.
* A basic `plot()` method now operational
* Added a linearHypothesis method to give Wald tests for hypotheses about coefficients or their linear combinations.
* Expanded vignette to illustrate some other methods.
* Added a `models()` generic and method to extract separate models from the `"nestedLogit"` object

