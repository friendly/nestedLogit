## Test environments
* local Windows 10, Windows 11 R version 4.5.2 (2025-10-31 ucrt)
* win-builder R Under development (unstable) (2026-03-04 r89536 ucrt)
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


## nestedLogit 0.4.0

This is a major release of the package adding considerable functionality for plotting and other features

* add `gators` data, food choice of alligators from Agresti
* added a vignette illustrating use of the `ggeffects` package for nestedLogit models 
* added an article showing use of the `easystats` packages for nestedLogit models [not yet a vignette, because work on this is continuing]
* re-factored `predict.nestedLogit()` and related to its own file for improved documentation
* added ability for direct labels in `plot.nested-ci.R`
* Better description of nested logit and other models in README
* changed default colors for `plot.nestedLogit()` to use `scales::hue_pal()` for consistency with ggplot
* added `scale` argument to `plot.nestedLogit()` to allow plotting on the logit (log-odds) scale (`scale = "logit"`) in addition to the default probability scale (`scale = "prob"`)
* added `as.tree()` method for ASCII printing of the tree of nested dichotomies
* Added vignette: "Other Examples of Nested Logit Models"
* In the `GSS` data, "l.t.highschool" changed to "<highschool"
* Fixed problem with `lty` not passed to `matplot()` in `plot.nestedLogit()`
* In `plot.nestedLogit()`, added `label.col` argument (defaulting to the value of `col` in the call), to control the color of the curve label
* Add example of plotting predictions for dichotomies to `ggeffects` vignette
* Fixed problem in `ggeffects` vignette with `predict_response()` for nestedLogit models; see [ggeffects issue #671](https://github.com/strengejacke/ggeffects/issues/671)
