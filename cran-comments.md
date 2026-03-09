## Test environments
* local Windows 10, Windows 11 R version 4.5.2 (2025-10-31 ucrt)
* win-builder R Under development (unstable) (2026-03-04 r89536 ucrt)
* R-hub: Ubuntu Linux 24.04.3 LTS, R-devel, GCC
* R-hub: macOS 15.7.4 Sequoia, R-devel, arm64

> devtools::check_win_devel()
> rhub::rhub_check(platforms = c("linux", "macos-arm64"))
Note: the `macos` (macOS 13, x86_64) R-hub platform is no longer supported;
replaced by `macos-arm64` (macOS 15 Sequoia, arm64) which reflects current CRAN infrastructure.

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


## Response to CRAN review of 0.4.0

The vignette `other-examples.Rmd` failed on CRAN with:

    Error in `xtfrm.data.frame()`: cannot xtfrm data frames

The `mlogit::Fishing` dataset is a `dfidx` object, which inherits from
`tbl_df` (tibble). When passed to `nestedLogit()` it is stored as-is,
and `plot.nestedLogit()` then called `data[, x.var]` on a tibble, which
never drops dimensions and so returned a one-column data frame rather
than a vector, eventually causing `sort()` to fail.

Fixed by coercing `x$data` to a plain data frame at the top of
`plot.nestedLogit()` (`data <- as.data.frame(x$data)`). The vignette
built cleanly on local Windows machines and `win-builder` because
`mlogit` was not installed there and the chunk is guarded by
`eval = have_mlogit`.

No other changes; version not bumped.

Added `rhub::rhub_check()` workflow

---

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
* Fixed problem in `other-examples` vignette traced to tibble not handled well in `plot.nestedLogit`
* Added `rhub::rhub_check()` workflow.
