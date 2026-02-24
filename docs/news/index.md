# Changelog

## nestedLogit 0.3.5

- add gators data, food choice of aligators from Agresti
- added an article illustrating use of the `ggeffects` package for
  nestedLogit models

## nestedLogit 0.3.4

CRAN release: 2026-02-01

This is a minor release, improving documentation

- add link to `pkgdown` documentation in DESCRIPTION
- edit description of nested dichotomies in README to include examples
  and relations with other models

## nestedLogit 0.3.3

- Fixed documentation error in GSS.R
- `createDichotomies` methods now explicitly internal

## nestedLogit 0.3.2

CRAN release: 2023-06-22

- new `Effect` method for `"nestedLogit"` objects to create effect
  displays.
- add Effect example to vignette

## nestedLogit 0.3.1

- correct buglet in `as.data.frame` method when `newdata` has one column
- [`predict.nestedLogit()`](https://friendly.github.io/nestedLogit/reference/nestedMethods.md)
  now includes the `newdata` data frame as an additional component
  (`.data`) in its result. Consequently, the `newdata` argument is no
  longer required in the `as.data.frame` method.

## nestedLogit 0.3.0

CRAN release: 2023-05-30

This is a major enhancement to the package, adding computations of
standard errors and confidence intervals to predicted probabilities and
logits.

### Enhancements

- The [`predict()`](https://rdrr.io/r/stats/predict.html) method now
  computes standard errors for probabilities and logits using the delta
  method. These can be obtained for either the response probabilities
  (and equivalent logits) or for the predicted log odds of the
  individual dichotomies.
- A [`confint()`](https://rdrr.io/r/stats/confint.html) method for the
  result of [`predict()`](https://rdrr.io/r/stats/predict.html)
  generates the corresponding confidence intervals.
- [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) methods
  for predicted values, either for the nested logit model or for the
  separate dichotomies converts these to a data frame in long format,
  handy for using
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).
- Added a [`confint()`](https://rdrr.io/r/stats/confint.html) method for
  predicted probabilities and logits
- The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method
  for `"nestedLogit"` objects now plots confidence intervals for
  predicted probabilities.
- A new vignette, “standard-errors”, describes the mathematics behind
  the standard error calculations.
- An old vignette on plotting methods was completely re-written using
  the new [`predict()`](https://rdrr.io/r/stats/predict.html) methods
  and focusing exclusively on \`ggplot2().

### Other

- Added an example of
  [`lobstr::tree()`](https://lobstr.r-lib.org/reference/tree.html) to
  print nested lists
- Now use `fig.show="hold"` to keep `par(op)` with the code.

## nestedLogit 0.2.1

CRAN release: 2023-05-15

- Reset all [`par()`](https://rdrr.io/r/graphics/par.html) and
  [`options()`](https://rdrr.io/r/base/options.html) calls so as to not
  alter user’s workspace.
- now document all return values.
- added a reference to DESCRIPTION. It is a book, so no doi:, url, etc.
- fixed one URL that win-builder (spuriously) complains about.

## nestedLogit 0.2.0

- Now allow dichotomies to be specified by a nested (recursive) of
  binary splits of the categories \[suggestion of Achim Zeileis\]
- The model object is now of class “nestedLogit” for uniformity.
- A basic [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
  method now operational
- Added a
  [`linearHypothesis()`](https://rdrr.io/pkg/car/man/linearHypothesis.html)
  method to give Wald tests for hypotheses about coefficients or their
  linear combinations.
- Expanded vignette to illustrate some other methods.
- Added a
  [`models()`](https://friendly.github.io/nestedLogit/reference/models.md)
  generic and method to extract separate models from the `"nestedLogit"`
  object
- Added a `logLike()` method, and through it, gets
  [`AIC()`](https://rdrr.io/r/stats/AIC.html) and
  [`BIC()`](https://rdrr.io/r/stats/AIC.html)
- Reorganized documentation to separate nested hypothesis methods.

## nestedLogit 0.1.0

- Initial version
- Added a `NEWS.md` file to track changes to the package.
