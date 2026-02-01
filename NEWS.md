## nestedLogit 0.3.4

This is a minor release, improving documentation 

* add link to `pkgdown` documentation in DESCRIPTION
* edit description of nested dichotomies in README to include examples and relations with other models

## nestedLogit 0.3.3

* Fixed documentation error in GSS.R
* `createDichotomies` methods now explicitly internal

## nestedLogit 0.3.2

* new `Effect` method for `"nestedLogit"` objects to create effect displays.
* add Effect example to vignette


## nestedLogit 0.3.1

* correct buglet in `as.data.frame` method when `newdata` has one column
* `predict.nestedLogit()` now includes the `newdata` data frame as an additional component (`.data`) in its result. Consequently, the `newdata` argument is no longer required in the `as.data.frame` method.

## nestedLogit 0.3.0

This is a major enhancement to the package, adding computations of standard errors and confidence intervals to predicted probabilities and logits.

### Enhancements
* The `predict()` method now computes standard errors for probabilities and logits using the delta method. These can be obtained for either the response probabilities (and equivalent logits) or for the predicted log odds of the individual dichotomies.
* A `confint()` method for the result of `predict()` generates the corresponding confidence intervals.
* `as.data.frame()` methods for predicted values, either for the nested logit model or for the separate dichotomies converts these to a data frame in long format, handy for using `ggplot()`.
* Added a `confint()` method for predicted probabilities and logits
* The `plot()` method for `"nestedLogit"` objects now plots confidence intervals for predicted probabilities.
* A new vignette, "standard-errors", describes the mathematics behind the standard error calculations.
* An old vignette on plotting methods was completely re-written using the new `predict()` methods and focusing exclusively on `ggplot2().

### Other
* Added an example of `lobstr::tree()` to print nested lists
* Now use `fig.show="hold"` to keep `par(op)` with the code.

## nestedLogit 0.2.1

* Reset all `par()` and `options()` calls so as to not alter user's workspace. 
* now document all return values. 
* added a reference to DESCRIPTION. It is a book, so no doi:, url, etc.
* fixed one URL that win-builder (spuriously) complains about.


## nestedLogit 0.2.0

* Now allow dichotomies to be specified by a nested (recursive) of binary splits of the categories [suggestion of Achim Zeileis]
* The model object is now of class "nestedLogit" for uniformity.
* A basic `plot()` method now operational
* Added a `linearHypothesis()` method to give Wald tests for hypotheses about coefficients or their linear combinations.
* Expanded vignette to illustrate some other methods.
* Added a `models()` generic and method to extract separate models from the `"nestedLogit"` object
* Added a `logLike()` method, and through it, gets `AIC()` and `BIC()`
* Reorganized documentation to separate nested hypothesis methods.

## nestedLogit 0.1.0

* Initial version
* Added a `NEWS.md` file to track changes to the package.

