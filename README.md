
# nestedLogit

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The `nestedLogit` package provides functions for fitting nested dichotomy logistic regression models
for a polytomous response. Nested dichotomies are statistically independent, and hence provide an
additive decomposition of tests for the overall polytomous response.

## Package overview

The package provides one main function, `nestedLogit()` for fitting the set of $(m-1)$
binary logistic regression models for a polytomous response with $m$ levels.
These can be specified using helper functions,

* `dichotomy()`: constructs a _single_ dichotomy among the levels of a response factor;
* `logits()`: creates the set of dichotomies, typically using `dichotomy()` for each.
* `continuationLogits()`: provides a convenient way to generate all dichotomies for an ordered response.

There are methods including `as.matrix.dichotomies()`, `as.character.dichotomies()`
to facilitate working with `dichotomies` objects in other representations.

The result of `nestedLogit()` is an object of class `nested`. It contains
the set of $(m-1)$ `glm()` models fit to the dichotomies, ...

### Methods


As befits a model-fitting function, the package defines methods for class `nested` objects:

* `print()`, `summary()`
* `update()` re-fits a model, allowing changes in the model formula, data subset, and conrasts.
* `coef()` returns the coefficients for the predictors in each dichotomy
* `vcov()` returns the variance-covariance matrix of the predictors
* `Anova()` uses `car::Anova()` to provide ANOVA tests for each dichotomy; the   
   `print.Anova()` method also gives these tests for the combined model.
* `predict()` obtains predicted probabilities for the response categories, useful for producing plots to aid interpretation.
* `glance()`, `tidy()` are extensions of `broom::glance.glm()` and `broom::tidy.glm()` to obtain compact summaries of a `nested` model object`.


## Authors
* John Fox
* Michael Friendly

