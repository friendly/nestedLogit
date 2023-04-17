
# nestedLogits

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The `nestedLogits` package provides functions for fitting nested dichotomy logistic regression models
for a polytomous response. Nested dichotomies are statistically independent, and hence provide an
additive decomposition of tests for the overall polytomous response.

## Package overview

The package provides one main function, `nestedLogit()` for fitting the set of $(m-1)$
binary logistic regression models for a polytomous response with $m$ levels.
These can be specified using helper functions,

* `dichotomy()`: constructs a _single_ dichotomy among the levels of a response factor;
* `logits()`: creates the set of dichotomies, typically using `dichotomy()` for each.
* `continuationLogits()`: provides a convenient way to generate all dichotomies for an ordered response.

The result of `nestedLogit()` is an object of class `nested`. It contains
the set of $(m-1)$ `glm()` models fit to the dichotomies, ...

### Methods


As befits a model-fitting method, the package contains methods for class `nested` objects

...

## Authors
* John Fox
* Michael Friendly

