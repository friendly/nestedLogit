
<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

# nestedLogit <img src="man/figures/logo.png" style="float:right; height:200px;" />

The `nestedLogit` package provides functions for fitting nested dichotomy logistic regression models
for a polytomous response. Nested dichotomies are statistically independent, and hence provide an
additive decomposition of tests for the overall polytomous response.

## Installation

This package has not yet been submited to CRAN.
This development version can be installed to your R library directly from the [GitHub repo](https://github.com/friendly/nestedLogit) via:

     if (!require(remotes)) install.packages("remotes")
     remotes::install_github("friendly/nestedLogit", build_vignettes = TRUE)



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


As befits a model-fitting function, the package defines a nearly complete set of methods for class `nested` objects:

* `print()`, `summary()`
* `update()` re-fits a model, allowing changes in the model `formula`, `data`, `subset`, and `contrasts`.
* `coef()` returns the coefficients for the predictors in each dichotomy
* `vcov()` returns the variance-covariance matrix of the predictors
* `anova()` provides ANOVA Type I (sequential) tests for each dichotomy and for the combined model. When given a sequence of objects, `anova()` tests the models against one another in the order specified.
* `Anova()` uses `car::Anova()` to provide ANOVA Type II (partial) tests for each dichotomy and for the combined model.
* `predict()` obtains predicted probabilities for the response categories, useful for producing plots to aid interpretation.
* `glance()`, `tidy()` are extensions of `broom::glance.glm()` and `broom::tidy.glm()` to obtain compact summaries of a `nested` model object`.


## Authors
* John Fox
* Michael Friendly

## References

S. Fienberg (1980) _The Analysis of Cross-Classified Categorical Data_, 2nd Edition, MIT Press, Section 6.6.

J. Fox (2016) _Applied Regression Analysis and Generalized Linear Models_, 3rd Edition, Sage, Section 14.2.2.

M. Friendly and D. Meyers (2016) _Discrete Data Analysis with R_, CRC Press, Section 8.2.
