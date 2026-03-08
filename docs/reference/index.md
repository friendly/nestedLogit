# Package index

## Fitting nested logit models

Core functions for specifying dichotomies and fitting nested logit
models.

- [`nestedLogit()`](https://friendly.github.io/nestedLogit/reference/nestedLogit.md)
  [`logits()`](https://friendly.github.io/nestedLogit/reference/nestedLogit.md)
  [`dichotomy()`](https://friendly.github.io/nestedLogit/reference/nestedLogit.md)
  [`continuationLogits()`](https://friendly.github.io/nestedLogit/reference/nestedLogit.md)
  : Binary Logit Models for Nested Dichotomies

- [`print(`*`<nestedLogit>`*`)`](https://friendly.github.io/nestedLogit/reference/nestedMethods.md)
  [`summary(`*`<nestedLogit>`*`)`](https://friendly.github.io/nestedLogit/reference/nestedMethods.md)
  [`print(`*`<summary.nestedLogit>`*`)`](https://friendly.github.io/nestedLogit/reference/nestedMethods.md)
  [`print(`*`<dichotomies>`*`)`](https://friendly.github.io/nestedLogit/reference/nestedMethods.md)
  [`coef(`*`<nestedLogit>`*`)`](https://friendly.github.io/nestedLogit/reference/nestedMethods.md)
  [`vcov(`*`<nestedLogit>`*`)`](https://friendly.github.io/nestedLogit/reference/nestedMethods.md)
  [`update(`*`<nestedLogit>`*`)`](https://friendly.github.io/nestedLogit/reference/nestedMethods.md)
  [`as.matrix(`*`<dichotomies>`*`)`](https://friendly.github.io/nestedLogit/reference/nestedMethods.md)
  [`as.character(`*`<dichotomies>`*`)`](https://friendly.github.io/nestedLogit/reference/nestedMethods.md)
  [`as.matrix(`*`<continuationDichotomies>`*`)`](https://friendly.github.io/nestedLogit/reference/nestedMethods.md)
  [`as.dichotomies()`](https://friendly.github.io/nestedLogit/reference/nestedMethods.md)
  :

  Methods for `"nestedLogit"` and Related Objects

- [`models()`](https://friendly.github.io/nestedLogit/reference/models.md)
  :

  Extract Binary Logit Models from a `nestedLogit` Object

- [`as.tree()`](https://friendly.github.io/nestedLogit/reference/as.tree.md)
  : Display the Tree Structure of Nested Dichotomies

## Plotting methods

Methods for constructing plots of nested logit models.

- [`predict(`*`<nestedLogit>`*`)`](https://friendly.github.io/nestedLogit/reference/predict.nestedLogit.md)
  [`print(`*`<predictNestedLogit>`*`)`](https://friendly.github.io/nestedLogit/reference/predict.nestedLogit.md)
  [`confint(`*`<predictNestedLogit>`*`)`](https://friendly.github.io/nestedLogit/reference/predict.nestedLogit.md)
  [`print(`*`<predictDichotomies>`*`)`](https://friendly.github.io/nestedLogit/reference/predict.nestedLogit.md)
  [`fitted(`*`<nestedLogit>`*`)`](https://friendly.github.io/nestedLogit/reference/predict.nestedLogit.md)
  :

  Predicted Probabilities and Logits for `"nestedLogit"` Models

- [`as.data.frame(`*`<predictNestedLogit>`*`)`](https://friendly.github.io/nestedLogit/reference/as.data.frame.predictNestedLogit.md)
  : Convert a Predicted Objects to a data.frame

- [`plot(`*`<nestedLogit>`*`)`](https://friendly.github.io/nestedLogit/reference/plot.nestedLogit.md)
  : Plotting Nested Logit Models

- [`Effect(`*`<nestedLogit>`*`)`](https://friendly.github.io/nestedLogit/reference/Effect.nestedLogit.md)
  : Effect Displays for Nested Logit Models

## Hypothesis tests

Anova, linear hypothesis, and likelihood ratio tests for nested logit
models.

- [`Anova(`*`<nestedLogit>`*`)`](https://friendly.github.io/nestedLogit/reference/nestedHypotheses.md)
  [`print(`*`<Anova.nestedLogit>`*`)`](https://friendly.github.io/nestedLogit/reference/nestedHypotheses.md)
  [`linearHypothesis(`*`<nestedLogit>`*`)`](https://friendly.github.io/nestedLogit/reference/nestedHypotheses.md)
  [`anova(`*`<nestedLogit>`*`)`](https://friendly.github.io/nestedLogit/reference/nestedHypotheses.md)
  [`print(`*`<anova.nestedLogit>`*`)`](https://friendly.github.io/nestedLogit/reference/nestedHypotheses.md)
  [`logLik(`*`<nestedLogit>`*`)`](https://friendly.github.io/nestedLogit/reference/nestedHypotheses.md)
  :

  Hypothesis-Testing and Related Methods for `"nestedLogit"` Objects

- [`RSQ()`](https://friendly.github.io/nestedLogit/reference/RSQ.md)
  [`print(`*`<RSQ.nestedLogit>`*`)`](https://friendly.github.io/nestedLogit/reference/RSQ.md)
  : Pseudo-R² Measures for Nested Logit Models

## Support for other packages

Methods for broom and effects packages.

- [`glance(`*`<nestedLogit>`*`)`](https://friendly.github.io/nestedLogit/reference/broomMethods.md)
  [`tidy(`*`<nestedLogit>`*`)`](https://friendly.github.io/nestedLogit/reference/broomMethods.md)
  : Broom Related Methods
- [`Effect(`*`<nestedLogit>`*`)`](https://friendly.github.io/nestedLogit/reference/Effect.nestedLogit.md)
  : Effect Displays for Nested Logit Models

## Datasets

Example datasets included in the package.

- [`GSS`](https://friendly.github.io/nestedLogit/reference/GSS.md) :
  Data From the U.S. General Social Survey 1972-2016
- [`HealthInsurance`](https://friendly.github.io/nestedLogit/reference/HealthInsurance.md)
  : Choice of Health Insurance Product
- [`gators`](https://friendly.github.io/nestedLogit/reference/gators.md)
  : Alligator Food Choice
