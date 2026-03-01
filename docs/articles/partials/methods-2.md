# 

As befits a model-fitting function, the package defines a nearly
complete set of methods for `"nestedLogit"` objects:

- [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) print the results
  for each of the submodels.
- [`update()`](https://rdrr.io/r/stats/update.html) re-fits the model,
  allowing changes to the model `formula`, `data`, `subset`, and
  `contrasts` arguments.
- [`coef()`](https://rdrr.io/r/stats/coef.html) returns the coefficients
  for the predictors in each dichotomy.
- [`vcov()`](https://rdrr.io/r/stats/vcov.html) returns the
  variance-covariance matrix of the predictors.
- [`predict()`](https://rdrr.io/r/stats/predict.html) computes predicted
  probabilities for the response categories, either for the cases in the
  data or for arbitrary combinations of the predictors; the latter is
  useful for producing plots to aid interpretation.
- [`glance()`](https://generics.r-lib.org/reference/glance.html) and
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) are
  extensions of
  [`broom::glance.glm()`](https://broom.tidymodels.org/reference/glance.glm.html)
  and
  [`broom::tidy.glm()`](https://broom.tidymodels.org/reference/tidy.glm.html)
  to obtain compact summaries of a `"nestedLogit"` model object.
- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) provides
  basic plots of the predicted probabilities over a range of values of
  the predictor variables.
- [`models()`](https://friendly.github.io/nestedLogit/reference/models.md)
  is an extractor function for the binary logit models in the
  `"nestedLogit"` object

These are supplemented by various methods for testing hypotheses about
and comparing nested logit models:

- [`anova()`](https://rdrr.io/r/stats/anova.html) provides
  analysis-of-deviance Type I (sequential) tests for each dichotomy and
  for the combined model. When given a sequence of model objects,
  [`anova()`](https://rdrr.io/r/stats/anova.html) tests the models
  against one another in the order specified.
- [`Anova()`](https://rdrr.io/pkg/car/man/Anova.html) uses
  [`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html) to provide
  analysis-of-deviance Type II or III (partial) tests for each dichotomy
  and for the combined model.
- [`linearHypothesis()`](https://rdrr.io/pkg/car/man/linearHypothesis.html)
  computes Wald tests for hypotheses about coefficients or their linear
  combinations.
- [`logLik()`](https://rdrr.io/r/stats/logLik.html) returns the
  log-likelihood and degrees of freedom for the nested-dichotomies logit
  model.
- Through [`logLik()`](https://rdrr.io/r/stats/logLik.html), the
  [`AIC()`](https://rdrr.io/r/stats/AIC.html) and
  [`BIC()`](https://rdrr.io/r/stats/AIC.html) functions compute the
  Akaike and Bayesian information criteria model-comparison statistics.
