# 

As befits a model-fitting function, the package defines a nearly
complete set of methods for class `nestedLogit` objects:

- [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html): prints the results
  for each of the submodels
- [`update()`](https://rdrr.io/r/stats/update.html) re-fits a model,
  allowing changes in the model `formula`, `data`, `subset`, and
  `contrasts`.
- [`coef()`](https://rdrr.io/r/stats/coef.html) returns the coefficients
  for the predictors in each dichotomy
- [`vcov()`](https://rdrr.io/r/stats/vcov.html) returns the
  variance-covariance matrix of the predictors
- [`predict()`](https://rdrr.io/r/stats/predict.html) obtains predicted
  probabilities for the response categories, useful for producing plots
  to aid interpretation.
- [`glance()`](https://generics.r-lib.org/reference/glance.html),
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) are
  extensions of
  [`broom::glance.glm()`](https://broom.tidymodels.org/reference/glance.glm.html)
  and
  [`broom::tidy.glm()`](https://broom.tidymodels.org/reference/tidy.glm.html)
  to obtain compact summaries of a `nestedLogit` model object\`.
- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) provides
  basic plots of the predicted probabilities over a range of values of
  the predictor variables.
- [`models()`](https://friendly.github.io/nestedLogit/reference/models.md)
  is an extractor function to extract the separate models binary logit
  models from the `"nestedLogit"` object

These are supplemented by various methods for testing hypotheses about
nested logit models:

- [`anova()`](https://rdrr.io/r/stats/anova.html) provides ANOVA Type I
  (sequential) tests for each dichotomy and for the combined model. When
  given a sequence of objects,
  [`anova()`](https://rdrr.io/r/stats/anova.html) tests the models
  against one another in the order specified.
- [`Anova()`](https://rdrr.io/pkg/car/man/Anova.html) uses
  [`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html) to provide
  ANOVA Type II (partial) tests for each dichotomy and for the combined
  model.
- [`linearHypothesis()`](https://rdrr.io/pkg/car/man/linearHypothesis.html)
  gives Wald tests for hypotheses about coefficients or their linear
  combinations
- `logLike()` returns the log-likelihood and degrees of freedom for the
  nested-dichotomies model;
- through the last, [`AIC()`](https://rdrr.io/r/stats/AIC.html) and
  [`BIC()`](https://rdrr.io/r/stats/AIC.html) provide model-comparison
  statistics.
