# Draft GitHub issue for parameters/see

**Target repo**: https://github.com/easystats/parameters (or https://github.com/easystats/see)

---

## Title

`standardize = "refit"` in `model_parameters()` has no visible effect on forest plots for multi-response models; also, `plot()` fails on dichotomy names containing `{}`

---

## Description

When calling `model_parameters()` on a `nestedLogit` model and then plotting with
`see::plot()`, two issues arise:

### Issue 1: `plot()` errors on `{}` in Response column

`nestedLogit` labels multi-category dichotomy sides with curly-brace notation,
e.g. `{parttime} vs. {fulltime}`. The `see::plot.parameters_model()` method passes
`Response` column values to `gsub()` as regular-expression patterns, causing an error:

```
Warning in gsub(i, "", x$Parameter) :
  TRE pattern compilation error 'Invalid contents of {}'
Error in gsub(i, "", x$Parameter) :
  invalid regular expression '{parttime} vs. {fulltime}', reason 'Invalid contents of {}'
```

**Fix**: Use `fixed = TRUE` in the internal `gsub()` call, or escape special regex
characters in the `Response` values before passing them to `gsub()`.

**Workaround** (user-side):
```r
mp_plot <- mp
mp_plot$Response <- gsub("[{}]", "", mp_plot$Response)
plot(mp_plot)
```

### Issue 2: `standardize = "refit"` produces an unchanged forest plot

When predictors are on different scales (e.g., a continuous income variable and a
binary factor), standardizing coefficients before plotting is desirable. The
`standardize = "refit"` option is documented to re-fit the model with z-scored
predictors, but the resulting forest plot appears identical to the unstandardized one:

```r
library(nestedLogit)
library(parameters)
library(see)

data(Womenlf, package = "carData")
wlf.nested <- nestedLogit(partic ~ hincome + children,
                          dichotomies = logits(
                            work = dichotomy("not.work", c("parttime", "fulltime")),
                            full = dichotomy("parttime", "fulltime")),
                          data = Womenlf)

mp <- model_parameters(wlf.nested)

# Workaround for Issue 1
mp_plot <- mp
mp_plot$Response <- gsub("[{}]", "", mp_plot$Response)

# This should standardize, but plot looks the same as plot(mp_plot)
plot(mp_plot, standardize = "refit")
```

**Expected**: coefficient estimates rescaled to standard-deviation units, with
noticeably different magnitudes (especially for `hincome`, which is on a
continuous scale, vs. `children`, which is binary).

**Actual**: the plot appears identical to the unstandardized version.

**Likely cause**: `standardize_parameters()` may not have a method for `nestedLogit`
objects, so the `standardize` argument in `plot.parameters_model()` either silently
fails or is not passed through correctly.

---

## Session info

```r
sessionInfo()
# (fill in before filing)
```

## Related

- `nestedLogit` package: https://friendly.github.io/nestedLogit/
- The `nestedLogit` support in `insight` / `parameters` was added via
  `insight::model_parameters.nestedLogit()` and related methods.
