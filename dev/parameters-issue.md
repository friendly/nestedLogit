# Draft GitHub issues for easystats packages

---

## Issue A — `see` / `parameters`

**Target repo**: https://github.com/easystats/see (Issue 1) and https://github.com/easystats/parameters (Issue 2)

**Title**: `plot()` fails on dichotomy names containing `{}`; `standardize = "refit"` has no visible effect

### Description

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

## Issue B — `performance` / `insight`

**Target repo**: https://github.com/easystats/performance

**Title**: `binned_residuals()` errors on `glm` sub-models extracted from `nestedLogit` — "undefined columns selected"

### Description

Calling `binned_residuals()` on a `glm` sub-model extracted from a `nestedLogit`
object via `models()` errors with:

```
Error in `[.data.frame`(model_data, , rn, drop = FALSE) :
  undefined columns selected
```

### Root cause

`nestedLogit` builds each binary sub-model by creating a temporary response column
(`..y`) in a local copy of the data, fitting the `glm`, then **renaming** the
formula's response to the dichotomy name (e.g. `work`) and resetting `call$data`
to the name of the original dataset (e.g. `Womenlf`). The binary response column
is never written back to the original data frame.

When `binned_residuals()` calls `insight::get_data(model)`, it evaluates
`call$data` and retrieves the original dataset — which does not contain the
binary response column. `insight::find_response(model)` then returns `"work"`,
and selecting `model_data[, "work"]` fails because the column does not exist.

### Reproducible example

```r
library(nestedLogit)
library(performance)

data(Womenlf, package = "carData")
wlf.nested <- nestedLogit(partic ~ hincome + children,
                          dichotomies = logits(
                            work = dichotomy("not.work", c("parttime", "fulltime")),
                            full = dichotomy("parttime", "fulltime")),
                          data = Womenlf)

wlf_work <- models(wlf.nested, "work")   # class "glm"
binned_residuals(wlf_work)               # Error: undefined columns selected
```

### Workaround (user-side)

Reconstruct a proper `glm` with the binary response explicitly in the data:

```r
df_work <- within(Womenlf, work <- as.integer(partic != "not.work"))
glm_work2 <- glm(work ~ hincome + children, data = df_work, family = binomial)
plot(binned_residuals(glm_work2))
```

### Possible fix

In `insight::get_data()`, fall back to `model.frame(model)` when the response
column named by `find_response()` is absent from the data retrieved via
`call$data`. The stored model frame (`model$model`) contains the correct binary
response under its original name (`..y`), though that name also won't match
`find_response()`. A more robust fix would require `nestedLogit` to store the
binary response columns in the data, or for `insight` to add a
`get_data.nestedLogit_glm` method.

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
