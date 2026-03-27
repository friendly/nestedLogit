# Feature request: `logit_notation` option for GLM equations

## Summary

For logistic regression models, `extract_eq()` currently renders the LHS as an
explicit log-odds fraction:

```latex
\log\left[ \frac{P(\hat{Y} = 1)}{1 - P(\hat{Y} = 1)} \right] = \alpha + \beta_1 X_1 + \cdots
```

It would be useful to have an option to render this in the more compact and
widely-used **logit notation**:

```latex
\operatorname{logit}\left[ P(\hat{Y}) \right] = \alpha + \beta_1 X_1 + \cdots
```

Both forms are mathematically equivalent, but the logit form is standard in
many textbooks and journals, and is considerably more compact — especially when
equations are long or displayed in presentations.

## Motivating context: nested logit models

This came up while adding an `extract_eq()` S3 method to the
[`nestedLogit`](https://friendly.github.io/nestedLogit/) package (CRAN).  A
nested logit model is a collection of binary logit sub-models, one per
dichotomy.  For a model with several dichotomies and several predictors, the
equations become long, and the log-fraction LHS adds visual noise on top of an
already busy RHS.

See: [the vignette](https://friendly.github.io/nestedLogit/articles/latex-equations.html) 
illustrating the current implementation.

In this context:
- Each sub-model is a standard `glm(..., family = binomial)` object.
- `extract_eq()` is called on each sub-model in turn; the results are assembled
  into a named list and printed separately in an R Markdown document.
- Because there are multiple equations (one per dichotomy), compactness of the
  LHS matters more than in the single-equation case.

The `nestedLogit` method currently works around the limitation by post-processing
the raw LaTeX string returned by `extract_eq()`, but it would be cleaner to
handle this upstream.

## Proposed interface

A new logical argument on `extract_eq.glm()` (and propagated through
`extract_eq.default()` for any `binomial`-family GLM):

```r
extract_eq(model, logit_notation = FALSE, ...)
```

When `logit_notation = TRUE`:
- Replace `\log\left[\frac{P(Y)}{1 - P(Y)}\right]` with
  `\operatorname{logit}\left[P(Y)\right]` on the LHS.
- Drop the redundant `= 1` from `P(Y = 1)` when collapsing to `P(Y)`.

Default `FALSE` preserves current behaviour.

## Implementation sketch

The substitution could be applied as a post-processing step on the assembled
LaTeX string, using two passes:

```r
# 1. Strip "= 1" from P(Y = 1)  →  P(Y)
eq_str <- gsub(
  "(P\\(\\s*\\\\operatorname\\{[^}]+\\})\\s*=\\s*\\\\operatorname\\{1\\}",
  "\\1", eq_str, perl = TRUE
)

# 2. Replace log-fraction with logit[...]
eq_str <- gsub(
  "\\\\log\\\\left\\[\\s*\\\\frac\\s*\\{(P\\([^)]+\\))\\}\\{\\s*1\\s*-\\s*\\1\\s*\\}\\s*\\\\right\\]",
  "\\\\operatorname{logit}\\\\left[\\1\\\\right]",
  eq_str, perl = TRUE
)
```

(Note: back-references in the pattern require `perl = TRUE`; R's default TRE
engine only supports back-references in *replacement* strings.)

## Related

- The same simplification would apply to `probit` and other binary-response
  GLMs, where the LHS could similarly be written as
  `\operatorname{probit}[P(Y)]` rather than `\Phi^{-1}[P(Y)]`, though that is
  a separate decision.
- For `polr` (ordered logit/probit), the threshold form of the LHS is different
  and would need separate treatment.
- GLMs in general allow a wide variety of link functions `g(Y)` so this idea could be more generally useful.

## Testing the `logit-notation` branch

Installed via `remotes::install_github("datalorax/equatiomatic@logit_notation")` and
tested with the `nestedLogit` package.

### What works

`logit_notation = TRUE` produces the correct LHS on a single GLM sub-model:

```r
mod.work <- models(wlf.nested, "work")
extract_eq(mod.work, logit_notation = TRUE)
```

Output (as expected):

```latex
$$
\operatorname{logit}\left[ P( \operatorname{work} ) \right] = \alpha + \beta_{1}(\operatorname{hincome}) + \beta_{2}(\operatorname{children}_{\operatorname{present}})
$$
```

The argument also passes through correctly to the `nestedLogit` S3 method, which
calls `extract_eq()` on each sub-model in turn — so the full nested model works too.

### Bug: misleading message when `logit_notation = TRUE` is actually applied

Every call to `extract_eq(model, logit_notation = TRUE)` emits:

```
logit_notation = TRUE ignored when show_distribution is TRUE.
```

But the output IS correct — the logit notation is applied.  The message is
contradicted by the result.  It appears that the warning fires unconditionally
(perhaps whenever `show_distribution` takes its default `TRUE` value) rather than
only when the substitution is genuinely being skipped.  This should either be
suppressed when the substitution succeeds, or the condition guarding it should be
inverted.

### Downstream issue: `preview_eq()` fails on already-rendered equation strings

`preview_eq()` works fine on a single sub-model:

```r
extract_eq(mod.work, logit_notation = TRUE) |> preview_eq()   # OK
```

But piping the full `nestedLogit` output to `preview_eq()` errors:

```r
extract_eq(wlf.nested, logit_notation = TRUE) |> preview_eq()
# logit_notation = TRUE ignored when show_distribution is TRUE.
# logit_notation = TRUE ignored when show_distribution is TRUE.
# Error in UseMethod("extract_lhs", model) :
#   no applicable method for 'extract_lhs' applied to an object of class
#   "c('equation', 'character')"
```

The `nestedLogit` method returns a named list of `"equation"` objects that are
already-rendered character strings (class `c("equation", "character")`).
`preview_eq()` then tries to call `extract_lhs()` on each one as if it were still
a model object, which fails.  This is most likely a `nestedLogit`-side issue —
the package needs a `preview_eq` method for its `"nestedLogit_equations"` list
class — rather than a bug in equatiomatic.  Noting it here for completeness.

### Summary

| Test | Result |
|------|--------|
| `extract_eq(glm_submodel, logit_notation = TRUE)` | ✓ correct output |
| `extract_eq(nestedLogit_model, logit_notation = TRUE)` | ✓ correct output |
| Message "ignored when show_distribution is TRUE" | ✗ misleading — fires even when substitution succeeds |
| `extract_eq(...) \|> preview_eq()` on single sub-model | ✓ works |
| `extract_eq(nestedLogit_model, ...) \|> preview_eq()` | ✗ errors (nestedLogit-side fix needed) |
