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
