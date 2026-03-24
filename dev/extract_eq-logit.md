# Notes: logit() notation in `extract_eq.nestedLogit`

## What equatiomatic currently produces

For a binary logistic regression, `extract_eq()` always renders the LHS as a
log-odds fraction.  Two variants appear depending on how equatiomatic reads the
model's response:

**Case 1** — response column was `..y` coded 0/1 (the normal nestedLogit case):
```latex
\log\left[ \frac { P( \operatorname{work} = \operatorname{1} ) }
                 { 1 - P( \operatorname{work} = \operatorname{1} ) } \right]
  = \alpha + \beta_{1}(\operatorname{hincome}) + \beta_{2}(\operatorname{children}_{\operatorname{present}})
```

**Case 2** — response column name used directly (e.g. `fish_inv`):
```latex
\log\left[ \frac { P( \operatorname{fish.inv} ) }
                 { 1 - P( \operatorname{fish.inv} ) } \right]
  = \ldots
```

The desired output in logit notation would be:
```latex
\operatorname{logit}\left[ P(\operatorname{work}) \right]
  = \alpha + \beta_{1}(\operatorname{hincome}) + \beta_{2}(\operatorname{children}_{\operatorname{present}})
```

---

## Does equatiomatic support logit notation natively?

**No.**  As of equatiomatic 0.3.x (the current CRAN release), there is no
argument like `use_logit = TRUE` or `link_notation` to switch the LHS from the
log-odds fraction to `logit[P(...)]`.  The fraction form is hard-coded in the
`glm` template inside equatiomatic.

The equatiomatic GitHub issues mention requests for this kind of simplification,
but it has not been implemented upstream.

---

## What implementation would entail

Since we already post-process the LaTeX string in `extract_eq.nestedLogit`
(replacing `..y` with the dichotomy name, replacing `_` with `.`), a
`logit_notation` option could be added as a third post-processing step.

### Proposed interface

```r
extract_eq(wlf.nested, logit_notation = TRUE)
```

Default would be `FALSE` to preserve current behaviour.

### Regex approach

The LaTeX pattern to replace is (condensed, ignoring optional whitespace):

```
\log\left[ \frac { P( <lhs> ) }{ 1 - P( <lhs> ) } \right]
```

where `<lhs>` is one of:
- `\operatorname{work} = \operatorname{1}`  (Case 1 after ..y replacement)
- `\operatorname{work}`                     (Case 2)

A single regex can cover both:

```r
# Match: \log\left[ \frac { P( STUFF ) }{ 1 - P( STUFF ) } \right]
# and replace with: \operatorname{logit}\left[ P( STUFF ) \right]

pattern <- paste0(
  "\\\\log\\\\left\\[\\s*\\\\frac\\s*\\{\\s*P\\(\\s*(",
  "[^)]+",            # capture group 1: the LHS content
  ")\\s*\\)\\s*\\}",
  "\\{\\s*1\\s*-\\s*P\\([^)]+\\)\\s*\\}",
  "\\s*\\\\right\\]"
)
replacement <- "\\\\operatorname{logit}\\\\left[ P(\\1) \\\\right]"
```

There are a few wrinkles:

1. **Whitespace variation**: equatiomatic inserts spaces inconsistently around
   `\frac`, `\left[`, and inside `P(...)`.  The regex must use `\s*` liberally
   or normalise whitespace first.

2. **`= \operatorname{1}` in the numerator**: The desired logit form drops the
   `= \operatorname{1}` part (it is redundant once we write `logit[P(work)]`).
   The capture group should be defined to exclude it, or a second gsub should
   strip it.  One clean approach: strip `\s*=\s*\\operatorname\{1\}` from the
   captured group in the replacement.

3. **Already-replaced name**: By the time this third gsub runs, the dichotomy
   name has already been substituted (e.g. `\operatorname{work}`), so the regex
   only needs to handle that form — not `..y`.

4. **`= \operatorname{1}` drop**: needs careful handling so
   `P(\operatorname{work} = \operatorname{1})` becomes `P(\operatorname{work})`
   in the output, not `P(\operatorname{work} = \operatorname{1})`.

### Alternative: two-step gsub

Instead of one complicated regex, two simpler substitutions may be cleaner:

```r
# Step A: collapse  P( X = \operatorname{1} )  to  P( X )
eq_str <- gsub("(P\\(\\s*\\\\operatorname\\{[^}]+\\})\\s*=\\s*\\\\operatorname\\{1\\}",
               "\\1", eq_str)

# Step B: replace log-fraction with logit[...]
eq_str <- gsub(
  "\\\\log\\\\left\\[\\s*\\\\frac\\s*\\{(P\\([^)]+\\))\\}\\{1\\s*-\\s*\\1\\}\\s*\\\\right\\]",
  "\\\\operatorname{logit}\\\\left[\\1\\\\right]",
  eq_str)
```

Step B uses a back-reference `\1` to ensure the denominator exactly mirrors the
numerator — this is robust but requires that whitespace was already normalised in
Step A.

---

## Complexity assessment

| Aspect | Difficulty |
|---|---|
| Core regex | Moderate — log-fraction pattern is predictable |
| Whitespace variation in equatiomatic output | Low–medium — add `\s*` guards |
| `= \operatorname{1}` stripping | Low — separate gsub before the main one |
| Back-reference matching numerator/denominator | Medium — PCRE supports it; R's `gsub` uses TRE which does NOT support back-references in patterns (only in replacements) |
| Testing across use_coefs, coloring, wrap | Low — the LHS structure does not change with those options |

**Key constraint**: R's default `gsub` (using the TRE engine) does **not**
support back-references in the *pattern* — only `perl = TRUE` does.
The two-step approach above avoids this by using a separate pass for the
numerator normalisation, then a simpler pattern for the replacement.

---

## Recommendation

This is a cosmetically attractive change — `logit[P(work)]` is unambiguous,
standard notation, and much more compact.  The implementation is achievable with
`perl = TRUE` or a two-step gsub.

However, it does add a new user-facing argument (`logit_notation`) and
a non-trivial regex dependency.  Since equatiomatic may eventually add this
natively, keeping the post-processing approach localised in
`extract_eq.nestedLogit` would make it easy to remove later.

**Suggested action**: defer unless the fraction form proves to be a genuine
usability complaint.  If implemented, default to `FALSE` and document the
option in `@param` with a note that it elides the `= 1` from `P(work = 1)`.
