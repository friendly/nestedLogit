# easystats vignette — session notes

**File**: `vignettes/articles/easystats.Rmd`

## Status (as of 2026-02-26)

Most of the vignette is working and knits cleanly. Two issues remain under review:

### 1. `plot(mp)` — curly-brace workaround (WORKING, prose needs review)

`see::plot.parameters_model()` passes `Response` column values to `gsub()` as
regex patterns. `nestedLogit` labels dichotomy sides with `{}` (e.g.
`{parttime} vs. {fulltime}`), which are invalid regex quantifiers.

**Workaround in vignette** (params-forest chunk):
```r
mp_plot <- mp
mp_plot$Response <- gsub("[{}]", "", mp_plot$Response)
plot(mp_plot)
```
The prose explaining this was just written — **needs review** before finalising.

### 2. `standardize = "refit"` — silent no-op (NOTED, issue drafted)

`plot(mp_plot, standardize = "refit")` produces a plot identical to the
unstandardized one. Likely cause: no `standardize_parameters()` method for
`nestedLogit` objects. A draft GitHub issue is at `dev/parameters-issue.md`.
**Needs review before filing.**

### 3. `binned_residuals()` — WORKAROUND IN PLACE, needs test run

Root cause identified: `nestedLogit` stores sub-model `call$data` as the
original dataset name (e.g. `Womenlf`), but the binary response column was
only ever in a temporary local data frame during fitting. `insight::get_data()`
retrieves `Womenlf` which lacks the binary response column, causing
"undefined columns selected".

Workaround: reconstruct proper `glm` objects with binary response in the data.
The `diag-binned-*` chunks now do this explicitly. Still need a test knit to
confirm the plots render correctly.

## Chunks that are confirmed working

- `insight-model-info`, `insight-formula`, `insight-params`, `insight-vcov`
- `params-basic`, `params-compare`, `params-compare-plot`
- `params-bootstrap` (slow — 500 iterations)
- `perf-summary`, `perf-r2`, `perf-r2-other`
- `submodel-extract`, `diag-work`, `diag-full`

## Known limitations (already in vignette Limitations section)

- `modelbased::estimate_expectation()` does not work on `nestedLogit`
- `check_model()` does not work directly on `nestedLogit`; use `models()` first

## Next steps

1. Review and polish prose around the `{}` workaround (params-forest section)
2. Review `dev/parameters-issue.md` and file the two bugs upstream
3. Run the `diag-binned-*` chunks and check output
4. Final knit / `devtools::build_vignettes()` check
