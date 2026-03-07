# Plan: R² Measures for `nestedLogit` Models

## Overview

We want an `RSQ()` generic with a primary `RSQ.nestedLogit()` method that:

- Computes pseudo-R² measures for each binary logit sub-model (dichotomy)
- Computes an aggregate "Combined" row for the overall polytomous model
- Returns a structured object with a nice `print()` method
- Optionally has methods for `glm`, `polr`, `multinom` (bonus)

---

## Key insight: everything we need is already in `broom::glance()`

`broom::glance()` on a `glm` object returns (among others):

| column          | meaning                                       |
|-----------------|-----------------------------------------------|
| `null.deviance` | deviance of null (intercept-only) model       |
| `df.null`       | df for null model = n − 1                     |
| `logLik`        | log-likelihood of fitted model, L             |
| `AIC`, `BIC`    | information criteria                          |
| `deviance`      | residual deviance = −2L                       |
| `df.residual`   | df for fitted model = n − p                   |
| `nobs`          | number of observations used in this sub-model |

From these we can derive:
- **L₀** = `−null.deviance / 2`   (null log-likelihood)
- **L**  = `logLik`
- **n**  = `nobs`
- **k**  = `df.null − df.residual`  (number of non-intercept parameters)

And `glance.nestedLogit()` already calls this for each sub-model (see `R/broomMethods.R`),
so we can reuse that output directly — no new dependencies needed.

---

## Pseudo-R² formulas

For each dichotomy sub-model (and for the combined model):

| measure        | formula                                               | notes                         |
|----------------|-------------------------------------------------------|-------------------------------|
| McFadden       | `1 − L / L₀`                                         | most common                   |
| McFadden Adj.  | `1 − (L − k) / L₀`                                   | penalises complexity          |
| Cox-Snell      | `1 − exp(2(L₀ − L) / n)`                             | bounded below 1               |
| Nagelkerke     | `CoxSnell / (1 − exp(2 L₀ / n))`                     | rescaled so max = 1           |
| Tjur           | `mean(ŷ | y=1) − mean(ŷ | y=0)`                      | per-dichotomy only (see below)|

L₀ and L are both negative; L > L₀ for any model with predictors, so all R² > 0.

### Tjur's R²

Tjur's coefficient of discrimination requires the binary response and fitted values, which
are available from the `glm` sub-model objects (`model$y`, `fitted(model)`). It is
well-defined for each individual dichotomy. **It is not naturally defined for the "Combined"
row** (the combined model predicts multinomial probabilities, not a single binary outcome),
so Tjur should be `NA` in the Combined row, or omitted from the default display.

---

## Computing the "Combined" row

Because nested logit likelihoods factor as a product of independent binary logit likelihoods,
the combined log-likelihood is simply the sum:

```
L_combined  = Σ L_i         (= logLik.nestedLogit(x), already implemented)
L₀_combined = Σ L₀_i        (= Σ −null.deviance_i / 2)
k_combined  = Σ k_i         (total non-intercept parameters across all dichotomies)
n_combined  = nrow(x$data)  (full polytomous sample; NOT the sum of per-dichotomy nobs,
                              which double-counts observations appearing in multiple sub-models)
```

Using `n_combined = nrow(x$data)` is correct because the pseudo-R² is meant to summarise
the polytomous model as a whole.

AIC and BIC for the combined row come directly from `AIC(x)` and `BIC(x)`, which are
already implemented (via `logLik.nestedLogit`).

---

## Return value and class

`RSQ.nestedLogit()` returns an object of class `c("RSQ.nestedLogit", "data.frame")` with:

- **rows**: one per dichotomy, plus a final `"Combined"` row
- **columns**: `model`, then each requested measure, then (optionally) `AIC`, `BIC`

Example (default `which = c("McFadden", "CoxSnell", "Nagelkerke")`):

```r
#   model  McFadden  CoxSnell  Nagelkerke
#   work     0.1023    0.1293      0.1743
#   full     0.3330    0.3900      0.5103
#   Combined 0.1884    0.2175      0.2673
```

Attributes attached to the object:
- `formula`: the model formula (for printing the header)
- `which`: the measures that were computed

---

## `print.RSQ.nestedLogit()` sketch

```
Pseudo R^2 measures for nestedLogit model:
  partic ~ hincome + children

           McFadden  CoxSnell  Nagelkerke     AIC      BIC
  work        0.102     0.129       0.174   325.7    336.4
  full        0.333     0.390       0.510   224.3    234.9
  Combined    0.188     0.218       0.267   550.0    572.9
```

- Numbers formatted to `digits` decimal places (default 4)
- Separator line or blank line between dichotomies and Combined row
- AIC/BIC right-aligned, wider field

---

## Function sketches

### Helper: compute pseudo-R² from ingredients

```r
# Internal helper — not exported
.pseudo_r2 <- function(L, L0, n, k, which, fitted_vals = NULL, y = NULL) {
  # L, L0: scalars (log-likelihoods of fitted and null model)
  # n:     number of observations
  # k:     number of non-intercept parameters
  # which: character vector of measures to compute
  # fitted_vals, y: needed for Tjur only

  cs <- 1 - exp(2 * (L0 - L) / n)   # Cox-Snell
  cs_max <- 1 - exp(2 * L0 / n)     # Cox-Snell upper bound

  result <- list()
  if ("McFadden"    %in% which) result$McFadden    <- 1 - L / L0
  if ("McFaddenAdj" %in% which) result$McFaddenAdj <- 1 - (L - k) / L0
  if ("CoxSnell"    %in% which) result$CoxSnell    <- cs
  if ("Nagelkerke"  %in% which) result$Nagelkerke  <- cs / cs_max
  if ("Tjur"        %in% which) {
    if (!is.null(fitted_vals) && !is.null(y)) {
      result$Tjur <- mean(fitted_vals[y == 1]) - mean(fitted_vals[y == 0])
    } else {
      result$Tjur <- NA_real_
    }
  }
  as.data.frame(result)
}
```

### Main generic and method

```r
RSQ <- function(x, ...) UseMethod("RSQ")

RSQ.nestedLogit <- function(x,
                             which = c("McFadden", "CoxSnell", "Nagelkerke"),
                             include.AIC = TRUE,
                             include.BIC = FALSE,
                             digits = 4L,
                             ...) {
  which <- match.arg(which,
                     choices = c("McFadden", "McFaddenAdj",
                                 "CoxSnell", "Nagelkerke", "Tjur"),
                     several.ok = TRUE)

  # --- Per-dichotomy rows ---
  gl <- broom::glance(x)   # one row per dichotomy via existing glance.nestedLogit()
  # gl has: null.deviance, df.null, logLik, AIC, BIC, deviance, df.residual, nobs

  mod_names <- gl$response   # dichotomy names

  rows <- vector("list", nrow(gl))
  for (i in seq_along(mod_names)) {
    L  <- gl$logLik[i]
    L0 <- -gl$null.deviance[i] / 2
    n  <- gl$nobs[i]
    k  <- gl$df.null[i] - gl$df.residual[i]   # non-intercept params

    # For Tjur, extract fitted values and response from the glm sub-model
    m <- models(x, mod_names[i])

    rows[[i]] <- .pseudo_r2(L, L0, n, k, which,
                             fitted_vals = fitted(m),
                             y = m$y)
  }

  sub_df <- dplyr::bind_rows(rows)

  # --- Combined row ---
  L_combined  <- sum(gl$logLik)
  L0_combined <- sum(-gl$null.deviance / 2)
  n_combined  <- nrow(x$data)
  k_combined  <- sum(gl$df.null - gl$df.residual)

  # Tjur is NA for combined (not defined for multinomial)
  combined_which <- setdiff(which, "Tjur")
  combined_row <- .pseudo_r2(L_combined, L0_combined, n_combined, k_combined,
                              combined_which)
  if ("Tjur" %in% which) combined_row$Tjur <- NA_real_

  # --- Assemble ---
  result <- dplyr::bind_rows(sub_df, combined_row)
  result <- dplyr::bind_cols(
    data.frame(model = c(mod_names, "Combined")),
    result
  )

  # --- Optionally append AIC, BIC ---
  if (include.AIC) result$AIC <- c(gl$AIC, AIC(x))
  if (include.BIC) result$BIC <- c(gl$BIC, BIC(x))

  structure(result,
            class   = c("RSQ.nestedLogit", "data.frame"),
            formula = formula(x),
            which   = which,
            digits  = digits)
}
```

### Print method

```r
print.RSQ.nestedLogit <- function(x, digits = attr(x, "digits"), ...) {
  cat("Pseudo R\u00b2 measures for nestedLogit model:\n")
  cat(" ", deparse(attr(x, "formula")), "\n\n")

  # separate Combined row visually
  n_dichot <- nrow(x) - 1L

  # Format numeric columns
  num_cols <- setdiff(names(x), "model")
  fmt <- x
  fmt[, num_cols] <- lapply(x[, num_cols, drop = FALSE],
                             function(col) round(col, digits))

  # Print with row separation before Combined
  print(fmt[seq_len(n_dichot), ], row.names = FALSE, ...)
  cat(strrep("-", 40), "\n")
  print(fmt[nrow(fmt), ], row.names = FALSE, ...)

  invisible(x)
}
```

### Bonus: `RSQ.glm()` method

For single `glm` models this is straightforward — one-row result, no Combined row.
`DescTools::PseudoR2()` already does this well, so we could either delegate to it
or implement directly (to avoid a new dependency).

```r
RSQ.glm <- function(x,
                    which = c("McFadden", "CoxSnell", "Nagelkerke"),
                    include.AIC = TRUE,
                    include.BIC = FALSE,
                    ...) {
  which <- match.arg(which, several.ok = TRUE)
  L  <- as.numeric(logLik(x))
  L0 <- -x$null.deviance / 2
  n  <- nobs(x)
  k  <- x$df.null - x$df.residual

  result <- .pseudo_r2(L, L0, n, k, which, fitted(x), x$y)
  if (include.AIC) result$AIC <- AIC(x)
  if (include.BIC) result$BIC <- BIC(x)

  structure(result, class = c("RSQ.glm", "data.frame"),
            formula = formula(x), which = which)
}
```

Methods for `polr` and `multinom` could follow the same pattern (both have `logLik`,
`nobs`, and null deviance accessible, though the null model would need to be fitted
separately via `update(x, . ~ 1)`).

---

## Dependencies

No new package dependencies are needed:

- `broom::glance()` — already imported
- `dplyr::bind_rows()`, `dplyr::bind_cols()` — already imported
- `AIC()`, `BIC()`, `logLik()` — base R / already implemented
- `models()`, `nrow(x$data)` — internal package infrastructure

If `Tjur` is included and we want to keep things clean, note that `m$y` (the binary
response vector from the `glm`) is reliably available for all sub-models.

---

## Implementation plan

1. **Create `R/RSQ.R`** with:
   - `.pseudo_r2()` internal helper
   - `RSQ()` generic
   - `RSQ.nestedLogit()` method
   - `print.RSQ.nestedLogit()` method
   - (optional) `RSQ.glm()` method + `print.RSQ.glm()` [MF: Wait on this until RSQ.nestedLogit is implemented 
   and tested, but this would be desirable at some point, for logistic regression models.]

2. **Export** `RSQ`, `RSQ.nestedLogit`, `RSQ.glm` via roxygen `@export`.

3. **Document** with roxygen, showing the `wlf.nested` example as the primary illustration.

4. **Add to NEWS.md**: "Added `RSQ()` function for pseudo-R² measures for nestedLogit
   models (and optionally glm, polr, multinom)."

5. **Vignette**: A short section in `other-examples` vignette, or a new vignette,
   demonstrating `RSQ()` on the `wlf.nested` and `gators.nested` models side-by-side.
   [MF: Not a new vignette yet. It would make the most sense to include something on this
   in the main `nestedLogit.Rmd` vignette. But wait on this until the functions are fully
   tested and documented.]

---

## Open questions / design choices

1. **Name**: `RSQ()` is clear. Alternative: `pseudoR2()` (mirrors DescTools naming).
   The upper-case generic is more distinctive and avoids namespace collision with
   `rsq::rsq()`.

2. **Default `which`**: `c("McFadden", "CoxSnell", "Nagelkerke")` matches the user's
   spec and is the most commonly reported trio.

3. **AIC/BIC in the return object**: Included by default since they appear in the
   desired display. Arguments `include.AIC` and `include.BIC` control this.
   [MF: Instead, use a single argument, `include = list()`, which can contain "AIC", "BIC" and perhaps other things, like "n"]

4. **`n` for Combined row**: Using `nrow(x$data)` (full sample) rather than the sum of
   per-dichotomy `nobs` (which double-counts). This is the right choice since the
   combined model represents the full polytomous model. [MF: Absolutely!]

5. **`McFaddenAdj`**: The penalty `k` uses non-intercept parameters only (consistent
   with DescTools implementation).

6. **`Tjur` in Combined row**: Set to `NA`. Could alternatively omit the column
   entirely if Tjur is requested — or include it with a footnote in the print method.
   [MF: If Tjur doesn't make sense for the combined row, just use `NA`]
