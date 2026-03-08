# Pseudo-R² Measures for Nested Logit Models

Computes pseudo-R² and related fit measures for a `"nestedLogit"`
object, with one row per binary logit sub-model (dichotomy) and an
additional `"Combined"` row for the overall polytomous model.

## Usage

``` r
RSQ(x, ...)

# S3 method for class 'nestedLogit'
RSQ(
  x,
  which = c("McFadden", "CoxSnell", "Nagelkerke"),
  include = "AIC",
  digits = 4L,
  ...
)

# S3 method for class 'RSQ.nestedLogit'
print(x, digits = attr(x, "digits"), ...)
```

## Arguments

- x:

  a `"nestedLogit"` object.

- ...:

  currently unused.

- which:

  character vector naming the pseudo-R² measures to compute. Any subset
  of `c("McFadden", "McFaddenAdj", "CoxSnell", "Nagelkerke", "Tjur")`.
  Default: `c("McFadden", "CoxSnell", "Nagelkerke")`.

- include:

  character vector of additional columns to append to the result. Any
  subset of `c("AIC", "BIC", "n")`, where `"n"` adds the number of
  observations used for each row. Default: `"AIC"`.

- digits:

  integer; number of decimal places used when printing (default `4L`).

## Value

An object of class `c("RSQ.nestedLogit", "data.frame")` with one row per
dichotomy plus a final `"Combined"` row, and columns for the model name,
the requested pseudo-R² measures, and any additional statistics
requested via `include`. The `formula` used to fit the model and the
`digits` argument are stored as attributes and used by the `print`
method.

## Details

`RSQ` is implemented as an S3 generic to allow for similar functions for
related models

The following measures are available via the `which` argument:

- `"McFadden"`:

  1 - L/L\\\_0\\

- `"McFaddenAdj"`:

  1 - (L - k)/L\\\_0\\, penalised for number of predictors

- `"CoxSnell"`:

  1 - exp(2(L\\\_0\\ - L)/n), bounded strictly below 1

- `"Nagelkerke"`:

  Cox-Snell rescaled to have a maximum of 1

- `"Tjur"`:

  Difference in mean fitted values between the two response categories;
  per-dichotomy only (`NA` in the Combined row)

For the **Combined** row the log-likelihood is the sum of the sub-model
log-likelihoods (exploiting the independence of the nested dichotomies),
and \\n\\ is `nrow(x$data)` — the full sample size of the polytomous
model — not the sum of per-dichotomy observation counts, which would
double-count observations that appear in more than one sub-model.

A wider range of pseudo-R² measures for logistic-type models (`glm`,
`polr`, `multinom`, `vglm`) is available in
[`PseudoR2`](https://andrisignorell.github.io/DescTools/reference/PseudoR2.html).
For an accessible overview of these measures see
<https://statisticalhorizons.com/r2logistic/>.

## See also

[`nestedLogit`](https://friendly.github.io/nestedLogit/reference/nestedLogit.md),
[`glance`](https://generics.r-lib.org/reference/glance.html),
[`PseudoR2`](https://andrisignorell.github.io/DescTools/reference/PseudoR2.html)

## Author

Michael Friendly

## Examples

``` r
data("Womenlf", package = "carData")
wlf.nested <- nestedLogit(partic ~ hincome + children,
  logits(work = dichotomy("not.work", c("parttime", "fulltime")),
         full = dichotomy("parttime", "fulltime")),
  data = Womenlf)

# Default: McFadden, CoxSnell, Nagelkerke + AIC
RSQ(wlf.nested)
#> Error in RSQ(wlf.nested): could not find function "RSQ"

# All measures, with AIC, BIC & n
RSQ(wlf.nested,
    which   = c("McFadden", "McFaddenAdj", "CoxSnell", "Nagelkerke", "Tjur"),
    include = c("AIC", "BIC", "n"))
#> Error in RSQ(wlf.nested, which = c("McFadden", "McFaddenAdj", "CoxSnell",     "Nagelkerke", "Tjur"), include = c("AIC", "BIC", "n")): could not find function "RSQ"
```
