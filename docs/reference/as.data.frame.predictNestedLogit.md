# Convert a Predicted Objects to a data.frame

These functions provide simple ways to convert the results of
[`predict.nestedLogit`](https://friendly.github.io/nestedLogit/reference/nestedMethods.md)
to a data frame in a consistent format for plotting and other actions.

## Usage

``` r
# S3 method for class 'predictNestedLogit'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  a `"predictNestedLogit"` object

- row.names:

  row.names for result (for conformity with generic; not currently used)

- optional:

  logical. If TRUE, setting row names and converting column names (to
  syntactic names: see
  [`make.names`](https://rdrr.io/r/base/make.names.html) is optional

- ...:

  other arguments (unused)

## Value

- For `predict(..., model="nested")` (the default), returns a data frame
  containing the values of predictors along with the columns `response`,
  `p`, `se.p`, `logit`, `se.logit`.

- For `predict(..., model="dichotomies")`, returns a data frame
  containing the values of predictors along with the columns `response`,
  `logit`, and `se.logit`.

## Examples

``` r
data("Womenlf", package = "carData")
comparisons <- logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                     full=dichotomy("parttime", "fulltime"))

wlf.nested <- nestedLogit(partic ~ hincome + children,
                          dichotomies = comparisons,
                          data=Womenlf)
# get predicted values for a grid of `hincome` and `children`
new <- expand.grid(hincome=seq(0, 45, length=10),
                   children=c("absent", "present"))

pred.nested <- predict(wlf.nested, new)
plotdata <- as.data.frame(pred.nested)
str(plotdata)
#> 'data.frame':    60 obs. of  7 variables:
#>  $ hincome : num  0 0 0 5 5 5 10 10 10 15 ...
#>  $ children: Factor w/ 2 levels "absent","present": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ response: chr  "not.work" "parttime" "fulltime" "not.work" ...
#>  $ p       : num  0.2082 0.0237 0.7681 0.2452 0.0378 ...
#>  $ se.p    : num  0.0633 0.0177 0.0639 0.058 0.0222 ...
#>  $ logit   : num  -1.34 -3.72 1.2 -1.12 -3.24 ...
#>  $ se.logit: num  0.384 0.767 0.358 0.313 0.61 ...

# Predicted logit values for the dichotomies
pred.dichot <- predict(wlf.nested, new, model = "dichotomies")
plotlogit <- as.data.frame(pred.dichot)
str(plotlogit)
#> 'data.frame':    40 obs. of  5 variables:
#>  $ hincome : num  0 5 10 15 20 25 30 35 40 45 ...
#>  $ children: Factor w/ 2 levels "absent","present": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ response: chr  "work" "work" "work" "work" ...
#>  $ logit   : num  1.336 1.124 0.913 0.701 0.49 ...
#>  $ se.logit: num  0.384 0.313 0.262 0.242 0.261 ...
#>  - attr(*, "out.attrs")=List of 2
#>   ..$ dim     : Named int [1:2] 10 2
#>   .. ..- attr(*, "names")= chr [1:2] "hincome" "children"
#>   ..$ dimnames:List of 2
#>   .. ..$ hincome : chr [1:10] "hincome= 0" "hincome= 5" "hincome=10" "hincome=15" ...
#>   .. ..$ children: chr [1:2] "children=absent" "children=present"
```
