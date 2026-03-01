# Broom Related Methods

These functions give compact summaries of a `"nestedLogit"` object

- `glance`:

  Construct a single row summaries for the dichotomies `"nestedLogit"`
  model.

- `tidy`:

  Summarizes the terms in `"nestedLogit"` model.

## Usage

``` r
# S3 method for class 'nestedLogit'
glance(x, ...)

# S3 method for class 'nestedLogit'
tidy(x, ...)
```

## Arguments

- x:

  an object of class `"nestedLogit"`.

- ...:

  arguments to be passed down.

## Value

- `glance` returns a `tibble` containing one row of fit statistics for
  each dichotomy, labeled `response`. See
  [`glance`](https://generics.r-lib.org/reference/glance.html) for
  details.

- `tidy` returns a `tibble` containing coefficient estimates and test
  statistics for the combinations of `response` and `term`. See
  [`tidy`](https://generics.r-lib.org/reference/tidy.html) for details.

## See also

[`nestedMethods`](https://friendly.github.io/nestedLogit/reference/nestedMethods.md),
[`glance`](https://generics.r-lib.org/reference/glance.html),
[`tidy`](https://generics.r-lib.org/reference/tidy.html)

## Examples

``` r
data("Womenlf", package = "carData")
m <-  nestedLogit(partic ~ hincome + children,
                  dichotomies = logits(work=dichotomy("not.work",
                                                      working=c("parttime", "fulltime")),
                                       full=dichotomy("parttime", "fulltime")),
                  data=Womenlf)

# one-line summaries
broom::glance(m)
#> # A tibble: 2 × 9
#>   response null.deviance df.null logLik   AIC   BIC deviance df.residual  nobs
#>   <chr>            <dbl>   <int>  <dbl> <dbl> <dbl>    <dbl>       <int> <int>
#> 1 work              356.     262 -160.   326.  336.     320.         260   263
#> 2 full              144.     107  -52.2  110.  119.     104.         105   108
# coefficients and tests
broom::tidy(m)
#> # A tibble: 6 × 6
#>   response term            estimate std.error statistic      p.value
#>   <chr>    <chr>              <dbl>     <dbl>     <dbl>        <dbl>
#> 1 work     (Intercept)       1.34      0.384       3.48 0.000500    
#> 2 work     hincome          -0.0423    0.0198     -2.14 0.0324      
#> 3 work     childrenpresent  -1.58      0.292      -5.39 0.0000000700
#> 4 full     (Intercept)       3.48      0.767       4.53 0.00000580  
#> 5 full     hincome          -0.107     0.0392     -2.74 0.00615     
#> 6 full     childrenpresent  -2.65      0.541      -4.90 0.000000957 
```
