# Alligator Food Choice

Agresti (1996, p. 207) gives this data on 59 alligators sampled from a
lake in Florida. It has the length of the alligator in meters and the
primary food type found in the alligator's stomach. The food type was
classified into three categories: "Fish", "Invertebrates", and "Other".

Of interest is whether or not the length of an alligator is associated
with the primary food type. Does knowing the length of an alligator give
us some indication about its primary food type? If so, how is length
associated with the choice of food type?

## Usage

``` r
data("gators", package = "nestedLogit")
```

## Format

A data frame with 59 rows and 2 columns.

- food:

  Primary food type found in the alligator's stomach, a factor with
  levels `"Other"`, `"Fish"`, and `"Invertebrates"`.

- length:

  Length of the alligator in meters, a numeric vector.

## Source

Agresti, A. (1996). *An Introduction to Categorical Data Analysis*.
Wiley.

## References

An example using this from
<https://data.library.virginia.edu/getting-started-with-multinomial-logit-models/>.

## See also

[`nestedLogit`](https://friendly.github.io/nestedLogit/reference/nestedLogit.md).

## Examples

``` r
data(gators)
table(gators$food)
#> 
#>         Other          Fish Invertebrates 
#>             8            31            20 
# average length of gators by food
with(gators, tapply(length, food, mean))
#>         Other          Fish Invertebrates 
#>      2.422500      2.358387      1.660000 

# create dichotomies
dichot <- logits(d1=dichotomy("Other", c("Fish", "Invertebrates")),
                 d2=dichotomy("Fish", "Invertebrates"))

gators.nested <- nestedLogit(food ~ length,
                            dichotomies = dichot,
                            data = gators)
car::Anova(gators.nested)
#> 
#>  Analysis of Deviance Tables (Type II tests)
#>  
#> Response d1: {Other} vs. {Fish, Invertebrates}
#>        LR Chisq Df Pr(>Chisq)
#> length   1.3685  1     0.2421
#> 
#> 
#> Response d2: {Fish} vs. {Invertebrates}
#>        LR Chisq Df Pr(>Chisq)    
#> length   15.865  1  6.802e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> Combined Responses
#>        LR Chisq Df Pr(>Chisq)    
#> length   17.234  2   0.000181 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# use the plot method
plot(gators.nested, x.var = "length")
```
