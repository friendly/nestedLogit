# Methods for `"nestedLogit"` and Related Objects

Various methods for processing `"nestedLogit"` and related objects. Most
of these are the standard methods for a model-fitting function.

- `coef`, `vcov`:

  Return the coefficients and their variance-covariance matrix
  respectively.

- `update`:

  Re-fit a `"nestedLogit"` model with a change in any of the `formula`,
  `dichotomies`, `data`, `subset`, or `contrasts`, arguments.

- `summary`:

  Summarize a `"nestedLogit"` model, giving the summary for each binary
  logit model in the nested dichotomies.

- `print`:

  Print the model or a summary of the model.

- `as.matrix`, `as.character`, `as.dichotomies`:

  Coerce dichotomy-related objects to matrices, character vectors, and
  dichotomies objects.

## Usage

``` r
# S3 method for class 'nestedLogit'
print(x, ...)

# S3 method for class 'nestedLogit'
summary(object, ...)

# S3 method for class 'summary.nestedLogit'
print(x, ...)

# S3 method for class 'dichotomies'
print(x, ...)

# S3 method for class 'nestedLogit'
coef(object, as.matrix = TRUE, ...)

# S3 method for class 'nestedLogit'
vcov(object, as.matrix = FALSE, ...)

# S3 method for class 'nestedLogit'
update(object, formula, dichotomies, data, subset, contrasts, ...)

# S3 method for class 'dichotomies'
as.matrix(x, ...)

# S3 method for class 'dichotomies'
as.character(x, ...)

# S3 method for class 'continuationDichotomies'
as.matrix(x, ...)

as.dichotomies(x, ...)

# S3 method for class 'matrix'
as.dichotomies(x, ...)
```

## Arguments

- x, object:

  in most cases, an object of class `"nestedLogit"`.

- ...:

  arguments to be passed down.

- as.matrix:

  if `TRUE` (the default for `coef`) return coefficients as a matrix
  with one column for each nested dichotomy, or coefficient covariances
  as a matrix with one row and column for each combination of
  dichotomies and coefficients; if `FALSE` (the default for `vcov`),
  return a list of coefficients or coefficient covariances with one
  element for each dichotomy.

- formula:

  optional updated model formula.

- dichotomies:

  optional updated dichotomies object.

- data:

  optional updated data argument

- subset:

  optional updated subset argument.

- contrasts:

  optional updated contrasts argument.

## Value

- The `coef` and `vcov` methods return either matrices or lists of
  regression coefficients and their covariances, respectively.

- The `update` method returns an object of class `"nestedLogit"` (see
  [`nestedLogit`](https://friendly.github.io/nestedLogit/reference/nestedLogit.md))
  derived from the original nested-logit model.

- The `summary` method returns an object of class
  `"summary.nestedLogit"`, which is a list of summaries of the
  [`glm`](https://rdrr.io/r/stats/glm.html) objects that comprise the
  nested-dichotomies model; the object is normally printed.

- The methods for `as.matrix`, `as.character`, and `as.dichotomies`
  coerce various objects to matrices, character vectors, and dichotomies
  objects.

- The various `print` methods invisibly return their `x` arguments.

## See also

[`nestedLogit`](https://friendly.github.io/nestedLogit/reference/nestedLogit.md),
[`predict.nestedLogit`](https://friendly.github.io/nestedLogit/reference/predict.nestedLogit.md),
[`plot.nestedLogit`](https://friendly.github.io/nestedLogit/reference/plot.nestedLogit.md),
[`glance.nestedLogit`](https://friendly.github.io/nestedLogit/reference/broomMethods.md),
[`tidy.nestedLogit`](https://friendly.github.io/nestedLogit/reference/broomMethods.md)

## Author

John Fox and Michael Friendly

## Examples

``` r
# define continuation dichotomies for level of education
cont.dichots <- continuationLogits(c("<highschool",
                                     "highschool",
                                     "college",
                                     "graduate"))

# Show dichotomies in various forms
print(cont.dichots)
#> above_.highschool: {<highschool} vs. {highschool, college, graduate}
#> above_highschool: {highschool} vs. {college, graduate}
#> above_college: {college} vs. {graduate}
as.matrix(cont.dichots)
#>                   <highschool highschool college graduate
#> above_.highschool           0          1       1        1
#> above_highschool           NA          0       1        1
#> above_college              NA         NA       0        1
as.character(cont.dichots)
#> [1] "above_.highschool = {{<highschool}} {{highschool college graduate}}; above_highschool = {{highschool}} {{college graduate}}; above_college = {{college}} {{graduate}}"

# fit a nested model for the GSS data examining education degree in relation to parent & year
m <- nestedLogit(degree ~ parentdeg + year,
                 cont.dichots,
                 data=GSS)

coef(m)                             # coefficient estimates
#>                     above_.highschool above_highschool above_college
#> (Intercept)               -42.6261632     -26.21317138 -18.253162254
#> parentdeghighschool         1.9632884       0.60103280  -0.345479234
#> parentdegcollege            3.1006736       1.70574631  -0.414683439
#> parentdeggraduate           3.3512189       2.21122349   0.145720269
#> year                        0.0216427       0.01231442   0.008909557
sqrt(diag(vcov(m, as.matrix=TRUE))) # standard errors
#>         above_.highschool.(Intercept) above_.highschool.parentdeghighschool 
#>                          2.2232428045                          0.0313786223 
#>    above_.highschool.parentdegcollege   above_.highschool.parentdeggraduate 
#>                          0.0927607066                          0.1275872686 
#>                above_.highschool.year          above_highschool.(Intercept) 
#>                          0.0011178164                          1.9459383633 
#>  above_highschool.parentdeghighschool     above_highschool.parentdegcollege 
#>                          0.0334567993                          0.0410059198 
#>    above_highschool.parentdeggraduate                 above_highschool.year 
#>                          0.0468809978                          0.0009765976 
#>             above_college.(Intercept)     above_college.parentdeghighschool 
#>                          3.3030647705                          0.0630462422 
#>        above_college.parentdegcollege       above_college.parentdeggraduate 
#>                          0.0703836929                          0.0708577702 
#>                    above_college.year 
#>                          0.0016570885 
print(m)
#> Nested logit models: degree ~ parentdeg + year
#> <environment: 0x0000027a5965c698>
#> 
#> Call:  glm(formula = above_.highschool ~ parentdeg + year, family = binomial, 
#>     data = GSS, contrasts = contrasts)
#> 
#> Coefficients:
#>         (Intercept)  parentdeghighschool     parentdegcollege  
#>           -42.62616              1.96329              3.10067  
#>   parentdeggraduate                 year  
#>             3.35122              0.02164  
#> 
#> Degrees of Freedom: 44090 Total (i.e. Null);  44086 Residual
#> Null Deviance:       40990 
#> Residual Deviance: 32880     AIC: 32890
#> 
#> Call:  glm(formula = above_highschool ~ parentdeg + year, family = binomial, 
#>     data = GSS, contrasts = contrasts)
#> 
#> Coefficients:
#>         (Intercept)  parentdeghighschool     parentdegcollege  
#>           -26.21317              0.60103              1.70575  
#>   parentdeggraduate                 year  
#>             2.21122              0.01231  
#> 
#> Degrees of Freedom: 36343 Total (i.e. Null);  36339 Residual
#>   (7747 observations deleted due to missingness)
#> Null Deviance:       44730 
#> Residual Deviance: 40690     AIC: 40700
#> 
#> Call:  glm(formula = above_college ~ parentdeg + year, family = binomial, 
#>     data = GSS, contrasts = contrasts)
#> 
#> Coefficients:
#>         (Intercept)  parentdeghighschool     parentdegcollege  
#>           -18.25316             -0.34548             -0.41468  
#>   parentdeggraduate                 year  
#>             0.14572              0.00891  
#> 
#> Degrees of Freedom: 11098 Total (i.e. Null);  11094 Residual
#>   (32992 observations deleted due to missingness)
#> Null Deviance:       14200 
#> Residual Deviance: 14050     AIC: 14060
summary(m)
#> Nested logit models: degree ~ parentdeg + year
#> <environment: 0x0000027a5965c698>
#> 
#> Response above_.highschool: {<highschool} vs. {highschool, college, graduate}
#> Call:
#> glm(formula = above_.highschool ~ parentdeg + year, family = binomial, 
#>     data = GSS, contrasts = contrasts)
#> 
#> Coefficients:
#>                       Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)         -42.626163   2.223243  -19.17   <2e-16 ***
#> parentdeghighschool   1.963288   0.031379   62.57   <2e-16 ***
#> parentdegcollege      3.100674   0.092761   33.43   <2e-16 ***
#> parentdeggraduate     3.351219   0.127587   26.27   <2e-16 ***
#> year                  0.021643   0.001118   19.36   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 40989  on 44090  degrees of freedom
#> Residual deviance: 32880  on 44086  degrees of freedom
#> AIC: 32890
#> 
#> Number of Fisher Scoring iterations: 6
#> 
#> Response above_highschool: {highschool} vs. {college, graduate}
#> Call:
#> glm(formula = above_highschool ~ parentdeg + year, family = binomial, 
#>     data = GSS, contrasts = contrasts)
#> 
#> Coefficients:
#>                       Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)         -2.621e+01  1.946e+00  -13.47   <2e-16 ***
#> parentdeghighschool  6.010e-01  3.346e-02   17.96   <2e-16 ***
#> parentdegcollege     1.706e+00  4.101e-02   41.60   <2e-16 ***
#> parentdeggraduate    2.211e+00  4.688e-02   47.17   <2e-16 ***
#> year                 1.231e-02  9.766e-04   12.61   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 44729  on 36343  degrees of freedom
#> Residual deviance: 40693  on 36339  degrees of freedom
#>   (7747 observations deleted due to missingness)
#> AIC: 40703
#> 
#> Number of Fisher Scoring iterations: 4
#> 
#> Response above_college: {college} vs. {graduate}
#> Call:
#> glm(formula = above_college ~ parentdeg + year, family = binomial, 
#>     data = GSS, contrasts = contrasts)
#> 
#> Coefficients:
#>                       Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)         -18.253162   3.303065  -5.526 3.27e-08 ***
#> parentdeghighschool  -0.345479   0.063046  -5.480 4.26e-08 ***
#> parentdegcollege     -0.414683   0.070384  -5.892 3.82e-09 ***
#> parentdeggraduate     0.145720   0.070858   2.057   0.0397 *  
#> year                  0.008910   0.001657   5.377 7.59e-08 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 14195  on 11098  degrees of freedom
#> Residual deviance: 14045  on 11094  degrees of freedom
#>   (32992 observations deleted due to missingness)
#> AIC: 14055
#> 
#> Number of Fisher Scoring iterations: 4
#> 
```
