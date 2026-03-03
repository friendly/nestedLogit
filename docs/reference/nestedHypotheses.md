# Hypothesis-Testing and Related Methods for `"nestedLogit"` Objects

Various methods for testing hypotheses about nested logit models.

- `Anova`:

  Calculates type-II or type-III analysis-of-variance tables for
  `"nestedLogit"` objects; see
  [`Anova`](https://rdrr.io/pkg/car/man/Anova.html) in the car package.

- `anova`:

  Computes sequential analysis of variance (or deviance) tables for one
  or more fitted `"nestedLogit"` objects; see
  [`anova`](https://rdrr.io/r/stats/anova.html).

- `linearHypothesis`:

  Computes Wald tests for linear hypotheses; see
  [`linearHypothesis`](https://rdrr.io/pkg/car/man/linearHypothesis.html)
  in the car package.

- `logLik`:

  Returns the log-likelihood and degrees of freedom for the
  nested-dichotomies model. (and through it
  [`AIC`](https://rdrr.io/r/stats/AIC.html) and
  [`BIC`](https://rdrr.io/r/stats/AIC.html) model-comparison
  statistics).

## Usage

``` r
# S3 method for class 'nestedLogit'
Anova(mod, ...)

# S3 method for class 'Anova.nestedLogit'
print(x, ...)

# S3 method for class 'nestedLogit'
linearHypothesis(model, ...)

# S3 method for class 'nestedLogit'
anova(object, object2, ...)

# S3 method for class 'anova.nestedLogit'
print(x, ...)

# S3 method for class 'nestedLogit'
logLik(object, ...)
```

## Arguments

- ...:

  arguments to be passed down. In the case of `linearHypothesis`, the
  second argument is typically the `hypothesis.matrix`. See the Details
  section of
  [`linearHypothesis`](https://rdrr.io/pkg/car/man/linearHypothesis.html).
  In the case of `anova`, additional sequential `"nestedLogit"` models.

- x, object, object2, mod, model:

  in most cases, an object of class `"nestedLogit"`.

## Value

- The `Anova` and `anova` methods return objects of class
  `"Anova.nestedLogit"` and `"anova.nestedLogit"`, respectively, each of
  which contains a list of `"anova"` objects (see
  [`anova`](https://rdrr.io/r/stats/anova.html)) and is usually printed.

- The `linearHypothesis` method is called for its side effect, printing
  the result of linear hypothesis tests, and invisibly returns `NULL`.

- The `logLik` method returns an object of class `"logLik"` (see
  [`logLik`](https://rdrr.io/r/stats/logLik.html)).

## See also

[`Anova`](https://rdrr.io/pkg/car/man/Anova.html),
[`anova`](https://rdrr.io/r/stats/anova.html),
[`linearHypothesis`](https://rdrr.io/pkg/car/man/linearHypothesis.html),
[`logLik`](https://rdrr.io/r/stats/logLik.html),
[`AIC`](https://rdrr.io/r/stats/AIC.html),
[`BIC`](https://rdrr.io/r/stats/AIC.html)

## Author

John Fox

## Examples

``` r
# define continuation dichotomies for level of education
cont.dichots <- continuationLogits(c("<highschool",
                                     "highschool",
                                     "college",
                                     "graduate"))
# fit a nested model for the GSS data examining education degree in relation to parent & year
m <- nestedLogit(degree ~ parentdeg + year,
                 cont.dichots,
                 data=GSS)

# Anova and anova tests
car::Anova(m) # type-II (partial) tests
#> 
#>  Analysis of Deviance Tables (Type II tests)
#>  
#> Response above_.highschool: {<highschool} vs. {highschool, college, graduate}
#>           LR Chisq Df Pr(>Chisq)    
#> parentdeg   6604.2  3  < 2.2e-16 ***
#> year         383.3  1  < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> Response above_highschool: {highschool} vs. {college, graduate}
#>           LR Chisq Df Pr(>Chisq)    
#> parentdeg   3541.7  3  < 2.2e-16 ***
#> year         159.8  1  < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> Response above_college: {college} vs. {graduate}
#>           LR Chisq Df Pr(>Chisq)    
#> parentdeg  121.317  3  < 2.2e-16 ***
#> year        29.074  1  6.966e-08 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> Combined Responses
#>           LR Chisq Df Pr(>Chisq)    
#> parentdeg  10267.2  9  < 2.2e-16 ***
#> year         572.1  3  < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

anova(update(m, . ~ . - year), m) # model comparison
#> 
#>  Analysis of Deviance Tables
#>  Model 1: degree ~ parentdeg
#>  Model 2: degree ~ parentdeg + year 
#>  
#> Response above_.highschool: {<highschool} vs. {highschool, college, graduate}
#>   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#> 1     44087      33263                          
#> 2     44086      32880  1   383.29 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> Response above_highschool: {highschool} vs. {college, graduate}
#>   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#> 1     36340      40853                          
#> 2     36339      40693  1   159.75 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> Response above_college: {college} vs. {graduate}
#>   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#> 1     11095      14074                          
#> 2     11094      14045  1   29.074 6.966e-08 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> Combined Responses
#>   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#> 1     91522      88190                          
#> 2     91519      87618  3   572.11 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Wald test
car::linearHypothesis(m, c("parentdeghighschool", "parentdegcollege",
                           "parentdeggraduate"))
#> 
#> Linear hypothesis test: 
#> parentdeghighschool = 0 
#> parentdegcollege = 0 
#> parentdeggraduate = 0 
#>  
#> Model 1: restricted model
#> Model 2: degree ~ parentdeg + year 
#>  
#> Response above_.highschool: {<highschool} vs. {highschool, college, graduate} 
#>   Res.Df Df Chisq Pr(>Chisq)    
#> 1  44089                        
#> 2  44086  3  5085  < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Response above_highschool: {highschool} vs. {college, graduate}
#>   Res.Df Df  Chisq Pr(>Chisq)    
#> 1  36342                         
#> 2  36339  3 3322.4  < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Response above_college: {college} vs. {graduate}
#>   Res.Df Df  Chisq Pr(>Chisq)    
#> 1  11097                         
#> 2  11094  3 122.17  < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Combined Responses
#> Chisq = 8529.624, Df = 9, Pr(>Chisq) = < 2.22e-16

# log-liklihood, AIC, and BIC
logLik(m)
#> 'log Lik.' -43809.17 (df=15)
AIC(m)
#> [1] 87648.33
BIC(m)
#> [1] 87778.74
```
