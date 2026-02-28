# Methods for `"nestedLogit"` and Related Objects

Various methods for processing `"nestedLogit"` and related objects. Most
of these are the standard methods for a model-fitting function.

- `coef`, `vcov`:

  Return the coefficients and their variance-covariance matrix
  respectively.

- `update`:

  Re-fit a `"nestedLogit"` model with a change in any of the `formula`,
  `dichotomies`, `data`, `subset`, or `contrasts`, arguments.

- `predict`, `fitted`:

  Computes predicted values from a fitted `"nestedLogit"` model.

- `confint`:

  Compute point-wise confidence limits for predicted response-category
  probabilities or logits.

- `glance`:

  Construct a single row summaries for the dichotomies `"nestedLogit"`
  model.

- `tidy`:

  Summarizes the terms in `"nestedLogit"` model.

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
predict(object, newdata, model = c("nested", "dichotomies"), ...)

# S3 method for class 'predictNestedLogit'
print(x, n = min(10L, nrow(x$p)), ...)

# S3 method for class 'predictNestedLogit'
confint(
  object,
  parm = c("prob", "logit"),
  level = 0.95,
  conf.limits.logit = TRUE,
  ...
)

# S3 method for class 'predictDichotomies'
print(x, n = 10L, ...)

# S3 method for class 'nestedLogit'
fitted(object, model = c("nested", "dichotomies"), ...)

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

- newdata:

  For the `predict` method, a data frame containing combinations of
  values of the predictors at which fitted probabilities (or other
  quantities) are to be computed.

- model:

  For the `predict` and `fitted` methods, either `"nested"` (the
  default), in which case fitted probabilities under the nested logit
  model are returned, or `"dichotomies"`, in which case
  [`predict.glm`](https://rdrr.io/r/stats/predict.glm.html) is invoked
  for each binary logit model fit to the nested dichotomies and a named
  list of the results is returned.

- n:

  For the print method of `predict.nestedLogit` or `predictDichotomies`,
  an integer or `"all"` to control how many rows are printed for each of
  the probabilities of response categories, corresponding logits and
  their standard errors.

- parm:

  For the `confint` method, one of `"prob"` or `"logit"`, indicating
  whether to generate confidence intervals for probabilities or logits
  of the responses.

- level:

  Confidence level for the `confint` method

- conf.limits.logit:

  When `parm = "prob"` ?????

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

- The `predict` and `fitted` methods return an object of class
  `"predictNested"` or `"predictDichotomies"`, which contain the
  predicted probabilities, predicted logits, and other information, such
  as standard errors of predicted values, and, if supplied, the
  `newdata` on which predictions are based.

- The `summary` method returns an object of class
  `"summary.nestedLogit"`, which is a list of summaries of the
  [`glm`](https://rdrr.io/r/stats/glm.html) objects that comprise the
  nested-dichotomies model; the object is normally printed.

- The methods for `as.matrix`, `as.character`, and `as.dichotomies`
  coerce various objects to matrices, character vectors, and dichotomies
  objects.

- The various `print` methods invisibly return their `x` arguments.

## Details

The `predict` method provides predicted values for two representations
of the model. `model = "nested"` gives the fitted probabilities for each
of the response categories. `model = "dichotomies"` gives the fitted log
odds for each binary logit models in the dichotomies.

## See also

[`nestedLogit`](https://friendly.github.io/nestedLogit/reference/nestedLogit.md),
[`plot.nestedLogit`](https://friendly.github.io/nestedLogit/reference/plot.nestedLogit.md),
[`glance.nestedLogit`](https://friendly.github.io/nestedLogit/reference/broomMethods.md),
[`tidy.nestedLogit`](https://friendly.github.io/nestedLogit/reference/broomMethods.md)

## Author

John Fox and Michael Friendly

## Examples

``` r
# define continuation dichotomies for level of education
cont.dichots <- continuationLogits(c("l.t.highschool",
                                     "highschool",
                                     "college",
                                     "graduate"))

# Show dichotomies in various forms
print(cont.dichots)
#> above_l.t.highschool: {l.t.highschool} vs. {highschool, college, graduate}
#> above_highschool: {highschool} vs. {college, graduate}
#> above_college: {college} vs. {graduate}
as.matrix(cont.dichots)
#>                      l.t.highschool highschool college graduate
#> above_l.t.highschool              0          1       1        1
#> above_highschool                 NA          0       1        1
#> above_college                    NA         NA       0        1
as.character(cont.dichots)
#> [1] "above_l.t.highschool = {{l.t.highschool}} {{highschool college graduate}}; above_highschool = {{highschool}} {{college graduate}}; above_college = {{college}} {{graduate}}"

# fit a nested model for the GSS data examining education degree in relation to parent & year
m <- nestedLogit(degree ~ parentdeg + year,
                 cont.dichots,
                 data=GSS)

coef(m)                             # coefficient estimates
#>                     above_l.t.highschool above_highschool above_college
#> (Intercept)                  -42.6261632     -26.21317138 -18.253162254
#> parentdeghighschool            1.9632884       0.60103280  -0.345479234
#> parentdegcollege               3.1006736       1.70574631  -0.414683439
#> parentdeggraduate              3.3512189       2.21122349   0.145720269
#> year                           0.0216427       0.01231442   0.008909557
sqrt(diag(vcov(m, as.matrix=TRUE))) # standard errors
#>         above_l.t.highschool.(Intercept) 
#>                             2.2232428045 
#> above_l.t.highschool.parentdeghighschool 
#>                             0.0313786223 
#>    above_l.t.highschool.parentdegcollege 
#>                             0.0927607066 
#>   above_l.t.highschool.parentdeggraduate 
#>                             0.1275872686 
#>                above_l.t.highschool.year 
#>                             0.0011178164 
#>             above_highschool.(Intercept) 
#>                             1.9459383633 
#>     above_highschool.parentdeghighschool 
#>                             0.0334567993 
#>        above_highschool.parentdegcollege 
#>                             0.0410059198 
#>       above_highschool.parentdeggraduate 
#>                             0.0468809978 
#>                    above_highschool.year 
#>                             0.0009765976 
#>                above_college.(Intercept) 
#>                             3.3030647705 
#>        above_college.parentdeghighschool 
#>                             0.0630462422 
#>           above_college.parentdegcollege 
#>                             0.0703836929 
#>          above_college.parentdeggraduate 
#>                             0.0708577702 
#>                       above_college.year 
#>                             0.0016570885 
print(m)
#> Nested logit models: degree ~ parentdeg + year
#> <environment: 0x000001ce831aba50>
#> 
#> Call:  glm(formula = above_l.t.highschool ~ parentdeg + year, family = binomial, 
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
#> <environment: 0x000001ce831aba50>
#> 
#> Response above_l.t.highschool: {l.t.highschool} vs. {highschool, college, graduate}
#> Call:
#> glm(formula = above_l.t.highschool ~ parentdeg + year, family = binomial, 
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

# broom methods
broom::glance(m)
#> # A tibble: 3 × 9
#>   response      null.deviance df.null  logLik    AIC    BIC deviance df.residual
#>   <chr>                 <dbl>   <int>   <dbl>  <dbl>  <dbl>    <dbl>       <int>
#> 1 above_l.t.hi…        40989.   44090 -16440. 32890. 32934.   32880.       44086
#> 2 above_highsc…        44729.   36343 -20346. 40703. 40745.   40693.       36339
#> 3 above_college        14195.   11098  -7023. 14055. 14092.   14045.       11094
#> # ℹ 1 more variable: nobs <int>
broom::tidy(m)
#> # A tibble: 15 × 6
#>    response             term              estimate std.error statistic   p.value
#>    <chr>                <chr>                <dbl>     <dbl>     <dbl>     <dbl>
#>  1 above_l.t.highschool (Intercept)       -4.26e+1  2.22        -19.2  6.23e- 82
#>  2 above_l.t.highschool parentdeghighsch…  1.96e+0  0.0314       62.6  0        
#>  3 above_l.t.highschool parentdegcollege   3.10e+0  0.0928       33.4  5.64e-245
#>  4 above_l.t.highschool parentdeggraduate  3.35e+0  0.128        26.3  4.68e-152
#>  5 above_l.t.highschool year               2.16e-2  0.00112      19.4  1.63e- 83
#>  6 above_highschool     (Intercept)       -2.62e+1  1.95        -13.5  2.33e- 41
#>  7 above_highschool     parentdeghighsch…  6.01e-1  0.0335       18.0  3.70e- 72
#>  8 above_highschool     parentdegcollege   1.71e+0  0.0410       41.6  0        
#>  9 above_highschool     parentdeggraduate  2.21e+0  0.0469       47.2  0        
#> 10 above_highschool     year               1.23e-2  0.000977     12.6  1.87e- 36
#> 11 above_college        (Intercept)       -1.83e+1  3.30         -5.53 3.27e-  8
#> 12 above_college        parentdeghighsch… -3.45e-1  0.0630       -5.48 4.26e-  8
#> 13 above_college        parentdegcollege  -4.15e-1  0.0704       -5.89 3.82e-  9
#> 14 above_college        parentdeggraduate  1.46e-1  0.0709        2.06 3.97e-  2
#> 15 above_college        year               8.91e-3  0.00166       5.38 7.59e-  8

# predicted probabilities and ploting
predict(m) # fitted probabilities for first few cases;
#> 
#> First 10 of 44091 rows:
#> 
#> predicted response-category probabilties
#>    l.t.highschool highschool    college   graduate
#> 2      0.48669496  0.4481946 0.04326738 0.02184305
#> 3      0.48669496  0.4481946 0.04326738 0.02184305
#> 4      0.04093763  0.5328723 0.31960957 0.10658049
#> 5      0.48669496  0.4481946 0.04326738 0.02184305
#> 6      0.03215663  0.4161182 0.34830386 0.20342132
#> 7      0.04093763  0.5328723 0.31960957 0.10658049
#> 8      0.04093763  0.5328723 0.31960957 0.10658049
#> 9      0.11747919  0.6976573 0.13619288 0.04867067
#> 10     0.48669496  0.4481946 0.04326738 0.02184305
#> 13     0.48669496  0.4481946 0.04326738 0.02184305
#>   . . .
#> 
#> predicted response-category logits
#>    l.t.highschool highschool    college  graduate
#> 2     -0.05323272 -0.2079679 -3.0961249 -3.801787
#> 3     -0.05323272 -0.2079679 -3.0961249 -3.801787
#> 4     -3.15390636  0.1316792 -0.7555666 -2.126156
#> 5     -0.05323272 -0.2079679 -3.0961249 -3.801787
#> 6     -3.40445160 -0.3387293 -0.6265031 -1.365047
#> 7     -3.15390636  0.1316792 -0.7555666 -2.126156
#> 8     -3.15390636  0.1316792 -0.7555666 -2.126156
#> 9     -2.01652113  0.8361667 -1.8472774 -2.972784
#> 10    -0.05323272 -0.2079679 -3.0961249 -3.801787
#> 13    -0.05323272 -0.2079679 -3.0961249 -3.801787
#>   . . .
#> 
#> standard errors of predicted probabilities
#>    l.t.highschool  highschool     college    graduate
#> 2     0.006302163 0.005851716 0.001707529 0.001183908
#> 3     0.006302163 0.005851716 0.001707529 0.001183908
#> 4     0.003707404 0.009246535 0.008481557 0.005474035
#> 5     0.006302163 0.005851716 0.001707529 0.001183908
#> 6     0.004011760 0.010684551 0.010589359 0.009031287
#> 7     0.003707404 0.009246535 0.008481557 0.005474035
#> 8     0.003707404 0.009246535 0.008481557 0.005474035
#> 9     0.003595434 0.005098858 0.003676266 0.002182298
#> 10    0.006302163 0.005851716 0.001707529 0.001183908
#> 13    0.006302163 0.005851716 0.001707529 0.001183908
#>   . . .
#> 
#> standard errors of predicted logits
#>    l.t.highschool highschool    college   graduate
#> 2      0.02522651 0.02366087 0.04124933 0.05541104
#> 3      0.02522651 0.02366087 0.04124933 0.05541104
#> 4      0.09442790 0.03714670 0.03900296 0.05748764
#> 5      0.02522651 0.02366087 0.04124933 0.05541104
#> 6      0.12890190 0.04397589 0.04665157 0.05573455
#> 7      0.09442790 0.03714670 0.03900296 0.05748764
#> 8      0.09442790 0.03714670 0.03900296 0.05748764
#> 9      0.03467891 0.02417304 0.03124897 0.04713199
#> 10     0.02522651 0.02366087 0.04124933 0.05541104
#> 13     0.02522651 0.02366087 0.04124933 0.05541104
#>   . . .

new <- expand.grid(parentdeg=c("l.t.highschool",  "highschool",
                               "college", "graduate"),
                   year=c(1972, 2016))
fit <- predict(m, newdata=new)
cbind(new, fit) # fitted probabilities at specific values of predictors
#>         parentdeg year      parentdeg year       response          p
#> 1  l.t.highschool 1972 l.t.highschool 1972 l.t.highschool 0.48669496
#> 2      highschool 1972 l.t.highschool 1972     highschool 0.44819461
#> 3         college 1972 l.t.highschool 1972        college 0.04326738
#> 4        graduate 1972 l.t.highschool 1972       graduate 0.02184305
#> 5  l.t.highschool 2016     highschool 1972 l.t.highschool 0.11747919
#> 6      highschool 2016     highschool 1972     highschool 0.69765725
#> 7         college 2016     highschool 1972        college 0.13619288
#> 8        graduate 2016     highschool 1972       graduate 0.04867067
#> 9  l.t.highschool 1972        college 1972 l.t.highschool 0.04093763
#> 10     highschool 1972        college 1972     highschool 0.53287231
#> 11        college 1972        college 1972        college 0.31960957
#> 12       graduate 1972        college 1972       graduate 0.10658049
#> 13 l.t.highschool 2016       graduate 1972 l.t.highschool 0.03215663
#> 14     highschool 2016       graduate 1972     highschool 0.41611819
#> 15        college 2016       graduate 1972        college 0.34830386
#> 16       graduate 2016       graduate 1972       graduate 0.20342132
#> 17 l.t.highschool 1972 l.t.highschool 2016 l.t.highschool 0.26785921
#> 18     highschool 1972 l.t.highschool 2016     highschool 0.58583139
#> 19        college 1972 l.t.highschool 2016        college 0.08374198
#> 20       graduate 1972 l.t.highschool 2016       graduate 0.06256742
#> 21 l.t.highschool 2016     highschool 2016 l.t.highschool 0.04885547
#> 22     highschool 2016     highschool 2016     highschool 0.65346585
#> 23        college 2016     highschool 2016        college 0.19470249
#> 24       graduate 2016     highschool 2016       graduate 0.10297618
#> 25 l.t.highschool 1972        college 2016 l.t.highschool 0.01620361
#> 26     highschool 1972        college 2016     highschool 0.41423386
#> 27        college 1972        college 2016        college 0.38135421
#> 28       graduate 1972        college 2016       graduate 0.18820832
#> 29 l.t.highschool 2016       graduate 2016 l.t.highschool 0.01265796
#> 30     highschool 2016       graduate 2016     highschool 0.30107340
#> 31        college 2016       graduate 2016        college 0.36810041
#> 32       graduate 2016       graduate 2016       graduate 0.31816823
#>           se.p       logit   se.logit
#> 1  0.006302163 -0.05323272 0.02522651
#> 2  0.005851716 -0.20796791 0.02366087
#> 3  0.001707529 -3.09612491 0.04124933
#> 4  0.001183908 -3.80178733 0.05541104
#> 5  0.003595434 -2.01652113 0.03467891
#> 6  0.005098858  0.83616665 0.02417304
#> 7  0.003676266 -1.84727739 0.03124897
#> 8  0.002182298 -2.97278366 0.04713199
#> 9  0.003707404 -3.15390636 0.09442790
#> 10 0.009246535  0.13167918 0.03714670
#> 11 0.008481557 -0.75556665 0.03900296
#> 12 0.005474035 -2.12615578 0.05748764
#> 13 0.004011760 -3.40445160 0.12890190
#> 14 0.010684551 -0.33872927 0.04397589
#> 15 0.010589359 -0.62650313 0.04665157
#> 16 0.009031287 -1.36504661 0.05573455
#> 17 0.006798108 -1.00551133 0.03466465
#> 18 0.006948958  0.34675885 0.02863979
#> 19 0.003523212 -2.39255756 0.04591745
#> 20 0.003072415 -2.70690014 0.05238314
#> 21 0.001733643 -2.96879974 0.03730782
#> 22 0.005313294  0.63430892 0.02346361
#> 23 0.004494391 -1.41973906 0.02866441
#> 24 0.003441931 -2.16458467 0.03726159
#> 25 0.001500524 -4.10618498 0.09412952
#> 26 0.008148392 -0.34648993 0.03358165
#> 27 0.008510405 -0.48380426 0.03607278
#> 28 0.007056246 -1.46169432 0.04618387
#> 29 0.001605067 -4.35673022 0.12842860
#> 30 0.008518703 -0.84219163 0.04048270
#> 31 0.009972667 -0.54037476 0.04287429
#> 32 0.009700566 -0.76220268 0.04471601

# predicted logits for dichotomies
predictions <- predict(m, newdata=new, model="dichotomies")
predictions
#> 
#>  predictions for binary logit models from nested logit model: m 
#> 
#>  dichotomy: above_l.t.highschool 
#>          fit     se.fit residual.scale
#> 1 0.05323272 0.02522651              1
#> 2 2.01652113 0.03467891              1
#> 3 3.15390636 0.09442790              1
#> 4 3.40445160 0.12890190              1
#> 5 1.00551133 0.03466465              1
#> 6 2.96879974 0.03730782              1
#> 7 4.10618498 0.09412952              1
#> 8 4.35673022 0.12842860              1
#> 
#>  dichotomy: above_highschool 
#>          fit     se.fit residual.scale
#> 1 -1.9291427 0.03501272              1
#> 2 -1.3281099 0.02896659              1
#> 3 -0.2233964 0.03806677              1
#> 4  0.2820807 0.04445165              1
#> 5 -1.3873084 0.03693667              1
#> 6 -0.7862756 0.02531886              1
#> 7  0.3184379 0.03387511              1
#> 8  0.8239151 0.04064027              1
#> 
#>  dichotomy: above_college 
#>          fit     se.fit residual.scale
#> 1 -0.6835162 0.06476680              1
#> 2 -1.0289955 0.05203322              1
#> 3 -1.0981997 0.06219778              1
#> 4 -0.5377960 0.06313624              1
#> 5 -0.2914957 0.06656676              1
#> 6 -0.6369750 0.04354779              1
#> 7 -0.7061792 0.05173419              1
#> 8 -0.1457755 0.05184601              1
#> 
#>  dichotomy: .data 
#>        parentdeg year
#> 1 l.t.highschool 1972
#> 2     highschool 1972
#> 3        college 1972
#> 4       graduate 1972
#> 5 l.t.highschool 2016
#> 6     highschool 2016
#> 7        college 2016
#> 8       graduate 2016
```
