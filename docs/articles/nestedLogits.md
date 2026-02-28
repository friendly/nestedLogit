# Nested dichotomies logistic regression models

Load the packages we’ll use here:

``` r
library(nestedLogit)    # Nested Dichotomy Logistic Regression Models
library(knitr)          # A General-Purpose Package for Dynamic Report Generation in R
library(car)            # Companion to Applied Regression
library(broom)          # Convert Statistical Objects into Tidy Tibbles
library(dplyr)          # A Grammar of Data Manipulation
library(ggplot2)        # Create Elegant Data Visualisations Using the Grammar of Graphics
library(geomtextpath)   # Curved Text in 'ggplot2'
```

## Models for polytomous responses

The familiar logistic regression model applies when there is a binary
(“*dichotomous*”) response, such as “survived” vs. “died”, or voted
“yes” vs. “no” on a referendum. However, often the response variable is
multi-category (“*polytomous*”), taking on \\m \> 2\\ discrete values.
For example,

- Respondents in the U.S. General Social Survey are classified by their
  level of education, taking values `<highschool`, `highschool`,
  `college` and `graduate`.

- Women’s labor force participation might be classified as (a) not
  working outside the home; (b) working parttime; or (c) working
  fulltime.

There are several different ways to model the response probabilities.
Let \\\pi\_{ij} \equiv \pi_j \\ ( \vec{x}\_i )\\ be the probability of
response \\j\\ for case or group \\i\\, given the predictors
\\\vec{x}\_i\\. Because \\\sum_j \\ \pi\_{ij} = 1\\, only \\m - 1\\ of
these probabilities are independent. The essential idea is to construct
a model for the polytomous response composed of \\m-1\\ logit
comparisons among the response categories in a similar way to how
factors are treated in the predictor variables.

### Multinomial model

One natural generalization of the the standard logistic regression
*logit* model is the *multinomial logit* (or, *generalized logit*)
model. When the polytomous response has \\m\\ levels, the multinomial
logit model is comprised of \\m-1\\ log odds comparisons with a
reference level, typically the last, as described in Fox (2016), 14.2.1
and Friendly & Meyer (2016), 8.3. The standard implementation of this
model is [`multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html) in
the `nnet` package (Ripley, 2022).

### Nested logit model

A sometimes simpler approach, called *nested dichotomies*, is to fit a
collection of \\m-1\\ separate models for each of a hierarchically
nested set of binary comparisons among the response categories. This is
simpler,

- because it uses the familiar logistic regression model for each of the
  dichotomies, where standard methods for model summaries and tests are
  widely available.
- Taken together, this set of \\m-1\\ models comprises a complete model
  for the polytomous response, just as does the multinomial logit model.

This approach stems from Fienberg (1980) and is described in Fox (2016),
14.2.2 and Friendly & Meyer (2016), 8.2.

Moreover, by the construction of **nested** dichotomies, the submodels
are statistically independent (because the likelihood for the polytomous
response is the product of the likelihoods for the dichotomies), so that
test statistics, such as the likelihood ratio \\G^2\\ tests and tests
for individual coefficients can be added to give overall tests for the
full polytomy. In this way, the \\m-1\\ dichotomies are analogous to
\\m-1\\ orthogonal contrasts for a for an \\m\\-level factor in an ANOVA
design.

Thus, when applicable, nested logit models give the best of two worlds:

- **dichotomy models**: Think about, analyze, plot the separate model
  for each contrast among the response categories
- **overall model**: Summarize, analyze, plot the overall model
  comprising all categories.

Dichotomies are illustrated in the figure below, where response
categories \\Y = \\1, 2, 3, 4\\\\ can be divided first as \\\\1, 2\\\\
vs. \\\\3, 4\\\\. Then these dichotomies can be divided as \\\\1\\\\
vs. \\\\2\\\\ and then \\\\3\\\\ vs. \\\\4\\\\. Alternatively, as shown
in the right side of the figure, the response categories can be divided
progressively— first: \\\\1\\\\ vs. \\\\2, 3, 4\\\\, next: \\\\2\\\\
vs. \\\\3, 4\\\\, and finally: \\\\3\\\\ vs. \\\\4\\\\.

![\*\*Nested dichotomies\*\*: The boxes show two different ways a
four-category response can be represented as three nested
dichotomies.](nested.jpg)

**Nested dichotomies**: The boxes show two different ways a
four-category response can be represented as three nested dichotomies.

This figure makes clear that nested dichotomies are not unique or
equivalent. Different choices will have different interpretations and
different fitted probabilities. Such models make the most sense when
there are substantive reasons for considering the response categories in
terms of such dichotomies. For example, in the diagram above at the
right, this would make sense if the categories were *ordered*, \\1 \< 2
\< 3 \< 4\\, so each of the three submodels can be interpreted as
`above_1`, `above_2`, `above_3`. In this case, the scheme for generating
the dichotomies is often called *continuation logits*.

For another example, the figure below shows a case where psychiatric
patients are classified into four diagnostic categories. These might be
considered ordered categories and dichotomized as in the 4-category
above. However, it might make better sense to dichotomize the non-normal
groups into a comparison of the depressed and manic group
vs. schizophrenic and then a final contrast of the depressed and manic
categories. Then, a model predicting diagnosis can be interpreted in
terms of the log odds of being classed for each of the dichotomies. By
independence, ANOVA and coefficient tests for the full multi-category
response are based on the sums over the three dichotomies.

![\*\*Psychiatric classification\*\*: The figure shows how four
diagnostic categories might be represented by nested
dichotomies.](nested-psychiatric.png)

**Psychiatric classification**: The figure shows how four diagnostic
categories might be represented by nested dichotomies.

## Example: Women’s labor force participation

For a main example, we consider the data set `Womenlf` from the
`carData` package. This data gives the responses of 263 women from a
1977 survey carried out by the York University Institute for Social
Research (Atkinson, Blishen, Ornstein, & Stevenson, 1984). The variables
are:

- `partic`: labor force participation, the response, with levels:
  - `fulltime`: working full-time
  - `not.work`: not working outside the home
  - `parttime` : working part-time.
- `hincome`: Husband’s income, in \$1,000s
- `children`: Presence of children in the home, `absent` or `present`
- `region`: Region of Canada (Atlantic, BC, Ontario, Prairie, Quebec)

The response, `partic` is a factor, but the levels are ordered
alphabetically. To correct this, we make it an ordered factor. At that
time, the majority of the 263 women surveyed were not working.

``` r
data(Womenlf, package = "carData")
Womenlf <- Womenlf |>
  mutate(partic = ordered(partic, 
                          levels = c("not.work", "parttime", "fulltime")))

table(Womenlf$partic)
#> 
#> not.work parttime fulltime 
#>      155       42       66
```

The question is: How can we understand these womens’ labor choices in
terms of the available variables?

### Defining dichotomies

It is at least arguable to consider a woman’s labor choice as first
involving a dichotomy (`work`) between women who are not working
vs. those who are (part time or full time). A second dichotomy (`full`)
contrasts those who work full time vs. part time, but among only those
who work.

The two binary variables can be created by re-coding `partic` in the
data frame as follows.

``` r
Womenlf <- Womenlf |>
  mutate(work = ifelse(partic=="not.work", 0, 1)) |>
  mutate(full = case_when(
    work & partic == "fulltime" ~ 1,
    work & partic == "parttime" ~ 0)
  )
```

Note that the full sample of 263 cases is available for the `work`
dichotomy, while only 108 cases are involved in the distinction between
part time and full time work. The relations of `partic` to these
dichotomies can be seen below:

``` r
xtabs(~ partic + work, addNA=TRUE, data=Womenlf)
#>           work
#> partic       0   1
#>   not.work 155   0
#>   parttime   0  42
#>   fulltime   0  66
xtabs(~ partic + full, addNA=TRUE, data=Womenlf)
#>           full
#> partic       0   1 <NA>
#>   not.work   0   0  155
#>   parttime  42   0    0
#>   fulltime   0  66    0
```

We could then fit the separate models for these dichotomies manually:

``` r
mod.work <- glm(work ~ hincome + children, family=binomial, data=Womenlf)
mod.full <- glm(full ~ hincome + children, family=binomial, data=Womenlf)
```

The two log odds models are:

\\ L_1 =\log\left\[ \frac { P( \operatorname{work} = \operatorname{1} )
}{ 1 - P( \operatorname{work} = \operatorname{1} ) } \right\] =
\alpha_1 + \beta\_{11}(\operatorname{hincome}) +
\beta\_{12}(\operatorname{children}\_{\operatorname{present}}) \\ \\ L_2
= \log\left\[ \frac { P( \operatorname{full} ) }{ 1 - P(
\operatorname{full} ) } \right\] = \alpha_2 +
\beta\_{21}(\operatorname{hincome}) +
\beta\_{22}(\operatorname{children}\_{\operatorname{present}}) \\ But
then, it would be difficult to obtain tests for the combined model, get
and plot predicted values and so forth.

### Using `dichotomy()` and `logits()`

Instead, the `nestedLogit` package provides an easier way. The
[`dichotomy()`](https://friendly.github.io/nestedLogit/reference/nestedLogit.md)
function creates a list of length two defining a single dichotomy. The
[`logits()`](https://friendly.github.io/nestedLogit/reference/nestedLogit.md)
function uses \\m-1\\ calls to
[`dichotomy()`](https://friendly.github.io/nestedLogit/reference/nestedLogit.md)
to create a list of class `dichotomies` containing the symbolic
representation of these contrasts among the response categories.

``` r
comparisons <- logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                      full=dichotomy("parttime", "fulltime"))

comparisons
#> work: {not.work} vs. {parttime, fulltime}
#> full: {parttime} vs. {fulltime}
```

For convenience, there are functions to convert these dichotomies to a
matrix or to a character string representing the tree structure of the
dichotomies.

``` r
as.matrix(comparisons)
```

    #>      not.work parttime fulltime
    #> work        0        1        1
    #> full       NA        0        1

``` r
as.character(comparisons)
```

    #> [1] "{{not.work}} {{parttime fulltime}}; {{parttime}} {{fulltime}}"

### Fitting the nested logit model

To fit the model, we specify these `comparisons` as the `dichotomies`
argument. The model formula, `partic ~ hincome + children` specifies a
main effects model for husband’s income and presence of young children.

``` r
wlf.nested <- nestedLogit(partic ~ hincome + children, 
                          dichotomies = comparisons,
                          data=Womenlf)
```

The result, `wlf.nested` is a list containing the details of the model
for the nested dichotomy. The `models` component contains results
equivalent to what was fit manually above as `mod.work` and `mod.full`.

``` r
names(wlf.nested)
```

    #> [1] "models"          "formula"         "dichotomies"     "data"           
    #> [5] "data.name"       "subset"          "contrasts"       "contrasts.print"

``` r
names(wlf.nested$models) # equivalent: names(models(wlf.models))
```

    #> [1] "work" "full"

### Methods

As befits a model-fitting function, the package defines a nearly
complete set of methods for `"nestedLogit"` objects:

- [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) print the results
  for each of the submodels.
- [`update()`](https://rdrr.io/r/stats/update.html) re-fits the model,
  allowing changes to the model `formula`, `data`, `subset`, and
  `contrasts` arguments.
- [`coef()`](https://rdrr.io/r/stats/coef.html) returns the coefficients
  for the predictors in each dichotomy.
- [`vcov()`](https://rdrr.io/r/stats/vcov.html) returns the
  variance-covariance matrix of the predictors.
- [`predict()`](https://rdrr.io/r/stats/predict.html) computes predicted
  probabilities for the response categories, either for the cases in the
  data or for arbitrary combinations of the predictors; the latter is
  useful for producing plots to aid interpretation.
- [`glance()`](https://generics.r-lib.org/reference/glance.html) and
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) are
  extensions of
  [`broom::glance.glm()`](https://broom.tidymodels.org/reference/glance.glm.html)
  and
  [`broom::tidy.glm()`](https://broom.tidymodels.org/reference/tidy.glm.html)
  to obtain compact summaries of a `"nestedLogit"` model object.
- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) provides
  basic plots of the predicted probabilities over a range of values of
  the predictor variables.
- [`models()`](https://friendly.github.io/nestedLogit/reference/models.md)
  is an extractor function for the binary logit models in the
  `"nestedLogit"` object

These are supplemented by various methods for testing hypotheses about
and comparing nested logit models:

- [`anova()`](https://rdrr.io/r/stats/anova.html) provides
  analysis-of-deviance Type I (sequential) tests for each dichotomy and
  for the combined model. When given a sequence of model objects,
  [`anova()`](https://rdrr.io/r/stats/anova.html) tests the models
  against one another in the order specified.
- [`Anova()`](https://rdrr.io/pkg/car/man/Anova.html) uses
  [`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html) to provide
  analysis-of-deviance Type II or III (partial) tests for each dichotomy
  and for the combined model.
- [`linearHypothesis()`](https://rdrr.io/pkg/car/man/linearHypothesis.html)
  computes Wald tests for hypotheses about coefficients or their linear
  combinations.
- [`logLik()`](https://rdrr.io/r/stats/logLik.html) returns the
  log-likelihood and degrees of freedom for the nested-dichotomies logit
  model.
- Through [`logLik()`](https://rdrr.io/r/stats/logLik.html), the
  [`AIC()`](https://rdrr.io/r/stats/AIC.html) and
  [`BIC()`](https://rdrr.io/r/stats/AIC.html) functions compute the
  Akaike and Bayesian information criteria model-comparison statistics.

We illustrate some of these:

**Coefficients**: By default,
[`coef()`](https://rdrr.io/r/stats/coef.html) returns a matrix whose
rows are the terms in the model and whose columns represent the nested
dichotomies. The coefficients, \\\mathbf{\beta\_{i1}}\\ give the changes
in the log odds of working vs. not working associated with a \$1,000
increase in husband’s income and with having children present
vs. absent. \\\mathbf{\beta\_{i2}}\\ is similar for the log odds of
working full time vs. part time among those who are working.
\\e^\mathbf{\beta}\\ give the multiples of the odds for these
comparisons.

``` r
coef(wlf.nested)
#>                     work    full
#> (Intercept)      1.33583  3.4778
#> hincome         -0.04231 -0.1073
#> childrenpresent -1.57565 -2.6515

# show as odds
exp(coef(wlf.nested))
#>                   work     full
#> (Intercept)     3.8032 32.38753
#> hincome         0.9586  0.89829
#> childrenpresent 0.2069  0.07055
```

Thus, the odds of both working and working full time decrease with
husband’s income. Having young children also decreases the odds of both.

**Anova**: Extends [`Anova()`](https://rdrr.io/pkg/car/man/Anova.html)
from the `car` package (Fox, Weisberg, & Price, 2023), It gives Type II
tests for each term in the model. Note that the `LR` \\\chi^2\\ and `df`
for the `Combined Responses` is the sum of their values for the separate
dichotomies.

``` r
Anova(wlf.nested)
#> 
#>  Analysis of Deviance Tables (Type II tests)
#>  
#> Response work: {not.work} vs. {parttime, fulltime}
#>          LR Chisq Df Pr(>Chisq)    
#> hincome      4.83  1      0.028 *  
#> children    31.32  1    2.2e-08 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> Response full: {parttime} vs. {fulltime}
#>          LR Chisq Df Pr(>Chisq)    
#> hincome       9.0  1     0.0027 ** 
#> children     32.1  1    1.4e-08 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> Combined Responses
#>          LR Chisq Df Pr(>Chisq)    
#> hincome      13.8  2      0.001 ** 
#> children     63.5  2    1.7e-14 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

**linearHypothesis**: For a given model,
[`car::linearHypothesis()`](https://rdrr.io/pkg/car/man/linearHypothesis.html)
provides a very general method for testing specific hypotheses about
individual coefficients in a model or their linear combinations. This is
extended here in the `linearHypothesis` method for `"nestedLogit"`
models.

For example, the following call tests the hypothesis that the
coefficients for `hincome` and `children` are simultaneously all equal
to zero. This is equivalent to the test of the global null model, \\H_0
: \mathbf{B} = 0\\ against an alternative that one or more coefficients
\\\beta\_{ij} \ne 0\\.

[`linearHypothesis()`](https://rdrr.io/pkg/car/man/linearHypothesis.html)
gives these tests for each of the submodels, `work`, `full` as well as
for the combined model.

``` r
linearHypothesis(wlf.nested, c("hincome", "childrenpresent"))
#> Linear hypothesis test
#> 
#> Hypothesis: 
#> hincome = 0 
#> childrenpresent = 0 
#>  
#> Model 1: restricted model
#> Model 2: partic ~ hincome + children 
#>  
#> Response work: {not.work} vs. {parttime, fulltime} 
#>   Res.Df Df Chisq Pr(>Chisq)    
#> 1    262                        
#> 2    260  2  32.2      1e-07 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Response full: {parttime} vs. {fulltime}
#>   Res.Df Df Chisq Pr(>Chisq)    
#> 1    107                        
#> 2    105  2  25.6    2.8e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Combined Responses
#> Chisq = 57.813, Df = 4, Pr(>Chisq) = 8.4e-12
```

**glance** and **tidy**: The `broom` package (Robinson, Hayes, & Couch,
2023) provides some methods for giving compact and tidy summaries of
fitted models. The `glance` method for a `"nestedLogit"` model gives a
one-line summary of the statistics for each dichotomy. The `tidy` method
combines the coefficients for the sub-models, together with test
statistics.

``` r
glance(wlf.nested)   # summarize the sub-models
#> # A tibble: 2 × 9
#>   response null.deviance df.null logLik   AIC   BIC deviance df.residual  nobs
#>   <chr>            <dbl>   <int>  <dbl> <dbl> <dbl>    <dbl>       <int> <int>
#> 1 work              356.     262 -160.   326.  336.     320.         260   263
#> 2 full              144.     107  -52.2  110.  119.     104.         105   108

tidy(wlf.nested)     # summarize the coefficients
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

These functions make it easy to construct custom tables. For example, to
extract the likelihood ratio `deviance` (\\G^2\\) goodness-of fit tests
and compute (\\G^2 / df\\), you could do:

``` r
gl <- glance(wlf.nested)
gl |> 
  select(response, deviance, df.residual) |> 
  add_row(response = "Combined", deviance = sum(gl$deviance), df.residual = sum(gl$df.residual)) |>
  mutate(
    `P-value` = pchisq(deviance, df.residual, lower.tail = FALSE),
    `$G^2$/df` = deviance / df.residual) |>
  rename(`$G^2$` = deviance,
         df = df.residual) |>
  knitr::kable(digits = 3)
```

| response | \\G^2\\ |  df | P-value | \\G^2\\/df |
|:---------|--------:|----:|--------:|-----------:|
| work     |   319.7 | 260 |   0.007 |      1.230 |
| full     |   104.5 | 105 |   0.496 |      0.995 |
| Combined |   424.2 | 365 |   0.018 |      1.162 |

**update**: Makes it easy to create a new model from an old one, by
adding/subtracting terms from the model formula or changing the
observations used or contrasts for factors.

For example: You might ask: “*Does it make sense to include `region` of
Canada in the model?*”  
This can be tested by adding it to the model formula, and comparing the
new model with the original one using
[`anova()`](https://rdrr.io/r/stats/anova.html). The significance tests
here are for the additional contribution of `region` over the model that
just includes main effects of `husincome` and `children`.

``` r
wlf.nested.1 <- update(wlf.nested, formula = . ~ . + region)

anova(wlf.nested, wlf.nested.1)
#> 
#>  Analysis of Deviance Tables
#>  Model 1: partic ~ hincome + children
#>  Model 2: partic ~ hincome + children + region 
#>  
#> Response work: {not.work} vs. {parttime, fulltime}
#>   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
#> 1       260        320                     
#> 2       256        317  4     2.43     0.66
#> 
#> 
#> Response full: {parttime} vs. {fulltime}
#>   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
#> 1       105        104                     
#> 2       101        102  4     2.65     0.62
#> 
#> 
#> Combined Responses
#>   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
#> 1       365        424                     
#> 2       357        419  8     5.08     0.75
```

Note that [`anova()`](https://rdrr.io/r/stats/anova.html) with two or
models tests these against one another, in the order specified. This
assumes that the models compared are *nested* (an unintentional a pun),
in the sense that the terms in the smaller model are a subset of those
in the larger model.

In a similar way, we could fit and test a *larger* scope of models. For
example to add an interaction between husband’s income and children:

``` r
wlf.nested.2 <- update(wlf.nested, formula = . ~ .^2)
anova(wlf.nested, wlf.nested.2)
#> 
#>  Analysis of Deviance Tables
#>  Model 1: partic ~ hincome + children
#>  Model 2: partic ~ hincome + children + hincome:children 
#>  
#> Response work: {not.work} vs. {parttime, fulltime}
#>   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
#> 1       260        320                     
#> 2       259        319  1    0.608     0.44
#> 
#> 
#> Response full: {parttime} vs. {fulltime}
#>   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
#> 1       105        104                     
#> 2       104        104  1    0.256     0.61
#> 
#> 
#> Combined Responses
#>   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
#> 1       365        424                     
#> 2       363        423  2    0.864     0.65
```

We can see neither `region` nor an interaction make a difference in
goodness of fit of either of the sub-models or the combined model for
the three response categories.

## Obtaining fitted values: `predict()`

By default, [`predict()`](https://rdrr.io/r/stats/predict.html) for a
`"nestedLogit"` model object returns the predicted probabilities
(`type = "response"`) of each of the response categories for *all*
observations in the data set. Note that the computation is a bit tricky,
because the probabilities of working full time or part time are
conditional on working outside the home.

``` r
car::some(predict(wlf.nested))
#>     not.work parttime fulltime
#> 13    0.6504  0.18696  0.16268
#> 27    0.7396  0.20070  0.05974
#> 134   0.3222  0.08251  0.59525
#> 137   0.6968  0.20093  0.10226
#> 143   0.6693  0.19426  0.13640
#> 182   0.3410  0.09661  0.56240
#> 198   0.7925  0.18197  0.02556
#> 212   0.6878  0.19929  0.11291
#> 242   0.3131  0.07607  0.61087
#> 257   0.2225  0.02865  0.74886
```

## Plotting

The `nestedLogit` package has a basic
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
`"nestedLogit"` models. It calculates fitted probabilities for the
response categories and plots these against a single explanatory
variable on the horizontal axis and `other` explanatory variables fixed
to particular values. To produce multi-panel plots, it is necessary to
call this repeatedly for levels of other variables, and compose these
into a single figure using
[`par()`](https://rdrr.io/r/graphics/par.html) (or `knitr` chunk
options):

``` r
op <- par(mfcol=c(1, 2), mar=c(4, 4, 3, 1) + 0.1)
plot(wlf.nested, "hincome", list(children="absent"),
     xlab="Husband's Income", legend.location="top")
plot(wlf.nested, "hincome", list(children="present"),
     xlab="Husband's Income", legend=FALSE)
```

![\*\*plot method\*\*: Predicted probabilities of working at all or
working part time or full time](fig/wlf-plot-1.png)

**plot method**: Predicted probabilities of working at all or working
part time or full time

### Constructing plots

To explain how this works, and for better control of the details, it is
useful to describe how this can be done directly.

To construct a plot, it is sufficient to calculate predicted
probabilities over a grid of values of the predictor variables. Here, we
select a range of 0 - 45 in steps of 5, combined with the two values of
`children`.

``` r
new <- expand.grid(hincome=seq(0, 45, by = 5), 
                   children=c("absent", "present"))

pred.nested <- predict(wlf.nested, newdata = new)
plotdata <- cbind(new, pred.nested)
head(plotdata)
#>   hincome children not.work parttime fulltime
#> 1       0   absent   0.2082  0.02372   0.7681
#> 2       5   absent   0.2452  0.03785   0.7169
#> 3      10   absent   0.2864  0.05907   0.6545
#> 4      15   absent   0.3315  0.08936   0.5791
#> 5      20   absent   0.3800  0.12944   0.4906
#> 6      25   absent   0.4309  0.17691   0.3922
```

Because the fitted values are in multiple columns, it is easiest to plot
these using [`matplot()`](https://rdrr.io/r/graphics/matplot.html). We
could plot these all in a single (messy) figure, but it is clearer to
show separate panels for children `absent` and `present`.

To do this, the `plotdata` data set is subset within a loop over the
values of `children` and each subset is plotted by `matplot`. It is only
necessary to include the legend in one panel. The plots are combined
into a single figure using `par(mfrow())`.

``` r
op <- par(mfrow=c(1,2), mar=c(4,4,3,1)+.1)
cols=c("blue", "magenta", "darkgreen")
for ( kids in c("absent", "present") ) {
  data <- subset(plotdata, children==kids)
  matplot(data[, "hincome"], data[, 5:3], 
          type = "l", lwd=3, lty = 1:3, col = cols,
          xlab="Husband's Income", 
          ylab='Fitted Probability',
          main = paste("Children", kids),
          cex.lab = 1.1)
  if (kids=="absent") {
    legend("topright", lty=1:3, lwd=3, col=cols, bty = "n",
           legend=c("fulltime", "parttime", "not working"))
  }
}
```

![\*\*matplot\*\*: Predicted probabilities of working at all or working
part time or full time](fig/wlf-matplot-1.png)

**matplot**: Predicted probabilities of working at all or working part
time or full time

### Using `ggplot`

More control, and perhaps a more aesthetically pleasing figure can be
produced using `ggplot` (Wickham et al., 2023). However, `ggplot` wants
the data in long format. That makes it easy to plot probability against
one predictor and use `color` to distinguish the levels of `partic` and
facet the plot by `children`.

``` r
plotlong <- plotdata |>
  tidyr::pivot_longer(fulltime : not.work,
                      names_to = "Working",
                      values_to = "Probability") |>
  mutate(Working = ordered(Working, 
                           levels = c("not.work", "parttime", "fulltime")) )

head(plotlong)
#> # A tibble: 6 × 4
#>   hincome children Working  Probability
#>     <dbl> <fct>    <ord>          <dbl>
#> 1       0 absent   fulltime      0.768 
#> 2       0 absent   parttime      0.0237
#> 3       0 absent   not.work      0.208 
#> 4       5 absent   fulltime      0.717 
#> 5       5 absent   parttime      0.0378
#> 6       5 absent   not.work      0.245
```

Then, one call to `ggplot` produces both panels. To sort the levels of
`Working` appropriately, we made this an ordered factor in the step
above, but assign the colors with a discrete scale.

``` r
gg <- ggplot(plotlong,
             aes(x=hincome, y=Probability, color=Working)) +
  geom_line(linewidth = 2) +
  scale_color_discrete() +
  labs(x="Husband's Income", y= "Probability") +
  facet_wrap(~ children, labeller = label_both) +
  theme_bw(base_size = 14) +
  theme(legend.position = c(.3, .8))
gg
```

![\*\*ggplot\*\*: Predicted probabilities of working at all or working
part time or full time](fig/wlf-ggplot-1.png)

**ggplot**: Predicted probabilities of working at all or working part
time or full time

### Direct labels

It’s usually nicer to label the curves directly than to rely on a
legend.

The `geomtextpath` (Cameron & van den Brand, 2022) package offers
[`geom_textline()`](https://allancameron.github.io/geomtextpath/reference/geom_textpath.html)
as an alternative to
[`geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
that adds a text label to a curve.

``` r
ggplot(plotlong,
       aes(x=hincome, y=Probability, color=Working)) +
  geom_textline(aes(label = Working),
                linewidth = 2, size = 5, 
                hjust = 0.9, vjust = 0.2) +
  scale_color_discrete() +
  labs(x="Husband's Income", y= "Probability") +
  facet_wrap(~ children, labeller = label_both) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")
```

![\*\*geomtextpath\*\*: Predicted probabilities, with labels on the
curves](fig/wlf-geomtextpath-1.png)

**geomtextpath**: Predicted probabilities, with labels on the curves

### Plotting fitted log odds

It is sometimes easier to interpret logistic regression models by
plotting the linear predictors on the **log odds** scale, because these
appear as straight lines. That is, from the estimated coefficients in
the model, the predicted log odds of `work` and `full` are given by

\\ L_1 =\log\left\[ \frac { \widehat{P( \operatorname{work} =
\operatorname{1} )} }{ 1 - \widehat{P( \operatorname{work} =
\operatorname{1} )} } \right\] = 1.34 - 0.04(\operatorname{hincome}) -
1.58(\operatorname{children}\_{\operatorname{present}}) \\

\\ L_2 =\log\left\[ \frac { \widehat{P( \operatorname{full} =
\operatorname{1})} }{ 1 - \widehat{P( \operatorname{full} =
\operatorname{1})} } \right\] = 3.48 - 0.11(\operatorname{hincome}) -
2.65(\operatorname{children}\_{\operatorname{present}}) \\ Here,
\\\operatorname{children}\_{\operatorname{present}}\\ is either 0 or 1,
so this amounts to a shift in the intercept when children are present.

These values can be calculated by calling
[`predict.glm()`](https://rdrr.io/r/stats/predict.glm.html) directly for
the `models` component of `wlf.nested`, specifying `type = "link"`:

``` r
pred.logits <- sapply(models(wlf.nested), predict, newdata=new, type = "link")
plotdatal <- cbind(new, pred.logits)
head(plotdatal)
#>   hincome children   work   full
#> 1       0   absent 1.3358 3.4778
#> 2       5   absent 1.1243 2.9414
#> 3      10   absent 0.9127 2.4051
#> 4      15   absent 0.7012 1.8688
#> 5      20   absent 0.4897 1.3324
#> 6      25   absent 0.2781 0.7961
```

Then we plot these more or less as before:

``` r
cols=c("blue", "red")

op <- par(mfrow=c(1,2), mar=c(4,4,3,1)+.1)
for ( kids in c("absent", "present") ) {
  data <- subset(plotdatal, children==kids)
  matplot(data[, "hincome"], data[, 3:4],
          type = "l", lwd=3, lty = 1, col = cols,
          xlab="Husband's Income",
          ylab='Predicted Log Odds',
          main = paste("Children", kids),
          cex.lab = 1.1)
  if (kids=="absent") {
    legend("topright", lty=1, lwd=3, col=cols, bty = "n",
           title = "Dichotomy",
           legend=c("work", "full"))
  }
}
```

![\*\*log odds\*\*: Predicted logits of the \`work\` and \`full\`
dichotomies](fig/wlf-logits-1.png)

**log odds**: Predicted logits of the `work` and `full` dichotomies

Finally, we can use `ggplot` as before to plot the fitted logits, first
transforming the long format.

``` r
plotlongl <- plotdatal |>
  tidyr::pivot_longer(work : full,
                      names_to = "Dichotomy",
                      values_to = "logit") |>
  mutate(Dichotomy = ordered(Dichotomy,
                         levels = c("work", "full")) )
```

Doing this gives the flexibility that we can facet the plot either by
`children` (as before) or `Dichotomy`. The latter nicely shows that the
additive model has equal slopes for husband’s income within both panels:
increasing husband’s income decreases the log odds of working, but at a
faster rate for the `full` dichotomy. Having young children decreases
the log odds of working either at all or working fulltime as compared to
parttime.

``` r
ggplot(plotlongl,
       aes(x=hincome, y=logit, color=children)) +
  geom_line(linewidth = 3) +
  geom_point(size = 1.5, shape = 16, color = "black") +
  scale_color_discrete() +
  labs(x="Husband's Income", y= "Log Odds") +
  facet_wrap(~ Dichotomy, labeller = label_both) +
  theme_bw(base_size = 14) +
  theme(legend.position = c(.35, .82))
```

![\*\*log odds\*\*: Predicted logits of the \`work\` and \`full\`
dichotomies](fig/wlf-gglogits-1.png)

**log odds**: Predicted logits of the `work` and `full` dichotomies

``` r
# write a packages.bib file of the packages (.packages()) that have been used here
pkgs <- unique(c(to.cite, .packages()))
knitr::write_bib(pkgs, file = here::here("vignettes", "packages.bib"))
```

## References

Atkinson, T., Blishen, B. R., Ornstein, M. D., & Stevenson, H. M.
(1984). Quality of Canadian Life: Social Change in Canada, 1977. ICPSR -
Interuniversity Consortium for Political; Social Research.
http://doi.org/[10.3886/ICPSR07879.V1](https://doi.org/10.3886/ICPSR07879.V1)

Cameron, A., & van den Brand, T. (2022). *Geomtextpath: Curved text in
ggplot2*. Retrieved from <https://allancameron.github.io/geomtextpath/>

Fienberg, S. E. (1980). *The analysis of cross-classified categorical
data* (2nd ed.). Cambridge, MA: MIT Press.

Fox, J. (2016). *Applied regression analysis and generalized linear
models* (Third edition.). Los Angeles: SAGE.

Fox, J., Weisberg, S., & Price, B. (2023). *Car: Companion to applied
regression*. Retrieved from <https://CRAN.R-project.org/package=car>

Friendly, M., & Meyer, D. (2016). *Discrete data analysis with R:
Visualization and modeling techniques for categorical and count data*.
Boca Raton, FL: Chapman & Hall/CRC.

Ripley, B. (2022). *Nnet: Feed-forward neural networks and multinomial
log-linear models*. Retrieved from
<http://www.stats.ox.ac.uk/pub/MASS4/>

Robinson, D., Hayes, A., & Couch, S. (2023). *Broom: Convert statistical
objects into tidy tibbles*. Retrieved from
<https://CRAN.R-project.org/package=broom>

Wickham, H., Chang, W., Henry, L., Pedersen, T. L., Takahashi, K.,
Wilke, C., … Dunnington, D. (2023). *ggplot2: Create elegant data
visualisations using the grammar of graphics*. Retrieved from
<https://CRAN.R-project.org/package=ggplot2>
