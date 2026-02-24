# Choice of Health Insurance Product

A company recently introduced a new health insurance provider for its
employees. At the beginning of the year the employees had to choose one
of three (or four) different health plan products from this provider to
best suit their needs.

This dataset was modified from its original source (McNulty, 2022) for
the present purposes by adding a fourth choice, sampled randomly from
the original three.

## Usage

``` r
data("HealthInsurance", package = "nestedLogit")
```

## Format

A data frame with 1448 rows and 7 columns.

- product:

  Choice among three products, a factor with levels `"A"`, `"B"`, and
  `"C"`.

- product4:

  Choice among four products, a factor with levels `"A"`, `"B"`, `"C"`,
  and `"D"`.

- age:

  The age of the individual, in years.

- household:

  The number of people living with the individual in the same household.

- position_level:

  Position level in the company at the time the choice was made, where 1
  is is the lowest level and 5 is the highest, a numeric vector.

- gender:

  The gender of the individual, a factor with levels `"Female"` and
  `"Male"`.

- absent:

  The number of days the individual was absent from work in the year
  prior to the choice,

## Source

Originally taken from McNulty, K. (2022). *Handbook of Regression
Modeling in People Analytics*,
<https://peopleanalytics-regression-book.org/data/health_insurance.csv>.

## See also

[`nestedLogit`](https://friendly.github.io/nestedLogit/reference/nestedLogit.md).

## Examples

``` r
lbinary <- logits(AB_CD = dichotomy(c("A", "B"), c("C", "D")),
                  A_B   = dichotomy("A", "B"),
                  C_D   = dichotomy("C", "D"))
as.matrix(lbinary)
#>        A  B  C  D
#> AB_CD  0  0  1  1
#> A_B    0  1 NA NA
#> C_D   NA NA  0  1
health.nested <- nestedLogit(product4 ~ age  + gender * household + position_level,
                             dichotomies = lbinary, data = HealthInsurance)
                             car::Anova(health.nested)
#> 
#>  Analysis of Deviance Tables (Type II tests)
#>  
#> Response AB_CD: {A, B} vs. {C, D}
#>                  LR Chisq Df Pr(>Chisq)    
#> age               161.171  1  < 2.2e-16 ***
#> gender             30.412  1  3.493e-08 ***
#> household         128.772  1  < 2.2e-16 ***
#> position_level      0.044  1     0.8344    
#> gender:household   16.299  1  5.408e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> Response A_B: {A} vs. {B}
#>                  LR Chisq Df Pr(>Chisq)    
#> age               229.664  1  < 2.2e-16 ***
#> gender             75.537  1  < 2.2e-16 ***
#> household         127.743  1  < 2.2e-16 ***
#> position_level     27.164  1  1.869e-07 ***
#> gender:household    0.091  1     0.7633    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> Response C_D: {C} vs. {D}
#>                  LR Chisq Df Pr(>Chisq)    
#> age               116.663  1  < 2.2e-16 ***
#> gender              5.355  1    0.02066 *  
#> household          52.861  1   3.58e-13 ***
#> position_level      0.018  1    0.89305    
#> gender:household    1.545  1    0.21384    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> Combined Responses
#>                  LR Chisq Df Pr(>Chisq)    
#> age                507.50  3  < 2.2e-16 ***
#> gender             111.30  3  < 2.2e-16 ***
#> household          309.38  3  < 2.2e-16 ***
#> position_level      27.23  3  5.278e-06 ***
#> gender:household    17.94  3  0.0004536 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
coef(health.nested)
#>                            AB_CD         A_B          C_D
#> (Intercept)          -3.85986638 -2.19851256  4.826064163
#> age                   0.05740364  0.17267537 -0.071680487
#> genderMale            1.46728946 -2.45841955 -0.824955433
#> household             0.40271031 -0.70434692 -0.350313558
#> position_level        0.01029949 -0.56167558 -0.009193197
#> genderMale:household -0.22931808  0.05779896  0.107014721
```
