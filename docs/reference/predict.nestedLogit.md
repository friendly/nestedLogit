# Predicted Probabilities and Logits for `"nestedLogit"` Models

The `predict` and `fitted` methods compute predicted values from a
fitted `"nestedLogit"` model. The `confint` method computes point-wise
confidence limits for predicted response-category probabilities or
logits.

- `predict`, `fitted`:

  Compute predicted response-category probabilities (or predicted logits
  for each binary logit model in the dichotomies) from a fitted
  `"nestedLogit"` model.

- `confint`:

  Compute point-wise confidence limits for predicted response-category
  probabilities or logits.

- `print` methods:

  Print predicted probabilities, logits, and their standard errors, with
  control over how many rows to display.

## Usage

``` r
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
```

## Arguments

- object:

  a fitted object of class `"nestedLogit"`; for `confint`, an object of
  class `"predictNestedLogit"`.

- newdata:

  a data frame containing combinations of values of the predictors at
  which fitted probabilities (or other quantities) are to be computed.
  If missing, the original data are used.

- model:

  either `"nested"` (the default), in which case fitted probabilities
  under the nested logit model are returned, or `"dichotomies"`, in
  which case [`predict.glm`](https://rdrr.io/r/stats/predict.glm.html)
  is invoked for each binary logit model fit to the nested dichotomies
  and a named list of the results is returned.

- ...:

  arguments to be passed down.

- x:

  an object of class `"predictNestedLogit"` or `"predictDichotomies"`.

- n:

  an integer or `"all"` to control how many rows are printed for each of
  the predicted values.

- parm:

  one of `"prob"` or `"logit"`, indicating whether to generate
  confidence intervals for probabilities or logits of the responses.

- level:

  confidence level, a number between 0 and 1; default is `0.95`.

- conf.limits.logit:

  logical; when `parm = "prob"`, if `TRUE` (the default), confidence
  limits are computed on the logit scale and back-transformed to
  probabilities; if `FALSE`, Wald-type limits are computed directly on
  the probability scale.

## Value

- The `predict` and `fitted` methods return an object of class
  `"predictNestedLogit"` (when `model = "nested"`) or
  `"predictDichotomies"` (when `model = "dichotomies"`).

  A `"predictNestedLogit"` object is a list containing:

  - `p`:

    a data frame of predicted probabilities, with one column per
    response category.

  - `logit`:

    a data frame of predicted logits.

  - `se.p`:

    a data frame of standard errors of predicted probabilities.

  - `se.logit`:

    a data frame of standard errors of predicted logits.

  - `.data`:

    the `newdata` data frame, if supplied.

  A `"predictDichotomies"` object is a named list of data frames, one
  per dichotomy, each produced by
  [`predict.glm`](https://rdrr.io/r/stats/predict.glm.html).

- The `confint` method returns a data frame of point estimates and
  confidence limits.

- The various `print` methods invisibly return their `x` arguments.

## Details

The `predict` method provides predicted values for two representations
of the model. `model = "nested"` (the default) gives the fitted
probabilities for each of the response categories, along with the
corresponding logits and standard errors of each.
`model = "dichotomies"` gives the fitted log odds for each of the binary
logit models in the nested dichotomies.

The `fitted` method (with no `newdata`) is equivalent to `predict`
applied to the original data used to fit the model.

For the `confint` method with `parm = "prob"`, setting
`conf.limits.logit = TRUE` (the default) computes confidence limits on
the logit scale and back-transforms them to probabilities, which ensures
that the limits lie in \\\[0, 1\]\\. Setting `conf.limits.logit = FALSE`
computes Wald-type confidence intervals directly on the probability
scale, which may extend outside \\\[0, 1\]\\.

## See also

[`nestedLogit`](https://friendly.github.io/nestedLogit/reference/nestedLogit.md),
[`nestedMethods`](https://friendly.github.io/nestedLogit/reference/nestedMethods.md),
[`plot.nestedLogit`](https://friendly.github.io/nestedLogit/reference/plot.nestedLogit.md),
[`as.data.frame.predictNestedLogit`](https://friendly.github.io/nestedLogit/reference/as.data.frame.predictNestedLogit.md)

## Author

John Fox and Michael Friendly

## Examples

``` r
# define continuation dichotomies for level of education
cont.dichots <- continuationLogits(c("l.t.highschool",
                                     "highschool",
                                     "college",
                                     "graduate"))

# fit a nested model for the GSS data
m <- nestedLogit(degree ~ parentdeg + year,
                 cont.dichots,
                 data=GSS)

# predicted probabilities for first few cases
predict(m)
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

# predicted probabilities at specific values of predictors
new <- expand.grid(parentdeg=c("l.t.highschool",  "highschool",
                               "college", "graduate"),
                   year=c(1972, 2016))

fit <- predict(m, newdata=new)
cbind(new, fit)
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

# use fitted() -- equivalent to predict() on the original data
f <- fitted(m)

# predicted logits for each dichotomy
pred.dichot <- predict(m, newdata=new, model="dichotomies")
pred.dichot
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

# confidence intervals for predicted probabilities
fit.ci <- confint(fit)
head(fit.ci)
#>   l.t.highschool.p highschool.p  college.p graduate.p l.t.highschool.0.025
#> 1       0.48669496    0.4481946 0.04326738 0.02184305           0.47435358
#> 2       0.11747919    0.6976573 0.13619288 0.04867067           0.11061342
#> 3       0.04093763    0.5328723 0.31960957 0.10658049           0.03425783
#> 4       0.03215663    0.4161182 0.34830386 0.20342132           0.02515812
#> 5       0.26785921    0.5858314 0.08374198 0.06256742           0.25474701
#> 6       0.04885547    0.6534659 0.19470249 0.10297618           0.04556752
#>   highschool.0.025 college.0.025 graduate.0.025 l.t.highschool.0.975
#> 1        0.4367550    0.04004156     0.01963924           0.49905259
#> 2        0.6875711    0.12914674     0.04456744           0.12471137
#> 3        0.5147140    0.30322001     0.09631738           0.04885401
#> 4        0.3953399    0.32784710     0.18629229           0.04102008
#> 5        0.5721494    0.07709029     0.05680926           0.28139148
#> 6        0.6429798    0.18604443     0.09642305           0.05236764
#>   highschool.0.975 college.0.975 graduate.0.975
#> 1        0.4596893    0.04674043     0.02428803
#> 2        0.7075563    0.14356008     0.05313067
#> 3        0.5509440    0.33645724     0.11779464
#> 4        0.4371989    0.36933569     0.22169621
#> 5        0.5993823    0.09091108     0.06886661
#> 6        0.6638049    0.20366267     0.10992051

# confidence intervals for predicted logits
fit.ci.logit <- confint(fit, parm="logit")
head(fit.ci.logit)
#>   l.t.highschool.logit highschool.logit college.logit graduate.logit
#> 1          -0.05323272       -0.2079679    -3.0961249      -3.801787
#> 2          -2.01652113        0.8361667    -1.8472774      -2.972784
#> 3          -3.15390636        0.1316792    -0.7555666      -2.126156
#> 4          -3.40445160       -0.3387293    -0.6265031      -1.365047
#> 5          -1.00551133        0.3467588    -2.3925576      -2.706900
#> 6          -2.96879974        0.6343089    -1.4197391      -2.164585
#>   l.t.highschool.0.025 highschool.0.025 college.0.025 graduate.0.025
#> 1           -0.1026758      -0.25434235    -3.1769721      -3.910391
#> 2           -2.0844905       0.78878837    -1.9085242      -3.065161
#> 3           -3.3389816       0.05887299    -0.8320111      -2.238829
#> 4           -3.6570947      -0.42492042    -0.7179385      -1.474284
#> 5           -1.0734528       0.29062588    -2.4825541      -2.809569
#> 6           -3.0419217       0.58832109    -1.4759203      -2.237616
#>   l.t.highschool.0.975 highschool.0.975 college.0.975 graduate.0.975
#> 1         -0.003789662       -0.1615935    -3.0152777      -3.693184
#> 2         -1.948551711        0.8835449    -1.7860305      -2.880407
#> 3         -2.968831089        0.2044854    -0.6791222      -2.013482
#> 4         -3.151808517       -0.2525381    -0.5350677      -1.255809
#> 5         -0.937569863        0.4028918    -2.3025610      -2.604231
#> 6         -2.895677757        0.6802968    -1.3635578      -2.091553
```
