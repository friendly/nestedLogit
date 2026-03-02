# Using ggeffects with nestedLogit models

Load the packages we’ll use here:

``` r
library(nestedLogit)    # Nested Dichotomy Logistic Regression Models
library(ggeffects)      # Create Tidy Data Frames of Marginal Effects
library(ggplot2)        # Data Visualisations Using the Grammar of Graphics
```

## Overview

The `ggeffects` package (**R-ggeffects?**; **ggeffects2018?**) provides
a simple and unified interface for computing and plotting adjusted
predictions and marginal effects from a wide variety of regression
models. Its main function,
[`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.html),
returns a tidy data frame of model predictions that can be plotted
directly with a built-in
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) method or
further customized with `ggplot2`.

The package now supports `"nestedLogit"` objects, making it easy to
visualize predicted probabilities for each response category across
levels of the predictors, without the manual data wrangling described in
[`vignette("plotting-ggplot", package = "nestedLogit")`](https://friendly.github.io/nestedLogit/articles/plotting-ggplot.md).

## Women’s labor force participation

We use the standard `Womenlf` example from the main vignette. The
response `partic` has three categories — not working, working part-time,
and working full-time — modeled as nested dichotomies against husband’s
income and presence of young children.

``` r
data(Womenlf, package = "carData")
comparisons <- logits(work = dichotomy("not.work", c("parttime", "fulltime")),
                      full = dichotomy("parttime", "fulltime"))

wlf.nested <- nestedLogit(partic ~ hincome + children,
                          dichotomies = comparisons,
                          data = Womenlf)
```

## Predicted probabilities with `predict_response()`

The simplest way to obtain predicted probabilities and a plot is with
[`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.html),
specifying the focal predictors in the `terms` argument. This returns
predicted probabilities for each response level, with confidence
intervals, averaged over the non-focal predictors.

``` r
wlf.pred <- predict_response(wlf.nested, terms = c("hincome", "children"))
wlf.pred
#> # Predicted probabilities of partic
#> 
#> partic: not.work
#> children: absent
#> 
#> hincome | Predicted |     95% CI
#> --------------------------------
#>       0 |      0.21 | 0.11, 0.36
#>      12 |      0.30 | 0.21, 0.42
#>      22 |      0.40 | 0.28, 0.54
#>      46 |      0.65 | 0.34, 0.87
#> 
#> partic: not.work
#> children: present
#> 
#> hincome | Predicted |     95% CI
#> --------------------------------
#>       0 |      0.56 | 0.40, 0.70
#>      12 |      0.68 | 0.60, 0.75
#>      22 |      0.76 | 0.67, 0.83
#>      46 |      0.90 | 0.71, 0.97
#> 
#> partic: parttime
#> children: absent
#> 
#> hincome | Predicted |     95% CI
#> --------------------------------
#>       0 |      0.02 | 0.01, 0.10
#>      12 |      0.07 | 0.03, 0.15
#>      22 |      0.15 | 0.07, 0.29
#>      46 |      0.29 | 0.10, 0.60
#> 
#> partic: parttime
#> children: present
#> 
#> hincome | Predicted |     95% CI
#> --------------------------------
#>       0 |      0.13 | 0.06, 0.29
#>      12 |      0.20 | 0.14, 0.27
#>      22 |      0.19 | 0.13, 0.28
#>      46 |      0.10 | 0.03, 0.28
#> 
#> partic: fulltime
#> children: absent
#> 
#> hincome | Predicted |     95% CI
#> --------------------------------
#>       0 |      0.77 | 0.62, 0.87
#>      12 |      0.63 | 0.51, 0.73
#>      22 |      0.45 | 0.32, 0.60
#>      46 |      0.07 | 0.01, 0.41
#> 
#> partic: fulltime
#> children: present
#> 
#> hincome | Predicted |     95% CI
#> --------------------------------
#>       0 |      0.31 | 0.18, 0.47
#>      12 |      0.12 | 0.08, 0.19
#>      22 |      0.04 | 0.02, 0.10
#>      46 |      0.00 | 0.00, 0.03
```

The default [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
method produces a panel for each response category:

``` r
plot(wlf.pred)
```

![Predicted probabilities from \`predict_response()\` with default
plot.](fig/wlf-ggeffects-plot1-1.png)

Predicted probabilities from
[`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.html)
with default plot.

## Customizing the plot

The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method
returns a `ggplot` object, so it can be further customized with standard
`ggplot2` functions. For example, we can adjust the line size, labels,
theme, and legend position:

``` r
plot(wlf.pred,
     line_size = 2) +
  labs(title = "Predicted Probabilities of Work by Husband's Income",
       y = "Probability",
       x = "Husband's Income") +
  theme_ggeffects(base_size = 16) +
  theme(legend.position = "top")
```

![Customized \`ggeffects\` plot with adjusted labels and
theme.](fig/wlf-ggeffects-plot2-1.png)

Customized `ggeffects` plot with adjusted labels and theme.

## Plotting on the logit scale

`ggplot2` provides a built-in `"logit"` transformation for axes via
`scale_y_continuous(transform = "logit")`. This displays predicted
probabilities on the logit scale, $`\text{logit}(p) = \log(p / (1-p))`$,
where the axis labels remain as probabilities but their spacing reflects
the logit transformation. This is useful because the logistic regression
model is linear on the logit scale, so the predicted curves appear as
straighter lines.

Since the [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
method for `ggeffects` returns a `ggplot` object, we can simply add this
scale transformation. Note that we need to specify breaks manually,
because the automatic break algorithm does not work well with the logit
transformation.

``` r
plot(wlf.pred,
     line_size = 2) +
  scale_y_continuous(
    transform = "logit",
    breaks = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)
  ) +
  labs(title = "Predicted Probabilities (logit scale)",
       y = "Probability (logit scale)",
       x = "Husband's Income") +
  theme_ggeffects(base_size = 16) +
  theme(legend.position = "top")
```

![Predicted probabilities on the logit
scale.](fig/wlf-logit-scale-1.png)

Predicted probabilities on the logit scale.

## Alligator food choice

As a simpler example with a single continuous predictor, we fit a nested
logit model to the `gators` data, predicting primary food choice from
alligator length. The first dichotomy contrasts {Other} vs. {Fish,
Invertebrates}, and the second contrasts {Fish} vs. {Invertebrates}.

``` r
data(gators)
gators.nested <- nestedLogit(food ~ length,
                             dichotomies = logits(
                               other   = dichotomy("Other", c("Fish", "Invertebrates")),
                               fish_inv = dichotomy("Fish", "Invertebrates")),
                             data = gators)
```

``` r
predict_response(gators.nested, terms = "length") |> 
  plot(line_size = 2)
```

![Predicted food choice probabilities for alligators by
length.](fig/gators-ggeffects-1.png)

Predicted food choice probabilities for alligators by length.

As you can see the main thing going on here is that larger alligators
prefer fish, which smaller ones prefer invertebrates.

For comparison, the basic
[`nestedLogit::plot()`](https://rdrr.io/r/graphics/plot.default.html)
method using default arguments gives a similar plot, with the three
curves overlaid in a single panel (it uses
[`graphics::matplot()`](https://rdrr.io/r/graphics/matplot.html)).

``` r
plot(gators.nested, x.var = "length", 
     legend.bty = "o")
```

![Predicted food choice probabilities for alligators by
length.](fig/gators-plot-1.png)

Predicted food choice probabilities for alligators by length.

## Limitations

The `ggeffects` package computes and plots predicted probabilities for
the *response categories* of a nested logit model. As shown above, these
can be displayed on the logit scale using `ggplot2`’s built-in axis
transformation.

However, `ggeffects` does not currently provide access to the individual
dichotomy sub-models that comprise the nested logit — for example,
plotting predicted values for the `work` and `full` dichotomies
separately.

For these more specialized displays, see
[`vignette("plotting-ggplot", package = "nestedLogit")`](https://friendly.github.io/nestedLogit/articles/plotting-ggplot.md),
which describes a manual workflow using
[`predict()`](https://rdrr.io/r/stats/predict.html) with
`model = "dichotomies"` and
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) to
construct fully customized `ggplot2` plots.

## References
