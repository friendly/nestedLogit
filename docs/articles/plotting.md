# Plotting nestedLogit models

Load the packages we’ll use here:

``` r
library(nestedLogit)    # Nested Dichotomy Logistic Regression Models
library(knitr)          # A General-Purpose Package for Dynamic Report Generation in R
library(dplyr)          # A Grammar of Data Manipulation
library(tidyr)          # Tidy Messy Data
library(ggplot2)        # Create Elegant Data Visualisations Using the Grammar of Graphics
library(geomtextpath)   # Curved Text in 'ggplot2'
```

The main vignette illustrated the basic plot method,
[`plot.nestedLogit()`](https://friendly.github.io/nestedLogit/reference/plot.nestedLogit.md)
in the package. However, to explain plotting nested-dichotomies models
works, and for better control of the details, it is useful to describe
how graphs can be constructed directly directly. We’ll use the same
example of women’s labor force participation, using the original
dichotomies:

``` r
data(Womenlf, package = "carData")
Womenlf$partic <- with(Womenlf,
                       factor(partic, levels = c("not.work", "parttime", "fulltime")))

wlf.nested <- nestedLogit(partic ~ hincome + children,
                          dichotomies = logits(work=dichotomy("not.work", working=c("parttime", "fulltime")),
                                               full=dichotomy("parttime", "fulltime")),
                          data=Womenlf)
```

## Fitted probabilities

To draw a plot, it is sufficient to calculate predicted probabilities
over a grid of values of the predictor variables. Here, we select a
range of 0 - 45 in steps of 5, combined with the two values of
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

### Using `matplot()`

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

``` r
par(op)
```

### Using `ggplot()`

More control, and perhaps a more aesthetically pleasing figure can be
produced using `ggplot` (Wickham et al., 2023). However, `ggplot` wants
the data in long format. That makes it easy to plot probability against
one predictor and use `color` to distinguish the levels of `partic` and
facet the plot by `children`. (The result of
[`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
doesn’t recognize an ordered nature of `"Working"`, so this is done in a
separate step.)

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
legend. The `geomtextpath` (**R-geomtextpath?**) package offers
[`geom_textline()`](https://allancameron.github.io/geomtextpath/reference/geom_textpath.html)
as an alternative to
[`geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
that adds a text label to a curve. `hjust` and `vjust` position the
labels along the curve.

``` r
ggplot(plotlong,
       aes(x=hincome, y=Probability, color=Working)) +
  geom_textline(aes(label = Working),
                linewidth = 2, size = 5, 
                hjust = 0.9, vjust = 0.2) +
  scale_color_discrete() +
  labs(x = "Husband's Income", y = "Probability") +
  facet_wrap(~ children, labeller = label_both) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")
```

![\*\*geomtextpath\*\*: Predicted probabilities, with labels on the
curves](fig/wlf-geomtextpath-1.png)

**geomtextpath**: Predicted probabilities, with labels on the curves

## Plotting fitted log odds

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
cols <- c("blue", "red")

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
dichotomies, by \`children\`](fig/wlf-logits-1.png)

**log odds**: Predicted logits of the `work` and `full` dichotomies, by
`children`

``` r
par(op)
```

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
dichotomies, by \`Dichotomy\`](fig/wlf-gglogits-1.png)

**log odds**: Predicted logits of the `work` and `full` dichotomies, by
`Dichotomy`

## References

Wickham, H., Chang, W., Henry, L., Pedersen, T. L., Takahashi, K.,
Wilke, C., … Dunnington, D. (2023). *ggplot2: Create elegant data
visualisations using the grammar of graphics*. Retrieved from
<https://CRAN.R-project.org/package=ggplot2>
