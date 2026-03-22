#' ---
#' title: test using equatiomatic
#' ---

# Goal: A  `equatiomatic::extract_eq()` method for nestedLogit models.
# The docs say:  "Supports any model where there is a `broom::tidy()` method".
# So, it should work to give the equations for a nested Logit object, but that doesn't work in this example-- it gives erroneous output.
# For comparison, in the `nestedLogits.Rmd` vignette, I display symbolic equations for Womenlf data in lines 244::255
#
# Also, and these extensions might require a new frontend to `extract_eq()`, perhaps `as_eqn()`, a new generic, with all of the equatiomatic methods,
# plus some more.
#   * would like this to present equations either using symbols or values for the coefficients.
#   * would like to be able to view these equations as rendered from LaTeX in the Viewer pane. `matlib::Eqn()` and other tools probably handle this.
#

library(nestedLogit)
library(equatiomatic)
data(Womenlf, package = "carData")

# Use `logits()` and `dichotomy()` to specify the comparisons of interest
comparisons <- logits(work=dichotomy("not.work",
                                     working=c("parttime", "fulltime")),
                      full=dichotomy("parttime", "fulltime"))

wlf.nested <- nestedLogit(partic ~ hincome + children,
                 dichotomies = comparisons,
                 data=Womenlf)
coef(wlf.nested)

# broom::tidy works !
broom::tidy(m)

# # A tibble: 6 × 6
#   response term            estimate std.error statistic      p.value
#   <chr>    <chr>              <dbl>     <dbl>     <dbl>        <dbl>
# 1 work     (Intercept)       1.34      0.384       3.48 0.000500
# 2 work     hincome          -0.0423    0.0198     -2.14 0.0324
# 3 work     childrenpresent  -1.58      0.292      -5.39 0.0000000700
# 4 full     (Intercept)       3.48      0.767       4.53 0.00000580
# 5 full     hincome          -0.107     0.0392     -2.74 0.00615
# 6 full     childrenpresent  -2.65      0.541      -4.90 0.000000957


# extract the two sub-models, for testing
mod.work <- models(wlf.nested, "work")
mod.full <- models(wlf.nested, "full")

# Extract the separate equations -- this is already handled.
eqn.work <- extract_eq(mod.work)

# gives:
# $$
# \log\left[ \frac { P( \operatorname{..y} = \operatorname{1} ) }{ 1 - P( \operatorname{..y} = \operatorname{1} ) } \right] = \alpha + \beta_{1}(\operatorname{hincome}) + \beta_{2}(\operatorname{children}_{\operatorname{present}})
# $$

eqn.full <- extract_eq(mod.full)

# What about nestedLogit models?

eqn.full <- extract_eq(mod.full)

# gives a result, but this is wrong
#
# > eqn.full
# $$
# \log\left[ \frac { P( \operatorname{..y} ) }{ 1 - P( \operatorname{..y} ) } \right] = \alpha + \beta_{1}(\operatorname{hincome}) + \beta_{2}(\operatorname{children}_{\operatorname{present}})
# $$


