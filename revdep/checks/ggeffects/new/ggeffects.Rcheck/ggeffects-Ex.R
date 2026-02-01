pkgname <- "ggeffects"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('ggeffects')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("coffee_data")
### * coffee_data

flush(stderr()); flush(stdout())

### Name: coffee_data
### Title: Sample dataset from a course about analysis of factorial designs
### Aliases: coffee_data
### Keywords: data

### ** Examples

# Attach coffee-data
data(coffee_data)



cleanEx()
nameEx("collapse_by_group")
### * collapse_by_group

flush(stderr()); flush(stdout())

### Name: collapse_by_group
### Title: Collapse raw data by random effect groups
### Aliases: collapse_by_group

### ** Examples

## Don't show: 
if (require("lme4", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(ggeffects)
data(efc, package = "ggeffects")
efc$e15relat <- as.factor(efc$e15relat)
efc$c161sex <- as.factor(efc$c161sex)
levels(efc$c161sex) <- c("male", "female")
model <- lme4::lmer(neg_c_7 ~ c161sex + (1 | e15relat), data = efc)
me <- predict_response(model, terms = "c161sex")
head(attributes(me)$rawdata)
collapse_by_group(me, model, "e15relat")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("efc")
### * efc

flush(stderr()); flush(stdout())

### Name: efc
### Title: Sample dataset from the EUROFAMCARE project
### Aliases: efc efc_test
### Keywords: data

### ** Examples

# Attach EFC-data
data(efc)

# Show structure
str(efc)

# show first rows
head(efc)



cleanEx()
nameEx("get_title")
### * get_title

flush(stderr()); flush(stdout())

### Name: get_title
### Title: Get titles and labels from data
### Aliases: get_title get_x_title get_y_title get_legend_title
###   get_legend_labels get_x_labels get_complete_df

### ** Examples

## Don't show: 
if (require("datawizard", quietly = TRUE) && require("ggplot2", quietly = TRUE) && require("effects", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(ggeffects)
library(ggplot2)
data(efc, package = "ggeffects")
efc$c172code <- datawizard::to_factor(efc$c172code)
fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

mydf <- predict_response(fit, terms = c("c12hour", "c161sex", "c172code"))

ggplot(mydf, aes(x = x, y = predicted, colour = group)) +
  stat_smooth(method = "lm") +
  facet_wrap(~facet, ncol = 2) +
  labs(
    x = get_x_title(mydf),
    y = get_y_title(mydf),
    colour = get_legend_title(mydf)
  )

# adjusted predictions, a list of data frames (one data frame per term)
eff <- ggeffect(fit)
eff
get_complete_df(eff)

# adjusted predictions for education only, and get x-axis-labels
mydat <- eff[["c172code"]]
ggplot(mydat, aes(x = x, y = predicted, group = group)) +
  stat_summary(fun = sum, geom = "line") +
  scale_x_discrete(labels = get_x_labels(mydat))
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("install_latest")
### * install_latest

flush(stderr()); flush(stdout())

### Name: install_latest
### Title: Update latest ggeffects-version from R-universe (GitHub) or CRAN
### Aliases: install_latest

### ** Examples

## Don't show: 
if (FALSE) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# install latest development-version of ggeffects from the
# r-universe repository
install_latest()
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("new_data")
### * new_data

flush(stderr()); flush(stdout())

### Name: new_data
### Title: Create a data frame from all combinations of predictor values
### Aliases: new_data data_grid

### ** Examples

## Don't show: 
if (requireNamespace("datawizard", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
data(efc, package = "ggeffects")
fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
new_data(fit, c("c12hour [meansd]", "c161sex"))

nd <- new_data(fit, c("c12hour [meansd]", "c161sex"))
pr <- predict(fit, type = "response", newdata = nd)
nd$predicted <- pr
nd

# compare to
predict_response(fit, c("c12hour [meansd]", "c161sex"))
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("plot")
### * plot

flush(stderr()); flush(stdout())

### Name: plot
### Title: Plot ggeffects-objects
### Aliases: plot plot.ggeffects theme_ggeffects ggeffects_palette
###   show_palettes

### ** Examples

## Don't show: 
if (requireNamespace("ggplot2") && requireNamespace("sjlabelled")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(sjlabelled)
data(efc)
efc$c172code <- as_label(efc$c172code)
fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

dat <- predict_response(fit, terms = "c12hour")
plot(dat)


# show color codes of specific palette
ggeffects_palette("okabe-ito")

# show all color palettes
show_palettes()
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("pool_comparisons")
### * pool_comparisons

flush(stderr()); flush(stdout())

### Name: pool_comparisons
### Title: Pool contrasts and comparisons from 'test_predictions()'
### Aliases: pool_comparisons

### ** Examples

## Don't show: 
if (require("mice")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
data("nhanes2", package = "mice")
imp <- mice::mice(nhanes2, printFlag = FALSE)
comparisons <- lapply(1:5, function(i) {
  m <- lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
  test_predictions(m, "age")
})
pool_comparisons(comparisons)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("pool_predictions")
### * pool_predictions

flush(stderr()); flush(stdout())

### Name: pool_predictions
### Title: Pool Predictions or Estimated Marginal Means
### Aliases: pool_predictions

### ** Examples

## Don't show: 
if (require("mice")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# example for multiple imputed datasets
data("nhanes2", package = "mice")
imp <- mice::mice(nhanes2, printFlag = FALSE)
predictions <- lapply(1:5, function(i) {
  m <- lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
  predict_response(m, "age")
})
pool_predictions(predictions)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("predict_response")
### * predict_response

flush(stderr()); flush(stdout())

### Name: predict_response
### Title: Adjusted predictions and estimated marginal means from
###   regression models
### Aliases: predict_response

### ** Examples

## Don't show: 
if (requireNamespace("sjlabelled") && requireNamespace("ggplot2")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(sjlabelled)
data(efc)
fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

predict_response(fit, terms = "c12hour")
predict_response(fit, terms = c("c12hour", "c172code"))
# more compact table layout for printing
out <- predict_response(fit, terms = c("c12hour", "c172code", "c161sex"))
print(out, collapse_table = TRUE)

# specified as formula
predict_response(fit, terms = ~ c12hour + c172code + c161sex)

# only range of 40 to 60 for variable 'c12hour'
predict_response(fit, terms = "c12hour [40:60]")

# terms as named list
predict_response(fit, terms = list(c12hour = 40:60))

# covariate "neg_c_7" is held constant at a value of 11.84 (its mean value).
# To use a different value, use "condition"
predict_response(fit, terms = "c12hour [40:60]", condition = c(neg_c_7 = 20))

# to plot ggeffects-objects, you can use the 'plot()'-function.
# the following examples show how to build your ggplot by hand.


# predictions for polynomial terms
data(efc)
fit <- glm(
  tot_sc_e ~ c12hour + e42dep + e17age + I(e17age^2) + I(e17age^3),
  data = efc,
  family = poisson()
)
predict_response(fit, terms = "e17age")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("pretty_range")
### * pretty_range

flush(stderr()); flush(stdout())

### Name: pretty_range
### Title: Create a pretty sequence over a range of a vector
### Aliases: pretty_range

### ** Examples

data(iris)
# pretty range for vectors with decimal points
pretty_range(iris$Petal.Length)

# pretty range for large range, increasing by 50
pretty_range(1:1000)

# increasing by 20
pretty_range(1:1000, n = 7)

# return 10 intervals
pretty_range(1:1000, length = 10)

# same result
pretty_range(1:1000, n = 2.5)

# function factory
range_n_5 <- pretty_range(n = 5)
range_n_5(1:1000)



cleanEx()
nameEx("print")
### * print

flush(stderr()); flush(stdout())

### Name: format.ggeffects
### Title: Print and format ggeffects-objects
### Aliases: format.ggeffects format.ggcomparisons print print.ggeffects
###   print_md.ggeffects print_html.ggeffects print.ggcomparisons
###   print_html.ggcomparisons print_md.ggcomparisons

### ** Examples

## Don't show: 
if (requireNamespace("datawizard", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
data(efc, package = "ggeffects")
fit <- lm(barthtot ~ c12hour + e42dep, data = efc)

# default print
predict_response(fit, "e42dep")

# surround CI values with parentheses
print(predict_response(fit, "e42dep"), ci_brackets = c("(", ")"))
# you can also use `options(ggeffects_ci_brackets = c("[", "]"))`
# to set this globally

# collapse CI columns into column with predicted values
print(predict_response(fit, "e42dep"), collapse_ci = TRUE)

# include value labels
print(predict_response(fit, "e42dep"), value_labels = TRUE)

# include variable labels in column headers
print(predict_response(fit, "e42dep"), variable_labels = TRUE)

# include value labels and variable labels
print(predict_response(fit, "e42dep"), variable_labels = TRUE, value_labels = TRUE)

data(iris)
m <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)

# default print with subgroups
predict_response(m, c("Petal.Length", "Species"))

# omit name of grouping variable in subgroup table headers
print(predict_response(m, c("Petal.Length", "Species")), group_name = FALSE)

# collapse tables into one
print(predict_response(m, c("Petal.Length", "Species")), collapse_tables = TRUE, n = 3)

# increase number of digits
print(predict_response(fit, "e42dep"), digits = 5)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("residualize_over_grid")
### * residualize_over_grid

flush(stderr()); flush(stdout())

### Name: residualize_over_grid
### Title: Compute partial residuals from a data grid
### Aliases: residualize_over_grid residualize_over_grid.data.frame
###   residualize_over_grid.ggeffects

### ** Examples

library(ggeffects)
set.seed(1234)
x <- rnorm(200)
z <- rnorm(200)
# quadratic relationship
y <- 2 * x + x^2 + 4 * z + rnorm(200)

d <- data.frame(x, y, z)
model <- lm(y ~ x + z, data = d)

pr <- predict_response(model, c("x [all]", "z"))
head(residualize_over_grid(pr, model))



cleanEx()
nameEx("test_predictions")
### * test_predictions

flush(stderr()); flush(stdout())

### Name: test_predictions
### Title: (Pairwise) comparisons between predictions (marginal effects)
### Aliases: test_predictions hypothesis_test test_predictions.default
###   test_predictions.ggeffects

### ** Examples

## Don't show: 
if (all(insight::check_if_installed(c("parameters", "emmeans"), quietly = TRUE)) && interactive()) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("values_at")
### * values_at

flush(stderr()); flush(stdout())

### Name: values_at
### Title: Calculate representative values of a vector
### Aliases: values_at representative_values

### ** Examples

data(efc)
values_at(efc$c12hour)
values_at(efc$c12hour, "quartiles2")

mean_sd <- values_at(values = "meansd")
mean_sd(efc$c12hour)



cleanEx()
nameEx("vcov")
### * vcov

flush(stderr()); flush(stdout())

### Name: vcov
### Title: Calculate variance-covariance matrix for adjusted predictions
### Aliases: vcov vcov.ggeffects

### ** Examples

data(efc)
model <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
result <- predict_response(model, c("c12hour [meansd]", "c161sex"))

vcov(result)

# compare standard errors
sqrt(diag(vcov(result)))
as.data.frame(result)

# only two predicted values, no further terms
# vcov() returns a 2x2 matrix
result <- predict_response(model, "c161sex")
vcov(result)

# 2 levels for c161sex multiplied by 3 levels for c172code
# result in 6 combinations of predicted values
# thus vcov() returns a 6x6 matrix
result <- predict_response(model, c("c161sex", "c172code"))
vcov(result)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
