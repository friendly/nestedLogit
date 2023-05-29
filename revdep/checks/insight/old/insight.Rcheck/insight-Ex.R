pkgname <- "insight"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('insight')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("all_models_equal")
### * all_models_equal

flush(stderr()); flush(stdout())

### Name: all_models_equal
### Title: Checks if all objects are models of same class
### Aliases: all_models_equal all_models_same_class

### ** Examples

if (require("lme4")) {
  data(mtcars)
  data(sleepstudy)

  m1 <- lm(mpg ~ wt + cyl + vs, data = mtcars)
  m2 <- lm(mpg ~ wt + cyl, data = mtcars)
  m3 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  m4 <- glm(formula = vs ~ wt, family = binomial(), data = mtcars)

  all_models_same_class(m1, m2)
  all_models_same_class(m1, m2, m3)
  all_models_same_class(m1, m4, m2, m3, verbose = TRUE)
  all_models_same_class(m1, m4, mtcars, m2, m3, verbose = TRUE)
}



cleanEx()
nameEx("check_if_installed")
### * check_if_installed

flush(stderr()); flush(stdout())

### Name: check_if_installed
### Title: Checking if needed package is installed
### Aliases: check_if_installed

### ** Examples

## Don't show: 
if (interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Not run: 
##D check_if_installed("insight")
##D try(check_if_installed("nonexistent_package"))
##D try(check_if_installed("insight", minimum_version = "99.8.7"))
##D try(check_if_installed(c("nonexistent", "also_not_here"), stop = FALSE))
## End(Not run)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("clean_names")
### * clean_names

flush(stderr()); flush(stdout())

### Name: clean_names
### Title: Get clean names of model terms
### Aliases: clean_names clean_names.character

### ** Examples

# example from ?stats::glm
counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
outcome <- as.numeric(gl(3, 1, 9))
treatment <- gl(3, 3)
m <- glm(counts ~ log(outcome) + as.factor(treatment), family = poisson())
clean_names(m)

# difference "clean_names()" and "find_variables()"
if (require("lme4")) {
  m <- glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = cbpp,
    family = binomial
  )

  clean_names(m)
  find_variables(m)
  find_variables(m, flatten = TRUE)
}



cleanEx()
nameEx("clean_parameters")
### * clean_parameters

flush(stderr()); flush(stdout())

### Name: clean_parameters
### Title: Get clean names of model parameters
### Aliases: clean_parameters

### ** Examples

## Don't show: 
if (require("curl", quietly = TRUE) && curl::has_internet()) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Not run: 
##D library(brms)
##D model <- download_model("brms_zi_2")
##D clean_parameters(model)
## End(Not run)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("color_if")
### * color_if

flush(stderr()); flush(stdout())

### Name: color_if
### Title: Color-formatting for data columns based on condition
### Aliases: color_if colour_if

### ** Examples

# all values in Sepal.Length larger than 5 in green, all remaining in red
x <- color_if(iris[1:10, ], columns = "Sepal.Length", predicate = `>`, value = 5)
x
cat(x$Sepal.Length)

# all levels "setosa" in Species in green, all remaining in red
x <- color_if(iris, columns = "Species", predicate = `==`, value = "setosa")
cat(x$Species)

# own function, argument "value" not needed here
p <- function(x, y) {
  x >= 4.9 & x <= 5.1
}
# all values in Sepal.Length between 4.9 and 5.1 in green, all remaining in red
x <- color_if(iris[1:10, ], columns = "Sepal.Length", predicate = p)
cat(x$Sepal.Length)



cleanEx()
nameEx("compact_character")
### * compact_character

flush(stderr()); flush(stdout())

### Name: compact_character
### Title: Remove empty strings from character
### Aliases: compact_character

### ** Examples

compact_character(c("x", "y", NA))
compact_character(c("x", "NULL", "", "y"))




cleanEx()
nameEx("compact_list")
### * compact_list

flush(stderr()); flush(stdout())

### Name: compact_list
### Title: Remove empty elements from lists
### Aliases: compact_list

### ** Examples

compact_list(list(NULL, 1, c(NA, NA)))
compact_list(c(1, NA, NA))
compact_list(c(1, NA, NA), remove_na = TRUE)



cleanEx()
nameEx("display")
### * display

flush(stderr()); flush(stdout())

### Name: display
### Title: Generic export of data frames into formatted tables
### Aliases: display print_md print_html display.data.frame
###   print_md.data.frame print_html.data.frame

### ** Examples

display(iris[1:5, ])



cleanEx()
nameEx("ellipsis_info")
### * ellipsis_info

flush(stderr()); flush(stdout())

### Name: ellipsis_info
### Title: Gather information about objects in ellipsis (dot dot dot)
### Aliases: ellipsis_info ellipsis_info.default

### ** Examples

m1 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
m2 <- lm(Sepal.Length ~ Species, data = iris)
m3 <- lm(Sepal.Length ~ Petal.Width, data = iris)
m4 <- lm(Sepal.Length ~ 1, data = iris)
m5 <- lm(Petal.Width ~ 1, data = iris)

objects <- ellipsis_info(m1, m2, m3, m4)
class(objects)

objects <- ellipsis_info(m1, m2, m4)
attributes(objects)$is_nested

objects <- ellipsis_info(m1, m2, m5)
attributes(objects)$same_response



cleanEx()
nameEx("export_table")
### * export_table

flush(stderr()); flush(stdout())

### Name: export_table
### Title: Data frame and Tables Pretty Formatting
### Aliases: export_table

### ** Examples

export_table(head(iris))
export_table(head(iris), cross = "+")
export_table(head(iris), sep = " ", header = "*", digits = 1)

# split longer tables
export_table(head(iris), table_width = 30)

## Not run: 
##D # colored footers
##D data(iris)
##D x <- as.data.frame(iris[1:5, ])
##D attr(x, "table_footer") <- c("This is a yellow footer line.", "yellow")
##D export_table(x)
##D 
##D attr(x, "table_footer") <- list(
##D   c("\nA yellow line", "yellow"),
##D   c("\nAnd a red line", "red"),
##D   c("\nAnd a blue line", "blue")
##D )
##D export_table(x)
##D 
##D attr(x, "table_footer") <- list(
##D   c("Without the ", "yellow"),
##D   c("new-line character ", "red"),
##D   c("we can have multiple colors per line.", "blue")
##D )
##D export_table(x)
## End(Not run)

# column-width
d <- data.frame(
  x = c(1, 2, 3),
  y = c(100, 200, 300),
  z = c(10000, 20000, 30000)
)
export_table(d)
export_table(d, width = 8)
export_table(d, width = c(x = 5, z = 10))
export_table(d, width = c(x = 5, y = 5, z = 10), align = "lcr")



cleanEx()
nameEx("find_algorithm")
### * find_algorithm

flush(stderr()); flush(stdout())

### Name: find_algorithm
### Title: Find sampling algorithm and optimizers
### Aliases: find_algorithm

### ** Examples

if (require("lme4")) {
  data(sleepstudy)
  m <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  find_algorithm(m)
}
## Not run: 
##D library(rstanarm)
##D m <- stan_lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
##D find_algorithm(m)
## End(Not run)



cleanEx()
nameEx("find_formula")
### * find_formula

flush(stderr()); flush(stdout())

### Name: find_formula
### Title: Find model formula
### Aliases: find_formula formula_ok

### ** Examples

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
find_formula(m)

if (require("lme4")) {
  m <- lmer(Sepal.Length ~ Sepal.Width + (1 | Species), data = iris)
  f <- find_formula(m)
  f
  format(f)
}



cleanEx()
nameEx("find_interactions")
### * find_interactions

flush(stderr()); flush(stdout())

### Name: find_interactions
### Title: Find interaction terms from models
### Aliases: find_interactions

### ** Examples

data(mtcars)

m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
find_interactions(m)

m <- lm(mpg ~ wt * cyl + vs * hp * gear + carb, data = mtcars)
find_interactions(m)



cleanEx()
nameEx("find_offset")
### * find_offset

flush(stderr()); flush(stdout())

### Name: find_offset
### Title: Find possible offset terms in a model
### Aliases: find_offset

### ** Examples

# Generate some zero-inflated data
set.seed(123)
N <- 100 # Samples
x <- runif(N, 0, 10) # Predictor
off <- rgamma(N, 3, 2) # Offset variable
yhat <- -1 + x * 0.5 + log(off) # Prediction on log scale
dat <- data.frame(y = NA, x, logOff = log(off))
dat$y <- rpois(N, exp(yhat)) # Poisson process
dat$y <- ifelse(rbinom(N, 1, 0.3), 0, dat$y) # Zero-inflation process

if (require("pscl")) {
  m1 <- zeroinfl(y ~ offset(logOff) + x | 1, data = dat, dist = "poisson")
  find_offset(m1)

  m2 <- zeroinfl(y ~ x | 1, data = dat, offset = logOff, dist = "poisson")
  find_offset(m2)
}



cleanEx()
nameEx("find_parameters.BGGM")
### * find_parameters.BGGM

flush(stderr()); flush(stdout())

### Name: find_parameters.BGGM
### Title: Find names of model parameters from Bayesian models
### Aliases: find_parameters.BGGM find_parameters.BFBayesFactor
###   find_parameters.MCMCglmm find_parameters.bamlss
###   find_parameters.brmsfit find_parameters.bayesx
###   find_parameters.stanreg find_parameters.sim.merMod

### ** Examples

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
find_parameters(m)



cleanEx()
nameEx("find_parameters")
### * find_parameters

flush(stderr()); flush(stdout())

### Name: find_parameters
### Title: Find names of model parameters
### Aliases: find_parameters find_parameters.default

### ** Examples

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
find_parameters(m)



cleanEx()
nameEx("find_parameters.averaging")
### * find_parameters.averaging

flush(stderr()); flush(stdout())

### Name: find_parameters.averaging
### Title: Find model parameters from models with special components
### Aliases: find_parameters.averaging find_parameters.betareg
###   find_parameters.DirichletRegModel find_parameters.mjoint
###   find_parameters.glmx

### ** Examples

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
find_parameters(m)



cleanEx()
nameEx("find_parameters.betamfx")
### * find_parameters.betamfx

flush(stderr()); flush(stdout())

### Name: find_parameters.betamfx
### Title: Find names of model parameters from marginal effects models
### Aliases: find_parameters.betamfx find_parameters.logitmfx

### ** Examples

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
find_parameters(m)



cleanEx()
nameEx("find_parameters.emmGrid")
### * find_parameters.emmGrid

flush(stderr()); flush(stdout())

### Name: find_parameters.emmGrid
### Title: Find model parameters from estimated marginal means objects
### Aliases: find_parameters.emmGrid

### ** Examples

data(mtcars)
model <- lm(mpg ~ wt * factor(cyl), data = mtcars)
if (require("emmeans", quietly = TRUE)) {
  emm <- emmeans(model, c("wt", "cyl"))
  find_parameters(emm)
}



cleanEx()
nameEx("find_parameters.gamlss")
### * find_parameters.gamlss

flush(stderr()); flush(stdout())

### Name: find_parameters.gamlss
### Title: Find names of model parameters from generalized additive models
### Aliases: find_parameters.gamlss find_parameters.gam

### ** Examples

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
find_parameters(m)



cleanEx()
nameEx("find_parameters.glmmTMB")
### * find_parameters.glmmTMB

flush(stderr()); flush(stdout())

### Name: find_parameters.glmmTMB
### Title: Find names of model parameters from mixed models
### Aliases: find_parameters.glmmTMB find_parameters.nlmerMod
###   find_parameters.merMod

### ** Examples

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
find_parameters(m)



cleanEx()
nameEx("find_parameters.zeroinfl")
### * find_parameters.zeroinfl

flush(stderr()); flush(stdout())

### Name: find_parameters.zeroinfl
### Title: Find names of model parameters from zero-inflated models
### Aliases: find_parameters.zeroinfl find_parameters.mhurdle

### ** Examples

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
find_parameters(m)



cleanEx()
nameEx("find_predictors")
### * find_predictors

flush(stderr()); flush(stdout())

### Name: find_predictors
### Title: Find names of model predictors
### Aliases: find_predictors find_predictors.default

### ** Examples

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
find_predictors(m)



cleanEx()
nameEx("find_random")
### * find_random

flush(stderr()); flush(stdout())

### Name: find_random
### Title: Find names of random effects
### Aliases: find_random

### ** Examples

if (require("lme4")) {
  data(sleepstudy)
  sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
  sleepstudy$mysubgrp <- NA
  for (i in 1:5) {
    filter_group <- sleepstudy$mygrp == i
    sleepstudy$mysubgrp[filter_group] <-
      sample(1:30, size = sum(filter_group), replace = TRUE)
  }

  m <- lmer(
    Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject),
    data = sleepstudy
  )

  find_random(m)
  find_random(m, split_nested = TRUE)
}



cleanEx()
nameEx("find_random_slopes")
### * find_random_slopes

flush(stderr()); flush(stdout())

### Name: find_random_slopes
### Title: Find names of random slopes
### Aliases: find_random_slopes

### ** Examples

if (require("lme4")) {
  data(sleepstudy)
  m <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
  find_random_slopes(m)
}



cleanEx()
nameEx("find_response")
### * find_response

flush(stderr()); flush(stdout())

### Name: find_response
### Title: Find name of the response variable
### Aliases: find_response

### ** Examples

if (require("lme4")) {
  data(cbpp)
  cbpp$trials <- cbpp$size - cbpp$incidence
  m <- glm(cbind(incidence, trials) ~ period, data = cbpp, family = binomial)

  find_response(m, combine = TRUE)
  find_response(m, combine = FALSE)
}



cleanEx()
nameEx("find_smooth")
### * find_smooth

flush(stderr()); flush(stdout())

### Name: find_smooth
### Title: Find smooth terms from a model object
### Aliases: find_smooth

### ** Examples

if (require("mgcv")) {
  data(iris)
  model <- gam(Petal.Length ~ Petal.Width + s(Sepal.Length), data = iris)
  find_smooth(model)
}



cleanEx()
nameEx("find_statistic")
### * find_statistic

flush(stderr()); flush(stdout())

### Name: find_statistic
### Title: Find statistic for model
### Aliases: find_statistic

### ** Examples

# regression model object
data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
find_statistic(m)



cleanEx()
nameEx("find_terms")
### * find_terms

flush(stderr()); flush(stdout())

### Name: find_terms
### Title: Find all model terms
### Aliases: find_terms

### ** Examples

if (require("lme4")) {
  data(sleepstudy)
  m <- suppressWarnings(lmer(
    log(Reaction) ~ Days + I(Days^2) + (1 + Days + exp(Days) | Subject),
    data = sleepstudy
  ))

  find_terms(m)
}



cleanEx()
nameEx("find_transformation")
### * find_transformation

flush(stderr()); flush(stdout())

### Name: find_transformation
### Title: Find possible transformation of response variables
### Aliases: find_transformation

### ** Examples

# identity, no transformation
model <- lm(Sepal.Length ~ Species, data = iris)
find_transformation(model)

# log-transformation
model <- lm(log(Sepal.Length) ~ Species, data = iris)
find_transformation(model)

# log+2
model <- lm(log(Sepal.Length + 2) ~ Species, data = iris)
find_transformation(model)



cleanEx()
nameEx("find_variables")
### * find_variables

flush(stderr()); flush(stdout())

### Name: find_variables
### Title: Find names of all variables
### Aliases: find_variables

### ** Examples

if (require("lme4")) {
  data(cbpp)
  data(sleepstudy)
  # some data preparation...
  cbpp$trials <- cbpp$size - cbpp$incidence
  sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
  sleepstudy$mysubgrp <- NA
  for (i in 1:5) {
    filter_group <- sleepstudy$mygrp == i
    sleepstudy$mysubgrp[filter_group] <-
      sample(1:30, size = sum(filter_group), replace = TRUE)
  }

  m1 <- glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = cbpp,
    family = binomial
  )
  find_variables(m1)

  m2 <- lmer(
    Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject),
    data = sleepstudy
  )
  find_variables(m2)
  find_variables(m2, flatten = TRUE)
}



cleanEx()
nameEx("find_weights")
### * find_weights

flush(stderr()); flush(stdout())

### Name: find_weights
### Title: Find names of model weights
### Aliases: find_weights

### ** Examples

data(mtcars)
mtcars$weight <- rnorm(nrow(mtcars), 1, .3)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars, weights = weight)
find_weights(m)



cleanEx()
nameEx("format_bf")
### * format_bf

flush(stderr()); flush(stdout())

### Name: format_bf
### Title: Bayes Factor formatting
### Aliases: format_bf

### ** Examples

format_bf(bfs <- c(0.000045, 0.033, NA, 1557, 3.54))
format_bf(bfs, exact = TRUE, name = NULL)
format_bf(bfs, stars = TRUE)
format_bf(bfs, protect_ratio = TRUE)
format_bf(bfs, protect_ratio = TRUE, exact = TRUE)
format_bf(bfs, na_reference = 1)



cleanEx()
nameEx("format_capitalize")
### * format_capitalize

flush(stderr()); flush(stdout())

### Name: format_capitalize
### Title: Capitalizes the first letter in a string
### Aliases: format_capitalize

### ** Examples

format_capitalize("hello")
format_capitalize(c("hello", "world"))
unique(format_capitalize(iris$Species))



cleanEx()
nameEx("format_ci")
### * format_ci

flush(stderr()); flush(stdout())

### Name: format_ci
### Title: Confidence/Credible Interval (CI) Formatting
### Aliases: format_ci

### ** Examples

format_ci(1.20, 3.57, ci = 0.90)
format_ci(1.20, 3.57, ci = NULL)
format_ci(1.20, 3.57, ci = NULL, brackets = FALSE)
format_ci(1.20, 3.57, ci = NULL, brackets = c("(", ")"))
format_ci(c(1.205645, 23.4), c(3.57, -1.35), ci = 0.90)
format_ci(c(1.20, NA, NA), c(3.57, -1.35, NA), ci = 0.90)

# automatic alignment of width, useful for printing multiple CIs in columns
x <- format_ci(c(1.205, 23.4, 100.43), c(3.57, -13.35, 9.4))
cat(x, sep = "\n")

x <- format_ci(c(1.205, 23.4, 100.43), c(3.57, -13.35, 9.4), width = "auto")
cat(x, sep = "\n")



cleanEx()
nameEx("format_message")
### * format_message

flush(stderr()); flush(stdout())

### Name: format_message
### Title: Format messages and warnings
### Aliases: format_message format_alert format_warning format_error

### ** Examples

msg <- format_message("Much too long string for just one line, I guess!",
  line_length = 15
)
message(msg)

msg <- format_message("Much too long string for just one line, I guess!",
  "First new line",
  "Second new line",
  "(both indented)",
  line_length = 30
)
message(msg)

msg <- format_message("Much too long string for just one line, I guess!",
  "First new line",
  "Second new line",
  "(not indented)",
  line_length = 30,
  indent = ""
)
message(msg)

# Caution, experimental! See 'Details'
msg <- format_message(
  "This is {.i italic}, visit {.url easystats.github.io/easystats}",
  line_length = 30
)
message(msg)

## Don't show: 
if (identical(Sys.getenv("NOT_CRAN"), "true")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# message
format_alert("This is a message.")
format_alert("This is a warning.", type = "message")

# error
try(format_error("This is an error."))
## Don't show: 
}) # examplesIf
## End(Don't show)
## Don't show: 
if (getOption("warn") < 2L) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# warning
format_warning("This is a warning.")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("format_number")
### * format_number

flush(stderr()); flush(stdout())

### Name: format_number
### Title: Convert number to words
### Aliases: format_number

### ** Examples

format_number(2)
format_number(45)
format_number(324.68765)



cleanEx()
nameEx("format_p")
### * format_p

flush(stderr()); flush(stdout())

### Name: format_p
### Title: p-values formatting
### Aliases: format_p

### ** Examples

format_p(c(.02, .065, 0, .23))
format_p(c(.02, .065, 0, .23), name = NULL)
format_p(c(.02, .065, 0, .23), stars_only = TRUE)

model <- lm(mpg ~ wt + cyl, data = mtcars)
p <- coef(summary(model))[, 4]
format_p(p, digits = "apa")
format_p(p, digits = "scientific")
format_p(p, digits = "scientific2")



cleanEx()
nameEx("format_pd")
### * format_pd

flush(stderr()); flush(stdout())

### Name: format_pd
### Title: Probability of direction (pd) formatting
### Aliases: format_pd

### ** Examples

format_pd(0.12)
format_pd(c(0.12, 1, 0.9999, 0.98, 0.995, 0.96), name = NULL)
format_pd(c(0.12, 1, 0.9999, 0.98, 0.995, 0.96), stars = TRUE)



cleanEx()
nameEx("format_rope")
### * format_rope

flush(stderr()); flush(stdout())

### Name: format_rope
### Title: Percentage in ROPE formatting
### Aliases: format_rope

### ** Examples

format_rope(c(0.02, 0.12, 0.357, 0))
format_rope(c(0.02, 0.12, 0.357, 0), name = NULL)



cleanEx()
nameEx("format_string")
### * format_string

flush(stderr()); flush(stdout())

### Name: format_string
### Title: String Values Formatting
### Aliases: format_string format_string.character

### ** Examples

s <- "This can be considered as very long string!"
# string is shorter than max.length, so returned as is
format_string(s, 60)

# string is shortened to as many words that result in
# a string of maximum 20 chars
format_string(s, 20)



cleanEx()
nameEx("format_table")
### * format_table

flush(stderr()); flush(stdout())

### Name: format_table
### Title: Parameter table formatting
### Aliases: format_table

### ** Examples

format_table(head(iris), digits = 1)

if (require("parameters")) {
  x <- model_parameters(lm(Sepal.Length ~ Species * Sepal.Width, data = iris))
  as.data.frame(format_table(x))
  as.data.frame(format_table(x, p_digits = "scientific"))
}



cleanEx()
nameEx("format_value")
### * format_value

flush(stderr()); flush(stdout())

### Name: format_value
### Title: Numeric Values Formatting
### Aliases: format_value format_value.data.frame format_value.numeric
###   format_percent

### ** Examples

format_value(1.20)
format_value(1.2)
format_value(1.2012313)
format_value(c(0.0045, 234, -23))
format_value(c(0.0045, .12, .34))
format_value(c(0.0045, .12, .34), as_percent = TRUE)
format_value(c(0.0045, .12, .34), digits = "scientific")
format_value(c(0.0045, .12, .34), digits = "scientific2")
format_value(c(0.045, .12, .34), lead_zero = FALSE)

# default
format_value(c(0.0045, .123, .345))
# significant figures
format_value(c(0.0045, .123, .345), digits = "signif")

format_value(as.factor(c("A", "B", "A")))
format_value(iris$Species)

format_value(3)
format_value(3, protect_integers = TRUE)

format_value(head(iris))



cleanEx()
nameEx("get_auxiliary")
### * get_auxiliary

flush(stderr()); flush(stdout())

### Name: get_auxiliary
### Title: Get auxiliary parameters from models
### Aliases: get_auxiliary

### ** Examples

# from ?glm
clotting <- data.frame(
  u = c(5, 10, 15, 20, 30, 40, 60, 80, 100),
  lot1 = c(118, 58, 42, 35, 27, 25, 21, 19, 18),
  lot2 = c(69, 35, 26, 21, 18, 16, 13, 12, 12)
)
model <- glm(lot1 ~ log(u), data = clotting, family = Gamma())
get_auxiliary(model, type = "dispersion") # same as summary(model)$dispersion



cleanEx()
nameEx("get_call")
### * get_call

flush(stderr()); flush(stdout())

### Name: get_call
### Title: Get the model's function call
### Aliases: get_call

### ** Examples

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
get_call(m)

if (require("lme4")) {
  m <- lmer(Sepal.Length ~ Sepal.Width + (1 | Species), data = iris)
  get_call(m)
}



cleanEx()
nameEx("get_data")
### * get_data

flush(stderr()); flush(stdout())

### Name: get_data
### Title: Get the data that was used to fit the model
### Aliases: get_data get_data.default get_data.glmmTMB get_data.afex_aov
###   get_data.rma

### ** Examples

if (require("lme4")) {
  data(cbpp, package = "lme4")
  cbpp$trials <- cbpp$size - cbpp$incidence
  m <- glm(cbind(incidence, trials) ~ period, data = cbpp, family = binomial)
  head(get_data(m))
}



cleanEx()
nameEx("get_datagrid")
### * get_datagrid

flush(stderr()); flush(stdout())

### Name: get_datagrid
### Title: Create a reference grid
### Aliases: get_datagrid get_datagrid.data.frame get_datagrid.numeric
###   get_datagrid.factor get_datagrid.default

### ** Examples

# Datagrids of variables and dataframes =====================================
if (require("bayestestR", quietly = TRUE) & require("datawizard", quietly = TRUE)) {
  # Single variable is of interest; all others are "fixed" ------------------
  # Factors
  get_datagrid(iris, at = "Species") # Returns all the levels
  get_datagrid(iris, at = "Species = c('setosa', 'versicolor')") # Specify an expression

  # Numeric variables
  get_datagrid(iris, at = "Sepal.Length") # default spread length = 10
  get_datagrid(iris, at = "Sepal.Length", length = 3) # change length
  get_datagrid(iris[2:150, ],
    at = "Sepal.Length",
    factors = "mode", numerics = "median"
  ) # change non-targets fixing
  get_datagrid(iris, at = "Sepal.Length", range = "ci", ci = 0.90) # change min/max of target
  get_datagrid(iris, at = "Sepal.Length = [0, 1]") # Manually change min/max
  get_datagrid(iris, at = "Sepal.Length = [sd]") # -1 SD, mean and +1 SD
  # identical to previous line: -1 SD, mean and +1 SD
  get_datagrid(iris, at = "Sepal.Length", range = "sd", length = 3)
  get_datagrid(iris, at = "Sepal.Length = [quartiles]") # quartiles

  # Numeric and categorical variables, generating a grid for plots
  # default spread length = 10
  get_datagrid(iris, at = c("Sepal.Length", "Species"), range = "grid")
  # default spread length = 3 (-1 SD, mean and +1 SD)
  get_datagrid(iris, at = c("Species", "Sepal.Length"), range = "grid")


  # Standardization and unstandardization
  data <- get_datagrid(iris, at = "Sepal.Length", range = "sd", length = 3)
  data$Sepal.Length # It is a named vector (extract names with `names(out$Sepal.Length)`)
  datawizard::standardize(data, select = "Sepal.Length")
  data <- get_datagrid(iris, at = "Sepal.Length = c(-2, 0, 2)") # Manually specify values
  data
  datawizard::unstandardize(data, select = "Sepal.Length")


  # Multiple variables are of interest, creating a combination --------------
  get_datagrid(iris, at = c("Sepal.Length", "Species"), length = 3)
  get_datagrid(iris, at = c("Sepal.Length", "Petal.Length"), length = c(3, 2))
  get_datagrid(iris, at = c(1, 3), length = 3)
  get_datagrid(iris, at = c("Sepal.Length", "Species"), preserve_range = TRUE)
  get_datagrid(iris, at = c("Sepal.Length", "Species"), numerics = 0)
  get_datagrid(iris, at = c("Sepal.Length = 3", "Species"))
  get_datagrid(iris, at = c("Sepal.Length = c(3, 1)", "Species = 'setosa'"))

  # With list-style at-argument
  get_datagrid(iris, at = list(Sepal.Length = c(1, 3), Species = "setosa"))
}

# With models ===============================================================
# Fit a linear regression
model <- lm(Sepal.Length ~ Sepal.Width * Petal.Length, data = iris)
# Get datagrid of predictors
data <- get_datagrid(model, length = c(20, 3), range = c("range", "sd"))
# same as: get_datagrid(model, range = "grid", length = 20)
# Add predictions
data$Sepal.Length <- get_predicted(model, data = data)
# Visualize relationships (each color is at -1 SD, Mean, and + 1 SD of Petal.Length)
plot(data$Sepal.Width, data$Sepal.Length,
  col = data$Petal.Length,
  main = "Relationship at -1 SD, Mean, and + 1 SD of Petal.Length"
)



cleanEx()
nameEx("get_deviance")
### * get_deviance

flush(stderr()); flush(stdout())

### Name: get_deviance
### Title: Model Deviance
### Aliases: get_deviance get_deviance.default

### ** Examples

data(mtcars)
x <- lm(mpg ~ cyl, data = mtcars)
get_deviance(x)



cleanEx()
nameEx("get_df")
### * get_df

flush(stderr()); flush(stdout())

### Name: get_df
### Title: Extract degrees of freedom
### Aliases: get_df get_df.default

### ** Examples

model <- lm(Sepal.Length ~ Petal.Length * Species, data = iris)
get_df(model) # same as df.residual(model)
get_df(model, type = "model") # same as attr(logLik(model), "df")



cleanEx()
nameEx("get_family")
### * get_family

flush(stderr()); flush(stdout())

### Name: get_family
### Title: A robust alternative to stats::family
### Aliases: get_family

### ** Examples

data(mtcars)
x <- glm(vs ~ wt, data = mtcars, family = "binomial")
get_family(x)

if (require("mgcv")) {
  x <- mgcv::gamm(
    vs ~ am + s(wt),
    random = list(cyl = ~1),
    data = mtcars,
    family = "binomial"
  )
  get_family(x)
}



cleanEx()
nameEx("get_intercept")
### * get_intercept

flush(stderr()); flush(stdout())

### Name: get_intercept
### Title: Get the value at the intercept
### Aliases: get_intercept

### ** Examples

get_intercept(lm(Sepal.Length ~ Petal.Width, data = iris))
get_intercept(lm(Sepal.Length ~ 0 + Petal.Width, data = iris))

if (require("lme4")) {
  get_intercept(lme4::lmer(Sepal.Length ~ Sepal.Width + (1 | Species), data = iris))
}
if (require("gamm4")) {
  get_intercept(gamm4::gamm4(Sepal.Length ~ s(Petal.Width), data = iris))
}



cleanEx()
nameEx("get_loglikelihood")
### * get_loglikelihood

flush(stderr()); flush(stdout())

### Name: get_loglikelihood
### Title: Log-Likelihood
### Aliases: get_loglikelihood loglikelihood get_loglikelihood.lm

### ** Examples

x <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)

get_loglikelihood(x, estimator = "ML") # Equivalent to stats::logLik(x)
get_loglikelihood(x, estimator = "REML") # Equivalent to stats::logLik(x, REML=TRUE)
get_loglikelihood(x, estimator = "OLS")



cleanEx()
nameEx("get_modelmatrix")
### * get_modelmatrix

flush(stderr()); flush(stdout())

### Name: get_modelmatrix
### Title: Model Matrix
### Aliases: get_modelmatrix

### ** Examples

data(mtcars)

model <- lm(am ~ vs, data = mtcars)
get_modelmatrix(model)



cleanEx()
nameEx("get_parameters.BGGM")
### * get_parameters.BGGM

flush(stderr()); flush(stdout())

### Name: get_parameters.BGGM
### Title: Get model parameters from Bayesian models
### Aliases: get_parameters.BGGM get_parameters.MCMCglmm
###   get_parameters.BFBayesFactor get_parameters.stanmvreg
###   get_parameters.brmsfit get_parameters.stanreg get_parameters.bayesx
###   get_parameters.bamlss get_parameters.sim.merMod get_parameters.sim

### ** Examples

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
get_parameters(m)



cleanEx()
nameEx("get_parameters")
### * get_parameters

flush(stderr()); flush(stdout())

### Name: get_parameters
### Title: Get model parameters
### Aliases: get_parameters get_parameters.default

### ** Examples

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
get_parameters(m)



cleanEx()
nameEx("get_parameters.betamfx")
### * get_parameters.betamfx

flush(stderr()); flush(stdout())

### Name: get_parameters.betamfx
### Title: Get model parameters from marginal effects models
### Aliases: get_parameters.betamfx get_parameters.logitmfx

### ** Examples

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
get_parameters(m)



cleanEx()
nameEx("get_parameters.betareg")
### * get_parameters.betareg

flush(stderr()); flush(stdout())

### Name: get_parameters.betareg
### Title: Get model parameters from models with special components
### Aliases: get_parameters.betareg get_parameters.DirichletRegModel
###   get_parameters.averaging get_parameters.glmx get_parameters.clm2
###   get_parameters.mvord get_parameters.mjoint

### ** Examples

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
get_parameters(m)



cleanEx()
nameEx("get_parameters.emmGrid")
### * get_parameters.emmGrid

flush(stderr()); flush(stdout())

### Name: get_parameters.emmGrid
### Title: Get model parameters from estimated marginal means objects
### Aliases: get_parameters.emmGrid get_parameters.emm_list

### ** Examples

data(mtcars)
model <- lm(mpg ~ wt * factor(cyl), data = mtcars)
if (require("emmeans", quietly = TRUE)) {
  emm <- emmeans(model, "cyl")
  get_parameters(emm)

  emm <- emmeans(model, pairwise ~ cyl)
  get_parameters(emm)
}



cleanEx()
nameEx("get_parameters.gamm")
### * get_parameters.gamm

flush(stderr()); flush(stdout())

### Name: get_parameters.gamm
### Title: Get model parameters from generalized additive models
### Aliases: get_parameters.gamm get_parameters.gam get_parameters.rqss

### ** Examples

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
get_parameters(m)



cleanEx()
nameEx("get_parameters.glmm")
### * get_parameters.glmm

flush(stderr()); flush(stdout())

### Name: get_parameters.glmm
### Title: Get model parameters from mixed models
### Aliases: get_parameters.glmm get_parameters.coxme
###   get_parameters.nlmerMod get_parameters.merMod get_parameters.glmmTMB
###   get_parameters.glimML

### ** Examples

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
get_parameters(m)



cleanEx()
nameEx("get_parameters.htest")
### * get_parameters.htest

flush(stderr()); flush(stdout())

### Name: get_parameters.htest
### Title: Get model parameters from htest-objects
### Aliases: get_parameters.htest

### ** Examples

get_parameters(t.test(1:10, y = c(7:20)))



cleanEx()
nameEx("get_parameters.zeroinfl")
### * get_parameters.zeroinfl

flush(stderr()); flush(stdout())

### Name: get_parameters.zeroinfl
### Title: Get model parameters from zero-inflated and hurdle models
### Aliases: get_parameters.zeroinfl get_parameters.zcpglm
###   get_parameters.mhurdle

### ** Examples

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
get_parameters(m)



cleanEx()
nameEx("get_predicted")
### * get_predicted

flush(stderr()); flush(stdout())

### Name: get_predicted
### Title: Model predictions (robust) and their confidence intervals
### Aliases: get_predicted get_predicted.default get_predicted.lm
###   get_predicted.stanreg get_predicted.gam get_predicted.lmerMod
###   get_predicted.principal

### ** Examples

data(mtcars)
x <- lm(mpg ~ cyl + hp, data = mtcars)

predictions <- get_predicted(x, ci = 0.95)
predictions

# Options and methods ---------------------
get_predicted(x, predict = "prediction")

# Get CI
as.data.frame(predictions)

if (require("boot")) {
  # Bootstrapped
  as.data.frame(get_predicted(x, iterations = 4))
  # Same as as.data.frame(..., keep_iterations = FALSE)
  summary(get_predicted(x, iterations = 4))
}

# Different prediction types ------------------------
data(iris)
data <- droplevels(iris[1:100, ])

# Fit a logistic model
x <- glm(Species ~ Sepal.Length, data = data, family = "binomial")

# Expectation (default): response scale + CI
pred <- get_predicted(x, predict = "expectation", ci = 0.95)
head(as.data.frame(pred))

# Prediction: response scale + PI
pred <- get_predicted(x, predict = "prediction", ci = 0.95)
head(as.data.frame(pred))

# Link: link scale + CI
pred <- get_predicted(x, predict = "link", ci = 0.95)
head(as.data.frame(pred))

# Classification: classification "type" + PI
pred <- get_predicted(x, predict = "classification", ci = 0.95)
head(as.data.frame(pred))




cleanEx()
nameEx("get_predicted_ci")
### * get_predicted_ci

flush(stderr()); flush(stdout())

### Name: get_predicted_ci
### Title: Confidence intervals around predicted values
### Aliases: get_predicted_ci get_predicted_ci.default

### ** Examples

# Confidence Intervals for Model Predictions
# ------------------------------------------

data(mtcars)

# Linear model
# ------------
x <- lm(mpg ~ cyl + hp, data = mtcars)
predictions <- predict(x)
ci_vals <- get_predicted_ci(x, predictions, ci_type = "prediction")
head(ci_vals)
ci_vals <- get_predicted_ci(x, predictions, ci_type = "confidence")
head(ci_vals)
ci_vals <- get_predicted_ci(x, predictions, ci = c(0.8, 0.9, 0.95))
head(ci_vals)

# Bootstrapped
# ------------
if (require("boot")) {
  predictions <- get_predicted(x, iterations = 500)
  get_predicted_ci(x, predictions)
}

if (require("datawizard") && require("bayestestR")) {
  ci_vals <- get_predicted_ci(x, predictions, ci = c(0.80, 0.95))
  head(ci_vals)
  datawizard::reshape_ci(ci_vals)

  ci_vals <- get_predicted_ci(x,
    predictions,
    dispersion_method = "MAD",
    ci_method = "HDI"
  )
  head(ci_vals)
}


# Logistic model
# --------------
x <- glm(vs ~ wt, data = mtcars, family = "binomial")
predictions <- predict(x, type = "link")
ci_vals <- get_predicted_ci(x, predictions, ci_type = "prediction")
head(ci_vals)
ci_vals <- get_predicted_ci(x, predictions, ci_type = "confidence")
head(ci_vals)



cleanEx()
nameEx("get_predictors")
### * get_predictors

flush(stderr()); flush(stdout())

### Name: get_predictors
### Title: Get the data from model predictors
### Aliases: get_predictors

### ** Examples

m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
head(get_predictors(m))



cleanEx()
nameEx("get_priors")
### * get_priors

flush(stderr()); flush(stdout())

### Name: get_priors
### Title: Get summary of priors used for a model
### Aliases: get_priors get_priors.brmsfit

### ** Examples

## Not run: 
##D library(rstanarm)
##D model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data = iris)
##D get_priors(model)
## End(Not run)




cleanEx()
nameEx("get_random")
### * get_random

flush(stderr()); flush(stdout())

### Name: get_random
### Title: Get the data from random effects
### Aliases: get_random

### ** Examples

if (require("lme4")) {
  data(sleepstudy)
  # prepare some data...
  sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
  sleepstudy$mysubgrp <- NA
  for (i in 1:5) {
    filter_group <- sleepstudy$mygrp == i
    sleepstudy$mysubgrp[filter_group] <-
      sample(1:30, size = sum(filter_group), replace = TRUE)
  }

  m <- lmer(
    Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject),
    data = sleepstudy
  )

  head(get_random(m))
}



cleanEx()
nameEx("get_residuals")
### * get_residuals

flush(stderr()); flush(stdout())

### Name: get_residuals
### Title: Extract model residuals
### Aliases: get_residuals get_residuals.default

### ** Examples

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
get_residuals(m)

m <- glm(vs ~ wt + cyl + mpg, data = mtcars, family = binomial())
get_residuals(m) # type = "deviance" by default
get_residuals(m, type = "response")



cleanEx()
nameEx("get_response")
### * get_response

flush(stderr()); flush(stdout())

### Name: get_response
### Title: Get the values from the response variable
### Aliases: get_response

### ** Examples

if (require("lme4")) {
  data(cbpp)
  cbpp$trials <- cbpp$size - cbpp$incidence
  dat <<- cbpp

  m <- glm(cbind(incidence, trials) ~ period, data = dat, family = binomial)
  head(get_response(m))
  get_response(m, select = "incidence")
}

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
get_response(m)



cleanEx()
nameEx("get_sigma")
### * get_sigma

flush(stderr()); flush(stdout())

### Name: get_sigma
### Title: Get residual standard deviation from models
### Aliases: get_sigma

### ** Examples

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
get_sigma(m)



cleanEx()
nameEx("get_statistic")
### * get_statistic

flush(stderr()); flush(stdout())

### Name: get_statistic
### Title: Get statistic associated with estimates
### Aliases: get_statistic get_statistic.default get_statistic.glmmTMB
###   get_statistic.clm2 get_statistic.betamfx get_statistic.logitmfx
###   get_statistic.mjoint get_statistic.emmGrid get_statistic.gee
###   get_statistic.betareg get_statistic.DirichletRegModel

### ** Examples

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
get_statistic(m)



cleanEx()
nameEx("get_transformation")
### * get_transformation

flush(stderr()); flush(stdout())

### Name: get_transformation
### Title: Return function of transformed response variables
### Aliases: get_transformation

### ** Examples

# identity, no transformation
model <- lm(Sepal.Length ~ Species, data = iris)
get_transformation(model)

# log-transformation
model <- lm(log(Sepal.Length) ~ Species, data = iris)
get_transformation(model)

# log-function
get_transformation(model)$transformation(.3)
log(.3)

# inverse function is exp()
get_transformation(model)$inverse(.3)
exp(.3)



cleanEx()
nameEx("get_varcov")
### * get_varcov

flush(stderr()); flush(stdout())

### Name: get_varcov
### Title: Get variance-covariance matrix from models
### Aliases: get_varcov get_varcov.default get_varcov.nestedLogit
###   get_varcov.betareg get_varcov.clm2 get_varcov.truncreg
###   get_varcov.hurdle get_varcov.glmmTMB get_varcov.MixMod
###   get_varcov.brmsfit get_varcov.betamfx get_varcov.aov get_varcov.mixor

### ** Examples

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
get_varcov(m)

# vcov of zero-inflation component from hurdle-model
if (require("pscl")) {
  data("bioChemists", package = "pscl")
  mod <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
  get_varcov(mod, component = "zero_inflated")
}

# robust vcov of, count component from hurdle-model
if (require("pscl") && require("sandwich")) {
  data("bioChemists", package = "pscl")
  mod <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
  get_varcov(
    mod,
    component = "conditional",
    vcov = "BS",
    vcov_args = list(R = 50)
  )
}



cleanEx()
nameEx("get_variance")
### * get_variance

flush(stderr()); flush(stdout())

### Name: get_variance
### Title: Get variance components from random effects models
### Aliases: get_variance get_variance_residual get_variance_fixed
###   get_variance_random get_variance_distribution get_variance_dispersion
###   get_variance_intercept get_variance_slope
###   get_correlation_slope_intercept get_correlation_slopes

### ** Examples

## Not run: 
##D library(lme4)
##D data(sleepstudy)
##D m <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
##D 
##D get_variance(m)
##D get_variance_fixed(m)
##D get_variance_residual(m)
## End(Not run)



cleanEx()
nameEx("get_weights")
### * get_weights

flush(stderr()); flush(stdout())

### Name: get_weights
### Title: Get the values from model weights
### Aliases: get_weights get_weights.default

### ** Examples

data(mtcars)
set.seed(123)
mtcars$weight <- rnorm(nrow(mtcars), 1, .3)

# LMs
m <- lm(mpg ~ wt + cyl + vs, data = mtcars, weights = weight)
get_weights(m)

get_weights(lm(mpg ~ wt, data = mtcars), null_as_ones = TRUE)

# GLMs
m <- glm(vs ~ disp + mpg, data = mtcars, weights = weight, family = quasibinomial)
get_weights(m)
m <- glm(cbind(cyl, gear) ~ mpg, data = mtcars, weights = weight, family = binomial)
get_weights(m)



cleanEx()
nameEx("has_intercept")
### * has_intercept

flush(stderr()); flush(stdout())

### Name: has_intercept
### Title: Checks if model has an intercept
### Aliases: has_intercept

### ** Examples

model <- lm(mpg ~ 0 + gear, data = mtcars)
has_intercept(model)

model <- lm(mpg ~ gear, data = mtcars)
has_intercept(model)

if (require("lme4")) {
  model <- lmer(Reaction ~ 0 + Days + (Days | Subject), data = sleepstudy)
  has_intercept(model)

  model <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
  has_intercept(model)
}



cleanEx()
nameEx("is_converged")
### * is_converged

flush(stderr()); flush(stdout())

### Name: is_converged
### Title: Convergence test for mixed effects models
### Aliases: is_converged

### ** Examples

if (require("lme4")) {
  data(cbpp)
  set.seed(1)
  cbpp$x <- rnorm(nrow(cbpp))
  cbpp$x2 <- runif(nrow(cbpp))

  model <- glmer(
    cbind(incidence, size - incidence) ~ period + x + x2 + (1 + x | herd),
    data = cbpp,
    family = binomial()
  )

  is_converged(model)
}

## Don't show: 
if (getOption("warn") < 2L) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Not run: 
##D if (require("glmmTMB")) {
##D   model <- glmmTMB(Sepal.Length ~ poly(Petal.Width, 4) * poly(Petal.Length, 4) +
##D     (1 + poly(Petal.Width, 4) | Species), data = iris)
##D 
##D   is_converged(model)
##D }
## End(Not run)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("is_empty_object")
### * is_empty_object

flush(stderr()); flush(stdout())

### Name: is_empty_object
### Title: Check if object is empty
### Aliases: is_empty_object

### ** Examples

is_empty_object(c(1, 2, 3, NA))
is_empty_object(list(NULL, c(NA, NA)))
is_empty_object(list(NULL, NA))



cleanEx()
nameEx("is_gam_model")
### * is_gam_model

flush(stderr()); flush(stdout())

### Name: is_gam_model
### Title: Checks if a model is a generalized additive model
### Aliases: is_gam_model

### ** Examples

if (require("mgcv")) {
  data(iris)
  model1 <- lm(Petal.Length ~ Petal.Width + Sepal.Length, data = iris)
  model2 <- gam(Petal.Length ~ Petal.Width + s(Sepal.Length), data = iris)
  is_gam_model(model1)
  is_gam_model(model2)
}



cleanEx()
nameEx("is_mixed_model")
### * is_mixed_model

flush(stderr()); flush(stdout())

### Name: is_mixed_model
### Title: Checks if a model is a mixed effects model
### Aliases: is_mixed_model

### ** Examples

data(mtcars)
model <- lm(mpg ~ wt + cyl + vs, data = mtcars)
is_mixed_model(model)

if (require("lme4")) {
  data(sleepstudy)
  model <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  is_mixed_model(model)
}



cleanEx()
nameEx("is_model")
### * is_model

flush(stderr()); flush(stdout())

### Name: is_model
### Title: Checks if an object is a regression model or statistical test
###   object
### Aliases: is_model is_regression_model

### ** Examples

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)

is_model(m)
is_model(mtcars)

test <- t.test(1:10, y = c(7:20))
is_model(test)
is_regression_model(test)



cleanEx()
nameEx("is_model_supported")
### * is_model_supported

flush(stderr()); flush(stdout())

### Name: is_model_supported
### Title: Checks if a regression model object is supported in 'insight'
###   package
### Aliases: is_model_supported supported_models

### ** Examples


data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)

is_model_supported(m)
is_model_supported(mtcars)

# to see all supported models
supported_models()




cleanEx()
nameEx("is_multivariate")
### * is_multivariate

flush(stderr()); flush(stdout())

### Name: is_multivariate
### Title: Checks if an object stems from a multivariate response model
### Aliases: is_multivariate

### ** Examples

## Not run: 
##D library(rstanarm)
##D data("pbcLong")
##D model <- suppressWarnings(stan_mvmer(
##D   formula = list(
##D     logBili ~ year + (1 | id),
##D     albumin ~ sex + year + (year | id)
##D   ),
##D   data = pbcLong,
##D   chains = 1, cores = 1, seed = 12345, iter = 1000,
##D   show_messages = FALSE, refresh = 0
##D ))
##D 
##D f <- find_formula(model)
##D is_multivariate(model)
##D is_multivariate(f)
## End(Not run)



cleanEx()
nameEx("is_nested_models")
### * is_nested_models

flush(stderr()); flush(stdout())

### Name: is_nested_models
### Title: Checks whether a list of models are nested models
### Aliases: is_nested_models

### ** Examples

m1 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
m2 <- lm(Sepal.Length ~ Species, data = iris)
m3 <- lm(Sepal.Length ~ Petal.Width, data = iris)
m4 <- lm(Sepal.Length ~ 1, data = iris)

is_nested_models(m1, m2, m4)
is_nested_models(m4, m2, m1)
is_nested_models(m1, m2, m3)



cleanEx()
nameEx("is_nullmodel")
### * is_nullmodel

flush(stderr()); flush(stdout())

### Name: is_nullmodel
### Title: Checks if model is a null-model (intercept-only)
### Aliases: is_nullmodel

### ** Examples

model <- lm(mpg ~ 1, data = mtcars)
is_nullmodel(model)

model <- lm(mpg ~ gear, data = mtcars)
is_nullmodel(model)

if (require("lme4")) {
  model <- lmer(Reaction ~ 1 + (Days | Subject), data = sleepstudy)
  is_nullmodel(model)

  model <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
  is_nullmodel(model)
}



cleanEx()
nameEx("link_function")
### * link_function

flush(stderr()); flush(stdout())

### Name: link_function
### Title: Get link-function from model object
### Aliases: link_function link_function.betamfx link_function.gamlss
###   link_function.betareg link_function.DirichletRegModel

### ** Examples

# example from ?stats::glm
counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
outcome <- gl(3, 1, 9)
treatment <- gl(3, 3)
m <- glm(counts ~ outcome + treatment, family = poisson())

link_function(m)(.3)
# same as
log(.3)



cleanEx()
nameEx("link_inverse")
### * link_inverse

flush(stderr()); flush(stdout())

### Name: link_inverse
### Title: Get link-inverse function from model object
### Aliases: link_inverse link_inverse.betareg
###   link_inverse.DirichletRegModel link_inverse.betamfx
###   link_inverse.gamlss

### ** Examples

# example from ?stats::glm
counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
outcome <- gl(3, 1, 9)
treatment <- gl(3, 3)
m <- glm(counts ~ outcome + treatment, family = poisson())

link_inverse(m)(.3)
# same as
exp(.3)



cleanEx()
nameEx("model_info")
### * model_info

flush(stderr()); flush(stdout())

### Name: model_info
### Title: Access information from model objects
### Aliases: model_info model_info.default

### ** Examples

ldose <- rep(0:5, 2)
numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
sex <- factor(rep(c("M", "F"), c(6, 6)))
SF <- cbind(numdead, numalive = 20 - numdead)
dat <- data.frame(ldose, sex, SF, stringsAsFactors = FALSE)
m <- glm(SF ~ sex * ldose, family = binomial)

model_info(m)
## Not run: 
##D library(glmmTMB)
##D data("Salamanders")
##D m <- glmmTMB(
##D   count ~ spp + cover + mined + (1 | site),
##D   ziformula = ~ spp + mined,
##D   dispformula = ~DOY,
##D   data = Salamanders,
##D   family = nbinom2
##D )
## End(Not run)

model_info(m)



cleanEx()
nameEx("model_name")
### * model_name

flush(stderr()); flush(stdout())

### Name: model_name
### Title: Name the model
### Aliases: model_name model_name.default

### ** Examples

m <- lm(Sepal.Length ~ Petal.Width, data = iris)
model_name(m)
model_name(m, include_formula = TRUE)
model_name(m, include_call = TRUE)

if (require("lme4")) {
  model_name(lmer(Sepal.Length ~ Sepal.Width + (1 | Species), data = iris))
}



cleanEx()
nameEx("n_grouplevels")
### * n_grouplevels

flush(stderr()); flush(stdout())

### Name: n_grouplevels
### Title: Count number of random effect levels in a mixed model
### Aliases: n_grouplevels

### ** Examples

if (require("lme4")) {
  data(sleepstudy)
  set.seed(12345)
  sleepstudy$grp <- sample(1:5, size = 180, replace = TRUE)
  sleepstudy$subgrp <- NA
  for (i in 1:5) {
    filter_group <- sleepstudy$grp == i
    sleepstudy$subgrp[filter_group] <-
      sample(1:30, size = sum(filter_group), replace = TRUE)
  }
  model <- lmer(
    Reaction ~ Days + (1 | grp / subgrp) + (1 | Subject),
    data = sleepstudy
  )
  n_grouplevels(model)
}



cleanEx()
nameEx("n_obs")
### * n_obs

flush(stderr()); flush(stdout())

### Name: n_obs
### Title: Get number of observations from a model
### Aliases: n_obs n_obs.glm n_obs.svyolr n_obs.afex_aov n_obs.stanmvreg

### ** Examples

data(mtcars)
m <- lm(mpg ~ wt + cyl + vs, data = mtcars)
n_obs(m)

if (require("lme4")) {
  data(cbpp, package = "lme4")
  m <- glm(
    cbind(incidence, size - incidence) ~ period,
    data = cbpp,
    family = binomial(link = "logit")
  )
  n_obs(m)
  n_obs(m, disaggregate = TRUE)
}



cleanEx()
nameEx("n_parameters")
### * n_parameters

flush(stderr()); flush(stdout())

### Name: n_parameters
### Title: Count number of parameters in a model
### Aliases: n_parameters n_parameters.default n_parameters.merMod
###   n_parameters.glmmTMB n_parameters.zeroinfl n_parameters.gam
###   n_parameters.brmsfit

### ** Examples

data(iris)
model <- lm(Sepal.Length ~ Sepal.Width * Species, data = iris)
n_parameters(model)



cleanEx()
nameEx("null_model")
### * null_model

flush(stderr()); flush(stdout())

### Name: null_model
### Title: Compute intercept-only model for regression models
### Aliases: null_model

### ** Examples

if (require("lme4")) {
  data(sleepstudy)
  m <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
  summary(m)
  summary(null_model(m))
}



cleanEx()
nameEx("object_has_names")
### * object_has_names

flush(stderr()); flush(stdout())

### Name: object_has_names
### Title: Check names and rownames
### Aliases: object_has_names object_has_rownames

### ** Examples


# check if specified names are present in the given object
object_has_names(mtcars, "am")
object_has_names(anscombe, c("x1", "z1", "y1"))
object_has_names(list("x" = 1, "y" = 2), c("x", "a"))

# check if a dataframe has rownames
object_has_rownames(mtcars)




cleanEx()
nameEx("print_color")
### * print_color

flush(stderr()); flush(stdout())

### Name: print_color
### Title: Coloured console output
### Aliases: print_color print_colour color_text colour_text color_theme

### ** Examples

print_color("I'm blue dabedi dabedei", "blue")



cleanEx()
nameEx("print_parameters")
### * print_parameters

flush(stderr()); flush(stdout())

### Name: print_parameters
### Title: Prepare summary statistics of model parameters for printing
### Aliases: print_parameters

### ** Examples

## Don't show: 
if (require("curl", quietly = TRUE) && curl::has_internet()) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Not run: 
##D library(bayestestR)
##D model <- download_model("brms_zi_2")
##D x <- hdi(model, effects = "all", component = "all")
##D 
##D # hdi() returns a data frame; here we use only the
##D # information on parameter names and HDI values
##D tmp <- as.data.frame(x)[, 1:4]
##D tmp
##D 
##D # Based on the "split_by" argument, we get a list of data frames that
##D # is split into several parts that reflect the model components.
##D print_parameters(model, tmp)
##D 
##D # This is the standard print()-method for "bayestestR::hdi"-objects.
##D # For printing methods, it is easy to print complex summary statistics
##D # in a clean way to the console by splitting the information into
##D # different model components.
##D x
## End(Not run)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("standardize_column_order")
### * standardize_column_order

flush(stderr()); flush(stdout())

### Name: standardize_column_order
### Title: Standardize column order
### Aliases: standardize_column_order
###   standardize_column_order.parameters_model

### ** Examples

# easystats conventions
df1 <- cbind.data.frame(
  CI_low      = -2.873,
  t           = 5.494,
  CI_high     = -1.088,
  p           = 0.00001,
  Parameter   = -1.980,
  CI          = 0.95,
  df          = 29.234,
  Method      = "Student's t-test"
)

standardize_column_order(df1, style = "easystats")

# broom conventions
df2 <- cbind.data.frame(
  conf.low   = -2.873,
  statistic  = 5.494,
  conf.high  = -1.088,
  p.value    = 0.00001,
  estimate   = -1.980,
  conf.level = 0.95,
  df         = 29.234,
  method     = "Student's t-test"
)

standardize_column_order(df2, style = "broom")



cleanEx()
nameEx("standardize_names")
### * standardize_names

flush(stderr()); flush(stdout())

### Name: standardize_names
### Title: Standardize column names
### Aliases: standardize_names standardize_names.parameters_model

### ** Examples

if (require("parameters")) {
  model <- lm(mpg ~ wt + cyl, data = mtcars)
  mp <- model_parameters(model)

  as.data.frame(mp)
  standardize_names(mp)
  standardize_names(mp, style = "broom")
}



cleanEx()
nameEx("text_remove_backticks")
### * text_remove_backticks

flush(stderr()); flush(stdout())

### Name: text_remove_backticks
### Title: Remove backticks from a string
### Aliases: text_remove_backticks text_remove_backticks.data.frame

### ** Examples

# example model
data(iris)
iris$`a m` <- iris$Species
iris$`Sepal Width` <- iris$Sepal.Width
model <- lm(`Sepal Width` ~ Petal.Length + `a m`, data = iris)

# remove backticks from string
names(coef(model))
text_remove_backticks(names(coef(model)))

# remove backticks from character variable in a data frame
# column defaults to "Parameter".
d <- data.frame(
  Parameter = names(coef(model)),
  Estimate = unname(coef(model))
)
d
text_remove_backticks(d)



cleanEx()
nameEx("trim_ws")
### * trim_ws

flush(stderr()); flush(stdout())

### Name: trim_ws
### Title: Small helper functions
### Aliases: trim_ws trim_ws.data.frame n_unique n_unique.default
###   safe_deparse safe_deparse_symbol has_single_value

### ** Examples

trim_ws("  no space!  ")
n_unique(iris$Species)
has_single_value(c(1, 1, 2))

# safe_deparse_symbol() compared to deparse(substitute())
safe_deparse_symbol(as.name("test"))
deparse(substitute(as.name("test")))



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
