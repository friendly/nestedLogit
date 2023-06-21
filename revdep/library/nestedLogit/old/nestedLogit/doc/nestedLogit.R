## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.align = "center",
  fig.height = 6,
  fig.width = 7,
  fig.path = "fig/",
  dev = "png",
  comment = "#>"
)

# save some typing
knitr::set_alias(w = "fig.width",
                 h = "fig.height",
                 cap = "fig.cap")

# colorize text
colorize <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color,
      x)
  } else x
}


set.seed(47)
.opts <- options(digits = 4)

# packages to be cited here. Code at the end automatically updates packages.bib
to.cite <- c("nnet", "car", "broom", "ggplot2", "geomtextpath")

# removed: "equatiomatic" -- now in references.bib

## ----setup--------------------------------------------------------------------
library(nestedLogit)    # Nested Dichotomy Logistic Regression Models
library(knitr)          # A General-Purpose Package for Dynamic Report Generation in R
library(car)            # Companion to Applied Regression
library(nnet)           # Feed-Forward Neural Networks and Multinomial Log-Linear Models
library(broom)          # Convert Statistical Objects into Tidy Tibbles
library(dplyr)          # A Grammar of Data Manipulation

## -----------------------------------------------------------------------------
knitr::include_graphics("nested.jpg")

## -----------------------------------------------------------------------------
knitr::include_graphics("nested-psychiatric.png")

## ----order-partic-------------------------------------------------------------
data(Womenlf, package = "carData")
Womenlf$partic <- with(Womenlf, 
                       factor(partic, levels = c("not.work", "parttime", "fulltime")))

## ----response-distribution----------------------------------------------------
xtabs(~ partic, data=Womenlf)

## ----recode-------------------------------------------------------------------
Womenlf <- within(Womenlf, {
  work = ifelse(partic == "not.work", 0, 1)
  full = ifelse(partic == "fulltime",  1,
                ifelse(partic == "parttime", 0, NA))
})

## ----dichots-distributions----------------------------------------------------
xtabs(~ work, data=Womenlf)
xtabs(~ full, data=Womenlf, addNA=TRUE)

## ----xtabs--------------------------------------------------------------------
xtabs(~ partic + work, data=Womenlf)
xtabs(~ partic + full, addNA=TRUE, data=Womenlf)

## ----submodels----------------------------------------------------------------
mod.work <- glm(work ~ hincome + children, family=binomial, data=Womenlf)
mod.full <- glm(full ~ hincome + children, family=binomial, data=Womenlf)

## ----direct-estimates---------------------------------------------------------
mod.work
mod.full

## ----comparisons--------------------------------------------------------------
comparisons <- logits(work=dichotomy("not.work", working=c("parttime", "fulltime")),
                      full=dichotomy("parttime", "fulltime"))

comparisons

## ----coerce-comparisons-------------------------------------------------------
as.matrix(comparisons)

cat(as.character(comparisons))

## ----wlf.nested---------------------------------------------------------------
wlf.nested <- nestedLogit(partic ~ hincome + children, 
                          dichotomies = comparisons,
                          data=Womenlf)

## ----names-etc----------------------------------------------------------------
names(wlf.nested)

names(wlf.nested$models) # equivalent: names(models(wlf.models))

# view the separate models
models(wlf.nested, 1) 

models(wlf.nested, 2)

## ----coef---------------------------------------------------------------------
coef(wlf.nested)

# show as odds ratios
exp(coef(wlf.nested))

## ----Anova--------------------------------------------------------------------
Anova(wlf.nested)

## ----linearHyp----------------------------------------------------------------
linearHypothesis(wlf.nested, c("hincome", "childrenpresent"))

## -----------------------------------------------------------------------------
glance(wlf.nested)   # summarize the sub-models

tidy(wlf.nested)     # summarize the coefficients

## -----------------------------------------------------------------------------
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

## ----update-------------------------------------------------------------------
wlf.nested.1 <- update(wlf.nested, formula = . ~ . + region)

anova(wlf.nested, wlf.nested.1)

## ----more-models--------------------------------------------------------------
wlf.nested.2 <- update(wlf.nested, formula = . ~ .^2)

anova(wlf.nested, wlf.nested.2)

## -----------------------------------------------------------------------------
wlf.pred <- predict(wlf.nested)
print(wlf.pred, n=5)

## -----------------------------------------------------------------------------
new <- expand.grid(hincome=seq(0, 45, length=4),
                   children=c("absent", "present"))

wlf.new <- predict(wlf.nested, new)

## -----------------------------------------------------------------------------
as.data.frame(wlf.new, newdata = new) 

## ----wlf-plot-----------------------------------------------------------------
op <- par(mfcol=c(1, 2), mar=c(4, 4, 3, 1) + 0.1)
plot(wlf.nested, "hincome", list(children="absent"), # left panel
     xlab="Husband's Income", legend.location="top")
plot(wlf.nested, "hincome", list(children="present"), # right panel
     xlab="Husband's Income", legend=FALSE)
par(op)

## ----alt-model----------------------------------------------------------------
wlf.nested.alt <- nestedLogit(partic ~ hincome + children,
                              logits(full=dichotomy(nonfulltime=c("not.work", "parttime"), "fulltime"),
                                     part=dichotomy("not.work", "parttime")),
                              data=Womenlf)

## ----alt-anova----------------------------------------------------------------
Anova(wlf.nested.alt)

summary(wlf.nested.alt)

## ----fit-correlations---------------------------------------------------------
fit1 <- predict(wlf.nested)$p
fit2 <- predict(wlf.nested.alt)$p
diag(cor(fit1, fit2))
mean(as.matrix(abs(fit1 - fit2)))
max(abs(fit1 - fit2))

## ----compare-logLik-----------------------------------------------------------
logLik(wlf.nested)
logLik(wlf.nested.alt)

## ----compare-AIC--------------------------------------------------------------
AIC(wlf.nested, wlf.nested.alt)

## ----wlf-alt-plot-------------------------------------------------------------
op <- par(mfcol=c(1, 2), mar=c(4, 4, 3, 1) + 0.1)
plot(wlf.nested.alt, "hincome", list(children="absent"), # left panel
     xlab="Husband's Income", legend.location="top")
plot(wlf.nested.alt, "hincome", list(children="present"), # right panel
     xlab="Husband's Income", legend=FALSE)
par(op)

## -----------------------------------------------------------------------------
wlf.multinom <- multinom(partic ~ hincome + children, data = Womenlf)
summary(wlf.multinom)
logLik(wlf.multinom)

## ----check-corr---------------------------------------------------------------
fit3 <- predict(wlf.multinom, type="probs")[, c("not.work", "parttime", "fulltime")]
diag(cor(fit2, fit3))
mean(as.matrix(abs(fit2 - fit3)))
max(abs(fit2 - fit3))

## ----write-bib, echo = FALSE--------------------------------------------------
# write a packages.bib file of the packages (.packages()) that have been used here
pkgs <- unique(c(to.cite, .packages()))
knitr::write_bib(pkgs, file = here::here("vignettes", "packages.bib"))

## ---- include = FALSE---------------------------------------------------------
options(.opts)

