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
#to.cite <- c("ggplot2", "geomtextpath", "equatiomatic")

## ----setup--------------------------------------------------------------------
library(nestedLogit)    # Nested Dichotomy Logistic Regression Models
library(knitr)          # A General-Purpose Package for Dynamic Report Generation in R
library(dplyr)          # A Grammar of Data Manipulation
library(tidyr)          # Tidy Messy Data
library(ggplot2)        # Create Elegant Data Visualisations Using the Grammar of Graphics
library(geomtextpath)   # Curved Text in 'ggplot2'

## ----wlf-model----------------------------------------------------------------
data(Womenlf, package = "carData")
comparisons <- logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                      full=dichotomy("parttime", "fulltime"))

wlf.nested <- nestedLogit(partic ~ hincome + children,
                          dichotomies = comparisons,
                          data=Womenlf)

## ----pred.nested--------------------------------------------------------------
new <- expand.grid(hincome=seq(0, 45, by = 5),
                   children=c("absent", "present"))

pred.nested <- predict(wlf.nested, newdata = new)
names(pred.nested)

## -----------------------------------------------------------------------------
head(pred.nested[["p"]])

## -----------------------------------------------------------------------------
plotdata <- as.data.frame(pred.nested, newdata=new)
head(plotdata)

## ----wlf-ggplot-p1------------------------------------------------------------
theme_set(theme_bw(base_size = 14))

gg1 <- ggplot(plotdata,
       aes(x=hincome, y=p, color=response)) +
  geom_line(linewidth = 2) +
  geom_point(size = 1.5, shape = 16, color = "black") +
  labs(x="Husband's Income", y= "Probability") +
  facet_wrap(~ children, labeller = label_both) +
  geom_ribbon(aes(ymin=p - se.p,
                  ymax=p + se.p,
                  fill = response), alpha = 0.3) 

gg1

## ----wlf-ggplot-p2------------------------------------------------------------
gg1 + geom_textline(aes(label = response), 
                    hjust = -0.01, vjust=-0.5, size=5) +
  theme(legend.position = "none")

## ----wlf-ggplot-logit---------------------------------------------------------
ggplot(plotdata,
       aes(x=hincome, y=logit, color=response)) +
  geom_line(linewidth = 2) +
  geom_point(size = 1.5, shape = 16, color = "black") +
  labs(x="Husband's Income", y= "Log Odds") +
  facet_wrap(~ children, labeller = label_both) +
  geom_ribbon(aes(ymin=logit - se.logit,
                  ymax=logit + se.logit,
                  fill = response), alpha = 0.3) +
  geom_textline(aes(label = response), 
                hjust = -0.01, vjust=-0.5, size=5) +
  theme(legend.position = "none")

## -----------------------------------------------------------------------------
names(models(wlf.nested))

## -----------------------------------------------------------------------------
pred.dichot <- predict(wlf.nested, newdata = new,
                       model = "dichotomies")
str(pred.dichot)

## -----------------------------------------------------------------------------
plotlogit <- as.data.frame(pred.dichot, newdata = new)
head(plotlogit)

## ----wlf-ggplot-dichot1-------------------------------------------------------
ggplot(plotlogit,
       aes(x=hincome, y=logit, color=response)) +
  geom_line(linewidth = 2) +
  geom_point(size = 1.5, shape = 16, color = "black") +
  labs(x="Husband's Income", y= "Log Odds") +
  facet_wrap(~ children, labeller = label_both) +
  geom_ribbon(aes(ymin=logit - se.logit,
                  ymax=logit + se.logit,
                  fill = response), alpha = 0.3) +
  geom_textline(aes(label = response),
                hjust = -0.01, vjust=-0.5, size=5) +
  theme(legend.position = "none")

## ----wlf-ggplot-dichot2-------------------------------------------------------
ggplot(plotlogit,
       aes(x=hincome, y=logit, color=children)) +
  geom_line(linewidth = 2) +
  geom_point(size = 1.5, shape = 16, color = "black") +
  labs(x="Husband's Income", y= "Log Odds") +
  facet_wrap(~ response, labeller = label_both) +
  geom_ribbon(aes(ymin=logit - se.logit,
                  ymax=logit + se.logit,
                  fill = children), alpha = 0.3) +
  geom_textline(aes(label = children),
                hjust = -0.01, vjust=-0.5, size=5) +
  theme(legend.position = "none")

## ----alt-model----------------------------------------------------------------
wlf.nested.alt <- nestedLogit(partic ~ hincome + children,
                              logits(full=dichotomy(nonfulltime=c("not.work", "parttime"), "fulltime"),
                                     part=dichotomy("not.work", "parttime")),
                              data=Womenlf)

## -----------------------------------------------------------------------------
pred.dichot.alt <- predict(wlf.nested.alt, newdata = new,
                       model = "dichotomies")
plotlogit.alt <- as.data.frame(pred.dichot.alt, newdata = new)
head(plotlogit.alt)

## ----wlf-ggplot-alt1----------------------------------------------------------
ggplot(plotlogit.alt,
       aes(x=hincome, y=logit, color=children)) +
  geom_line(linewidth = 2) +
  geom_point(size = 1.5, shape = 16, color = "black") +
  labs(x="Husband's Income", y= "Log Odds") +
  facet_wrap(~ response, labeller = label_both) +
  geom_ribbon(aes(ymin=logit - se.logit,
                  ymax=logit + se.logit,
                  fill = children), alpha = 0.3) +
  geom_textline(aes(label = children),
                hjust = -0.01, vjust=-0.5, size=5) +
  theme(legend.position = "none")

## ---- include = FALSE---------------------------------------------------------
options(.opts)

