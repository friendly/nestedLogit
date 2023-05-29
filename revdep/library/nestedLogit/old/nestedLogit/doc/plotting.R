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
to.cite <- c("ggplot2", "geomtextpath", "equatiomatic")

## ----setup--------------------------------------------------------------------
library(nestedLogit)    # Nested Dichotomy Logistic Regression Models
library(knitr)          # A General-Purpose Package for Dynamic Report Generation in R
library(dplyr)          # A Grammar of Data Manipulation
library(tidyr)          # Tidy Messy Data
library(ggplot2)        # Create Elegant Data Visualisations Using the Grammar of Graphics
library(geomtextpath)   # Curved Text in 'ggplot2'

## ----wld-model----------------------------------------------------------------
data(Womenlf, package = "carData")
Womenlf$partic <- with(Womenlf,
                       factor(partic, levels = c("not.work", "parttime", "fulltime")))

wlf.nested <- nestedLogit(partic ~ hincome + children,
                          dichotomies = logits(work=dichotomy("not.work", working=c("parttime", "fulltime")),
                                               full=dichotomy("parttime", "fulltime")),
                          data=Womenlf)

## ----plotdata-----------------------------------------------------------------
new <- expand.grid(hincome=seq(0, 45, by = 5),
                   children=c("absent", "present"))

pred.nested <- predict(wlf.nested, newdata = new)
plotdata <- cbind(new, pred.nested)
head(plotdata)

## ----wlf-matplot--------------------------------------------------------------
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
par(op)

## ----plotlong-----------------------------------------------------------------
plotlong <- plotdata |>
  tidyr::pivot_longer(fulltime : not.work,
                      names_to = "Working",
                      values_to = "Probability") |>
  mutate(Working = ordered(Working, 
                           levels = c("not.work", "parttime", "fulltime")) )

head(plotlong)

## ----wlf-ggplot---------------------------------------------------------------
gg <- ggplot(plotlong,
             aes(x=hincome, y=Probability, color=Working)) +
  geom_line(linewidth = 2) +
  scale_color_discrete() +
  labs(x="Husband's Income", y= "Probability") +
  facet_wrap(~ children, labeller = label_both) +
  theme_bw(base_size = 14) +
  theme(legend.position = c(.3, .8))
gg

## ----wlf-geomtextpath---------------------------------------------------------
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

## ----pred-logits--------------------------------------------------------------
pred.logits <- sapply(models(wlf.nested), predict, newdata=new, type = "link")
plotdatal <- cbind(new, pred.logits)
head(plotdatal)

## ----wlf-logits---------------------------------------------------------------
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
par(op)

## ----plotlongl----------------------------------------------------------------
plotlongl <- plotdatal |>
  tidyr::pivot_longer(work : full,
                      names_to = "Dichotomy",
                      values_to = "logit") |>
  mutate(Dichotomy = ordered(Dichotomy,
                         levels = c("work", "full")) )

## ----wlf-gglogits-------------------------------------------------------------
ggplot(plotlongl,
       aes(x=hincome, y=logit, color=children)) +
  geom_line(linewidth = 3) +
  geom_point(size = 1.5, shape = 16, color = "black") +
  scale_color_discrete() +
  labs(x="Husband's Income", y= "Log Odds") +
  facet_wrap(~ Dichotomy, labeller = label_both) +
  theme_bw(base_size = 14) +
  theme(legend.position = c(.35, .82))


## ----write-bib, echo = FALSE--------------------------------------------------
# write a packages.bib file of the packages (.packages()) that have been used here
#pkgs <- unique(c(to.cite, .packages()))
#knitr::write_bib(pkgs, file = here::here("vignettes", "packages.bib"))

## ---- include = FALSE---------------------------------------------------------
options(.opts)

