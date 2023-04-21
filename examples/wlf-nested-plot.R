#' ---
#' title: Plotting methods for nested dichotomies
#' ---
#'

#' This example explores different ways of plotting the results for nested dichotomies.
#' The `predict()` method for `nested` object generates predicted probabilities
#' of the response categories for _all_ observations in the dataset.
#'
#' Most of these generate the predicted values on a grid of `hincome` and `children`
#' and show the result using lines. A final method calculates predicted probabilities
#' for all observations and shows these with points and lines. This shows where the
#' actual observations are in this space.
#'
#' **Methods**: the following plotting methods are illustrated
#'
#' * graphics::plot()
#' * graphics::matplot()
#' * ggplot
#' * ggplot + directlabels
#' * plotting individual observations with the fitted curves
#'
#'

library(nestedLogit)
library(ggplot2)
library(dplyr)
library(directlabels)


data(Womenlf, package = "carData")

#' ## Fit the model
#' Use `logits()` and `dichotomy()` to specify the comparisons of interest
comparisons <- logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                      full=dichotomy("parttime", "fulltime"))

wlf.nested <- nestedLogit(partic ~ hincome + children,
                          dichotomies = comparisons,
                          data=Womenlf)

#' ## Get predicted probabilities
new <- expand.grid(hincome=seq(0, 45, length=10),
                   children=c("absent", "present"))

pred.nested <- predict(wlf.nested, new)

plotdata <- cbind(new, pred.nested)


#' ## Basic plotting
op <- par(mfrow=c(1,2), mar=c(4,4,3,1)+.1)
cols=c("blue", "magenta", "darkgreen")
for ( kids in c("absent", "present") ) {
  data <- subset(plotdata, children==kids)
  with(data, {
    plot( range(hincome), c(0,1),
          type="n",
          xlab="Husband's Income",
          ylab='Fitted Probability',
          main = paste("Children", kids),
          cex.lab = 1.1)
    lines(hincome, fulltime,  lwd=3, lty=1, col=cols[1])
    lines(hincome, parttime,  lwd=3, lty=2, col=cols[2])
    lines(hincome, not.work,  lwd=3, lty=3, col=cols[3])
  })

  if (kids=="absent") {
    legend("topright", lty=1:3, lwd=3, col=cols, bty = "n",
           legend=c("fulltime", "parttime", "not working"))
  }
}
par(op)


#' ## Try matplot()
#' This is moderately simpler than using `plot()` because we can just plot each column
#' of predicted probabilities as lines.
#'
op <- par(mfrow=c(1,2), mar=c(4,4,3,1)+.1)
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

#' ## Try ggplot
#' `ggplot` wants the data in long format. That makes it easy to plot probability
#' against one predictor and use `color` to distinguish the levels of
#' `partic`.
#'
plotlong <- plotdata |>
  tidyr::pivot_longer(fulltime : not.work,
                      names_to = "Working",
                      values_to = "prob") |>
  mutate(Working = ordered(Working,
                           levels = c("not.work", "parttime", "fulltime")) )


ggplot(plotlong,
       aes(x=hincome, y=prob, color=Working)) +
  geom_line(linewidth = 2) +
  scale_color_discrete() +
  labs(x="Husband's Income", y= "Probability") +
  facet_wrap(~ children, labeller = label_both) +
  theme_bw(base_size = 14) +
  theme(legend.position = c(.3, .85))

#' ## Try using direct labels
#' It's nicer to label the curves directly

library(directlabels)
gg <- ggplot(plotlong,
             aes(x=hincome, y=prob, color=Working)) +
  geom_line(linewidth = 2) +
  scale_color_discrete() +
  labs(x="Husband's Income", y= "Probability") +
  facet_wrap(~ children, labeller = label_both)

direct.label(gg, list("top.bumptwice", dl.trans(y = y + 0.2)))

#' ## Try plotting individual observations
#' This will give an indication of where the observations actually are
#'

pred.obs <- predict(wlf.nested)
plotobs <- cbind(Womenlf[, c("hincome", "children")], pred.obs)

plotlobs <- plotobs |>
  tidyr::pivot_longer(fulltime : not.work,
                      names_to = "Working",
                      values_to = "prob") |>
  mutate(Working = ordered(Working,
                           levels = c("not.work", "parttime", "fulltime")) )

ggplot(plotlobs,
       aes(x=hincome, y=prob, color=Working)) +
  geom_line(linewidth = 3) +
  geom_point(size = 1.5, shape = 16, color = "black") +
  scale_color_discrete() +
  labs(x="Husband's Income", y= "Probability") +
  facet_wrap(~ children, labeller = label_both) +
  theme_bw(base_size = 14) +
  theme(legend.position = c(.3, .82))

#' ## Find the predicted response category for each observation ???
#' This attempt doesn't work.
#'
# library(dplyr)
# pred.obs <- predict(wlf.nested)
# pred.obs <- tibble::as_tibble(pred.obs) |>
#   rowwise() |>
#   mutate(pred_cat = colnames(pred.obs)[which.max(c_across(not.work:fulltime))]) |>
#   bind_cols(partic = Womenlf[,"partic"])
#
# with(pred.obs, table(partic, pred_cat))
#
#

#' ## Plot predicted logits
#' The predicted logits for the separate dichotomies have different numbers of
#' observations for each. We can get them from the `predict.glm()` method for
#' each dichotomy

pred.logits <- sapply(wlf.nested$models, predict, newdata=new, type = "link")
plotdatal <- cbind(new, pred.logits)

cols=c("blue", "red")

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

#' Do this with ggplot, but make panels for the dichotomy

plotlongl <- plotdatal |>
  tidyr::pivot_longer(work : full,
                      names_to = "Dichotomy",
                      values_to = "logit") |>
  mutate(Dichotomy = ordered(Dichotomy,
                         levels = c("work", "full")) )

ggplot(plotlongl,
       aes(x=hincome, y=logit, color=children)) +
  geom_line(linewidth = 3) +
  geom_point(size = 1.5, shape = 16, color = "black") +
  scale_color_discrete() +
  labs(x="Husband's Income", y= "Log Odds") +
  facet_wrap(~ Dichotomy, labeller = label_both) +
  theme_bw(base_size = 14) +
  theme(legend.position = c(.35, .85))

