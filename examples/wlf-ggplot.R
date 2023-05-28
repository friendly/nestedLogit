#' ---
#' title: Test ggplot2 methods for Womenlf data
#' ---
#'
#' This example explores different ways of plotting the results for nested dichotomies using ggplot2.
#' The `predict()` method for `nestedLogit` objects generates predicted probabilities
#' of the response categories, their logits and corresponding standard errors.
#'
#' **Methods**: the following plotting methods are illustrated
#'
#' * ggplot
#' * ggplot + geomtextpath
#' * plotting the logit transformation of the probabilities
#' * plotting the predicted results for the separate binary logit models that comprise the full model.
#' * doing the same, for an alternative model for the nested dichotomies.

#' This uses the new as.data.frame.predictDichotomies method
source("R/as.data.frame.predict.R")
library(ggplot2)
library(geomtextpath)

data(Womenlf, package="carData")

comparisons <- logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                      full=dichotomy("parttime", "fulltime"))

wlf.nested <- nestedLogit(partic ~ hincome + children,
                          dichotomies = comparisons,
                          data=Womenlf)

# get predicted values for a grid
new <- expand.grid(hincome=seq(0, 45, length=4),
                   children=c("absent", "present"))

pred.nested <- predict(wlf.nested, new)
plotdata <- as.data.frame(pred.nested, newdata=new)

ggplot(plotdata,
       aes(x=hincome, y=p, color=response)) +
  geom_line(linewidth = 2) +
  geom_point(size = 1.5, shape = 16, color = "black") +
  scale_color_discrete() +
  labs(x="Husband's Income", y= "Probability") +
  facet_wrap(~ children, labeller = label_both) +
  theme_bw(base_size = 14) +
  geom_ribbon(aes(ymin=p - se.p,
                  ymax=p + se.p,
                  fill = response), alpha = 0.3) +
  geom_textline(aes(label = response),
                hjust = -0.01, vjust=-0.5, size=5) +
  theme(legend.position = "none")

# Plot the corresponding logits
ggplot(plotdata,
       aes(x=hincome, y=logit, color=response)) +
  geom_line(linewidth = 2) +
  geom_point(size = 1.5, shape = 16, color = "black") +
#  scale_color_discrete() +
  labs(x="Husband's Income", y= "Log Odds") +
  facet_wrap(~ children, labeller = label_both) +
  theme_bw(base_size = 14) +
  geom_ribbon(aes(ymin=logit - se.logit,
                  ymax=logit + se.logit,
                  fill = response), alpha = 0.3) +
  geom_textline(aes(label = response),
                hjust = -0.01, vjust=-0.5, size=5) +
  theme(legend.position = "none")


# Predicted logit values for the dichotomies

pred.dichot <- predict(wlf.nested, newdata = new,
                       model = "dichotomies", se.fit = TRUE)
str(pred.dichot)

# this is now the as.data.frame.predictDichotomies method

plotlogit <- as.data.frame(pred.dichot, newdata = new)

# plotlogit <- do.call(rbind, pred.dichot) |>
#   select(- residual.scale) |>
#   tibble::rownames_to_column(var = "response") |>
#   mutate(response =  stringr::str_remove(response, ".\\d+")) |>
#   rename(logit = fit,
#          se.logit = se.fit)
#
# idx <- rep(seq_len(nrow(new)), length(pred.dichot))
# plotlogit <- bind_cols(new[idx,], plotlogit)

ggplot(plotlogit,
       aes(x=hincome, y=logit, color=response)) +
  geom_line(linewidth = 2) +
  geom_point(size = 1.5, shape = 16, color = "black") +
#  scale_color_discrete() +
  labs(x="Husband's Income", y= "Log Odds") +
  facet_wrap(~ children, labeller = label_both) +
  theme_bw(base_size = 14) +
  geom_ribbon(aes(ymin=logit - se.logit,
                  ymax=logit + se.logit,
                  fill = response), alpha = 0.3) +
  geom_textline(aes(label = response),
                hjust = -0.01, vjust=-0.5, size=5) +
  theme(legend.position = "none")

# plot the other way -- by dichotomy
ggplot(plotlogit,
       aes(x=hincome, y=logit, color=children)) +
  geom_line(linewidth = 2) +
  geom_point(size = 1.5, shape = 16, color = "black") +
  #  scale_color_discrete() +
  labs(x="Husband's Income", y= "Log Odds") +
  facet_wrap(~ response, labeller = label_both) +
  theme_bw(base_size = 14) +
  geom_ribbon(aes(ymin=logit - se.logit,
                  ymax=logit + se.logit,
                  fill = children), alpha = 0.3) +
  geom_textline(aes(label = children),
                hjust = -0.01, vjust=-0.5, size=5) +
  theme(legend.position = "none")

#' ## Do this for the alternative model
#'
wlf.nested.alt <- nestedLogit(partic ~ hincome + children,
                              logits(full=dichotomy(nonfulltime=c("not.work", "parttime"), "fulltime"),
                                     part=dichotomy("not.work", "parttime")),
                              data=Womenlf)

pred.dichot.alt <- predict(wlf.nested.alt, newdata = new,
                       model = "dichotomies", se.fit = TRUE)
str(pred.dichot.alt)

plotlogit.alt <- as.data.frame(pred.dichot.alt, newdata = new)

ggplot(plotlogit.alt,
       aes(x=hincome, y=logit, color=response)) +
  geom_line(linewidth = 2) +
  geom_point(size = 1.5, shape = 16, color = "black") +
  #  scale_color_discrete() +
  labs(x="Husband's Income", y= "Log Odds") +
  facet_wrap(~ children, labeller = label_both) +
  theme_bw(base_size = 14) +
  geom_ribbon(aes(ymin=logit - se.logit,
                  ymax=logit + se.logit,
                  fill = response), alpha = 0.3) +
  geom_textline(aes(label = response),
                hjust = -0.01, vjust=-0.5, size=5) +
  theme(legend.position = "none")

# plot the other way -- by dichotomy
ggplot(plotlogit.alt,
       aes(x=hincome, y=logit, color=children)) +
  geom_line(linewidth = 2) +
  geom_point(size = 1.5, shape = 16, color = "black") +
   scale_color_discrete() +
  labs(x="Husband's Income", y= "Log Odds") +
  facet_wrap(~ response, labeller = label_both) +
  theme_bw(base_size = 14) +
  geom_ribbon(aes(ymin=logit - se.logit,
                  ymax=logit + se.logit,
                  fill = children), alpha = 0.3) +
  geom_textline(aes(label = children),
                hjust = -0.01, vjust=-0.5, size=5) +
  theme(legend.position = "none")
