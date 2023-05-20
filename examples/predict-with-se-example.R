library(nestedLogit)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(ggplot2)
library(geomtextpath)
source("./experimental/predict-with-se.R")

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
names(pred.nested)

rowSums(pred.nested$p)


pred.dichotomies <- predict(wlf.nested, new, model = "dichotomies")

# print method doesn't do anything useful
pred.dichotomies

sapply(pred.dichotomies, cbind)

#' ## Transform predicted values to long suitable for plotting

# matrices don't work well with tidy processing; these should be data.frames
pred.nested.df <- lapply(pred.nested, as.data.frame)

# add predictors to each data set
pred.nested.df <-lapply(pred.nested.df, cbind, new)


# make a dataset for plotting

prob  <- pred.nested.df$p  |> relocate(hincome, children)

se.prob <- pred.nested.df$se.p |> relocate(hincome, children)
resp.names <- colnames(pred.nested.df$p)

p_long <- prob |>
  tidyr::pivot_longer(cols = not.work:fulltime,
                      names_to = "response",
                      values_to = "prob")

se_long <- se.prob |>
  tidyr::pivot_longer(cols = not.work:fulltime,
                      names_to = "response",
                      values_to = "se.prob")

plotprob <- cbind(p_long, se.prob = se_long$se.prob)

ggplot(plotprob,
             aes(x=hincome, y=prob, color=response)) +
  geom_line(linewidth = 2) +
  geom_point(size = 1.5, shape = 16, color = "black") +
  scale_color_discrete() +
  labs(x="Husband's Income", y= "Probability") +
  facet_wrap(~ children, labeller = label_both) +
  theme_bw(base_size = 14) +
  geom_ribbon(aes(ymin=prob - se.prob,
                     ymax=prob + se.prob,
                     fill = response), alpha = 0.3) +
  geom_textline(aes(label = response), hjust = -0.01, vjust=-0.5, size=5) +
  theme(legend.position = "none")

# --------------------------------------------
# do the same for logit

logit  <- pred.nested.df$logit  |> relocate(hincome, children)

se.logit <- pred.nested.df$se.logit |> relocate(hincome, children)
resp.names <- colnames(pred.nested.df$p)

p_long <- logit |>
  tidyr::pivot_longer(cols = not.work:fulltime,
                      names_to = "response",
                      values_to = "logit")

se_long <- se.logit |>
  tidyr::pivot_longer(cols = not.work:fulltime,
                      names_to = "response",
                      values_to = "se.logit")

plotlogit <- cbind(p_long, se.logit = se_long$se.logit)

ggplot(plotlogit,
       aes(x=hincome, y=logit, color=response)) +
  geom_line(linewidth = 2) +
  geom_point(size = 1.5, shape = 16, color = "black") +
  scale_color_discrete() +
  labs(x="Husband's Income", y= "Log Odds") +
  facet_wrap(~ children, labeller = label_both) +
  theme_bw(base_size = 14) +
  geom_ribbon(aes(ymin=logit - se.logit,
                  ymax=logit + se.logit,
                  fill = response), alpha = 0.3) +
  geom_textline(aes(label = response), hjust = -0.01, vjust=-0.5, size=5) +
  theme(legend.position = "none")

