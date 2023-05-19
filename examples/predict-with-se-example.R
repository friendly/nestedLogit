library(nestedLogit)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(ggplot2)
library(geomtextpath)

data(Womenlf, package="carData")

comparisons <- logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                      full=dichotomy("parttime", "fulltime"))

wlf.nested <- nestedLogit(partic ~ hincome + children,
                          dichotomies = comparisons,
                          data=Womenlf)

new <- expand.grid(hincome=seq(0, 45, length=10),
                   children=c("absent", "present"))

pred.nested <- predict(wlf.nested, new)
pred.nested

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

p  <- pred.nested.df$p  |> relocate(hincome, children)

se <- pred.nested.df$se.p |> relocate(hincome, children)
resp.names <- colnames(p)

p_long <- p |>
  tidyr::pivot_longer(cols = not.work:fulltime,
                      names_to = "response",
                      values_to = "prob")

se_long <- se |>
  tidyr::pivot_longer(cols = not.work:fulltime,
                      names_to = "response",
                      values_to = "se")

plotdata <- cbind(p_long, se = se_long$se)

gg <- ggplot(plotdata,
             aes(x=hincome, y=prob, color=response)) +
  geom_line(linewidth = 2) +
  geom_point(size = 1.5, shape = 16, color = "black") +
  scale_color_discrete() +
  labs(x="Husband's Income", y= "Probability") +
  facet_wrap(~ children, labeller = label_both) +
  theme_bw(base_size = 14)

gg + geom_ribbon(aes(ymin=prob - se,
                     ymax=prob + se,
                     fill = response), alpha = 0.3) +
  geom_textline(aes(label = response), hjust = -0.05, vjust=-0.5, size=5) +
  theme(legend.position = "none")



