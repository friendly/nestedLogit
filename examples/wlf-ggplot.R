#' ---
#' title: Test ggplot methods for Womenlf data
#' ---
#'
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

library(ggplot2)
library(geomtextpath)
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
  scale_color_discrete() +
  labs(x="Husband's Income", y= "Log Odds") +
  facet_wrap(~ children, labeller = label_both) +
  theme_bw(base_size = 14) +
  geom_ribbon(aes(ymin=logit - se.logit,
                  ymax=logit + se.logit,
                  fill = response), alpha = 0.3) +
  geom_textline(aes(label = response),
                hjust = -0.01, vjust=-0.5, size=5) +
  theme(legend.position = "none")


# Predicted values for the dichotomies

pred.dichot <- predict(wlf.nested, newdata = new,
                       model = "dichotomies", se.fit = TRUE)


