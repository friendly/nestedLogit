#' ---
#' title: Health Insurance plots with std errors
#' ---

library(nestedLogit)
source("./experimental/predict-with-se.R")
source("./experimental/plot.nested--with-conf-limits.R")
source("./experimental/as.data.frame.predict.R")

# define logits as successive binary splits of A, B, C, D
lbinary <- logits(AB_CD = dichotomy(c("A", "B"), c("C", "D")),
                  A_B   = dichotomy("A", "B"),
                  C_D   = dichotomy("C", "D"))

health.mod <- nestedLogit(product4 ~ age  + gender * household + position_level,
                             dichotomies = lbinary, data = HealthInsurance)

new <- expand.grid(age=seq(20, 70, by = 10),
                   gender = c("Female", "Male"),
                   household = mean(HealthInsurance$household),
                   position_level = mean(HealthInsurance$position_level)
)

# package plot method
plot(health.mod, "age", list(gender="Male"),
     xlab="Age", legend.location="top")

# ggplot
library(ggplot2)
library(geomtextpath)

health.pred <- predict(health.mod, new)
plotdata <- as.data.frame(health.pred, newdata=new)

plotdata |>
  ggplot(aes(x = age, y = p, color = gender)) +
  geom_line(linewidth = 2) +
  geom_point(size = 1.5, shape = 16, color = "black") +
  scale_color_discrete() +
  labs(x="Age", y= "Fitted Probability") +
#  facet_wrap(~ children, labeller = label_both) +
  theme_bw(base_size = 14) +
  geom_ribbon(aes(ymin=p - se.p,
                  ymax=p + se.p,
                  fill = response), alpha = 0.3) +
  geom_textline(aes(label = response), hjust = -0.01, vjust=-0.5, size=5) +
  theme(legend.position = "none")


