#' ---
#' title: Health Insurance plots with std errors
#' ---

library(nestedLogit)
library(ggplot2)
library(geomtextpath)

#source("./experimental/predict-with-se.R")
#source("./experimental/plot.nested--with-conf-limits.R")
#source("./experimental/as.data.frame.predict.R")

# define logits as successive binary splits of A, B, C, D
lbinary <- logits(AB_CD = dichotomy(c("A", "B"), c("C", "D")),
                  A_B   = dichotomy("A", "B"),
                  C_D   = dichotomy("C", "D"))

health.mod <- nestedLogit(product4 ~ age  + gender * household + position_level,
                             dichotomies = lbinary, data = HealthInsurance)

# predict data for effects of age, gender, averaged over household and position_level
new <- expand.grid(age=seq(20, 70, by = 10),
                   gender = c("Female", "Male"),
                   household = mean(HealthInsurance$household),
                   position_level = mean(HealthInsurance$position_level)
)

#' ## package plot method
op <- par(mfcol=c(1, 2), mar=c(4, 4, 3, 1) + 0.1)
plot(health.mod, "age", list(gender="Female"),
     xlab="Age", legend.location="top",
     main = "gender: Female")
plot(health.mod, "age", list(gender="Male"),
     xlab="Age", legend = FALSE,
     main = "gender: Male")
par(op)

#' ## Using ggplot

# make a data.frame in long format
health.pred <- predict(health.mod, new)
plotdata <- as.data.frame(health.pred)

# plot probabilities
plotdata |>
  ggplot(aes(x = age, y = p, color = response)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5, shape = 16, color = "black") +
  scale_color_discrete() +
  labs(x="Age", y= "Fitted Probability") +
  facet_wrap(~ gender, labeller = label_both) +
  theme_bw(base_size = 14) +
  geom_ribbon(aes(ymin = p - 1.96 * se.p,
                  ymax = p + 1.96 * se.p,
                  fill = response), alpha = 0.3) +
  geom_textline(aes(label = response), hjust = 0.0, vjust=-0.5, size=5) +
  theme(legend.position = "none")

# plot logits
plotdata |>
  ggplot(aes(x = age, y = logit, color = response)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5, shape = 16, color = "black") +
  scale_color_discrete() +
  labs(x="Age", y= "Fitted Log Odds") +
  facet_wrap(~ gender, labeller = label_both) +
  theme_bw(base_size = 14) +
  geom_ribbon(aes(ymin = logit - 2 * se.logit,
                  ymax = logit + 2 * se.logit,
                  fill = response), alpha = 0.3) +
  geom_textline(aes(label = response), hjust = 0.0, vjust=-0.5, size=5) +
  theme(legend.position = "none")

