#' ---
#' title: Health Insurance plots
#' ---

# test different dichotomies for 4 level response

library(nestedLogit)
library(ggplot2)
library(directlabels)
library(nnet)

#' ## Binary splits
# define logits as successive binary splits of A, B, C, D
lbinary <- logits(AB_CD = dichotomy(c("A", "B"), c("C", "D")),
                  A_B   = dichotomy("A", "B"),
                  C_D   = dichotomy("C", "D"))

as.matrix(lbinary)

health.binary <- nestedLogit(product4 ~ age  + gender * household + position_level,
                             dichotomies = lbinary, data = HealthInsurance)
car::Anova(health.binary)

new <- expand.grid(age=seq(20, 70, by = 5),
                   gender = c("Female", "Male"),
                   household = mean(HealthInsurance$household),
                   position_level = mean(HealthInsurance$position_level)
                  )

fit.binary <-data.frame(new,
                        predict(health.binary, newdata = new))

plotdat <- fit.binary |>
  tidyr::gather(key="Level", value="Probability", A:D)

gg <- ggplot(plotdat, aes(x = age, y = Probability, colour= Level)) +
  geom_line(linewidth = 1.5) +
  facet_grid(~ gender, labeller= label_both) +
  ggtitle("Nested model: {{A B} {C D}}, {A B}, {C D}")

direct.label(gg, list("top.bumptwice", dl.trans(y = y + 0.2)))

#' ## Continuation logits

health.contin <- nestedLogit(product4 ~ age  + gender * household + position_level,
                             dichotomies = continuationLogits(4), data = HealthInsurance)
car::Anova(health.contin)

fit.contin <-data.frame(new,
                        predict(health.binary, newdata = new))
plotdat <- fit.contin |>
  tidyr::gather(key="Level", value="Probability", A:D)

gg <- ggplot(plotdat, aes(x = age, y = Probability, colour= Level)) +
  geom_line(linewidth = 1.5) +
  facet_grid(~ gender, labeller= label_both) +
  ggtitle("Continuation logits")

direct.label(gg, list("top.bumptwice", dl.trans(y = y + 0.2)))

#' ## Compare predictions
#'
diff <- predict(health.binary, newdata = new) - predict(health.contin, newdata = new)
max(diff)
summary(diff)

#' ## Compare with multinomial model
health.multi <- multinom(product4 ~ age  + gender * household + position_level,
                          data = HealthInsurance)

car::Anova(health.multi)

fit.multi <-data.frame(new,
                 predict(health.multi, newdata=new, type='probs'))

plotdat <- fit.multi |>
  tidyr::gather(key="Level", value="Probability", A:D)

gg <- ggplot(plotdat, aes(x = age, y = Probability, colour= Level)) +
  geom_line(linewidth = 1.5) +
  facet_grid(~ gender, labeller= label_both) +
  ggtitle("Multinomial model")

direct.label(gg, list("top.bumptwice", dl.trans(y = y + 0.2)))

#' ## Compare predicted values

diff <- predict(health.binary, newdata = new) - predict(health.multi, newdata = new, type = "probs")
max(diff)
summary(diff)

# get predicted 'class' from multinom

pred <- predict(health.multi, type = "class")
table(HealthInsurance$product4, pred)

