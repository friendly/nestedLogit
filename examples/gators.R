#' ---
#' title: Food choice of alligators
#' ---

# from: https://data.library.virginia.edu/getting-started-with-multinomial-logit-models/

library(car)
library(nnet)
library(ggeffects)

gators <- read.csv('https://static.lib.virginia.edu/statlab/materials/data/table_8-1.csv')
gators$food <- factor(gators$food,
                      levels = c("O", "F", "I"),
                      labels = c("Other", "Fish", "Invertebrates"))

gator.multi <- multinom(food ~ length, data = gators,
              Hess = TRUE, trace = FALSE)

summary(gator.multi, Wald.ratios = TRUE)
confint(gator.multi)

Anova(gator.multi)

pred <- predict(gator.multi, type = "probs")


eff <- ggeffect(gator.multi, terms = "length[1:4,by=0.5]")
ggplot(eff) +
  aes(x = x, y = predicted, fill = response.level, color = response.level) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 1/3) +
  labs(x = 'Length of Alligator', y = 'Predicted Probability') +
  ylim(c(0, 1))


#' ## use nested logits

dichot <- logits(d1=dichotomy("Other", c("Fish", "Invertebrates")),
                 d2=dichotomy("Fish", "Invertebrates"))

gator.nested <- nestedLogit(food ~ length,
                            dichotomies = dichot,
                            data = gators)

# plot.nested doesn't work, because there are no 'others' variables
#plot(gator.nested, x.var = "length")

new <- data.frame(length = seq(1, 4, by = 0.25))
plotdat <- cbind(new, predict(gator.nested, newdata = new))
matplot(plotdat[, "length"], plotdat[, 2:4],
        type ="b",
        xlab = "Length",
        ylab = "Probability")
