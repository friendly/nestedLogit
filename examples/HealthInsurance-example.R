#' ---
#' title: HealthInsurance example
#' ---

# test different dichotomies for 4 level response

library(ggplot2)
library(directlabels)


#' ## define logits as successive binary splits of A, B, C, D
lbinary <- logits(AB_CD = dichotomy(c("A", "B"), c("C", "D")),
                  A_B   = dichotomy("A", "B"),
                  C_D   = dichotomy("C", "D"))

as.matrix(lbinary)

health.nested <- nestedLogit(product4 ~ age  + gender * household + position_level,
                             dichotomies = lbinary, data = HealthInsurance)
car::Anova(health.nested)

#' ## predict and plot
#' 
new <- expand.grid(age=seq(20, 70, by = 5), 
                   gender = c("Female", "Male"),
                   household = mean(HealthInsurance$household),
                   position_level = mean(HealthInsurance$position_level)
                  )

fit <-data.frame(new,
                 predict(health.nested, newdata = new))

plotdat <- fit |>
  tidyr::gather(key="Level", value="Probability", A:D)

gg <- ggplot(plotdat, aes(x = age, y = Probability, colour= Level)) +
  geom_line(linewidth = 1.5) + 
  facet_grid(~ gender, labeller= label_both)

direct.label(gg, list("top.bumptwice", dl.trans(y = y + 0.2)))



#' ## check fitted probabilities

pred <- predict(health.nested, newdata = new)

pred.m.1 <- predict(health.nested$models[[1]], newdata=new, type="response")
pred.m.2 <- predict(health.nested$models[[2]], newdata=new, type="response")
pred.m.3 <- predict(health.nested$models[[3]], newdata=new, type="response")

pred.A <- (1 - pred.m.1)*(1 - pred.m.2)
all.equal(pred.A, pred[, "A"])
pred.B <- (1 - pred.m.1)*pred.m.2
all.equal(pred.B, pred[, "B"])
pred.C <- pred.m.1*(1 - pred.m.3)
all.equal(pred.C, pred[, "C"])
pred.D <-  pred.m.1*pred.m.3
all.equal(pred.D, pred[, "D"])

