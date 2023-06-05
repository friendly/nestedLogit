
library(effects)
library(nestedLogit)
source("experimental/Effect.nestedLogit.R")

example("nestedLogit")

eff <- Effect("hincome", m)
eff
summary(eff)
plot(eff)
plot(eff, axes=list(y=list(style="stacked")))

lbinary <- logits(AB_CD = dichotomy(c("A", "B"), c("C", "D")),
                  A_B   = dichotomy("A", "B"),
                  C_D   = dichotomy("C", "D"))
health.nested <- nestedLogit(product4 ~ age  + gender * household + position_level,
                             dichotomies = lbinary, data = HealthInsurance)
eff.h <- Effect(c("gender", "household"), health.nested)
eff.h
plot(eff.h)
plot(eff.h, axes=list(y=list(style="stacked")))
