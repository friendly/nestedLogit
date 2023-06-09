
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

# tests of correctness

eff.1 <- Effect("children", m)
new <- eff.1$x
new$hincome <- mean(Womenlf$hincome)
pred.1 <- predict(m, newdata=new)
all.equal(eff.1$prob, as.matrix(pred.1$p), check.attributes=FALSE)
all.equal(eff.1$se.prob, as.matrix(pred.1$se.p), check.attributes=FALSE)

Womenlf$kids <- with(Womenlf, ifelse(children == "present", 1, 0))
mm <- update(m, . ~ . - children + kids, data=Womenlf)
eff.2 <- Effect("hincome", m)
new <- eff.2$x
new$kids <- mean(Womenlf$kids)
pred.2 <- predict(mm, newdata=new)
all.equal(eff.2$prob, as.matrix(pred.2$p), check.attributes=FALSE)
all.equal(eff.2$se.prob, as.matrix(pred.2$se.p), check.attributes=FALSE)

eff.3 <- Effect("hincome", m, 
                 fixed.predictors=list(given.values=c(childrenpresent=0.5)))
new <- eff.3$x
new$kids <- c(kids=0.5)
pred.3 <- predict(mm, newdata=new)
all.equal(eff.3$prob, as.matrix(pred.3$p), check.attributes=FALSE)
all.equal(eff.3$se.prob, as.matrix(pred.3$se.p), check.attributes=FALSE)

new <- eff.h$x
new$age <- with(HealthInsurance, mean(age))
new$position_level <- with(HealthInsurance, mean(position_level))
pred.h <- predict(health.nested, newdata=new)
all.equal(eff.h$prob, as.matrix(pred.h$p), check.attributes=FALSE)
all.equal(eff.h$se.prob, as.matrix(pred.h$se.p), check.attributes=FALSE)
all.equal(eff.h$logit, as.matrix(pred.h$logit), check.attributes=FALSE)
all.equal(eff.h$se.logit, as.matrix(pred.h$se.logit), check.attributes=FALSE)

# tests with missing data

H <- HealthInsurance
H[2, 2] <- H[3, 3] <- H[4, 4] <- H[5, 5] <- H[6, 6] <- NA
h.n <- update(health.nested, data=H)
eff.h <- Effect(c("gender", "household"), h.n)
eff.h
plot(eff.h)
plot(eff.h, axes=list(y=list(style="stacked")))
new <- eff.h$x
H <- na.omit(H[, 2:6])
new$age <- with(H, mean(age))
new$position_level <- with(H, mean(position_level))
pred.h <- predict(h.n, newdata=new)
all.equal(eff.h$prob, as.matrix(pred.h$p), check.attributes=FALSE)
all.equal(eff.h$se.prob, as.matrix(pred.h$se.p), check.attributes=FALSE)
all.equal(eff.h$logit, as.matrix(pred.h$logit), check.attributes=FALSE)
all.equal(eff.h$se.logit, as.matrix(pred.h$se.logit), check.attributes=FALSE)

# check whether other Effect() args work

Effect("hincome", m, xlevels=10)
Effect("hincome", m, xlevels=list(hincome=seq(5, 45, by=5)))

plot(Effect("hincome", m))
plot(Effect("hincome", m, confidence.level=0.50))
