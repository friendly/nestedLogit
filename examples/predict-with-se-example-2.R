library(nestedLogit)
source("./experimental/predict-with-se.R")
source("./experimental/plot.nested--with-conf-limits.R")

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
pred.nested
print(pred.nested, object=TRUE)

confint(pred.nested)
confint(pred.nested, parm="logit")
confint(pred.nested, conf.limits.logit=FALSE) # note some limits < 0 and > 1

plot(wlf.nested, "hincome", list(children="absent"),
  xlab="Husband's Income", legend.location="top")

plot(wlf.nested, "hincome", list(children="absent"),
     xlab="Husband's Income", legend.location="top", conf.alpha=0.1)

plot(wlf.nested, "hincome", list(children="absent"),
     xlab="Husband's Income", legend.location="top", conf.level=0.5)

plot(wlf.nested, "children", list(hincome=round(mean(Womenlf$hincome))),
     legend.location="top")

plot(wlf.nested, "children", list(hincome=round(mean(Womenlf$hincome))),
     legend.location="top", conf.level=0.5)

plot(wlf.nested, "hincome", list(children="absent"),
     xlab="Husband's Income", legend.location="top", conf.level=NULL)

plot(wlf.nested, "children", list(hincome=round(mean(Womenlf$hincome))),
     legend.location="top", conf.level=NULL)

pred.dichotomies <- predict(wlf.nested, model="dichotomies", newdata=new)
pred.dichotomies
str(pred.dichotomies)

pred.dichotomies <- predict(wlf.nested, model="dichotomies")
head(pred.dichotomies)
tail(pred.dichotomies)
str(pred.dichotomies)

# compare to multinomial logit model

library(nnet)
library(effects)

m <- multinom(partic ~ hincome + children, data=Womenlf)
plot(Effect(c("hincome", "children"), m))

comparisons <- logits(full=dichotomy(c("not.work", "parttime"), "fulltime"),
                      part=dichotomy("not.work", "parttime"))

n <- nestedLogit(partic ~ hincome + children,
                 dichotomies = comparisons,
                 data=Womenlf)

plot(n, "hincome", list(children="absent"),
     xlab="Husband's Income", legend.location="top")

library(nnet)
library(effects)

m <- multinom(partic ~ hincome + children, data=Womenlf)
plot(Effect(c("hincome", "children"), m))

comparisons <- logits(full=dichotomy(c("not.work", "parttime"), "fulltime"),
                      part=dichotomy("not.work", "parttime"))

n <- nestedLogit(partic ~ hincome + children,
                 dichotomies = comparisons,
                 data=Womenlf)

plot(n, "hincome", list(children="absent"),
     xlab="Husband's Income", legend.location="top")

library(nnet)
library(effects)

m <- multinom(partic ~ hincome + children, data=Womenlf)
plot(Effect(c("hincome", "children"), m))

comparisons <- logits(full=dichotomy(c("not.work", "parttime"), "fulltime"),
                      part=dichotomy("not.work", "parttime"))

n <- nestedLogit(partic ~ hincome + children,
                 dichotomies = comparisons,
                 data=Womenlf)

plot(n, "hincome", list(children="absent"),
     xlab="Husband's Income", legend.location="top")

library(nnet)
library(effects)

m <- multinom(partic ~ hincome + children, data=Womenlf)
plot(Effect(c("hincome", "children"), m))

comparisons <- logits(full=dichotomy(c("not.work", "parttime"), "fulltime"),
                      part=dichotomy("not.work", "parttime"))

n <- nestedLogit(partic ~ hincome + children,
                 dichotomies = comparisons,
                 data=Womenlf)

plot(n, "hincome", list(children="absent"),
     xlab="Husband's Income", legend.location="top")

head(predict(m, type="probs")[, c("not.work", "parttime", "fulltime")])
head(predict(n)$p)
diag(cor(predict(m, type="probs")[, c("not.work", "parttime", "fulltime")], predict(n)$p))
apply(abs(predict(m, type="probs")[, c("not.work", "parttime", "fulltime")] - predict(n)$p), 2, mean)
