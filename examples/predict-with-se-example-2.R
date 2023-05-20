library(nestedLogit)
source("./experimental/predict-with-se.R")

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
