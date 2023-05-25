# this will eventually became a package test

library(nestedLogit)
source("./experimental/predict-with-se.R")
source("./experimental/plot.nested--with-conf-limits.R")

data(Womenlf, package="carData")

comparisons <- logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                      full=dichotomy("parttime", "fulltime"))

wlf.nested <- nestedLogit(partic ~ hincome + children,
                          dichotomies = comparisons,
                          data=Womenlf)
new <- expand.grid(hincome=seq(0, 45, length=4),
                   children=c("absent", "present"))

pred.nested <- predict(wlf.nested, new)

m1 <- models(wlf.nested, "work")
m2 <- models(wlf.nested, "full")

pr1 <- predict(m1, newdata=new, type="response", se.fit=TRUE)
p1 <- pr1$fit
v1 <- (pr1$se.fit)^2

pr2 <- predict(m2, newdata=new, type="response", se.fit=TRUE)
p2 <- pr2$fit
v2 <- (pr2$se.fit)^2

p.nw <- 1 - p1
p.pt <- p1*(1 - p2)
p.ft <- p1*p2
p.nw + p.pt + p.ft

# check that category probabilities are computed correctly
all.equal(cbind(p.nw, p.pt, p.ft), as.matrix(pred.nested$p), 
          check.attributes=FALSE)

se.p.nw <- sqrt(v1)
se.p.pt <- sqrt((1 - p2)^2*v1 + p1^2*v2)
se.p.ft <- sqrt(p2^2*v1 + p1^2*v2)

# check that SEs of category probabilities are comouted correctly
all.equal(cbind(se.p.nw, se.p.pt, se.p.ft), as.matrix(pred.nested$se.p),
          check.attributes=FALSE)
