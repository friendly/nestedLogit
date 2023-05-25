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

## ---- continuation logits exammple

m.gss <- nestedLogit(degree ~ parentdeg + year, 
                  continuationLogits(c("l.t.highschool",  "highschool", 
                                       "college", "graduate")),
                  data=GSS)

new <- expand.grid(parentdeg=c("l.t.highschool",  "highschool", 
                               "college", "graduate"),
                   year=seq(1972, 2016, length=5))
pred.gss <- predict(m.gss, new)

GSS$y1 <- with(GSS, ifelse(degree == "l.t.highschool", 0, 1))
GSS$y2 <- with(GSS, ifelse(y1 == 0, NA,
                           ifelse(degree == "highschool", 0, 1)))
GSS$y3 <- with(GSS, ifelse(degree == "college", 0,
                           ifelse(degree == "graduate", 1, NA)))
xtabs(~ degree + y1, data=GSS)
xtabs(~ degree + y2, data=GSS)
xtabs(~ degree + y3, data=GSS)

m1 <- glm(y1 ~ parentdeg + year, data=GSS, family=binomial)
m2 <- glm(y2 ~ parentdeg + year, data=GSS, family=binomial)
m3 <- glm(y3 ~ parentdeg + year, data=GSS, family=binomial)

pr1 <- predict(m1, newdata=new, type="response", se.fit=TRUE)
p1 <- pr1$fit
v1 <- (pr1$se.fit)^2

pr2 <- predict(m2, newdata=new, type="response", se.fit=TRUE)
p2 <- pr2$fit
v2 <- (pr2$se.fit)^2

pr3 <- predict(m3, newdata=new, type="response", se.fit=TRUE)
p3 <- pr3$fit
v3 <- (pr3$se.fit)^2

p.a <- 1 - p1
p.b <- p1*(1 - p2)
p.c <- p1*p2*(1 - p3)
p.d <- p1*p2*p3

all.equal(cbind(p.a, p.b, p.c, p.d), as.matrix(pred.gss$p), 
          check.attributes=FALSE)

se.p.a <- sqrt(v1)
se.p.b <- sqrt((1 - p2)^2*v1 + p1^2*v2)
se.p.c <- sqrt((p2*(1 - p3))^2*v1 + (p1*(1 - p3))^2*v2 + (p1*p2)^2*v3)
se.p.d <- sqrt((p2*p3)^2*v1 + (p1*p3)^2*v2 + (p1*p2)^2*v3)

all.equal(cbind(se.p.a, se.p.b, se.p.c, se.p.d), as.matrix(pred.gss$se.p), 
          check.attributes=FALSE)
