# test that predict.nestedLogit() correctly computes SEs of fitted probabilities

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

se.p.a <- sqrt(v1)
se.p.b <- sqrt((1 - p2)^2*v1 + p1^2*v2)
se.p.c <- sqrt((p2*(1 - p3))^2*v1 + (p1*(1 - p3))^2*v2 + (p1*p2)^2*v3)
se.p.d <- sqrt((p2*p3)^2*v1 + (p1*p3)^2*v2 + (p1*p2)^2*v3)

test_that("SEs of fitted probabilities computed correctly", {
  expect_equal(as.vector(cbind(se.p.a, se.p.b, se.p.c, se.p.d)), 
               as.vector(as.matrix(pred.gss$se.p)))
})
