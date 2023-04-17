#' ---
#' title: test nested-methods
#' ---

library(nestedLogit)
data(Womenlf, package = "carData")

m1 <- nestedLogit(partic ~ hincome, 
                 logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                        full=dichotomy("parttime", "fulltime")),
                 data=Womenlf)

m2 <- nestedLogit(partic ~ hincome + children, 
                 logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                        full=dichotomy("parttime", "fulltime")),
                 data=Womenlf)

#' ## Test `anova()` methods
anova(m1)
anova(m2)
anova(m1, m2)

#' ## Test `update()` methods
update(m1, partic ~ hincome + children)
update(m1,. ~ . + children)

summary(update(m2, dichotomies=logits(full=dichotomy(c("not.work", "parttime"), "fulltime"),
                              part=dichotomy("not.work", "parttime"))))

Ontario <- Womenlf[Womenlf$region == "Ontario", ]
update(m1, data=Ontario)

m2.ont <- update(m2, subset = "region == 'Ontario'", contrasts=list(children="contr.sum"))
m2.ont
summary(m2.ont)
