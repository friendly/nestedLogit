library(nestedLogit)
data(Womenlf, package="carData")

comparisons <- logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                      full=dichotomy("parttime", "fulltime"))

wlf.nested <- nestedLogit(partic ~ hincome + children,
                          dichotomies = comparisons,
                          data=Womenlf)

new <- expand.grid(hincome=seq(0, 45, length=4),
                   children=c("absent", "present"))

pred.nested <- predict(wlf.nested, new)
pred.nested
