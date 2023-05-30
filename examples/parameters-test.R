#' ---
#' title: Test parameters package
#' ---
#'
library(nestedLogit)
library(parameters)
data(Womenlf, package="carData")
comparisons <- logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                      full=dichotomy("parttime", "fulltime"))
wlf.nested <- nestedLogit(partic ~ hincome + children,
                          dichotomies = comparisons,
                          data=Womenlf)

(parm <- parameters(wlf.nested))

parameters(wlf.nested, exponentiate = TRUE)

# see::plot(parm) -- doesn't do anything useful

