#' ---
#' title: test nested dichotomies
#' ---

library(nestedLogit)
library(dplyr)
data(Womenlf, package = "carData")

#' ## Fit nested dichotomies 'by hand'

Womenlf <- Womenlf |>
  mutate(partic = ordered(partic, levels = c("not.work", "parttime", "fulltime"))) |>
  mutate(work = ifelse(partic=="not.work", 0, 1)) |>
  mutate(full = case_when(
    work & partic == "fulltime" ~ 1,
    work & partic == "parttime" ~ 0)
  )

wlf.work <- glm(work ~ hincome + children, family=binomial, data=Womenlf)
wlf.full <- glm(full ~ hincome + children, family=binomial, data=Womenlf)

#' ## Use nestedLogit()

wlf.nested <- nestedLogit(partic ~ hincome + children,
                 logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                        full=dichotomy("parttime", "fulltime")),
                 data=Womenlf)

#' ## Compare coefficients
#'
c.hand <- rbind(work = coef(wlf.work),
          full = coef(wlf.full))

c.nest <- t(coef(wlf.nested))

all.equal(c.hand, c.nest)

#' ## Test predict()
#'
new <- expand.grid(hincome=seq(0, 45, length=10),
                    children=c("absent", "present"))

#' ## by hand

#' predictions for the two submodels
p.work     <- predict(wlf.work, new, type='response')
p.fulltime <- predict(wlf.full, new, type='response')

#' calculate unconditional probs for the three response categories

pred.hand <- data.frame(
  not.work =  1 - p.work,
  parttime = p.work * (1 - p.fulltime),
  fulltime = p.work * p.fulltime
)

#' ## `predict.nested()`
pred.nested <- predict(wlf.nested, new)

#' ## Compare predictions
#'

all.equal(pred.hand, as.data.frame(pred.nested),
          check.attributes = FALSE)



