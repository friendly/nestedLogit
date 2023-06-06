# test that Effect.nestedLogit() correctly computes effects and their SEs

data(Womenlf, package = "carData")

m <- nestedLogit(partic ~ hincome + children, 
                  logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                         full=dichotomy("parttime", "fulltime")),
                  data=Womenlf)

eff.1 <- Effect("children", m)
new <- eff.1$x
new$hincome <- mean(Womenlf$hincome)
pred.1 <- predict(m, newdata=new)
test_that("Effects are computed correctly for a numeric predictor", {
  expect_equal(as.vector(eff.1$prob), 
               as.vector(as.matrix(pred.1$p)))
})
test_that("Effect SEs are computed correctly for a numeric predictor", {
  expect_equal(as.vector(eff.1$se.prob), 
               as.vector(as.matrix(pred.1$se.p)))
})

Womenlf$kids <- with(Womenlf, ifelse(children == "present", 1, 0))
mm <- update(m, . ~ . - children + kids, data=Womenlf)
eff.2 <- Effect("hincome", m)
new <- eff.2$x
new$kids <- mean(Womenlf$kids)
pred.2 <- predict(mm, newdata=new)
test_that("Effects are computed correctly for a factor predictor", {
  expect_equal(as.vector(eff.2$prob), 
               as.vector(as.matrix(pred.2$p)))
})
test_that("Effect SEs are computed correctly for a factor predictor", {
  expect_equal(as.vector(eff.2$se.prob), 
               as.vector(as.matrix(pred.2$se.p)))
})
