# test that update.nested() works properly

data(Womenlf, package = "carData")

m1 <- nestedLogit(partic ~ hincome, 
                  logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                         full=dichotomy("parttime", "fulltime")),
                  data=Womenlf)

m2 <- nestedLogit(partic ~ hincome + children, 
                  logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                         full=dichotomy("parttime", "fulltime")),
                  data=Womenlf)

m2a <- update(m1, . ~ . + children)
test_that("update.nested() works correctly, formula arg", {
  expect_equal(coef(m2), coef(m2a))
})

Ontario <- Womenlf[Womenlf$region == "Ontario", ]
m3 <- nestedLogit(partic ~ hincome + children, 
                  logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                         full=dichotomy("parttime", "fulltime")),
                  data=Ontario)
m3a <- update(m2, data=Ontario)
test_that("update.nested() works correctly, data arg", {
  expect_equal(coef(m3), coef(m3a))
})

m4 <- nestedLogit(partic ~ hincome + children, 
                  logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                         full=dichotomy("parttime", "fulltime")),
                  data=Womenlf,
                  contrasts=list(children=contr.sum))
m4a <- update(m2, contrasts=list(children=contr.sum))
test_that("update.nested() works correctly, contrasts arg", {
  expect_equal(coef(m4), coef(m4a))
})

