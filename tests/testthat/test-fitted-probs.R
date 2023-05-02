# test that nestedLogit()/predict.nested() correctly compute fitted probabilities

m2 <- nestedLogit(degree ~ parentdeg + year, 
                  continuationLogits(c("l.t.highschool",  "highschool", 
                                       "college", "graduate")),
                  data=GSS)

new <- expand.grid(parentdeg=c("l.t.highschool",  "highschool", 
                               "college", "graduate"),
                   year=1972:2016)
pred.GSS <- predict(m2, new)
p.m2.1 <- predict(models(m2, 1), newdata=new, type="response")
p.m2.2 <- predict(models(m2, 2), newdata=new, type="response")
p.m2.3 <- predict(models(m2, 3), newdata=new, type="response")

test_that("fitted probabilities computed correctly, test 1", {
  expect_equal(1 - p.m2.1, pred.GSS[, "l.t.highschool"])
})

test_that("fitted probabilities computed correctly, test 2", {
  expect_equal(p.m2.1*(1 - p.m2.2), pred.GSS[, "highschool"])
})

test_that("fitted probabilities computed correctly, test 3", {
  expect_equal(p.m2.1*p.m2.2*(1 - p.m2.3), pred.GSS[, "college"])
})

test_that("fitted probabilities computed correctly, test 1", {
  expect_equal(p.m2.1*p.m2.2*p.m2.3, pred.GSS[, "graduate"])
})
