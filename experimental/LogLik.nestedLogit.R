# also provides BIC() and AIC()

logLik.nestedLogit <- function(object, ...){
  result <- lapply(models(object), logLik)
  logLik <- sum(sapply(result, I))
  df <- sum(sapply(result, function(x) attr(x, "df")))
  nobs <- max(sapply(result, function(x) attr(x, "nobs")))
  result <- logLik
  attr(result, "df") <- df
  attr(result, "nobs") <- nobs
  class(result) <- "logLik"
  result
}

# Example
if (FALSE){
  data(Womenlf, package = "carData")

  # compare different ways of specifying the dichotomies
  m1 <- nestedLogit(partic ~ hincome + children,
                    logits(work=dichotomy("not.work", working=c("parttime", "fulltime")),
                           full=dichotomy("parttime", "fulltime")),
                    data=Womenlf)

  m2 <- nestedLogit(partic ~ hincome + children,
                    logits(work=dichotomy(nonfulltime=c("not.work", "parttime"), "fulltime"),
                           full=dichotomy("not.work", "parttime")),
                    data=Womenlf)
  logLik(m1)
  logLik(m2)
  AIC(m1, m2)
}
