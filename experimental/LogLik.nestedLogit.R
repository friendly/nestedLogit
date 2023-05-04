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
