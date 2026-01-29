# marginaleffects methods of nestedLogit models
# see:
library(marginaleffects)

options("marginaleffects_model_classes" = "nestedLogit")

# coef.nestedLogit returns a matrix; one col for each dichotomy
get_coef.nestedLogit <- function(model, ...) {
  b <- coef(model)
  return(b)
}


set_coef.nestedLogit <- function(model, coefs, ...) {
  out <- model
  out$b <- coefs
  return(out)
}

#' @importFrom stats vcov
get_vcov.nestedLogit <- function(model, ...) {
  return(vcov(model))
}

get_predict.nested <- function(model, newdata, ...) {
  # newX <- model.matrix(model$f, data = newdata)
  # Yhat <- newX %*% model$b
  # out <- data.frame(
  #   rowid = seq_len(nrow(Yhat)),
  #   estimate = as.vector(Yhat))
  out <- predict(model, newdata = newdata)
  return(out)
}

