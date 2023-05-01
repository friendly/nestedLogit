models <- function(model, select){
  UseMethod("models")
}

models.nestedLogit <- function(model, select){
  if (missing(select)) return(model$models)
  if (is.numeric(select)){
    model.nos <- seq(along=model$models)
    if (any(which <- !(select %in% model.nos))){
      stop("the following models are not available:\n", 
           paste(select[which], collapse=", "))
    }
  } else {
    model.names <- names(model$models)
    if (any(which <- !(select %in% model.names))){
      stop("the following models are not available:\n", 
           paste(select[which], collapse=", "))
    }
  }
  result <- model$models[select]
  if (length(result) > 1) return(result) else return(result[[1]])
}


if (FALSE){
  library(nestedLogit)
  library(car)
  example("nestedLogit")
  
  models(m)
  models(m, 2:1)
  models(m, 1)
  models(m, c("work", "full"))
  models(m, "full")
  
  models(m, "foo") # error
  models(m, 3:4) # error
}
