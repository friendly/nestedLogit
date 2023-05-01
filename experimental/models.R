#' \code{models} is used to extract \code{"glm"} objects representing binary logit
#' models from a \code{"nestedLogit"} object.

#' @export
models <- function(model, select, as.list=FALSE){
  UseMethod("models")
}

#' @export
models.nestedLogit <- function(model, select, as.list=FALSE){
  if (missing(select)) return(model$models)
  if (is.numeric(select)){
    model.nos <- seq(along=model$models)
    which <- !(select %in% model.nos)
  } else {
    model.names <- names(model$models)
    which <- !(select %in% model.names)
  }
  if (any(which)){
    stop("the following model", if (sum(which) > 1) "s are " else " is ",  
         "not available:\n", paste(select[which], collapse=", "))
  }
  result <- model$models[select]
  if (length(result) > 1 || as.list) return(result) else return(result[[1]])
}

#' @param model a \code{"nestedLogit"} model.
#' @param select a numeric or character vector giving the number(s) or names(s) 
#'        of one or more
#'        binary logit models to be extracted from \code{model}; if absent, a list of 
#'        all of the binary logits models in \code{model} is returned.
#' @param as.list if \code{TRUE} (the default is \code{FALSE}) and one binary logit
#'        model is selected, return the \code{"glm"} object in a one-element named list;
#'        otherwise a single model is returned directly as a \code{"glm"} object; 
#'        when more than one binary
#'        logit model is selected, the corresponding \code{"glm"} objects are \emph{always}
#'        returned as a named list.

if (FALSE){
  library(nestedLogit)
  library(car)
#  example("nestedLogit")

  m <-  nestedLogit(partic ~ hincome + children,
                dichotomies = logits(work=dichotomy("not.work",  working=c("parttime", "fulltime")),
                                     full=dichotomy("parttime", "fulltime")),
                data=Womenlf)

  models(m)
  models(m, 2:1)
  models(m, 1)
  models(m, 1, as.list=TRUE)
  models(m, c("work", "full"))
  models(m, "full")

  models(m, "foo") # error
  models(m, 3:4) # error

  # plot one model -- gives the 'regression quartet for a glm()
  op <- par(mfrow = c(2,2))
  plot(models(m, 1))
  par(op)
}
