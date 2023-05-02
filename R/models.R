#' Extract Binary Logit Models from a \code{nestedLogit} Object
#'
#' @name models
#' @aliases models models.nestedLogit
#'
#' @description
#' \code{models} is used to extract \code{"glm"} objects representing binary logit
#' models from a \code{"nestedLogit"} object.
#'
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
#'
#' @examples
#'   data(Womenlf, package = "carData")
#'
#'   comparisons <- logits(work=dichotomy("not.work",
#'                                        working=c("parttime", "fulltime")),
#'                         full=dichotomy("parttime", "fulltime"))
#'   m <- nestedLogit(partic ~ hincome + children,
#'                    dichotomies = comparisons,
#'                    data=Womenlf)
#'
#'   # extract a binomial logit model
#'   models(m, "work")
#'   # use that to plot residuals
#'   plot(density(residuals(models(m, "work"))))
#' @export
models <- function(model, select, as.list=FALSE){
  UseMethod("models")
}

#' @rdname models
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
