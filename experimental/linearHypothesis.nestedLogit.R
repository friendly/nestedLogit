# Make argument conform to car::linearHypothesis generic


#' Test Linear Hypothesis
#'
#' This function tests hypotheses for a \code{"nestedLogit"} model, represented as linear combinations of the
#' coefficients for terms in the model. These tests are presented for each of the dichotomies as well as for
#' the combined response.
#'
#' @param model the fitted model
#' @param ...   arguments to pass down. The second argument is typically the \code{hypothesis.matrix}. See the
#'              Details section of \code{\link[car]{linearHypothesis}}.
#'
#' @return      NULL
#' @seealso \code{\link[car]{linearHypothesis}}
#' @export
#'
#' @examples
#' wlf.nested <- nestedLogit(partic ~ hincome + children,
#'                          logits(work=dichotomy("not.work", c("parttime", "fulltime")),
#'                                 full=dichotomy("parttime", "fulltime")),
#'                          data=Womenlf)
#'
#' linearHypothesis(wlf.nested, "hincome")
#' linearHypothesis(wlf.nested, c("hincome", "childrenpresent"))

linearHypothesis.nestedLogit <- function(model, ...) {
  nms <- names(model$models)
  h <- linearHypothesis(model$models[[1L]], ...)
  formula <-  as.character(model$formula)
  heading <- attr(h, "heading")
  heading[length(heading) - 1] <- paste("Model 1: restricted model\nModel 2:",
                      paste(formula[2], formula[1], formula[3],
                            collapse = " "))
  for (line in heading){
    cat(paste(line, "\n"))
  }
  attr(h, "heading") <- NULL
  table <- h
  cat(composeResponse(nms[1L], model$models[[1L]]$dichotomy), "\n")
  print(h)
  for (i in 2L:length(nms)) {
    cat("\n", composeResponse(nms[i], model$models[[i]]$dichotomy), "\n", sep="")
    h <- linearHypothesis(model$models[[i]], ...)
    attr(h, "heading") <- NULL
    print(h)
    table <- table + h
  }
  chisq <- table$Chisq[2]
  df <- table$Df[2]
  p <- pchisq(chisq, df, lower.tail=FALSE)
  cat(paste0("\nCombined Responses\nChisq = ", round(chisq, 3), ", Df = ", df,
             ", Pr(>Chisq) = ", format.pval(p)))
  return(invisible(NULL))
}

# this can also be used for other functions to simplify code:
composeResponse <- function(name, dichotomy){
  paste0(
    "Response ",
    name,
    ": ",
    names(dichotomy[1L]),
    "{",
    paste(dichotomy[[1L]], collapse = ", "),
    "} vs. ",
    names(dichotomy[2L]),
    "{",
    paste(dichotomy[[2L]], collapse = ", "),
    "}"
  )
}


if (FALSE){
  library(nestedLogit)
  library(car)
  example("nestedLogit")
  linearHypothesis(m, "hincome")
  linearHypothesis(m, c("hincome", "childrenpresent"))
  linearHypothesis(m, "hincome = childrenpresent") # nonsense!
}
