linearHypothesis.nestedLogit <- function(mod, ...) {
  nms <- names(mod$models)
  h <- linearHypothesis(mod$models[[1L]], ...)
  formula <-  as.character(mod$formula)
  heading <- attr(h, "heading")
  heading[length(heading) - 1] <- paste("Model 1: restricted model\nModel 2:",
                      paste(formula[2], formula[1], formula[3],
                            collapse = " "))
  for (line in heading){
    cat(paste(line, "\n"))
  }
  attr(h, "heading") <- NULL
  table <- h
  printResponse(nms[1L], mod$models[[1L]]$dichotomy)
  cat("\n")
  print(h)
  for (i in 2L:length(nms)) {
    cat("\n")
    printResponse(nms[i], mod$models[[i]]$dichotomy)
    cat("\n")
    h <- linearHypothesis(mod$models[[i]], ...)
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
printResponse <- function(nm, dichotomy){
    cat(paste0(
    "Response ",
    nm,
    ": ",
    names(dichotomy[1L]),
    "{",
    paste(dichotomy[[1L]], collapse = ", "),
    "} vs. ",
    names(dichotomy[2L]),
    "{",
    paste(dichotomy[[2L]], collapse = ", "),
    "}"
  ))
}


if (FALSE){
  library(nestedLogit)
  library(car)
  example("nestedLogit")
  linearHypothesis(m, "hincome")
  linearHypothesis(m, c("hincome", "childrenpresent"))
  linearHypothesis(m, "hincome = childrenpresent") # nonsense!
}
