linearHypothesis.nestedLogit <- function(mod, ...) {
  nms <- names(mod$models)
  h <- linearHypothesis(mod$models[[1L]], ...)
  formula <-  as.character(mod$formula)
  heading <- attr(h, "heading")
  heading[4] <- paste("Model 1: restricted model\nModel 2:",
                      paste(formula[2], formula[1], formula[3],
                            collapse = " "))
  for (line in heading){
    cat(paste(line, "\n"))
  }
  attr(h, "heading") <- NULL
  table <- h
  cat(paste0(
    "\nResponse ",
    nms[1L],
    ": {",
    paste(mod$models[[1L]]$dichotomy[[1L]], collapse =
            ", "),
    "} vs. {",
    paste(mod$models[[1L]]$dichotomy[[2L]],
          collapse = ", "),
    "}\n\n"
  ))
  print(h)
  for (i in 2L:length(nms)) {
   cat(paste0(
      "\nResponse ",
      nms[i],
      ": {",
      paste(mod$models[[i]]$dichotomy[[1L]], collapse =
              ", "),
      "} vs. {",
      paste(mod$models[[i]]$dichotomy[[2L]],
            collapse = ", "),
      "}\n\n"
    ))
    h <- linearHypothesis(mod$models[[i]], ...)
    attr(h, "heading") <- NULL
    print(h)
    table <- table + h
  }
  chisq <- table$Chisq[2]
  df <- table$Df[2]
  p <- pchisq(chisq, df, lower.tail=FALSE)
  cat(paste0("\nCombined Responses\nChisq = ", round(chisq, 3), ", Df = ", df, ", p = ", 
             format.pval(p)))
  return(invisible(NULL))
}


if (FALSE){
  library(nestedLogit)
  library(car)
  example("linearHypothesis")
  linearHypothesis(m, "hincome")
  linearHypothesis(m, "hincome = childrenpresent") # nonsense!
}
