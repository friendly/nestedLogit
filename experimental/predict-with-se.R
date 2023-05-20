predict.nestedLogit <- function(object, newdata, model=c("nested", "dichotomies"), ...) {
  model <- match.arg(model)
  if (model == "nested"){
    
    if (missing(newdata))
      newdata <- models(object, 1)$data
    ndichot <- length(models(object))
    if (ndichot < 2L)
      stop("there are fewer than 2 nested dichotomies")
    var.fitted <- fitted <- vector(ndichot, mode = "list")
    
    for (j in seq_along(models(object))) {
      p <- predict(models(object, j), newdata = newdata, type = "response")
      p <- cbind(1 - p, p)
      attr(p, "columns") <- models(object, j)$dichotomy
      fitted[[j]] <- p
      fit <- predict(models(object, j), newdata = newdata, type = "link", 
                     se.fit=TRUE)
      lambda <- fit$fit
      var <- (fit$se.fit)^2
      var.fitted[[j]] <- ((exp(lambda)/((1 + exp(lambda))^2))^2) * var
    }
    
    response.levels <- unique(unlist(lapply(fitted, function(x) attr(x, "columns"))))
    p <- matrix(1, nrow(newdata), length(response.levels))
    v <- matrix(0, nrow(newdata), length(response.levels))
    colnames(v) <- colnames(p) <- response.levels
    
    # explanation of indices:
    
    #  k: indexes the m categories of the response
    #  j: indexes the subset of all m - 1 dichotomy models used for 
    #     fitted probabilities for a particular category k
    #     of the response
    #  jp: like j, but also excludes current value of j

    
    for (k in response.levels) {
      
      dev <- rep(1, nrow(newdata))
      
      for (j in seq_along(models(object))) {
        which <- sapply(models(object, j)$dichotomy, function(x) k %in% x)
        if (!any(which)) next
        
        for (jp in seq_along(models(object))){
          if (!any(which) || j == jp) next
          dev <- dev * fitted[[jp]][, which]
        }
        
        p[, k] <- p[, k] * fitted[[j]][, which]
        v[, k] <- v[, k] + dev^2 * var.fitted[[j]]
      }
    }
    
    logit <- log(p/(1 - p))
    v.logit <- (1/(p*(1 - p)))^2 * v
    rownames(v.logit) <- rownames(v) <- rownames(logit) <- rownames(p) <- rownames(newdata)
    result <- list(p = as.data.frame(p), logit = as.data.frame(logit), 
                   se.p = as.data.frame(sqrt(v)), se.logit = as.data.frame(sqrt(v.logit)))
    class(result) <- "predictNestedLogit"
    return(result)
    
  } else {
    
    result <- lapply(models(object), function(x) as.data.frame(predict(x,  newdata=newdata, ...)))
    attr(result, "model") <- deparse(substitute(object))
    class(result) <- "predictDichotomies"
    return(result)
  }
}

print.predictDichotomies <- function(x, ...){
  cat("\n predictions for binary logit models from nested logit model:",
      attr(x, "model"))
  cat("\n for responses:", paste(names(x), sep=", "))
  cat(paste0("\n access via $", names(x)[1], " etc."))
  invisible(x)
}

print.predictNestedLogit <- function(x, object=FALSE, n, ...){
  if (object){
    if (missing(n)) n <- nrow(x$p)
    cat("\npredicted response-category probabilties\n")
    print(x$p[1:n, ], ...)
    cat("\npredicted response-category logits\n")
    print(x$logit[1:n, ], ...)
    cat("\npredicted probabilities standard errors\n")
    print(x$se.p[1:n, ], ...)
    cat("\npredicted logits standard errors\n")
    print(x$se.logit[1:n, ], ...)
  } else {
    cat("\n predictions for nested-dichtomies logit model",
        attr(x, "model"))
    cat(paste0("\n\n access via:", 
               "\n   $p for predicted response-category probabilties",
               "\n   $logit for predicted response-category logits",
               "\n   $se.p for predicted probabilities standard errors",
               "\n   $se.logit for predicted logits standard errors"))
    }
  invisible(x)
}

confint.predictNestedLogit <- function (object, parm=c("prob", "logit"), 
                                        level=0.95, conf.limits.logit=TRUE, ...) {
  parm <- match.arg(parm)
  if (parm == "logit"){
    logit <- object$"logit"
    se <- object$"se.logit"
    z <- qnorm(1 - (1 - level)/2)
    lower <- logit - z*se
    upper <- logit + z*se
    result <- cbind(logit, lower, upper)
    cnames.1 <- colnames(logit)
    cnames.2 <- c("logit", round((1 - level)/2, 4), 
                  round(1 - (1 - level)/2, 4))
    cnames <- paste0(cnames.1, ".", rep(cnames.2, each=ncol(logit)))
    colnames(result) <- cnames
    return(as.data.frame(result))
  } else {
    if (conf.limits.logit){
      p <- object$"p"
      logit <- object$"logit"
      se <- object$"se.logit"
      z <- qnorm(1 - (1 - level)/2)
      lower <- 1/(1 + exp(-(logit - z*se)))
      upper <- 1/(1 + exp(-(logit + z*se)))
    } else {
    p <- object$"p"
    se <- object$"se.p"
    z <- qnorm(1 - (1 - level)/2)
    lower <- p - z*se
    upper <- p + z*se
    }
    result <- cbind(p, lower, upper)
    cnames.1 <- colnames(p)
    cnames.2 <- c("p", round((1 - level)/2, 4), 
                  round(1 - (1 - level)/2, 4))
    cnames <- paste0(cnames.1, ".", rep(cnames.2, each=ncol(p)))
    colnames(result) <- cnames
    return(as.data.frame(result))
  }
}
