#' Methods for \code{"nestedLogit"} and Related Objects
#'
#' @name nestedMethods
#' @aliases nestedMethods print.nestedLogit summary.nestedLogit print.summary.nestedLogit
#' update.nestedLogit predict.nestedLogit print.predictNestedLogit coef.nestedLogit vcov.nestedLogit print.dichotomies
#' as.dichotomies.matrix as.matrix.continuationDichotomies as.character.dichotomies
#' as.matrix.dichotomies
#'
#' @description Various methods for processing \code{"nestedLogit"} and related objects.
#' Most of these are the standard methods for a model-fitting function.
#' \describe{
#'   \item{\code{coef}, \code{vcov}}{Return the coefficients and their variance-covariance matrix respectively.}
#'   \item{\code{update}}{Re-fit a \code{"nestedLogit"} model with a change in any of the \code{formula}, \code{dichotomies},
#'        \code{data}, \code{subset}, or \code{contrasts}, arguments.}
#'   \item{\code{predict}, \code{fitted}}{Computes predicted values from a fitted \code{"nestedLogit"} model.}
#'   \item{\code{confint}}{Compute point-wise confidence limits for predicted response-category
#'        probabilities or logits.}
#'   \item{\code{glance}}{Construct a single row summaries for the dichotomies \code{"nestedLogit"} model.}
#'   \item{\code{tidy}}{Summarizes the terms in \code{"nestedLogit"} model.}
#' }
#'
#' @details
#' The \code{predict} method provides predicted values for two representations of the model.
#' \code{model = "nested"} gives the fitted probabilities for each of the response categories.
#' \code{model = "dichotomies"} gives the fitted log odds for each binary logit models in the
#' dichotomies.
#'
#' @seealso \code{\link{nestedLogit}}, \code{\link{plot.nestedLogit}},
#'          \code{\link{glance.nestedLogit}}, \code{\link{tidy.nestedLogit}}
#'
#' @param x,object in most cases, an object of class \code{"nestedLogit"}.
#' @param newdata For the \code{predict} method, a data frame containing combinations of values of the predictors
#'        at which fitted probabilities (or other quantities) are to be computed.
#' @param model For the \code{predict} and \code{fitted} methods, either \code{"nested"} (the default), in which case fitted probabilities
#' under the nested logit model are returned, or \code{"dichotomies"}, in which case
#' \code{\link{predict.glm}} is invoked for each binary logit model fit to the nested
#' dichotomies and a named list of the results is returned.
#' @param as.matrix if \code{TRUE} (the default for \code{coef}) return coefficients
#'        as a matrix with one column for each nested dichotomy,
#'        or coefficient covariances as a matrix with one row and column for each
#'        combination of dichotomies and coefficients; if \code{FALSE} (the default for
#'        \code{vcov}), return a list of coefficients or coefficient covariances
#'        with one element for each dichotomy.
#' @param formula optional updated model formula.
#' @param dichotomies optional updated dichotomies object.
#' @param data optional updated data argument
#' @param subset optional updated subset argument.
#' @param contrasts optional updated contrasts argument.
#' @param parm either \code{"prob"} (the default) or \code{"logit"}; whether to compute confidence limits for predicted response-category
#'        probabilities or logits.
#' @param level confidence level; the default is \code{0.95}
#' @param conf.limits.logit \code{TRUE} (the default) or \code{FALSE}; when computing confidence limits for predicted probabilities,
#'        do the computation on the logit scale and transform to the probability scale.
#' @param object \code{TRUE} or \code{FALSE} (the drault); if \code{TRUE}, when printing predicted values,
#'        print the predicted probabilities, logits, and their standard errors, or if \code{FALSE},
#'        print a message how to extract these values.
#' @param n the number of rows of the data frames of predicted probabilites, logits, and their
#'        standard errors tp print; if missing, all of the rows will be printed.
#' @param \dots arguments to be passed down.
#'
#' @return  \itemize{
#'    \item The \code{coef} and \code{vcov} methods return either matrices or lists of regression
#'    coefficients and their covariances, respectively.
#'    \item The \code{update} method returns an object of class \code{"nestedLogit"} (see \code{\link{nestedLogit}})
#'    derived from the original nested-logit model.
#'    \item The \code{predict} and \code{fitted} methods return either an object of
#'    class \code{"predictNestedLogit"}, which contains data frames of predicted probabilities with one column for each response category,
#'    predicted response-categories logits computed from the predicted probabilities,
#'    and standard errors for each computed by the delta method; or, if \code{model="dichotomies"}, a data frame of predicted
#'    probabilities with one column for each dichotomous nested logit model.
#'    \item The \code{summary} method returns an object of class \code{"summary.nestedLogit"}, which is
#'    a list of summaries of the \code{\link{glm}} objects that comprise the nested-dichotomies model; the
#'    object is normally printed.
#'    \item The methods for \code{as.matrix}, \code{as.character}, and \code{as.dichotomies} coerce
#'    various objects to matrices, character vectors, and dichotomies objects.
#'    \item The various \code{print} methods invisibly return their \code{x} arguments.
#'    }
#'
#' @author John Fox and Michael Friendly
#' @keywords regression
#' @examples
#' # define continuation dichotomies for level of education
#' cont.dichots <- continuationLogits(c("l.t.highschool",
#'                                      "highschool",
#'                                      "college",
#'                                      "graduate"))
#'
#' # Show dichotomies in various forms
#' print(cont.dichots)
#' as.matrix(cont.dichots)
#' as.character(cont.dichots)
#'
#' # fit a nested model for the GSS data examining education degree in relation to parent & year
#' m <- nestedLogit(degree ~ parentdeg + year,
#'                  cont.dichots,
#'                  data=GSS)
#'
#' coef(m)                             # coefficient estimates
#' sqrt(diag(vcov(m, as.matrix=TRUE))) # standard errors
#' print(m)
#' summary(m)
#'
#' # broom methods
#' broom::glance(m)
#' broom::tidy(m)
#'
#' # predicted probabilities and ploting
#' print(predict(m), object=TRUE, n=5) # fitted probabilities for first 5 cases;
#'                  # equivalent to print(fitted(m), object=TRUE, n=5)
#' new <- expand.grid(parentdeg=c("l.t.highschool",  "highschool",
#'                                "college", "graduate"),
#'                    year=c(1972, 2016))
#' fit <- predict(m, newdata=new)
#' # fitted probabilities and std. erros at specific values of predictors:
#' cbind(new, fit$p, fit$se.p)
#'
#' # predicted logits for each nested-dichotomy model
#' predictions <- predict(m, newdata=new, model="dichotomies", se.fit=TRUE)
#' predictions
#' predictions$above_l.t.highschool # on logit scale
#'

predict.nestedLogit <- function(object, newdata, model=c("nested", "dichotomies"), ...) {
  model <- match.arg(model)
  
  if (missing(newdata))
    newdata <- models(object, 1)$data
  
  if (model == "nested"){
  
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
    
    # result <- lapply(models(object), function(x) as.data.frame(predict(x, newdata=newdata, ...)))
    # attr(result, "model") <- deparse(substitute(object))
    # class(result) <- "predictDichotomies"
    
    result <- lapply(models(object), function(x) predict(x, newdata=newdata, ...))
    result <- as.data.frame(do.call(cbind, result))
    colnames(result) <- names(object$models)
    return(result)
  }
}

# print.predictDichotomies <- function(x, ...){
#   cat("\n predictions for binary logit models from nested logit model:",
#       attr(x, "model"))
#   cat("\n for responses:", paste(names(x), sep=", "))
#   cat(paste0("\n access via $", names(x)[1], " etc."))
#   invisible(x)
# }

#' @rdname nestedMethods
#' @export
print.predictNestedLogit <- function(x, object=FALSE, n, ...){
  if (object){
    if (missing(n)) n <- nrow(x$p)
    cat("\npredicted response-category probabilties\n")
    print(x$p[1:n, ], ...)
    cat("\npredicted response-category logits\n")
    print(x$logit[1:n, ], ...)
    cat("\nstandard errors of predicted probabilities\n")
    print(x$se.p[1:n, ], ...)
    cat("\nstandard errors of predicted logits\n")
    print(x$se.logit[1:n, ], ...)
  } else {
    cat("\n predictions for nested-dichotomies logit model",
        attr(x, "model"))
    cat(paste0("\n\n access via:", 
               "\n   $p for predicted response-category probabilities",
               "\n   $logit for predicted response-category logits",
               "\n   $se.p for predicted probabilities standard errors",
               "\n   $se.logit for predicted logits standard errors"))
    }
  invisible(x)
}

#' @importFrom stats confint
#' @rdname nestedMethods
#' @export
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
