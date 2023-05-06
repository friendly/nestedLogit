#' Hypothesis-Testing and Related Methods for \code{"nestedLogit"} Objects
#'
#' @name nestedHypotheses
#' @aliases nestedHypotheses Anova.nestedLogit print.Anova.nestedLogit 
#' anova.nestedLogit print.anova.nestedLogit logLik.nestedLogit
#'
#' @description Various methods for testing hypotheses about nested logit models.
#' \describe{
#'   \item{\code{Anova}}{Calculates type-II or type-III analysis-of-variance tables for \code{"nestedLogit"} objects;
#'   see \code{\link[car]{Anova}} in the \pkg{car} package.}
#'   \item{\code{anova}}{Computes sequential analysis of variance (or deviance) tables for one or more fitted 
#'   \code{"nestedLogit"}  objects; see \code{\link{anova}}.}
#'   \item{\code{linearHypothesis}}{Computes Wald tests for linear hypotheses;
#'   see \code{\link[car]{linearHypothesis}} in the \pkg{car} package.}
#'   \item{\code{logLik}}{Returns the log-likelihood and degrees of freedom for the nested-dichotomies model.
#'   (and through it \code{\link{AIC}} and \code{\link{BIC}} model-comparison statistics).}
#' }
#' @param x,object,object2,mod,model in most cases, an object of class \code{"nestedLogit"}.
#' @param \dots arguments to be passed down. In the case of \code{linearHypothesis},
#'        the second argument is typically the \code{hypothesis.matrix}. See the
#'        Details section of \code{\link[car]{linearHypothesis}}. In the case of \code{anova},
#'        additional sequential \code{"nestedLogit"} models.
#'
#' @seealso \code{\link[car]{Anova}}, \code{\link{anova}}, 
#' \code{\link[car]{linearHypothesis}}, \code{\link{logLik}}, \code{\link{AIC}},
#' \code{\link{BIC}}
#' @author John Fox
#' @keywords regression
#' @examples
#' # define continuation dichotomies for level of education
#' cont.dichots <- continuationLogits(c("l.t.highschool",
#'                                      "highschool",
#'                                      "college",
#'                                      "graduate"))
#' # fit a nested model for the GSS data examining education degree in relation to parent & year
#' m <- nestedLogit(degree ~ parentdeg + year,
#'                  cont.dichots,
#'                  data=GSS)
#'                  
#' # Anova and anova tests
#' car::Anova(m) # type-II (partial) tests
#'
#' anova(update(m, . ~ . - year), m) # model comparison
#'
#' # Wald test
#' car::linearHypothesis(m, c("parentdeghighschool", "parentdegcollege",
#'                            "parentdeggraduate"))
#'
#' # log-liklihood, AIC, and BIC
#' logLik(m)
#' AIC(m)
#' BIC(m)

#' @rdname nestedHypotheses
#' @importFrom car Anova
#' @exportS3Method car::Anova nestedLogit
#' @export
Anova.nestedLogit <- function(mod, ...) {
  result <- lapply(models(mod), Anova, ...)
  nms <- names(models(mod))
  heading <- attr(result[[1L]], "heading")[1L]
  heading <- sub("Table", "Tables", heading)
  for (i in seq(along = result)) {
    attr(result[[i]], "heading") <- composeResponse(nms[i], models(mod, i)$dichotomy)  
  }
  attr(result, "heading") <- heading
  class(result) <- "Anova.nestedLogit"
  result
}

#' @rdname nestedHypotheses
#' @exportS3Method print Anova.nestedLogit
print.Anova.nestedLogit <- function(x, ...) {
  if (length(x) < 2L) {
    return(invisible(print(x[[1L]], ...)))
  }
  cat("\n", attr(x, "heading"), "\n")
  table <- print(x[[1L]], ...)
  for (i in 2L:length(x)) {
    cat("\n\n")
    table <- table + print(x[[i]], ...)
  }
  table[, 3L] <- pchisq(table[, 1L], table[, 2L], lower.tail = FALSE)
  attr(table, "heading") <- "Combined Responses"
  class(table) <- c("anova", "data.frame")
  cat("\n\n")
  print(table)
  invisible(x)
}

#' @rdname nestedHypotheses
#' @importFrom car linearHypothesis
#' @exportS3Method car::linearHypothesis nestedLogit
#' @export
linearHypothesis.nestedLogit <- function(model, ...) {
  nms <- names(models(model))
  h <- car::linearHypothesis(models(model, 1L), ...)  
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
  cat(composeResponse(nms[1L], models(model, 1L)$dichotomy), "\n") 
  print(h)
  for (i in 2L:length(nms)) {
    cat("\n", composeResponse(nms[i], models(model, i)$dichotomy), "\n", sep="")  
    h <- car::linearHypothesis(models(model, i), ...)
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

#' @rdname nestedHypotheses
#' @export
anova.nestedLogit <- function(object, object2, ...){
  if (missing(object2)){
    result <- lapply(models(object), anova, test="LRT")
    heading <- attr(result[[1L]], "heading")[1L]
    heading <- sub("Table", "Tables", heading)
    heading <- sub("\\.\\.y", as.character(object$formula[[2]]), heading)
    heading <- sub("binomial, link: logit", "Nested Logit", heading)
  } else {
    if (!inherits(object2, "nestedLogit"))
      stop(deparse(substitute(object2)), " is not of class 'nestedLogit'")
    result <- mapply(anova, models(object), models(object2), MoreArgs=list(test="LRT"), SIMPLIFY=FALSE)
    heading <- attr(result[[1L]], "heading")
    heading <- sub("Table", "Tables", heading)
    heading <- gsub("\\.\\.y", as.character(object$formula[[2]]), heading)
    heading <- sub("Model 2", " Model 2", heading)
    heading <- c(heading, "\n")
  }
  nms <- names(models(object))
  for (i in seq(along = result)) {
    attr(result[[i]], "heading") <- composeResponse(nms[i], models(object, i)$dichotomy) 
  }
  attr(result, "heading") <- heading
  class(result) <- "anova.nestedLogit"
  result
}

#' @rdname nestedHypotheses
#' @exportS3Method print anova.nestedLogit
print.anova.nestedLogit <- function(x, ...) {
  if (length(x) < 2L) {
    return(invisible(print(x[[1L]], ...)))
  }
  cat("\n", attr(x, "heading"), "\n")
  table <- print(x[[1L]], ...)
  for (i in 2L:length(x)) {
    cat("\n\n")
    table <- table + print(x[[i]], ...)
  }
  table[, "Pr(>Chi)"] <- pchisq(table[, "Deviance"], table[, "Df"], lower.tail = FALSE)
  attr(table, "heading") <- "Combined Responses"
  class(table) <- c("anova", "data.frame")
  cat("\n\n")
  print(table)
  invisible(x)
}

# also provides BIC() and AIC():

#' @rdname nestedHypotheses
#' @exportS3Method stats::logLik nestedLogit
#' @export
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
