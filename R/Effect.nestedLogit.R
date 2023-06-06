#' Effect Displays for Nested Logit Models
#'
#' @name Effect.nestedLogit
#' @aliases Effect.nestedLogit
#' @description Computes effects (in the sense of the \pkg{effects} package---see, in
#' particular, \code{\link[effects]{Effect}})---for \code{"nestedLogit"} models, which then
#' can be used with other functions in the \pkg{effects} package, for example, 
#' \code{\link[effects]{predictorEffects}} and to produce effect plots.
#' 
#' @seealso  \code{\link[effects]{Effect}}), \code{\link[effects]{plot.effpoly}},
#' \code{\link[effects]{predictorEffects}})
#' 
#' @param focal.predictors a character vector of the names of one or more of 
#' the predictors in the model, for which the effect display should be computed.
#' @param mod a \code{"nestedLogit"} model object.
#' @param confidence.level for point-wise confidence bands around the effects
#' (the default is \code{0.95}).
#' @param fixed.predictors controls the values at which other predictors are fixed;
#' see \code{\link[effects]{Effect}} for details; if \code{NULL} (the default),
#' numeric predictors are set to their means, factors to their distribution in the data.
#' @param ... optional arguments to be passed to the \code{\link[effects]{Effect}} method for
#' binary logit models (fit by the \code{\link{glm}} function).
#' @return an object of class \code{"effpoly"} (see \code{\link[effects]{Effect}}).
#' @author John Fox 
#' @keywords regression
#' @references
#' John Fox and Sanford Weisberg (2019). \emph{An R Companion to Applied Regression},
#' 3rd Edition. Sage, Thousand Oaks, CA.
#' 
#' John Fox, Sanford Weisberg (2018). Visualizing Fit and Lack of Fit in Complex
#' Regression Models with Predictor Effect Plots and Partial Residuals. 
#' \emph{Journal of Statistical Software}, 87(9), 1-27.
#' @examples
#' data("Womenlf", package = "carData")
#' comparisons <- logits(work=dichotomy("not.work",
#'                                      working=c("parttime", "fulltime")),
#'                       full=dichotomy("parttime", "fulltime"))
#' m <- nestedLogit(partic ~ hincome + children,
#'                    dichotomies = comparisons,
#'                    data=Womenlf)
#' peff.women <- effects::predictorEffects(m)
#' plot(peff.women)
#' plot(peff.women, axes=list(y=list(style="stacked")))
#' summary(peff.women)
#' 
#' dichots <- logits(AB_CD = dichotomy(c("A", "B"), c("C", "D")),
#'                   A_B   = dichotomy("A", "B"),
#'                   C_D   = dichotomy("C", "D"))
#' m.health <- nestedLogit(product4 ~ age + gender*household + position_level,
#'                         dichotomies = dichots, data = HealthInsurance)
#' eff.gen.hh <- effects::Effect(c("gender", "household"), m.health,
#'                               xlevels=list(household=0:7))
#' eff.gen.hh
#' plot(eff.gen.hh)
#' plot(eff.gen.hh, axes=list(y=list(style="stacked")))

#' @importFrom effects Effect
#' @importFrom stats model.matrix
#' @exportS3Method effects::Effect nestedLogit
Effect.nestedLogit <- function(focal.predictors, mod, confidence.level=0.95, 
                               fixed.predictors=NULL, ...){
  findEffects <- function(m, eff, fixed.predictors, ...){
    env <- environment()
    mods <- models(m)
    
    biggest <- which.max(sapply(mods, function(x) nrow(model.matrix(x))))
    formula <- mods[[biggest]]$formula
    environment(formula) <- env
    data <- mods[[biggest]]$data
    environment(data) <- env
    names(data)[ncol(data)] <- as.character(formula[[2]])
    mod <- glm(formula, data=data, family=binomial)
    given <- colMeans(model.matrix(mod)[, -1])
    if (!is.null(fixed.predictors) &&
        !is.null(fixed.predictors$given.values)) 
      given[names(fixed.predictors$given.values)] <- fixed.predictors$given.values
    if (!is.null(fixed.predictors)){
      fixed.predictors$given.values <- given
    } else {
      fixed.predictors <- list(given.values=given)
    }
    
    effects <- vector(length(mods), mode="list")
    names(effects) <- names(mods)
    for (i in seq_along(mods)){
      # refit binomial logit models
      formula <- mods[[i]]$formula
      environment(formula) <- env
      data <- mods[[i]]$data
      environment(data) <- env
      names(data)[ncol(data)] <- as.character(formula[[2]])
      mod <- glm(formula, data=data, family=binomial)
      # find effects for binomial logit models
      effects[[i]] <- Effect(eff, mod, fixed.predictors=fixed.predictors, ...)
    }
    effects
  }
  effects <- findEffects(mod, focal.predictors, fixed.predictors, ...)
  logits <- lapply(effects, function(ef) as.vector(ef$fit))
  ps <- lapply(logits, function(x) 1/(1 + exp(-x)))
  ses.lam <- lapply(effects, function(ef) ef$se)
  var.p <- mapply(function(x, y) (exp(x)/(1 + exp(x))^2)^2 * y^2, logits, ses.lam,
                 SIMPLIFY=FALSE)
  names(logits) <- names(ps) <- names(var.p) <- names(ses.lam)
  ndichot <- length(models(mod))
  var.fitted <- fitted <- vector(ndichot, mode = "list")
  for (j in 1:ndichot) {
    p <- cbind(1 - ps[[j]], ps[[j]])
    attr(p, "columns") <- models(mod, j)$dichotomy
    fitted[[j]] <- p
    var.fitted[[j]] <- var.p[[j]]
  }
  
  response.levels <- unique(unlist(lapply(fitted, function(x) attr(x, "columns"))))
  
  # compute fitted effects for nested logit model from
  #  effects for binary logit models
  
  p <- matrix(1, nrow(fitted[[1]]), length(response.levels))
  v <- matrix(0, nrow(fitted[[1]]), length(response.levels))
  colnames(v) <- colnames(p) <- response.levels
  
  for (k in response.levels) {
    
    for (j in 1:ndichot) {
      
      deriv <- rep(1, nrow(fitted[[1]]))
      
      which.j <- sapply(models(mod, j)$dichotomy, function(x) k %in% x)
      if (!any(which.j)) next
      
      for (jp in 1:ndichot){
        which.jp <- sapply(models(mod, jp)$dichotomy, function(x) k %in% x)
        if (j == jp || !any(which.jp)) next
        deriv <- deriv * fitted[[jp]][, which.jp]
      }
      
      p[, k] <- p[, k] * fitted[[j]][, which.j]
      v[, k] <- v[, k] + deriv^2 * var.fitted[[j]]
    }
  }
  
  logit <- log(p/(1 - p)) 
  se.logit <- sqrt((1/(p*(1 - p)))^2 * v)
  z <- qnorm((1 - confidence.level)/2, lower.tail=FALSE)
  lower.logit <- logit - z*se.logit
  upper.logit <- logit + z*se.logit
  lower.prob <- (1/(1 + exp(- lower.logit)))
  upper.prob <- (1/(1 + exp(- upper.logit)))
  
  # fake a multinom "effpoly" object
  
  result <- effects[[1]]
  result[c("prob", "logit", "se.prob", "se.logit", 
           "lower.logit", "upper.logit", "lower.prob", "upper.prob",
           "y.levels", "response", "model")] <-
    list(p, logit, sqrt(v), se.logit, lower.logit, upper.logit,
         lower.prob, upper.prob, response.levels, as.character(formula(mod)[2]),
         "multinom")
  class(result) <- "effpoly"
  result
}
