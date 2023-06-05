
Effect.nestedLogit <- function(focal.predictors, mod, confidence.level=0.95, ...){
  findEffects <- function(m, eff, ...){
    env <- environment()
    mods <- models(m)
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
      effects[[i]] <- Effect(eff, mod, ...)
    }
    effects
  }
  effects <- findEffects(mod, focal.predictors, ...)
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
         lower.prob, upper.prob, response.levels, as.character(formula(m)[2]),
         "multinom")
  class(result) <- "effpoly"
  result
}
