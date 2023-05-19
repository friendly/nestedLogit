predict.nestedLogit <- function(object,
                                newdata,
                                model=c("nested", "dichotomies"), ...) {
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
    result <- list(p = p,
                   logit = logit,
                   se.p = sqrt(v),
                   se.logit = sqrt(v.logit))
    class(result) <- "predictNestedLogit"
    return(result)

  } else {

    result <- lapply(models(object), predict, newdata=newdata, ...)
    attr(result, "model") <- deparse(substitute(object))
    class(result) <- "predictDichotomies"
    return(result)
  }
}
