# old predict.nestedLogit

#' @rdname nestedMethods
#' @importFrom stats predict
#' @export
predict.nestedLogit <- function(object, newdata, model=c("nested", "dichotomies"), ...) {
  model <- match.arg(model)
  if (model == "nested"){
    if (missing(newdata))
      newdata <- models(object, 1)$data
    ndichot <- length(models(object))
    if (ndichot < 2L)
      stop("there are fewer than 2 nested dichotomies")
    fitted <- vector(ndichot, mode = "list")
    for (i in seq_along(models(object))) {
      p <- predict(models(object, i), newdata = newdata, type = "response")
      p <- cbind(1 - p, p)
      attr(p, "columns") <- models(object, i)$dichotomy
      fitted[[i]] <- p
    }
    response.levels <-
      unique(unlist(lapply(fitted, function(x)
        attr(x, "columns"))))
    p <- matrix(1, nrow(newdata), length(response.levels))
    colnames(p) <- response.levels
    for (level in response.levels) {
      for (i in seq_along(models(object))) {
        which <- sapply(models(object, i)$dichotomy, function(x)
          level %in% x)
        if (!any(which))
          next
        p[, level] <- p[, level] * fitted[[i]][, which]
      }
    }
    rownames(p) <- rownames(newdata)
    return(p)
  } else {
    result <- lapply(models(object), predict, newdata=newdata, ...)
    attr(result, "model") <- deparse(substitute(object))
    class(result) <- "predictDichotomies"
    return(result)
  }
}

#' @rdname nestedMethods
#' @export
print.predictDichotomies <- function(x, ...){
  cat("\n predictions for binary logit models from nested logit model:",
      attr(x, "model"))
  cat("\n for responses:", paste(names(x), sep=", "))
  cat(paste0("\n access via $", names(x)[1], " etc."))
}
