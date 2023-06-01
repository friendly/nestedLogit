#' Methods for \code{"nestedLogit"} and Related Objects
#'
#' @name nestedMethods
#' @aliases nestedMethods print.nestedLogit summary.nestedLogit print.summary.nestedLogit
#' update.nestedLogit predict.nestedLogit coef.nestedLogit vcov.nestedLogit print.dichotomies
#' as.dichotomies.matrix as.matrix.continuationDichotomies as.character.dichotomies
#' as.matrix.dichotomies print.predictNestedLogit confint.predictNestedLogit
#' fitted.nestedLogit as.dichotomies predict.nestedLogit
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
#' @param n For the print method of \code{predict.nestedLogit}
#'        or \code{predictDichotomies}, an integer or \code{"all"}
#'        to control how many rows are printed for each of the probabilities of
#'        response categories, corresponding logits and their standard errors.
#' @param parm  For the \code{confint} method, one of \code{"prob"} or \code{"logit"},
#'        indicating whether to generate confidence intervals for probabilities or logits
#'        of the responses.
#' @param  level Confidence level for the \code{confint} method
#' @param conf.limits.logit When \code{parm = "prob"} ?????
#' @param \dots arguments to be passed down.
#'
#' @return  \itemize{
#'    \item The \code{coef} and \code{vcov} methods return either matrices or lists of regression
#'    coefficients and their covariances, respectively.
#'    \item The \code{update} method returns an object of class \code{"nestedLogit"} (see \code{\link{nestedLogit}})
#'    derived from the original nested-logit model.
#'    \item The \code{predict} and \code{fitted} methods return either a matrix of predicted probabilities or an
#'    object of class \code{"predictDichotomies"}, which is a named list with predicted logits for
#'    each nested-dichotomy model.
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
#' predict(m) # fitted probabilities for first few cases;
#'
#' new <- expand.grid(parentdeg=c("l.t.highschool",  "highschool",
#'                                "college", "graduate"),
#'                    year=c(1972, 2016))
#' fit <- predict(m, newdata=new)
#' as.data.frame(new, fit) # fitted probabilities at specific values of predictors
#'
#' # predicted logits for dichotomies
#' predictions <- predict(m, newdata=new, model="dichotomies")
#' predictions
#'
#' @rdname nestedMethods
#' @export
print.nestedLogit <- function(x, ...) {
  cat("Nested logit models: ")
  print(x$formula)
  if (!is.null(x$subset)) cat("subset: ", x$subset, "\n")
  if ("NULL" != x$contrasts.print) cat("contrasts: ", x$contrasts.print, "\n")
  lapply(models(x), print, ...)
  invisible(x)
}

#' @rdname nestedMethods
#' @export
summary.nestedLogit <- function(object, ...) {
  result <- lapply(models(object), summary, ...)
  for (i in seq_along(result)) {
    result[[i]]$dichotomy <- models(object, i)$dichotomy
  }
  class(result) <- "summary.nestedLogit"
  attr(result, "formula") <- object$formula
  attr(result, "subset") <- object$subset
  attr(result, "contrasts.print") <- object$contrasts.print
  result
}

#' @rdname nestedMethods
#' @exportS3Method print summary.nestedLogit
print.summary.nestedLogit <- function(x, ...) {
  cat("Nested logit models: ")
  print(attr(x, "formula"))
  if (!is.null(subset <- attr(x, "subset"))){
    cat("subset: ", subset, "\n")
  }
  if ("NULL" != (contrasts <- attr(x, "contrasts.print"))){
    cat("contrasts: ", contrasts, "\n")
  }
  cat("\n")
  nms <- names(x)
  for (i in seq(along = x)) {
    cat(composeResponse(nms[i], x[[i]]$dichotomy))
    print(x[[i]], ...)
  }
  invisible(x)
}

#' @rdname nestedMethods
#' @export
print.dichotomies <- function(x, ...) {
  nms <- names(x)
  for (i in seq_along(x)) {
    cat(paste0(
      nms[i],
      ": ",
      names(x[[i]][1L]),
      "{",
      paste(x[[i]][[1L]], collapse = ", "),
      "} vs. ",
      names(x[[i]][2L]),
      "{",
      paste(x[[i]][[2L]], collapse = ", "),
      "}\n"
    ))
  }
  invisible(x)
}


#' @rdname nestedMethods
#' @importFrom stats predict
#' @export
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
      pred <- predict(models(object, j), newdata = newdata, type = "response",
                      se.fit=TRUE)
      p <- cbind(1 - pred$fit, pred$fit)
      attr(p, "columns") <- models(object, j)$dichotomy
      fitted[[j]] <- p
      var.fitted[[j]] <- (pred$se.fit)^2
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

      for (j in seq_along(models(object))) {

        deriv <- rep(1, nrow(newdata))

        which.j <- sapply(models(object, j)$dichotomy, function(x) k %in% x)
        if (!any(which.j)) next

        for (jp in seq_along(models(object))){
          which.jp <- sapply(models(object, jp)$dichotomy, function(x) k %in% x)
          if (j == jp || !any(which.jp)) next
          deriv <- deriv * fitted[[jp]][, which.jp]
        }

        p[, k] <- p[, k] * fitted[[j]][, which.j]
        v[, k] <- v[, k] + deriv^2 * var.fitted[[j]]
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
    result <- lapply(models(object),
                     function(x) as.data.frame(predict(x, newdata=newdata, se.fit=TRUE, ...)))
    attr(result, "model") <- deparse(substitute(object))
    class(result) <- "predictDichotomies"
    return(result)
  }
}

#' @rdname nestedMethods
#' @export
print.predictNestedLogit <- function(x, n=min(10L, nrow(x$p)), ...){
  if (n == "all") n <- nrow(x$p)
  if (truncate <- nrow(x$p) > n) cat(paste0("\nFirst ", n, " of ", nrow(x$p), " rows:\n"))
  cat("\npredicted response-category probabilties\n")
  print(x$p[1:n, ], ...)
  if (truncate) cat("  . . .\n")
  cat("\npredicted response-category logits\n")
  print(x$logit[1:n, ], ...)
  if (truncate) cat("  . . .\n")
  cat("\nstandard errors of predicted probabilities\n")
  print(x$se.p[1:n, ], ...)
  if (truncate) cat("  . . .\n")
  cat("\nstandard errors of predicted logits\n")
  print(x$se.logit[1:n, ], ...)
  if (truncate) cat("  . . .\n")
  invisible(x)
}

#' @importFrom stats confint qnorm
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

#' @rdname nestedMethods
#' @export
print.predictDichotomies <- function(x, n=10L, ...){
  cat("\n predictions for binary logit models from nested logit model:",
      attr(x, "model"), "\n")
  nms <- names(x)
  for (i in seq_along(x)){
    if (n == "all") n <- nrow(x[[i]])
    cat("\n dichotomy:", nms[i], "\n")
    print(x[[i]][1:min(n, nrow(x[[i]])), ])
  }
  invisible(x)
}


#' @rdname nestedMethods
#' @importFrom stats fitted
#' @export
fitted.nestedLogit <- function(object, model=c("nested", "dichotomies"), ...){
  predict(object, model=model)
}

#' @rdname nestedMethods
#' @importFrom stats coef
#' @export
coef.nestedLogit <- function(object, as.matrix=TRUE, ...) {
  result <- if(as.matrix) sapply(models(object), coef, ...)
  else lapply(models(object), coef, ...)
  result
}

#' @rdname nestedMethods
#' @export
vcov.nestedLogit <- function(object, as.matrix=FALSE, ...){
  vcovs <- lapply(models(object), vcov)
  if (!as.matrix) {
    return(vcovs)
  }
  n.models <- length(models(object))
  n.coefs <- nrow(vcovs[[1]])
  nms <- expand.grid(names(coef(models(object, 1L))), names(models(object)))
  nms <- paste0(nms[, 2], ".", nms[, 1])
  vcov <- matrix(0, n.models*n.coefs, n.models*n.coefs)
  rownames(vcov) <- colnames(vcov) <- nms
  for (i in 1:n.models){
    start <- (i - 1)*n.coefs + 1
    vcov[start:(start + n.coefs - 1), start:(start + n.coefs - 1)] <- vcovs[[i]]
  }
  vcov
}

#' @rdname nestedMethods
#' @export
update.nestedLogit <- function(object, formula, dichotomies, data, subset, contrasts,...){
  formula <- if (missing(formula)) {
    object$formula
  } else {
    update(object$formula, formula)
  }
  if (missing(dichotomies)) dichotomies <- object$dichotomies
  if (missing(data)) {
    data.name <- object$data.name
    data <- object$data
  } else {
    data.name <- deparse(substitute(data))
  }
  if (missing(subset)) {
    subset <- object$subset
  }
  if (missing(contrasts)){
    contrasts <- object$contrasts
    if (!is.null(contrasts) && contrasts == "NULL") contrasts <- NULL
  }
  result <- nestedLogit(formula, dichotomies=dichotomies, data=data,
                        subset=subset, contrasts=contrasts)
  result$data.name <- data.name
  for (i in seq_along(models(result))){
   result$models[[i]]$call$data <- as.symbol(data.name)
  }
  if (missing(contrasts)){
    result$contrasts <- object$contrasts
    result$contrasts.print <- object$contrasts.print
  } else {
    result$contrasts <- contrasts
    result$contrasts.print <- deparse(substitute(contrasts))
  }
  result
}

#' @rdname nestedMethods
#' @exportS3Method as.matrix dichotomies
as.matrix.dichotomies <- function(x, ...) {
  levels <- unique(unlist(x))
  responses <- matrix(NA, length(x), length(levels))
  for (i in seq_along(x)) {
    responses[i, levels %in% x[[i]][[1L]]] <- 0L
    responses[i, levels %in% x[[i]][[2L]]] <- 1L
  }
  rownames(responses) <- names(x)
  colnames(responses) <- levels
  responses
}

#' @rdname nestedMethods
#' @exportS3Method as.character dichotomies
as.character.dichotomies <- function(x, ...) {
  nms <- names(x)
  result <- ""
  for (i in seq_along(x)) {
    result <- paste0(result,
                     names(x[i]), " = ",
                     "{",
                     names(x[[i]][1L]),
                     "{",
                     paste(x[[i]][[1L]], collapse = " "),
                     "}} {",
                     names(x[[i]][2L]),
                     "{",
                     paste(x[[i]][[2L]], collapse = " "),
                     "}}; "
    )
  }
  result <- substr(result, 1, nchar(result)-2L)
  result
}


#' @rdname nestedMethods
#' @exportS3Method as.matrix continuationDichotomies
as.matrix.continuationDichotomies <- function(x, ...){
  result <- NextMethod()
  result[, attr(x, "levels")]
}


#'
#' @rdname nestedMethods
#' @export
as.dichotomies <- function(x, ...){
  UseMethod("as.dichotomies")
}
#'
#' @rdname nestedMethods
#' @exportS3Method as.dichotomies matrix
as.dichotomies.matrix <- function(x, ...) {
  nlevels <- ncol(x)
  nlogits <- nrow(x)
  if(is.null(rownames(x))) rownames(x) <- paste0("d_", 1:nlogits)
  if(is.null(colnames(x))) colnames(x) <- LETTERS[1:nlevels]
  levels <- colnames(x)
  dnames <- rownames(x)

  logits <- vector("list", nlogits)
  for(i in 1:nlogits) {
    if (!all(x[i,] %in% c(0, 1, NA)))
      stop("Row ", i, " does not define a dichotomy consisting of 0, 1, NA")
    ones  <- levels[which(x[i,] == 1)]
    zeros <- levels[which(x[i,] == 0)]
    logits[[i]] <- list(zeros, ones)
  }
  names(logits) <- dnames
  class(logits) <- "dichotomies"
  attr(logits, "levels") <- levels

  logits
}


# the following utility function isn't exported:

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
