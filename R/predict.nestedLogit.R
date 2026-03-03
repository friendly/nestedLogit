#' Predicted Probabilities and Logits for \code{"nestedLogit"} Models
#'
#' @name predict.nestedLogit
#' @aliases predict.nestedLogit fitted.nestedLogit print.predictNestedLogit
#' confint.predictNestedLogit print.predictDichotomies
#'
#' @description The \code{predict} and \code{fitted} methods compute predicted values from a fitted
#' \code{"nestedLogit"} model.
#' The \code{confint} method computes point-wise confidence limits for predicted response-category
#' probabilities or logits.
#'
#' \describe{
#'   \item{\code{predict}, \code{fitted}}{Compute predicted response-category probabilities
#'         (or predicted logits for each binary logit model in the dichotomies) from a fitted
#'         \code{"nestedLogit"} model.}
#'   \item{\code{confint}}{Compute point-wise confidence limits for predicted response-category
#'        probabilities or logits.}
#'   \item{\code{print} methods}{Print predicted probabilities, logits, and their standard errors,
#'         with control over how many rows to display.}
#' }
#'
#' @details
#' The \code{predict} method provides predicted values for two representations of the model.
#' \code{model = "nested"} (the default) gives the fitted probabilities for each of the response categories,
#' along with the corresponding logits and standard errors of each.
#' \code{model = "dichotomies"} gives the fitted log odds for each of the binary logit models in the
#' nested dichotomies.
#'
#' The \code{fitted} method (with no \code{newdata}) is equivalent to \code{predict} applied to the
#' original data used to fit the model.
#'
#' For the \code{confint} method with \code{parm = "prob"}, setting
#' \code{conf.limits.logit = TRUE} (the default) computes confidence limits on the logit scale
#' and back-transforms them to probabilities, which ensures that the limits lie in \eqn{[0, 1]}.
#' Setting \code{conf.limits.logit = FALSE} computes Wald-type confidence intervals directly on the
#' probability scale, which may extend outside \eqn{[0, 1]}.
#'
#' @seealso \code{\link{nestedLogit}}, \code{\link{nestedMethods}},
#'          \code{\link{plot.nestedLogit}},
#'          \code{\link{as.data.frame.predictNestedLogit}}
#'
#' @param object a fitted object of class \code{"nestedLogit"};
#'        for \code{confint}, an object of class \code{"predictNestedLogit"}.
#' @param x an object of class \code{"predictNestedLogit"} or \code{"predictDichotomies"}.
#' @param newdata a data frame containing combinations of values of the predictors
#'        at which fitted probabilities (or other quantities) are to be computed.
#'        If missing, the original data are used.
#' @param model either \code{"nested"} (the default), in which case fitted probabilities
#'        under the nested logit model are returned, or \code{"dichotomies"}, in which case
#'        \code{\link{predict.glm}} is invoked for each binary logit model fit to the nested
#'        dichotomies and a named list of the results is returned.
#' @param n an integer or \code{"all"} to control how many rows are printed for each of the
#'        predicted values.
#' @param parm one of \code{"prob"} or \code{"logit"},
#'        indicating whether to generate confidence intervals for probabilities or logits
#'        of the responses.
#' @param level confidence level, a number between 0 and 1; default is \code{0.95}.
#' @param conf.limits.logit logical; when \code{parm = "prob"}, if \code{TRUE} (the default),
#'        confidence limits are computed on the logit scale and back-transformed to probabilities;
#'        if \code{FALSE}, Wald-type limits are computed directly on the probability scale.
#' @param \dots arguments to be passed down.
#'
#' @return  \itemize{
#'    \item The \code{predict} and \code{fitted} methods return an object of class \code{"predictNestedLogit"}
#'    (when \code{model = "nested"}) or \code{"predictDichotomies"} (when \code{model = "dichotomies"}).
#'
#'    A \code{"predictNestedLogit"} object is a list containing:
#'    \describe{
#'      \item{\code{p}}{a data frame of predicted probabilities, with one column per response category.}
#'      \item{\code{logit}}{a data frame of predicted logits.}
#'      \item{\code{se.p}}{a data frame of standard errors of predicted probabilities.}
#'      \item{\code{se.logit}}{a data frame of standard errors of predicted logits.}
#'      \item{\code{.data}}{the \code{newdata} data frame, if supplied.}
#'    }
#'
#'    A \code{"predictDichotomies"} object is a named list of data frames, one per dichotomy,
#'    each produced by \code{\link{predict.glm}}.
#'
#'    \item The \code{confint} method returns a data frame of point estimates and confidence limits.
#'    \item The various \code{print} methods invisibly return their \code{x} arguments.
#'    }
#'
#' @author John Fox and Michael Friendly
#' @keywords regression
#' @examples
#' # define continuation dichotomies for level of education
#' cont.dichots <- continuationLogits(c("<highschool",
#'                                      "highschool",
#'                                      "college",
#'                                      "graduate"))
#'
#' # fit a nested model for the GSS data
#' m <- nestedLogit(degree ~ parentdeg + year,
#'                  cont.dichots,
#'                  data=GSS)
#'
#' # predicted probabilities for first few cases
#' predict(m)
#'
#' # predicted probabilities at specific values of predictors
#' new <- expand.grid(parentdeg=c("<highschool",  "highschool",
#'                                "college", "graduate"),
#'                    year=c(1972, 2016))
#'
#' fit <- predict(m, newdata=new)
#' cbind(new, fit)
#'
#' # use fitted() -- equivalent to predict() on the original data
#' f <- fitted(m)
#'
#' # predicted logits for each dichotomy
#' pred.dichot <- predict(m, newdata=new, model="dichotomies")
#' pred.dichot
#'
#' # confidence intervals for predicted probabilities
#' fit.ci <- confint(fit)
#' head(fit.ci)
#'
#' # confidence intervals for predicted logits
#' fit.ci.logit <- confint(fit, parm="logit")
#' head(fit.ci.logit)

#' @rdname predict.nestedLogit
#' @importFrom stats predict
#' @export
predict.nestedLogit <- function(object, newdata, model=c("nested", "dichotomies"), ...) {
  model <- match.arg(model)

  if (no.newdata <- missing(newdata))
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
    if (!no.newdata) result$.data <- newdata
    class(result) <- "predictNestedLogit"
    return(result)

  } else {
    result <- lapply(models(object),
                     function(x) as.data.frame(predict(x, newdata=newdata, se.fit=TRUE, ...)))
    attr(result, "model") <- deparse(substitute(object))
    attr(result, "dichotomies") <- names(result)
    if (!no.newdata) result$.data <- newdata
    class(result) <- "predictDichotomies"
    result
  }
}

#' @rdname predict.nestedLogit
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

#' @rdname predict.nestedLogit
#' @importFrom stats confint qnorm
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

#' @rdname predict.nestedLogit
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


#' @rdname predict.nestedLogit
#' @importFrom stats fitted
#' @export
fitted.nestedLogit <- function(object, model=c("nested", "dichotomies"), ...){
  predict(object, model=model)
}
