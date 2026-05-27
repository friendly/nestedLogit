#' Methods for \code{"nestedLogit"} and Related Objects
#'
#' @name nestedMethods
#' @aliases nestedMethods print.nestedLogit summary.nestedLogit print.summary.nestedLogit update.nestedLogit coef.nestedLogit vcov.nestedLogit print.dichotomies as.dichotomies.matrix as.matrix.continuationDichotomies as.character.dichotomies as.matrix.dichotomies as.dichotomies
#'
#' @description Various methods for processing \code{"nestedLogit"} and related objects.
#' Most of these are the standard methods for a model-fitting function.
#' \describe{
#'   \item{\code{coef}, \code{vcov}}{Return the coefficients and their variance-covariance matrix respectively.}
#'   \item{\code{update}}{Re-fit a \code{"nestedLogit"} model with a change in any of the \code{formula}, \code{dichotomies},
#'        \code{data}, \code{subset}, or \code{contrasts}, arguments.}
#'   \item{\code{summary}}{Summarize a \code{"nestedLogit"} model, giving the summary for each binary logit model in the
#'        nested dichotomies.}
#'   \item{\code{print}}{Print the model or a summary of the model.}
#'   \item{\code{as.matrix}, \code{as.character}, \code{as.dichotomies}}{Coerce
#'    dichotomy-related objects to matrices, character vectors, and dichotomies objects.}
#' }
#'
#' @seealso \code{\link{nestedLogit}}, \code{\link{predict.nestedLogit}},
#'          \code{\link{plot.nestedLogit}},
#'          \code{\link{glance.nestedLogit}}, \code{\link{tidy.nestedLogit}}
#'
#' @param x,object in most cases, an object of class \code{"nestedLogit"}.
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
#' @param \dots arguments to be passed down.
#'
#' @return  \itemize{
#'    \item The \code{coef} and \code{vcov} methods return either matrices or lists of regression
#'    coefficients and their covariances, respectively.
#'    \item The \code{update} method returns an object of class \code{"nestedLogit"} (see \code{\link{nestedLogit}})
#'    derived from the original nested-logit model.
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
#' cont.dichots <- continuationLogits(c("<highschool",
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
