# Nested logit models
# last modified 2023-04-16 by J. Fox
# 2023-04-06 MF: add coef method
# 2023-04-11 MF: add `dichotomies` attribute to result of nestedLogit

#' Binary Logit Models for Nested Dichotomies
#'
#' @description Fit a related set of binary logit models via the \code{\link{glm}}
#' function to nested dichotomies, comprising a model for the polytomy.
#' A polytomous response with \eqn{m} categories can be analyzed using
#' \eqn{m-1} binary logit comparisons.  When these comparisons are nested,
#' the \eqn{m-1} sub-models are statistically independent. Therefore,
#' the likelihood chi-square statistics for the sub-models are additive
#' and give overall tests for a model for the polytomy.
#' This method was introduced by Fienberg (1980),and subsequently illustrated by
#' Fox(2016) and Friendly & Meyer (2016).
#'
#' \code{dichotomy} and \code{logits} are helper functions to construct the dichotomies.
#'
#' @details A \emph{dichotomy} for a categorical variable is a comparison of one subset
#' of levels against another subset. A set of dichotomies is \emph{nested}, if after
#' an initial dichotomy, all subsequent ones are \emph{within} the groups of levels
#' lumped together in earlier ones. Nested dichotomies correspond to a binary tree
#' of the successive divisions.
#'
#' For example, for a 3-level response, a first
#' dichotomy could be \code{ {A}, {B, C}} and then the second one would be
#' just \code{{B}, {C}}. Note that in the second dichotomy, observations
#' with response \code{A} are treated as \code{NA}.
#'
#' The function \code{dichotomy} constructs a \emph{single} dichotomy in the required form,
#' which is a list of length 2 containing two character vectors giving the levels
#' defining the dichotomy. The function \code{logits} is used to create the
#' set of dichotomies for a response factor.

#' The function \code{continuationLogits} provides a
#' convenient way to generate all dichotomies for an ordered response.
#' For an ordered response with \eqn{m=4} levels, say, \code{A, B, C, D},
#' considered low to high:
#' The dichotomy first contrasts \code{B, C, D} against \code{A}.
#' The second ignores \code{A} and contrasts \code{C, D} against \code{B}.
#' The second ignores \code{A, B} and contrasts \code{D} against \code{C}.

#'
#' @aliases nestedLogit logits dichotomy continuationLogits
#'
#' @param formula a model formula with the polytomous response on the left-hand side
#'        and the usual linear-model-like specification on the right-hand side.
#' @param dichotomies specification of the logits for the nested dichotomies,
#'        constructed by the \code{logits} and \code{dichotomy} functions,
#'        or \code{continuationLogits}. See Details.
#' @param data a data frame with the data for the model; unlike in most statistical
#'        modeling functions, the \code{data} argument is required. Cases with \code{NA}s
#'        in any of the variables appearing in the model formula will be removed
#'        with a Note message.
#' @param subset a character string specifying an expression to fit the model
#'        to a subset of the data; the default, \code{NULL}, uses the full data set.
#' @param contrasts an optional list of contrast specification for specific factors in the
#'        model; see \code{\link{lm}} for details.
#' @param \dots for \code{nestedLogit}, optional named arguments to be passed to \code{\link{glm}};
#'        for \code{logits}, definitions of the nested logits---with each named argument specifying
#'        a dichotomy; for \code{dichotomy}, two character vectors giving the levels
#'        defining the dichotomy.
#' @param levels for \code{continuationLogits}, a character vector giving the ordered levels for a set of continuation dichotomies,
#'        or the number of levels, in which case the levels will be named \code{"A"}, \code{"B"}, \code{"C"}, etc.
#' @param names an optional character vector of names for the continuation dichotomies; if absent,
#'        names will be generated from the levels.
#' @param prefix a character string (default: \code{"above_"}) used as a prefix to the names of the continuation dichotomies.
#'
#' @return \code{nestedLogit} returns an object of class \code{"nested"} containing
#' the following elements:
#' \itemize{
#'    \item \code{models}, A named list of (normally) \eqn{m - 1} \code{"glm"} objects,
#'    each a binary logit model for one of the \eqn{m - 1} nested dichotomies representing
#'    the \eqn{m}-level response.
#'    \item \code{formula}, the model formula for the nested logit models.
#'    \item \code{dichotomies}, the \code{"dichotomies"} object defining the nested dichotomies
#'    for the model.
#'    \item \code{data.name}, the name of the data set to which the model is fit, of class \code{"name"}.
#'    \item \code{data}, the data set to which the model is fit.
#'    \item \code{subset}, a character representation of the \code{subset} argument or
#'    \code{"NULL"} if the argument isn't specified.
#'    \item \code{contrasts}, the \code{contrasts} argument or \code{NULL} if the argument
#'    isn't specified.
#'    \item{contrasts.print} a character representation of the \code{contrasts} argument or
#'    \code{"NULL"} if the argument isn't specified.
#' }
#'   \code{logits} and \code{continuationLogits} return objects of class \code{"dichotomies"}.
#'
#' @importFrom stats  anova binomial coef glm model.frame model.response na.omit pchisq predict update
#' @references
#' S. Fienberg (1980). \emph{The Analysis of Cross-Classified Categorical Data},
#'       2nd Edition, MIT Press, Section 6.6.
#' J. Fox (2016), \emph{Applied Linear Regression and Generalized Linear Models}, 3rd Edition, Sage,
#'       Section 14.2.2.
#' J. Fox and S. Weisberg (2011), \emph{An R Companion to Applied Regression}, 2nd Edition, Sage, Section 5.8.
#' M. Friendly and D. Meyers (2016), \emph{Discrete Data Analysis with R}, CRC Press,
#'       Section 8.2.
#' @seealso \code{\link{nestedMethods}}
#' @author John Fox
#' @keywords regression
#' @examples
#'   data(Womenlf, package = "carData")
#'
#'   #' Use `logits()` and `dichotomy()` to specify the comparisons of interest
#'   comparisons <- logits(work=dichotomy("not.work", c("parttime", "fulltime")),
#'                         full=dichotomy("parttime", "fulltime"))
#'   print(comparisons)
#'
#'   m <- nestedLogit(partic ~ hincome + children,
#'                    dichotomies = comparisons,
#'                    data=Womenlf)
#'   print(summary(m))
#'   print(car::Anova(m))
#'   coef(m)
#'
#'   # get predicted values
#'   new <- expand.grid(hincome=seq(0, 45, length=10),
#'                      children=c("absent", "present"))
#'   pred.nested <- predict(m, new)
#'
#'   # plot
#'   op <- par(mfcol=c(1, 2), mar=c(4, 4, 3, 1) + 0.1)
#'   plot(m, "hincome", list(children="absent"),
#'        xlab="Husband's Income", legend=FALSE)
#'   plot(m, "hincome", list(children="present"),
#'        xlab="Husband's Income")
#'   par(op)
#'
#' @export
nestedLogit <- function(formula, dichotomies, data, subset=NULL,
                        contrasts=NULL, ...) {

  makeDichotomies <- function(y, dichotomies) {
    responses <- matrix(NA, length(y), length(dichotomies))
    for (i in seq_along(dichotomies)) {
      responses[y %in% dichotomies[[i]][[1L]], i] <- 0L
      responses[y %in% dichotomies[[i]][[2L]], i] <- 1L
    }
    colnames(responses) <- names(dichotomies)
    responses
  }

  if (!inherits(dichotomies, "dichotomies"))
    stop("dichotomies must be of class 'dichotomies'")

  data.name <- substitute(data)
  data.save <- data
  if (!is.null(subset)){
    data <- subset(data, eval(parse(text=subset), envir=data))
  }
  n.original <- nrow(data)
  data <- na.omit(data[, all.vars(formula)])
  n.final <- nrow(data)
  if (n.final < n.original) message("Note: ", n.original - n.final,
                                   " cases omitted due to missingness.")

  nested.formula <- formula
  y <- model.response(model.frame(formula, data))
  n.dichot <- length(dichotomies)
  ys <- makeDichotomies(y, dichotomies)
  models <- vector(n.dichot, mode = "list")
  resp.names <- names(dichotomies)
  names(models) <- resp.names
  formula[[2]] <- quote(..y)

  for (i in seq(length = n.dichot)) {
    data$..y <- ys[, i]
    models[[i]] <- glm(formula, family = binomial, data = data, contrasts=contrasts, ...)
    form <- models[[i]]$formula
    form[[2]] <- as.symbol(resp.names[i])
    models[[i]]$formula <- form
    call <- models[[i]]$call
    call$formula <- form
    call$data <- data.name
    models[[i]]$call <- call
    models[[i]]$dichotomy <- dichotomies[[i]]
  }

  result <- list(
    models = models,
    formula = nested.formula,
    dichotomies = dichotomies,
    data = data.save,
    data.name = data.name,
    subset = subset,
    contrasts = contrasts,
    contrasts.print = deparse(substitute(contrasts))
  )
  class(result) <- "nested"
  result
}

#' @rdname nestedLogit
#' @export
logits <- function(...) {

  checkDichotomies <- function(logits){
    nlevels <- length(unique(unlist(logits)))
    nlogits <- length(logits)
    if (nlogits > (nlevels - 1L))
      stop("too many nested dichotomies (>", nlevels - 1L, ")")
    if (nlogits < (nlevels - 1L))
      message("Note: number of nested dicotomies not equal to ", nlevels - 1L)
    return(invisible(NULL))
  }

  hasParent <- function(dichotomy){
    any(sapply(all, function(each) setequal(union(dichotomy[[1L]], dichotomy[[2L]]), each)))
  }

  allChildren <- function(dichotomies){
    all <- unlist(dichotomies, recursive=FALSE)
    for (levels in all){
      if (length(levels) == 1L) next
      which <- sapply(dichotomies, function(dichotomy) setequal(levels,
                                                                union(dichotomy[[1L]], dichotomy[[2L]])))
      if (!any(which)) message("Note: {", paste(levels, collapse=", "), "} is not subdivided")
    }
  }

  logits <- list(...)
  duplicated <- duplicated(logits)
  if (any(duplicated)){
    stop("the following dichotomies are duplicated: ",
         paste(names(logits)[duplicated], collapse=", "))
  }
  all <- unlist(logits, recursive=FALSE)
  all$all <- levels <- unique(unlist(logits))
  nested <- sapply(logits, function(x) hasParent(x))
  if (any(!nested)) stop("dichotomies aren't nested")
  allChildren(logits)
  nms <- names(logits)
  if (is.null(nms) || any(nms == "") || any(is.na(nms)))
    stop("logits must be named")
  nms2 <- make.names(nms)
  if (!all(nms2 == nms)){
    names(logits) <- nms2
    warning("logit names coerced to valid names")
  }
  class(logits) <- "dichotomies"
  attr(logits, "levels") <- levels                      # MF
  checkDichotomies(logits)
  logits
}

#' @rdname nestedLogit
#' @export
dichotomy <- function(...) {
  logit <- list(...)
  if (length(logit) != 2L)
    stop("dichotomy must define two categories")
  logit
}


#' Construct Continuation Logits for an Ordered Categorical Variable
#'
#' \code{continuationLogits} constructs a set of \eqn{m-1} logit comparisons, called
#' continuation logits,
#' for an ordered response with \eqn{m=4} levels, say, \code{A, B, C, D},
#' considered low to high.
#' The first contrasts \code{B, C, D} against \code{A}.
#' The second ignores \code{A} and contrasts \code{C, D} against \code{B}.
#' The second ignores \code{A, B} and contrasts \code{D} against \code{C}.
#'
#' @param levels A character vector of set of levels of the variables or a number
#' specifying the numbers of levels (in which case, uppercase letters will be
#' use for the levels).
#' @param names  Names to be assigned to the dichotomies; if absent, names
#' will be generated from the levels.
#'
#' @return an object of class \code{dichotomies}
#' @rdname nestedLogit
#' @export
#'
#' @examples
#'   continuationLogits(c("none", "gradeschool", "highschool", "college"))
#'   continuationLogits(4)
#'
#'
#  TODO:
#  - add prefix = "above_" argument [DONE]
#  - allow reverse = FALSE (default) argument to do continuations in reverse order ???

continuationLogits <- function(levels, names, prefix = "above_"){
  if (is.numeric(levels)){
    levels <- as.integer(levels)
    levels <- LETTERS[1L:levels]
  }
  nlevels <- length(levels)
  if (nlevels < 3L) stop("too few levels: ", levels)
  logits <- vector((nlevels - 1L), mode="list")
  for (i in 1L:(nlevels - 1L)){
    logits[[i]] <- list(levels[i], levels[-(1L:i)])
  }
  if (missing(names)) names <- paste0(prefix, levels[1L:(nlevels - 1L)])
  if (length(names) != nlevels - 1L)
    stop("number of names not equal to ", nlevels - 1L)
  nms <- make.names(names)
  if (!all(names == nms)){
    names <- nms
  }
  names(logits) <- names
  attr(logits, "levels") <- levels
  class(logits) <- c("continuationDichotomies", "dichotomies")
  logits
}


