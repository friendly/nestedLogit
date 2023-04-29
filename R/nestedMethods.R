#' Methods for \code{"nestedLogit"} and Related Objects
#'
#' @aliases nestedMethods print.nestedLogit summary.nestedLogit print.summary.nestedLogit Anova.nestedLogit
#' print.Anova.nestedLogit update.nestedLogit predict.nestedLogit coef.nestedLogit vcov.nestedLogit print.dichotomies
#' as.dichotomies.matrix as.matrix.continuationDichotomies as.character.dichotomies
#' as.matrix.dichotomies
#' @description Various methods for processing \code{"nestedLogit"} and related objects.
#' Most of these are the standard methods for a model-fitting function.
#'
#' \describe{
#'   \item{\code{coef}, \code{vcov}}{Return the coefficients and their variance-covariance matrix respectively.}
#'   \item{\code{Anova}}{Calculates type-II or type-III analysis-of-variance tables for \code{"nestedLogit"} objects.}
#'   \item{\code{anova}}{Computes sequential analysis of variance (or deviance) tables for one or more fitted \code{"nestedLogit"}  objects.}
#'   \item{\code{update}}{Re-fit a \code{"nestedLogit"} model with a change in any of the \code{formula}, \code{dichotomies},
#'        \code{data}, \code{subset}, or \code{contrasts}, arguments.}
#'   \item{\code{predict}}{Computes predicted values from a fitted \code{"nestedLogit"} model.}
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
#' @seealso \code{\link{nestedLogit}}, \code{\link{plot.nestedLogit}}
#'
#' @param x,object,object2,mod in most cases, an object of class \code{"nestedLogit"}.
#' @param newdata For the \code{predict} method, a data frame containing combinations of values of the predictors
#'        at which fitted probabilities (or other quantities) are to be computed.
#' @param model For the \code{predict} method, either \code{"nested"} (the default), in which case fitted probabilities
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
#' @param \dots arguments to be passed down.
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
#' data(GSS)
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
#' # Anova tests
#' car::Anova(m) # type-II (partial) tests
#'
#' # anova() and update() methods
#' anova(m) # type-I (sequential) tests
#' anova(update(m, . ~ . - year), m) # model comparison
#'
#' # predicted probabilities and ploting
#' head(predict(m)) # fitted probabilities for first few cases
#' new <- expand.grid(parentdeg=c("l.t.highschool",  "highschool",
#'                                "college", "graduate"),
#'                    year=c(1972, 2016))
#' fit <- predict(m, newdata=new)
#' cbind(new, fit) # fitted probabilities at specific values of predictors
#' predictions <- predict(m, newdata=new, model="dichotomies", se.fit=TRUE)
#' predictions
#' predictions$above_l.t.highschool # on logit scale
#'
#' @rdname nestedMethods
#' @export
print.nestedLogit <- function(x, ...) {
  cat("Nested logit models: ")
  print(x$formula)
  if (!is.null(x$subset)) cat("subset: ", x$subset, "\n")
  if ("NULL" != x$contrasts.print) cat("contrasts: ", x$contrasts.print, "\n")
  lapply(x$models, print, ...)
  invisible(x)
}

#' @rdname nestedMethods
#' @export
summary.nestedLogit <- function(object, ...) {
  result <- lapply(object$models, summary, ...)
  for (i in seq_along(result)) {
    result[[i]]$dichotomy <- object$models[[i]]$dichotomy
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
    cat(paste0(
      "Response ",
      nms[i],
      ": ",
      names(x[[i]]$dichotomy[1L]),
      "{",
      paste(x[[i]]$dichotomy[[1L]], collapse = ", "),
      "} vs. ",
      names(x[[i]]$dichotomy[2L]),
      "{",
      paste(x[[i]]$dichotomy[[2L]], collapse = ", "),
      "}"
    ))
    print(x[[i]], ...)
  }
  invisible(x)
}

#' @rdname nestedMethods
#' @importFrom car Anova
#' @exportS3Method car::Anova nestedLogit
#' @export
Anova.nestedLogit <- function(mod, ...) {
  result <- lapply(mod$models, Anova)
  nms <- names(mod$models)
  heading <- attr(result[[1L]], "heading")[1L]
  heading <- sub("Table", "Tables", heading)
  for (i in seq(along = result)) {
    attr(result[[i]], "heading") <- paste0(
      "Response ",
      nms[i],
      ": {",
      paste(mod$models[[i]]$dichotomy[[1L]], collapse =
              ", "),
      "} vs. {",
      paste(mod$models[[i]]$dichotomy[[2L]],
            collapse = ", "),
      "}"
    )
  }
  attr(result, "heading") <- heading
  class(result) <- "Anova.nestedLogit"
  result
}

#' @rdname nestedMethods
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

# print.dichotomies <- function(x, ...) {
#   nms <- names(x)
#   for (i in seq_along(x)) {
#     cat(paste0(
#       nms[i],
#       ": {",
#       paste(x[[i]][[1L]], collapse = ", "),
#       "} vs. {",
#       paste(x[[i]][[2L]], collapse = ", "),
#       "}\n"
#     ))
#   }
#   invisible(x)
# }

#' @rdname nestedMethods
#' @importFrom stats predict
#' @export
predict.nestedLogit <- function(object, newdata, model=c("nested", "dichotomies"), ...) {
  model <- match.arg(model)
  if (model == "nested"){
    if (missing(newdata))
      newdata <- object$models[[1L]]$data
    ndichot <- length(object$models)
    if (ndichot < 2L)
      stop("there are fewer than 2 nested dichotomies")
    fitted <- vector(ndichot, mode = "list")
    for (i in seq_along(object$models)) {
      p <- predict(object$models[[i]], newdata = newdata, type = "response")
      p <- cbind(1 - p, p)
      attr(p, "columns") <- object$models[[i]]$dichotomy
      fitted[[i]] <- p
    }
    response.levels <-
      unique(unlist(lapply(fitted, function(x)
        attr(x, "columns"))))
    p <- matrix(1, nrow(newdata), length(response.levels))
    colnames(p) <- response.levels
    for (level in response.levels) {
      for (i in seq_along(object$models)) {
        which <- sapply(object$models[[i]]$dichotomy, function(x)
          level %in% x)
        if (!any(which))
          next
        p[, level] <- p[, level] * fitted[[i]][, which]
      }
    }
    rownames(p) <- rownames(newdata)
    return(p)
  } else {
    result <- lapply(object$models, predict, newdata=newdata, ...)
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

#' @rdname nestedMethods
#' @importFrom stats coef
#' @export
coef.nestedLogit <- function(object, as.matrix=TRUE, ...) {
  result <- if(as.matrix) sapply(object$models, coef, ...)
  else lapply(object$models, coef, ...)
  result
}

#' @rdname nestedMethods
#' @export
vcov.nestedLogit <- function(object, as.matrix=FALSE, ...){
  vcovs <- lapply(object$models, vcov)
  if (!as.matrix) {
    return(vcovs)
  }
  n.models <- length(object$models)
  n.coefs <- nrow(vcovs[[1]])
  nms <- expand.grid(names(coef(object$models[[1]])), names(object$models))
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
anova.nestedLogit <- function(object, object2, ...){
  if (missing(object2)){
    result <- lapply(object$models, anova, test="LRT")
    heading <- attr(result[[1L]], "heading")[1L]
    heading <- sub("Table", "Tables", heading)
    heading <- sub("\\.\\.y", as.character(object$formula[[2]]), heading)
    heading <- sub("binomial, link: logit", "Nested Logit", heading)
  } else {
    if (!inherits(object2, "nestedLogit"))
      stop(deparse(substitute(object2)), " is not of class 'nestedLogit'")
    result <- mapply(anova, object$models, object2$models, MoreArgs=list(test="LRT"), SIMPLIFY=FALSE)
    heading <- attr(result[[1L]], "heading")
    heading <- sub("Table", "Tables", heading)
    heading <- gsub("\\.\\.y", as.character(object$formula[[2]]), heading)
    heading <- sub("Model 2", " Model 2", heading)
    heading <- c(heading, "\n")
  }
  nms <- names(object$models)
  for (i in seq(along = result)) {
    attr(result[[i]], "heading") <- paste0(
      "Response ",
      nms[i],
      ": {",
      paste(object$models[[i]]$dichotomy[[1L]], collapse =
              ", "),
      "} vs. {",
      paste(object$models[[i]]$dichotomy[[2L]],
            collapse = ", "),
      "}"
    )
  }
  attr(result, "heading") <- heading
  class(result) <- "anova.nestedLogit"
  result
}

#' @rdname nestedMethods
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
  for (i in seq_along(result$models)){
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
                     "{{",
                     paste(x[[i]][[1L]], collapse = " "),
                     "}} {{",
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
  UseMethod("as.dichtomies")
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

# broom related methods
# [TODO] It seems these have to be called as broom::glance(). Why?

#' @importFrom broom glance
#' @rdname nestedMethods
#' @exportS3Method broom::glance nestedLogit
glance.nestedLogit <- function(x, ...){
  result <- dplyr::bind_rows(lapply(x$models, broom::glance))
  result <- dplyr::bind_cols(response = names(x$models), result)
  result
}

#' @importFrom broom tidy
#' @rdname nestedMethods
#' @exportS3Method broom::tidy nestedLogit
tidy.nestedLogit <- function(x, ...){
  result <- dplyr::bind_rows(lapply(x$models, broom::tidy, ...))
  response <- rep(names(x$models), each = nrow(result)/length(x$models))
  result <- dplyr::bind_cols(response = response, result)
  result
}


