#' Methods for \code{"nested"} and Related Objects
#' @aliases nestedMethods print.nested summary.nested print.summary.nested Anova.nested
#' print.Anova.nested update.nested predict.nested coef.nested vcov.nested print.dichotomies
#' as.dichotomies.matrix as.matrix.continuationDichotomies as.character.dichotomies
#' as.matrix.dichotomies
#' @description Various methods for processing \code{"nested"} and related objects.
#' @seealso \code{\link{nestedLogit}}
#' 
#' @param x,object,object2,mod in most cases, an object of class \code{"nested"}.
#' @param newdata a data frame containing combinations of values of the predictors 
#'        at which fitted probabilities are to be computed.
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
#' 
#' # TODO: add examples for at least some of the methods


#' @rdname nestedMethods
#' @export
print.nested <- function(x, ...) {
  cat("Nested logit models: ")
  print(x$formula)
  if (!is.null(x$subset)) cat("subset: ", x$subset, "\n")
  if ("NULL" != x$contrasts.print) cat("contrasts: ", x$contrasts.print, "\n")
  lapply(x$models, print, ...)
  invisible(x)
}

#' @rdname nestedMethods
#' @export
summary.nested <- function(object, ...) {
  result <- lapply(object$models, summary, ...)
  for (i in seq_along(result)) {
    result[[i]]$dichotomy <- object$models[[i]]$dichotomy
  }
  class(result) <- "summary.nested"
  attr(result, "formula") <- object$formula 
  attr(result, "subset") <- object$subset 
  attr(result, "contrasts.print") <- object$contrasts.print 
  result
}

#' @rdname nestedMethods
#' @exportS3Method print summary.nested
print.summary.nested <- function(x, ...) {
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
      ": {",
      paste(x[[i]]$dichotomy[[1L]], collapse = ", "),
      "} vs. {",
      paste(x[[i]]$dichotomy[[2L]], collapse = ", "),
      "}"
    ))
    print(x[[i]], ...)
  }
  invisible(return(x))
}

#' @rdname nestedMethods
#' @importFrom car Anova
#' @exportS3Method car::Anova nested
#' @export
Anova.nested <- function(mod, ...) {
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
  class(result) <- "Anova.nested"
  result
}

#' @rdname nestedMethods
#' @exportS3Method print Anova.nested
print.Anova.nested <- function(x, ...) {
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
      ": {",
      paste(x[[i]][[1L]], collapse = ", "),
      "} vs. {",
      paste(x[[i]][[2L]], collapse = ", "),
      "}\n"
    ))
  }
  invisible(x)
}

#' @rdname nestedMethods
#' @importFrom stats predict
#' @export
predict.nested <- function(object, newdata, ...) {
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
  p
}

#' @rdname nestedMethods
#' @importFrom stats coef
#' @export
coef.nested <- function(object, as.matrix=TRUE, ...) {
  result <- if(as.matrix) sapply(object$models, coef, ...)
  else lapply(object$models, coef, ...)
  result
}

#' @rdname nestedMethods
#' @export
vcov.nested <- function(object, as.matrix=FALSE, ...){
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
anova.nested <- function(object, object2, ...){
  if (missing(object2)){
    result <- lapply(object$models, anova, test="LRT")
    heading <- attr(result[[1L]], "heading")[1L]
    heading <- sub("Table", "Tables", heading)
    heading <- sub("\\.\\.y", as.character(object$formula[[2]]), heading)
    heading <- sub("binomial, link: logit", "Nested Logit", heading)
  } else {
    if (!inherits(object2, "nested"))
      stop(deparse(substitute(object2)), " is not of class 'nested'")
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
  class(result) <- "anova.nested"
  result
}

#' @rdname nestedMethods
#' @exportS3Method print anova.nested
print.anova.nested <- function(x, ...) {
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
update.nested <- function(object, formula, dichotomies, data, subset, contrasts,...){
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
#  TODO: Check whether each row defines a dichotomy [DONE]
#        Check whether dichotomies are nested
#        Why doesn't this work as a method for class `matrix`?  -- Need to call it as `as.dichotomies.matrix()`

#' @rdname nestedMethods
#' @export
as.dichotomies <- function(x, ...){
  UseMethod("as.dichtomies")
}
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

#' @importFrom broom glance
#' @rdname nestedMethods
#' @exportS3Method broom::glance nested
glance.nested <- function(x, ...){
  result <- dplyr::bind_rows(lapply(x$models, broom::glance))
  result <- dplyr::bind_cols(response = names(x$models), result)
  result
}

#' @importFrom broom tidy
#' @rdname nestedMethods
#' @exportS3Method broom::tidy nested
tidy.nested <- function(x, ...){
  result <- dplyr::bind_rows(lapply(x$models, broom::tidy, ...))
  response <- rep(names(x$models), each = nrow(result)/length(x$models))
  result <- dplyr::bind_cols(response = response, result)
  result
}

