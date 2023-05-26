#' Plotting Nested Logit Models
#'
#' @description A \code{\link{plot}} method for \code{"nestedLogit"} objects produced by the
#' \code{\link{nestedLogit}} function. Fitted probabilities under the model are plotted
#' for each level of the polytomous response variable, with one of the explanatory variables
#' on the horizontal axis and other explanatory variables fixed to particular values.
#'
#' @aliases plot.nestedLogit
#' @seealso \code{\link{nestedLogit}}, \code{\link[graphics]{matplot}}
#' @param x an object of \code{"nestedLogit"} produced by \code{\link{nestedLogit}}.
#' @param x.var quoted name of the variable to appear on the x-axis; if omitted, the first
#'        predictor in the model is used.
#' @param others a named list of values for the other variables in the model,
#'        that is, other than \code{x.var}; if any other predictor is omitted, it is set
#'        to an arbitrary value---the mean for a numeric predictor or the first level or
#'        value of a factor, character, or logical predictor; only one value may be
#'        specified for each variable in \code{others}.
#' @param n.x.values the number of evenly spaced values of \code{x.var} at which
#'        to evaluate fitted probabilities to be plotted (default \code{100}).
#' @param xlab label for the x-axis (defaults to the value of \code{x.var}).
#' @param ylab label for the y-axis (defaults to \code{"Fitted Probability"}).
#' @param main main title for the graph (if missing, constructed from the variables and
#'        values in \code{others}).
#' @param cex.main size of main title (see \code{\link{par}}).
#' @param font.main font for main title (see \code{\link{par}}).
#' @param pch plotting characters (see \code{\link{par}}).
#' @param lwd line width (see \code{\link{par}}).
#' @param lty line types (see \code{\link{par}}).
#' @param col line colors (see \code{\link{par}}).
#' @param legend if \code{TRUE} (the default), add a legend for the
#'        response levels to the graph.
#' @param legend.inset default \code{0.01} (see \code{\link{legend}}).
#' @param legend.location position of the legend (default \code{"topleft"},
#'        see \code{\link{legend}}).
#' @param legend.bty  the type of box to be drawn around the legend. The allowed values are "o" (the default) and "n".
#' @param \dots arguments to be passed to \code{\link{matplot}}.
#' @author John Fox \email{jfox@mcmaster.ca}
#' @examples
#' data(Womenlf, package = "carData")
#' m <- nestedLogit(partic ~ hincome + children,
#'                  logits(work=dichotomy("not.work", c("parttime", "fulltime")),
#'                         full=dichotomy("parttime", "fulltime")),
#'                         data=Womenlf)
#' plot(m, legend.location="top")
#' op <- par(mfcol=c(1, 2), mar=c(4, 4, 3, 1) + 0.1)
#' plot(m, "hincome", list(children="absent"),
#'      xlab="Husband's Income", legend=FALSE)
#' plot(m, "hincome", list(children="present"),
#'      xlab="Husband's Income")
#' par(op)
#' @importFrom grDevices palette
#' @importFrom graphics axis box matplot title
#' @importFrom stats formula
#' @rdname plot.nestedLogit
#' @return NULL Used for its side-effect of producing a plot
#' @export
plot.nestedLogit <- function(x, x.var, others, n.x.values=100L,
                             xlab=x.var, ylab="Fitted Probability",
                             main, cex.main=1, font.main=1L,
                             pch=1L:length(response.levels),
                             lwd=3, lty=1L:length(response.levels),
                             col=palette()[1L:length(response.levels)],
                             legend=TRUE, legend.inset=0.01,
                             legend.location="topleft",
                             legend.bty = "n", ...){
  data <- x$data
  vars <- all.vars(formula(x)[-2L])
  response <- setdiff(all.vars(formula(x)), vars)
  if (missing(x.var)) {
    x.var <- vars[1L]
    message("Note: ", x.var, " will be used for the horizontal axis")
  }
  if (!(x.var %in% vars)) stop (x.var, " is not in the model")
  if (!missing(others)){
    if (any(sapply(others, length) > 1))
      stop("more than one value specified for one or more variables in others")
    other.x <- names(others)
    check <- other.x %in% vars
    if (any(!check)) stop("not in the model: ", paste(other.x[!check], collapse=", "))
    values <- others
  } else {
    values <- list()
  }
  other.xs <- setdiff(vars, x.var)
  missing.xs <- !(other.xs %in% names(values))
  if (any(missing.xs)){
    missing.xs <- other.xs[missing.xs]
    for (missing.x in missing.xs){
      xx <- data[, missing.x]
      if (is.numeric(xx)){
        values[[missing.x]] <- signif(mean(xx, na.rm=TRUE))
        message("Note: missing predictor ", missing.x, " set to its mean, ",
                values[[missing.x]])
      } else if (is.factor(xx)){
        values[[missing.x]] <- levels(xx)[1L]
        message("Note: missing predictor ", missing.x, " set to its first level, '",
                values[[missing.x]], "'")
      } else {
        values[[missing.x]] <-  (sort(unique(xx)))[1L]
        message("Note: missing predictor ", missing.x, " set to its first value, '",
                values[[missing.x]], "'")
      }
    }
  }
  if (is.numeric(data[, x.var])){
    numeric.x <- TRUE
    range.x <- range(data[, x.var], na.rm=TRUE)
    values[[x.var]] <- seq(range.x[1L], range.x[2L], length=n.x.values)
  } else if (is.factor(data[, x.var])){
    numeric.x <- FALSE
    values[[x.var]] <- levels(data[, x.var])
  } else {
    numeric.x <- FALSE
    values[[x.var]] <- sort(unique(data[, x.var]))
  }
  new <- do.call(expand.grid, values)
  new <- cbind(new, predict(x, newdata=new))
  response.levels <- levels(data[[response]])
  if (numeric.x){
    matplot(new[, x.var], new[, response.levels], type="l", lwd=lwd,
            col=col, xlab=xlab, ylab=ylab, ...)
    if (legend) legend(legend.location, legend=response.levels,
                       lty=lty, lwd=lwd,
                       col=col, inset=legend.inset,
                       bty = legend.bty,
                       xpd=TRUE)
  } else {
    n.x.levels <- nrow(new)
    matplot(1L:n.x.levels, new[, response.levels], type="b", lwd=lwd,
            pch=pch, col=col, xlab=xlab, ylab=ylab, axes=FALSE, ...)
    box()
    axis(2L)
    axis(1L, at=1L:n.x.levels, labels=new[, x.var])
    if (legend) legend(legend.location, legend=response.levels,
                       lty=lty, lwd=lwd,
                       col=col, pch=pch,
                       inset=legend.inset,
                       bty = legend.bty,
                       xpd=TRUE)
  }
  if (!missing(main)) {
    title(main, cex.main=cex.main, font.main=font.main)
  } else {
    if (length(values) != 0L){
      which.main <- !(x.var == names(values))
      if (any(which.main)){
        main <- paste(paste(names(values[which.main]), "=",
                            as.character(unlist(values[which.main]))),
                      collapse=", ")
        title(main, cex.main=cex.main, font.main=font.main)
      }
    }
  }
  return(invisible(NULL))
}
