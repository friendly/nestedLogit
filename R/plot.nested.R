#' Plotting Nested Logit Models
#' @aliases plot.nested
#' @description A \code{\link{plot}} method for \code{"nested"} objects produced by the
#' \code{\link{nestedLogit}} function. Fitted probabilities under the model are plotted
#' for each level of the polytomous response variable, with one of the explanatory variables
#' on the horizontal axis and other explanatory variables fixed to particular values.
#' @seealso \code{\link{nestedLogit}}, \code{\link[graphics]{matplot}}
#' @param x an object of \code{"nested"} produced by \code{\link{nestedLogit}}.
#' @param x.var quoted name of the variable to appear on the x-axis.
#' @param others a named list of values for the other variables in the model,
#'        that is, other than \code{x.var}.
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
#' @param \dots arguments to be passed to \code{plot}.
#' @author John Fox \email{jfox@mcmaster.ca}
#' @examples
#' data(Womenlf, package = "carData")
#' m <- nestedLogit(partic ~ hincome + children,
#'                  logits(work=dichotomy("not.work", c("parttime", "fulltime")),
#'                         full=dichotomy("parttime", "fulltime")),
#'                         data=Womenlf)
#' op <- par(mfcol=c(1, 2), mar=c(4, 4, 3, 1) + 0.1)
#' plot(m, "hincome", list(children="absent"),
#'      xlab="Husband's Income", legend=FALSE)
#' plot(m, "hincome", list(children="present"),
#'      xlab="Husband's Income")
#' par(op)
#' @importFrom grDevices palette
#' @importFrom graphics axis box matplot title
#' @importFrom stats formula
#' @rdname plot.nested
#' @export
plot.nested <- function(x, x.var, others, n.x.values=100,
                        xlab=x.var, ylab="Fitted Probability",
                        main, cex.main=1, font.main=1,
                        pch=1:length(response.levels),
                        lwd=3, lty=1:length(response.levels),
                        col=palette()[1:length(response.levels)],
                        legend=TRUE, legend.inset=0.01,
                        legend.location="topleft", ...){
  data <- x$data
  vars <- all.vars(formula(x)[-2])
  response <- setdiff(all.vars(formula(x)), vars)
  if (!(x.var %in% vars)) stop (x.var, " is not in the model")
  other.x <- names(others)
  check <- other.x %in% vars
  if (any(!check)) stop("not in the model: ", paste(other.x[!check], collapse=", "))
  values <- others
  if (is.numeric(data[, x.var])){
    numeric.x <- TRUE
    range.x <- range(data[, x.var], na.rm=TRUE)
    values[[x.var]] <- seq(range.x[1], range.x[2], length=n.x.values)
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
          col=col, xlab=xlab, ylab=ylab)
    if (legend) legend(legend.location, legend=response.levels, lty=lty, lwd=lwd,
           col=col, inset=legend.inset, xpd=TRUE)
  } else {
    n.x.levels <- nrow(new)
    matplot(1:n.x.levels, new[, response.levels], type="b", lwd=lwd,
            pch=pch, col=col, xlab=xlab, ylab=ylab, axes=FALSE)
    box()
    axis(2)
    axis(1, at=1:n.x.levels, labels=new[, x.var])
    if (legend) legend(legend.location, legend=response.levels, lty=lty, lwd=lwd,
           col=col, pch=pch, inset=legend.inset, xpd=TRUE)
  }
  if (missing(main)) main <- paste(paste(other.x, "=", as.character(unlist(others))),
                                   collapse=", ")
  title(main, cex.main=cex.main, font.main=font.main)
}

