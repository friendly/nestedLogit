# DONE: changed default colors to use scales::hue_pal()
# DONE: Allow to plot results on the logit scale, using logit = log(p/1-p), as in the
#       example "Plotting log-odds" in `vignettes/plotting-ggplot.Rmd`
#       Added argument `scale = c("prob", "logit")` where "prob" gives the current behavior
# TODO: Allow to facet the plots with a by= variable, rather than producing separate plots by conditioning as in the womenlf example.
#       This should produce a single plot with separate panels, using `par(mfrow=())`
# DONE: Added `label.col` arg (defaulting to the value of `col` in the call), to control the color of the curve label

#' Plotting Nested Logit Models
#'
#' @description A \code{\link{plot}} method for \code{"nestedLogit"} objects produced by the
#' \code{\link{nestedLogit}} function. Fitted probabilities under the model,
#' or the corresponding logits are plotted
#' for each level of the polytomous response variable, with one of the explanatory variables
#' on the horizontal axis and other explanatory variables fixed to particular values.
#' By default, a 95% pointwise confidence envelope is added to the plot.
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
#' @param scale character string; \code{"prob"} (the default) plots fitted probabilities
#'        on the y-axis; \code{"logit"} plots fitted log odds (logits), i.e., \eqn{\log(p/(1-p))}.
#' @param xlab label for the x-axis (defaults to the value of \code{x.var}).
#' @param ylab label for the y-axis (defaults to \code{"Fitted Probability"} when
#'        \code{scale = "prob"} and \code{"Fitted Log Odds"} when \code{scale = "logit"}).
#' @param main main title for the graph (if missing, constructed from the variables and
#'        values in \code{others}).
#' @param cex.main size of main title (see \code{\link{par}}).
#' @param digits.main number of digits to retain when rounding values for the main title.
#' @param font.main font for main title (see \code{\link{par}}).
#' @param pch plotting characters (see \code{\link{par}}).
#' @param lwd line width (see \code{\link{par}}).
#' @param lty line types (see \code{\link{par}}).
#' @param col line colors for the response levels (see \code{\link{par}}).
#' @param legend if \code{TRUE} (the default), add a legend for the
#'        response levels to the graph. Ignored when \code{label = TRUE}.
#' @param legend.inset default \code{0.01} (see \code{\link{legend}}).
#' @param legend.location position of the legend (default \code{"topleft"},
#'        see \code{\link{legend}}).
#' @param legend.bty  the type of box to be drawn around the legend. The allowed values are "o" (the default) and "n".
#' @param conf.level the level for pointwise confidence envelopes around the predicted values;
#' the default is \code{0.95}. If \code{NULL}, the confidence envelopes are suppressed.
#' @param conf.alpha the opacity of the confidence envelopes; the default is \code{0.3}.
#' @param label if \code{TRUE}, label the curves directly instead of using a legend.
#'        Default is \code{FALSE}.
#' @param label.x where to place the label for each curve. Either a single string,
#'        \code{"min"} or \code{"max"} (applied to all curves), or a character vector of
#'        length equal to the number of response categories with each element being
#'        \code{"min"} or \code{"max"}, e.g. \code{label.x = c("min", "max", "max")}.
#'        \code{"min"} places the label at the left end of the curve;
#'        \code{"max"} (the default) places it at the right end.
#' @param label.cex character expansion factor for direct labels; default \code{1.25}.
#' @param label.col colors for direct labels; defaults to \code{col}, so labels match
#'        their curves. Supply a vector of length equal to the number of response
#'        categories to use different colors for the labels.
#' @param \dots arguments to be passed to \code{\link{matplot}}.
#' @author John Fox, Michael Friendly
#' @examples
#' data("Womenlf", package = "carData")
#' m <- nestedLogit(partic ~ hincome + children,
#'                  logits(work=dichotomy("not.work", c("parttime", "fulltime")),
#'                         full=dichotomy("parttime", "fulltime")),
#'                         data=Womenlf)
#' plot(m, legend.location="top")
#'
#' op <- par(mfcol=c(1, 2), mar=c(4, 4, 3, 1) + 0.1)
#' plot(m, "hincome", list(children="absent"),
#'      xlab="Husband's Income", legend=FALSE)
#' plot(m, "hincome", list(children="present"),
#'      xlab="Husband's Income")
#' par(op)
#'
#' # Plot on the logit (log-odds) scale
#' plot(m, "hincome", list(children="absent"), scale = "logit",
#'      xlab = "Husband's Income")
#'
#' # Gators example: direct curve labels instead of a legend
#' data("gators", package = "nestedLogit")
#' gators.nested <- nestedLogit(food ~ length,
#'   logits(d1 = dichotomy("Other", c("Fish", "Invertebrates")),
#'          d2 = dichotomy("Fish", "Invertebrates")),
#'   data = gators)
#'
#' # All labels at the right end (default)
#' plot(gators.nested, x.var = "length", label = TRUE,
#'      xlab = "Alligator length (m)")
#'
#' # Mixed placement: Other and Invertebrates labeled at left, Fish at right
#' # (food levels are: "Other", "Fish", "Invertebrates")
#' plot(gators.nested, x.var = "length", label = TRUE,
#'      label.x = c("min", "max", "min"),
#'      xlab = "Alligator length (m)")
#'
#' @importFrom grDevices palette adjustcolor
#' @importFrom graphics axis box matplot title arrows polygon text
#' @importFrom stats formula
#' @importFrom scales hue_pal
#' @rdname plot.nestedLogit
#' @return NULL Used for its side-effect of producing a plot
#' @export
plot.nestedLogit <- function(x,
                             x.var, others,
                             n.x.values=100L,
                             scale=c("prob", "logit"),
                             xlab=x.var, ylab=NULL,
                             main, cex.main=1,
                             digits.main=getOption("digits") - 2L,
                             font.main=1L,
                             pch=1L:length(response.levels),
                             lwd=3,
                             lty=1L:length(response.levels),
#                             col=palette()[1L:length(response.levels)],
                             col=scales::hue_pal()(length(response.levels)),
                             legend=TRUE, legend.inset=0.01,
                             legend.location="topleft",
                             legend.bty = "n", conf.level=0.95,
                             conf.alpha=0.25,
                             label=FALSE,
                             label.x="max", label.cex=1.25,
                             label.col=col,
                             ...){
  scale <- match.arg(scale)
  if (is.null(ylab)) ylab <- if (scale == "prob") "Fitted Probability" else "Fitted Log Odds"
  data <- x$data
  vars <- all.vars(formula(x)[-2L])
  response <- setdiff(all.vars(formula(x)), vars)
  if (missing(x.var)) {
    x.var <- vars[1L]
    message("Note: ", x.var, " will be used for the horizontal axis")
  }
  if (!(x.var %in% vars)) stop (x.var, " is not in the model")
  if (!missing(others)){
    if (any(sapply(others, length) > 1L))
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
  predictions <- predict(x, newdata=new)
  response.levels <- levels(data[[response]])
  lower.upper <- paste0(".", c(round((1 - conf.level)/2, 4L),
                                round(1 - (1 - conf.level)/2, 4L)))
  # suffix for point-estimate columns returned by confint()
  pt_suffix <- if (scale == "logit") ".logit" else ".p"

  if (!is.null(conf.level)){
    ci <- confint(predictions, level=conf.level,
                  parm=if (scale == "logit") "logit" else "prob")
    new <- cbind(new, ci[, paste0(response.levels, pt_suffix)],
                 ci[, paste0(response.levels, lower.upper[1L])],
                 ci[, paste0(response.levels, lower.upper[2L])])
    lower_mat <- as.matrix(new[, paste0(response.levels, lower.upper[1L])])
    upper_mat <- as.matrix(new[, paste0(response.levels, lower.upper[2L])])
    ymin <- min(lower_mat[is.finite(lower_mat)])
    ymax <- max(upper_mat[is.finite(upper_mat)])
  } else {
    new <- cbind(new, if (scale == "logit") predictions$logit else predictions$p)
  }

  # column names for point estimates (depends on scale and whether CIs are computed)
  p_cols <- if (!is.null(conf.level)) paste0(response.levels, pt_suffix) else response.levels

  # validate label.x
  if (label) {
    if (length(label.x) == 1L) label.x <- rep(label.x, length(response.levels))
    if (length(label.x) != length(response.levels))
      stop("label.x must be length 1 or the same length as the number of response categories")
    if (!all(label.x %in% c("min", "max")))
      stop('each element of label.x must be "min" or "max"')
  }

  if (numeric.x){
    matplot(new[, x.var],
            new[, p_cols],
            type="l", lwd=lwd, lty=lty,
            col=col, xlab=xlab, ylab=ylab,
            ylim=if (!is.null(conf.level)) c(ymin, ymax) else NULL, ...)
    if (!is.null(conf.level)){
      for (i in seq_along(response.levels)){
        level <- response.levels[i]
        lower_y <- new[, paste0(level, lower.upper[1L])]
        upper_y <- new[, paste0(level, lower.upper[2L])]
        lower_y[!is.finite(lower_y)] <- ymin
        upper_y[!is.finite(upper_y)] <- ymax
        polygon(c(new[, x.var], rev(new[, x.var])),
                c(lower_y, rev(upper_y)),
                col=adjustcolor(col[i], alpha.f=conf.alpha), border=NA)
      }
    }
    if (label) {
      for (i in seq_along(response.levels)) {
        if (label.x[i] == "max") {
          idx <- nrow(new)
          adj <- c(1, -0.1)    # right edge of text at curve end, slightly above
        } else {               # "min"
          idx <- 1L
          adj <- c(0, -0.1)    # left edge of text at curve start, slightly above
        }
        text(new[idx, x.var], new[idx, p_cols[i]],
             labels = response.levels[i],
             adj = adj, col = label.col[i], cex = label.cex)
      }
    } else if (legend) {
      legend(legend.location, legend=response.levels,
             lty=lty, lwd=lwd,
             col=col, inset=legend.inset,
             bty = legend.bty,
             xpd=TRUE)
    }
  } else {
    n.x.levels <- nrow(new)
    matplot(1L:n.x.levels,
            new[, p_cols],
            type="b", lwd=lwd, lty=lty,
            pch=pch, col=col, xlab=xlab, ylab=ylab, axes=FALSE,
            ylim= if (!is.null(conf.level)) c(ymin, ymax) else NULL,
            ...)
    box()
    axis(2L)
    levels <- new[, x.var]
    axis(1L, at=1L:n.x.levels, labels=new[, x.var])
    if (!is.null(conf.level)){
      for (i in 1L:n.x.levels){
        for (j in seq_along(response.levels)){
          level <- response.levels[j]
          arrows(x0=i, y0=new[i, paste0(level, lower.upper[1L])],
                 y1=new[i, paste0(level, lower.upper[2L])],
                 angle=90, col=j, lwd=3, code=3L, length=0.1)
        }
      }
    }
    if (label) {
      for (i in seq_along(response.levels)) {
        if (label.x[i] == "max") {
          idx <- n.x.levels
          adj <- c(1, -0.1)    # right edge of text at curve end, slightly above
        } else {               # "min"
          idx <- 1L
          adj <- c(0, -0.1)    # left edge of text at curve start, slightly above
        }
        text(idx, new[idx, p_cols[i]],
             labels = response.levels[i],
             adj = adj, col = label.col[i], cex = label.cex)
      }
    } else if (legend) {
      legend(legend.location, legend=response.levels,
             lty=lty, lwd=lwd,
             col=col, pch=pch,
             inset=legend.inset,
             bty = legend.bty,
             xpd=TRUE)
    }
  }
  if (!missing(main)) {
    title(main, cex.main=cex.main, font.main=font.main)
  } else {
    if (length(values) != 0L){
      which.main <- !(x.var == names(values))
      if (any(which.main)){
        values <- lapply(values, function(x) if (is.numeric(x)) signif(x, digits.main) else x)
        main <- paste(paste(names(values[which.main]), "=",
                            as.character(unlist(values[which.main]))),
                      collapse=", ")
        title(main, cex.main=cex.main, font.main=font.main)
      }
    }
  }
  return(invisible(NULL))
}
