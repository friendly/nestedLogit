plot.nestedLogit <- function(x, x.var, others, n.x.values=100L,
                             xlab=x.var, ylab="Fitted Probability",
                             main, cex.main=1, font.main=1L,
                             pch=1L:length(response.levels),
                             lwd=3, lty=1L:length(response.levels),
                             col=palette()[1L:length(response.levels)],
                             legend=TRUE, legend.inset=0.01,
                             legend.location="topleft", ...){
  data <- x$data
  vars <- all.vars(formula(x)[-2L])
  response <- setdiff(all.vars(formula(x)), vars)
  if (missing(x.var)) {
    x.var <- setdiff(vars, response)[1L]
    message("Note: ", x.var, " will be used for the horizontal axis")
  }
  if (!(x.var %in% vars)) stop (x.var, " is not in the model")
  if (!missing(others)){
    other.x <- names(others)
    check <- other.x %in% vars
    if (any(!check)) stop("not in the model: ", paste(other.x[!check], collapse=", "))
    values <- others
  } else {
    values <- list()
  }
  other.xs <- setdiff(vars, c(response, x.var))
  missing.xs <- !(other.xs %in% names(values))
  if (any(missing.xs)){
    missing.xs <- other.xs[missing.xs]
    for (missing.x in missing.xs){
      xx <- x$data[, missing.x]
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
            col=col, xlab=xlab, ylab=ylab)
    if (legend) legend(legend.location, legend=response.levels, lty=lty, lwd=lwd,
                       col=col, inset=legend.inset, xpd=TRUE)
  } else {
    n.x.levels <- nrow(new)
    matplot(1L:n.x.levels, new[, response.levels], type="b", lwd=lwd,
            pch=pch, col=col, xlab=xlab, ylab=ylab, axes=FALSE)
    box()
    axis(2L)
    axis(1L, at=1L:n.x.levels, labels=new[, x.var])
    if (legend) legend(legend.location, legend=response.levels, lty=lty, lwd=lwd,
                       col=col, pch=pch, inset=legend.inset, xpd=TRUE)
  }
  if (!missing(main)) {
    title(main, cex.main=cex.main, font.main=font.main)
  } else {
    if (length(values) != 0L){
      which.main <- !(x.var == names(values))
      main <- paste(paste(names(values[which.main]), "=", 
                          as.character(unlist(values[which.main]))),
                    collapse=", ")
      title(main, cex.main=cex.main, font.main=font.main)
    }
  }
  return(invisible(NULL))
}

if (FALSE){
  Womenlf$kids <- ifelse(Womenlf$children == "absent", 0, 1)
  mm <- nestedLogit(partic ~ hincome + kids + region, 
                    list("not.work", work=list("parttime", "fulltime")), 
                    data=Womenlf)
  plot(mm, x.var="hincome")
  plot(mm)
}
