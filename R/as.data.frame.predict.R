
#' Convert a predictNestedLogit object to a data.frame
#'
#' @param x         a \code{"predictNestedLogit"} object
#' @param row.names row.names for result (for conformity with generic; not currently used)
#' @param optional  logical. If TRUE, setting row names and converting column names
#'        (to syntactic names: see make.names) is optional
#' @param newdata   A  \code{newdata} data.frame used to generate predicted values. If not supplied,
#'        the original data frame is used.
#' @param ...       other arguments (unused)
#'
#' @return A data frame containing the newdata values of predictors along with the columns
#'         \code{response}, \code{p}, \code{se.p}, \code{logit}, \code{se.logit}
#' @export
#'
#' @examples
#' data("Womenlf", package = "carData")
#' comparisons <- logits(work=dichotomy("not.work", c("parttime", "fulltime")),
#'                      full=dichotomy("parttime", "fulltime"))
#'
#' wlf.nested <- nestedLogit(partic ~ hincome + children,
#'                           dichotomies = comparisons,
#'                           data=Womenlf)
#' # get predicted values for a grid of `hincome` and `children`
#' new <- expand.grid(hincome=seq(0, 45, length=10),
#'                    children=c("absent", "present"))
#'
#' pred.nested <- predict(wlf.nested, new)
#' plotdata <- as.data.frame(pred.nested, newdata=new)
#' str(plotdata)

as.data.frame.predictNestedLogit <- function(x, row.names = NULL, optional = FALSE, newdata, ...){
  resp.names <- colnames(x$p)

  result <- data.frame(
    response = rep(resp.names, nrow(x$p)),
    p        = as.vector(t(x$p)),
    se.p     = as.vector(t(x$se.p)),
    logit    = as.vector(t(x$logit)),
    se.logit = as.vector(t(x$se.logit))
  )
  if(!missing(newdata)) {
    if (nrow(newdata) != nrow(x$p)) stop("number of rows of newdata, ", nrow(newdata),
                                         ",  must match number of rows, ", nrow(x$p),
                                         ", in predictions.")
    idx <- rep(seq_len(nrow(newdata)), each = length(resp.names))
    result <- cbind(newdata[idx, ], result)
  }

  rownames(result) <- NULL
  result
}


