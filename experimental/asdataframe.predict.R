# convert the result of predict() to a data frame
# to_long <- function(x, ...) {
#   UseMethod("x")
# }

as.data.frame.predictNestedLogit <- function(x, row.names = NULL, newdata, ...){
  if(missing(newdata)) stop("`newdata` is required.")
  resp.names <- colnames(x$p)

  idx <- rep(seq_len(nrow(newdata)), each = length(resp.names))
  result <- newdata[idx, ]
  result <- cbind(
    result,
    response = rep(resp.names, each = nrow(x$p)),
    p        = as.vector(x$p),
    se.p     = as.vector(x$se.p),
    logit    = as.vector(x$logit),
    se.logit = as.vector(x$se.logit)
  )
  rownames(result) <- NULL
  result
}


if(FALSE){
  data(Womenlf, package="carData")

  comparisons <- logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                        full=dichotomy("parttime", "fulltime"))

  wlf.nested <- nestedLogit(partic ~ hincome + children,
                            dichotomies = comparisons,
                            data=Womenlf)
  # get predicted values for a grid
  new <- expand.grid(hincome=seq(0, 45, length=4),
                     children=c("absent", "present"))

  pred.nested <- predict(wlf.nested, new)
  plotdata <- as.data.frame(pred.nested, newdata=new)
}
