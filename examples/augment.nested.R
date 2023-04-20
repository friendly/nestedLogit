#' ---
#' title: try broom::augment
#' ---
#'
library(nestedLogit)
library(dplyr)
library(broom)
data(Womenlf, package = "carData")

wlf.nested <- nestedLogit(partic ~ hincome + children,
                          logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                                 full=dichotomy("parttime", "fulltime")),
                          data=Womenlf)

# what does broom return?
broom::augment(wlf.nested$models[[1]]) |> head()
broom::augment(wlf.nested$models[[1]], se_fit = TRUE) |> head()

broom::augment(wlf.nested$models[[1]], type.predict = "response") |> head()
broom::augment(wlf.nested$models[[1]], type.predict = "response", se_fit = TRUE) |> head()


wlf.augmented <- lapply(wlf.nested$models, broom::augment)
names(wlf.augmented)


# quick try at an augment function
nested.augment <- function(x,
                           data = NULL,
                           newdata = NULL,
                           type.predict = c("link", "response"),
                           type.residuals = c("deviance", "pearson"),
                           se_fit = TRUE,    # it is FALSE in augment.glm()
                           ...)
{
  if (is.null(data))
    data <- model.frame(x$models[[1L]])
  if (missing(newdata))
    newdata <- x$models[[1L]]$data

  type.predict <- match.arg(type.predict)
  if (type.predict != "link") stop("type.predict = ", type.predict, " is not implemented.")

  nms <- names(object$models)
  result <- lapply(x, augment,
                   data=data,
                   newdata=newdata,
                   type.predict = type.predict,
                   type.residuals = type.residuals,
                   se_fit = se_fit,
                   ...
                   )

  cls <- class(result[[1L]])
  for (i in seq_along(x)) {
    result[[i]] <- cbind(response = nms[i], result[[i]])
  }
  result <- dplyr::bind_rows(result)
  class(result) <- cls
  result

}

