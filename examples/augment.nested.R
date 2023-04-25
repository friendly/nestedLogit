#' Augment data with information from a nested object
#'
#' Augment accepts a model object and a dataset and adds information about each observation in the dataset,
#' typically to assist in preparing plots of the model. The general scheme is to append columns to the
#' data representing predicted values and other quantities (residuals, standard errors, ...) where available.
#'
#' A complication for nested dichotomies models is that there are at least two types of predicted values of
#' interest: the fitted \emph{logits} for the \eqn{m-1} dichotomies and the fitted \emph{probabilities} of the \eqn{m}
#' response categories.
#'
#' @param x       an object of class \code{"nested"} produced by \code{\link{nestedLogit}}.
#' @param data    the data set used to fit the model. If not supplied, this is extracted from the model object.
#' @param newdata a prediction data set containing values of all predictors used in the model
#' @param type.predict a character string, either \code{"link"} to calculate predicted values on the logit scale
#'                for each of the dichotomies, or \code{"probs"} to calculate fitted probabilities
#' @param se_fit  Logical. For predicted logits, whether to return the standard errors.
#' @param to_long Logical. For predicted probabilities, whether to convert these to long format.
#' @param ...     Other parameters passed to predict methods
#'
#' @return     Returns a \code{"tibble"} containing the values of the response variable, predictors and estimated
#'             values from the model.
#'             For \code{type.predict = "link"}, the dichotomies in the model are given as \code{response} and
#'             the predicted log odds is given as \code{.fitted}, with standard error \code{.se.fit}. The number
#'             of observations is
#' @importFrom generics augment
#' @importFrom broom augment.glm
#' @importFrom dplyr bind_rows bind_cols
#' @importFrom tidyr pivot_longer
#' @export
#'
#' @examples
augment.nested <- function(x,
                           data = NULL,
                           newdata = NULL,
                           type.predict = c("link", "probs"),
                           se_fit = TRUE,    # it is FALSE in augment.glm()
                           to_long = TRUE,
                           ...)
{
  if (is.null(data))
    data <- model.frame(x$models[[1L]])
  if (missing(newdata))
    newdata <- x$models[[1L]]$data

  type.predict <- match.arg(type.predict)

  if (type.predict == "link") {

    nms <- names(x$models)
    result <- lapply(x$models, broom::augment,
                     data=data,
                     newdata=newdata,
                     type.predict = type.predict,
                     se_fit = se_fit,
                     ... )

    cls <- class(result[[1L]])
    for (i in seq_along(x$models)) {
      result[[i]] <- cbind(response = nms[i], result[[i]])
    }
    result <- dplyr::bind_rows(result) |>
      select(-starts_with(".."))            # remove ..y
    class(result) <- cls
  }

  else {  #type.predict = "probs", as in multinom()
    result <- predict(x, newdata = newdata, ...)
    resp.names <- colnames(result)
    result <- dplyr::bind_cols(newdata, result) |>
      as_tibble()
#    if("..y" %in% colnames(result)) result <- select(result, -c("..y"))

    if(isTRUE(to_long)) {
      result <- result |>
      tidyr::pivot_longer(cols = all_of({{resp.names}}),
                                  names_to = "response.level",
                                  values_to = "prob")
    }
  }

  result

}



