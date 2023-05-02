#' Broom Related Methods
#'
#' These functions give compact summaries of a \code{"nestedLogit"} object
#' \describe{
#'   \item{\code{glance}}{Construct a single row summaries for the dichotomies \code{"nestedLogit"} model.}
#'   \item{\code{tidy}}{Summarizes the terms in \code{"nestedLogit"} model.}
#' }
#'
#' @name broomMethods
#' @aliases glance.nestedLogit tidy.nestedLogit
#' @param x     an object of class \code{"nestedLogit"}.
#' @param \dots arguments to be passed down.
#' @importFrom broom glance
#' @rdname broomMethods
#' @exportS3Method broom::glance nestedLogit
#' @examples
#' data(Womenlf, package = "carData")
#' m <-  nestedLogit(partic ~ hincome + children,
#'                   dichotomies = logits(work=dichotomy("not.work",
#'                                                       working=c("parttime", "fulltime")),
#'                   full=dichotomy("parttime", "fulltime")),
#'                   data=Womenlf)
#' broom::glance(m)
#' broom::tidy(m)
#'
glance.nestedLogit <- function(x, ...){
  result <- dplyr::bind_rows(lapply(x$models, broom::glance))
  result <- dplyr::bind_cols(response = names(x$models), result)
  result
}

#' @importFrom broom tidy
#' @rdname broomMethods
#' @exportS3Method broom::tidy nestedLogit
tidy.nestedLogit <- function(x, ...){
  result <- dplyr::bind_rows(lapply(x$models, broom::tidy, ...))
  response <- rep(names(x$models), each = nrow(result)/length(x$models))
  result <- dplyr::bind_cols(response = response, result)
  result
}
