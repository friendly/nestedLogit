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
#' @seealso \code{\link{nestedMethods}}
#' @exportS3Method broom::glance nestedLogit
#' @examples
#' data(Womenlf, package = "carData")
#' m <-  nestedLogit(partic ~ hincome + children,
#'                   dichotomies = logits(work=dichotomy("not.work",
#'                                                       working=c("parttime", "fulltime")),
#'                                        full=dichotomy("parttime", "fulltime")),
#'                   data=Womenlf)
#'
#' # one-line summaries
#' glance(m)
#' # coefficients and tests
#' tidy(m)
#'
glance.nestedLogit <- function(x, ...){
  result <- dplyr::bind_rows(lapply(models(x), broom::glance))
  result <- dplyr::bind_cols(response = names(models(x)), result)
  result
}

#' @importFrom broom tidy
#' @rdname broomMethods
#' @exportS3Method broom::tidy nestedLogit
tidy.nestedLogit <- function(x, ...){
  result <- dplyr::bind_rows(lapply(models(x), broom::tidy, ...))
  response <- rep(names(models(x)), each = nrow(result)/length(models(x)))
  result <- dplyr::bind_cols(response = response, result)
  result
}
