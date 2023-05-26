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
#' @seealso \code{\link{nestedMethods}}, \code{\link[broom]{glance}}, \code{\link[broom]{tidy}}
#' @exportS3Method broom::glance nestedLogit
#' @return \itemize{
#'    \item \code{glance} returns a \code{tibble} containing one row of fit statistics for each dichotomy,
#'          labeled \code{response}. See \code{\link[broom]{glance}} for details.
#'    \item \code{tidy} returns a \code{tibble} containing coefficient estimates and test statistics for
#'          the combinations of \code{response} and \code{term}. See \code{\link[broom]{tidy}} for details.
#' }
#' @examples
#' data("Womenlf", package = "carData")
#' m <-  nestedLogit(partic ~ hincome + children,
#'                   dichotomies = logits(work=dichotomy("not.work",
#'                                                       working=c("parttime", "fulltime")),
#'                                        full=dichotomy("parttime", "fulltime")),
#'                   data=Womenlf)
#'
#' # one-line summaries
#' broom::glance(m)
#' # coefficients and tests
#' broom::tidy(m)
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
