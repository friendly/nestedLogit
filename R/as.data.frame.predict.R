
#' Convert a Predicted Objects to a data.frame
#'
#' These functions provide simple ways to convert the results of \code{\link{predict.nestedLogit}}
#' to a data frame in a consistent format for plotting and other actions.
#'
#' @param x         a \code{"predictNestedLogit"} object
#' @param row.names row.names for result (for conformity with generic; not currently used)
#' @param optional  logical. If TRUE, setting row names and converting column names
#'        (to syntactic names: see \code{\link[base]{make.names}} is optional
#' @param ...       other arguments (unused)
#'
#' @return \itemize{
#'  \item For \code{predict(\dots, model="nested")} (the default), returns
#'  a data frame containing the values of predictors along with the columns
#'         \code{response}, \code{p}, \code{se.p}, \code{logit}, \code{se.logit}.
#'  \item For \code{predict(\dots, model="dichotomies")}, returns
#'  a data frame containing the values of predictors along with the columns
#'         \code{response}, \code{logit}, and \code{se.logit}.
#'    }
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
#' plotdata <- as.data.frame(pred.nested)
#' str(plotdata)
#'
#' # Predicted logit values for the dichotomies
#' pred.dichot <- predict(wlf.nested, new, model = "dichotomies")
#' plotlogit <- as.data.frame(pred.dichot)
#' str(plotlogit)

as.data.frame.predictNestedLogit <- function(x,
                                             row.names = NULL,
                                             optional = FALSE,
                                             ...){
  resp.names <- colnames(x$p)

  result <- data.frame(
    response = rep(resp.names, nrow(x$p)),
    p        = as.vector(t(x$p)),
    se.p     = as.vector(t(x$se.p)),
    logit    = as.vector(t(x$logit)),
    se.logit = as.vector(t(x$se.logit))
  )
  .data <- x$.data
  if (!is.null(.data)) {
    idx <- rep(seq_len(nrow(.data)), each = length(resp.names))
    result <- cbind(.data[idx, , drop = FALSE], result)
  }

  rownames(result) <- NULL
  result
}

#' @importFrom dplyr select bind_cols
#' @importFrom tibble rownames_to_column
#' @importFrom stringr str_remove
#' @export

as.data.frame.predictDichotomies <- function(x,
                                             row.names = NULL,
                                             optional = FALSE,
                                             ...){
  # quiet no visible binding
  response <- fit <- se.fit <- residual.scale <- NULL
  dichotomies <- attr(x, "dichotomies")
  .data <- x$.data

  result <- do.call(rbind, x[dichotomies]) |>
    dplyr::select(- residual.scale) |>
    tibble::rownames_to_column(var = "response") |>
    dplyr::mutate(response =  stringr::str_remove(response, ".\\d+")) |>
    dplyr::rename(logit = fit,
           se.logit = se.fit)

  if(!is.null(.data)) {
    nlogits <- length(x[dichotomies])
    idx <- rep(seq_len(nrow(.data)), nlogits)
    result <- dplyr::bind_cols(.data[idx,], result)
  }
  result
}


# Example of steps
# plotlogit <- do.call(rbind, pred.dichot) |>
#   select(- residual.scale) |>
#   tibble::rownames_to_column(var = "response") |>
#   mutate(response =  stringr::str_remove(response, ".\\d+")) |>
#   rename(logit = fit,
#          se.logit = se.fit)
# idx <- rep(seq_len(nrow(new)), length(pred.dichot))
# plotlogit <- bind_cols(new[idx,], plotlogit)
