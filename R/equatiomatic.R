#' Extract Equations for a \code{"nestedLogit"} Model
#'
#' @description
#' An \code{\link[equatiomatic]{extract_eq}} method for \code{"nestedLogit"} objects.
#' Extracts the LaTeX equation for each of the binary logit sub-models comprising
#' a nested logit model, substituting the dichotomy name for the internal
#' \code{..y} placeholder used by \code{\link{nestedLogit}}.
#'
#' @param model a \code{"nestedLogit"} object.
#' @param \dots additional arguments passed to \code{\link[equatiomatic]{extract_eq}}.
#'
#' @return An object of class \code{c("nestedLogit_equations", "list")} containing
#'   one \code{"equation"} per dichotomy in \code{model}, named by the dichotomy.
#'
#' @seealso \code{\link[equatiomatic]{extract_eq}}
#' @exportS3Method equatiomatic::extract_eq nestedLogit
#'
#' @examples
#' data("Womenlf", package = "carData")
#' wlf.nested <- nestedLogit(partic ~ hincome + children,
#'                  dichotomies = logits(work = dichotomy("not.work",
#'                                                        working = c("parttime", "fulltime")),
#'                                       full = dichotomy("parttime", "fulltime")),
#'                  data = Womenlf)
#' if (requireNamespace("equatiomatic", quietly = TRUE)) {
#'   equatiomatic::extract_eq(wlf.nested)
#'
#'  # render with katex
#'  eqns <- equatiomatic::extract_eq(wlf.nested)
#'  katex::katex_html(as.character(eqns$work), preview = TRUE)
#' }
#'
extract_eq.nestedLogit <- function(model, ...) {
  mods <- models(model)
  nms  <- names(mods)
  eqns <- lapply(seq_along(mods), function(i) {
    eq <- equatiomatic::extract_eq(mods[[i]], ...)
    # The sub-models were fit on a data column named `..y`; equatiomatic renders
    # that literally.  Replace it with the dichotomy name.
    eq_str <- gsub("\\\\operatorname\\{\\.\\.y\\}(\\s*=\\s*\\\\operatorname\\{1\\})?",
                   paste0("\\\\operatorname{", nms[i], "}"),
                   as.character(eq))
    class(eq_str) <- class(eq)
    eq_str
  })
  names(eqns) <- nms
  class(eqns) <- c("nestedLogit_equations", "list")
  eqns
}

#' @export
print.nestedLogit_equations <- function(x, ...) {
  for (nm in names(x)) {
    cat("\nDichotomy:", nm, "\n")
    print(x[[nm]])
  }
  invisible(x)
}
