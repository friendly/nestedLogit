#' Extract Equations for a `"nestedLogit"` Model
#'
#' @description
#' Provides an [equatiomatic::extract_eq()] method for `"nestedLogit"` objects
#' to render the models as LaTeX equations in documents.
#'
#' It extracts the LaTeX equation for each of the binary logit sub-models
#' comprising a nested logit model, substituting the dichotomy name for the
#' internal `..y` placeholder used by [nestedLogit()].
#'
#' @param model a `"nestedLogit"` object.
#' @param \dots additional arguments passed to [equatiomatic::extract_eq()].
#'   For example, use `use_coefs = TRUE` to display fitted coefficients rather
#'   than symbols; `wrap = TRUE` to split the RHS across multiple lines;
#'   `greek_colors`, `var_colors`, etc. to color the symbols.
#'
#' @details
#' Internally, [nestedLogit()] fits each sub-model on a temporary response
#' column named `..y`, so [equatiomatic::extract_eq()] would render the
#' response as `..y` rather than the dichotomy name.  This method replaces
#' `..y` with the name of each dichotomy.
#'
#' Because `"_"` is the subscript operator in LaTeX, any underscores in
#' dichotomy names are replaced by `"."` in the displayed equation
#' (e.g., a dichotomy named `fish_inv` appears as `fish.inv`).
#' This does not affect the names of the returned list, which retain the
#' original R names.
#'
#' @return An object of class `c("nestedLogit_equations", "list")` containing
#'   one `"equation"` per dichotomy in `model`, named by the dichotomy.
#'
#' @seealso [equatiomatic::extract_eq()]
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
#' }
#'
extract_eq.nestedLogit <- function(model, ...) {
  mods <- models(model)
  nms  <- names(mods)
  eqns <- lapply(seq_along(mods), function(i) {
    eq <- equatiomatic::extract_eq(mods[[i]], ...)
    # The sub-models were fit on a data column named `..y`; equatiomatic usually
    # renders that literally.  Replace it with the dichotomy name.
    # Replace underscores with dots: `_` is the subscript operator in LaTeX so
    # `fish_inv` would render as "fish" subscript "inv".  Dots are safe.
    nm_latex <- gsub("_", ".", nms[i], fixed = TRUE)
    # Case 1: equatiomatic used the `..y` data-column name (the normal case).
    eq_str <- gsub("\\\\operatorname\\{\\.\\.y\\}(\\s*=\\s*\\\\operatorname\\{1\\})?",
                   paste0("\\\\operatorname{", nm_latex, "}"),
                   as.character(eq))
    # Case 2: equatiomatic used the formula LHS directly (e.g. `fish_inv`).
    # Use fixed = TRUE so the literal backslash and braces are matched as-is.
    if (grepl("_", nms[i], fixed = TRUE)) {
      eq_str <- gsub(paste0("\\operatorname{", nms[i], "}"),
                     paste0("\\operatorname{", nm_latex, "}"),
                     eq_str, fixed = TRUE)
    }
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
