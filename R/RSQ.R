#' Pseudo-R² Measures for Nested Logit Models
#'
#' @description
#' Computes pseudo-R² and related fit measures for a \code{"nestedLogit"} object,
#' with one row per binary logit sub-model (dichotomy) and an additional
#' \code{"Combined"} row for the overall polytomous model.
#'
#' @details
#' `RSQ` is implemented as an S3 generic to allow for similar functions for related models
#'
#' The following measures are available via the \code{which} argument:
#' \describe{
#'   \item{\code{"McFadden"}}{1 - L/L\eqn{_0}}
#'   \item{\code{"McFaddenAdj"}}{1 - (L - k)/L\eqn{_0}, penalised for number of predictors}
#'   \item{\code{"CoxSnell"}}{1 - exp(2(L\eqn{_0} - L)/n), bounded strictly below 1}
#'   \item{\code{"Nagelkerke"}}{Cox-Snell rescaled to have a maximum of 1}
#'   \item{\code{"Tjur"}}{Difference in mean fitted values between the two response
#'     categories; per-dichotomy only (\code{NA} in the Combined row)}
#' }
#'
#' For the \strong{Combined} row the log-likelihood is the sum of the sub-model
#' log-likelihoods (exploiting the independence of the nested dichotomies), and
#' \eqn{n} is \code{nrow(x$data)} --- the full sample size of the polytomous model ---
#' not the sum of per-dichotomy observation counts, which would double-count
#' observations that appear in more than one sub-model.
#'
#' A wider range of pseudo-R² measures for logistic-type models (\code{glm},
#' \code{polr}, \code{multinom}, \code{vglm}) is available in
#' \code{\link[DescTools]{PseudoR2}}.  For an accessible overview of these
#' measures see \url{https://statisticalhorizons.com/r2logistic/}.
#'
#' @param x      a \code{"nestedLogit"} object.
#' @param which  character vector naming the pseudo-R² measures to compute.
#'   Any subset of \code{c("McFadden", "McFaddenAdj", "CoxSnell", "Nagelkerke", "Tjur")}.
#'   Default: \code{c("McFadden", "CoxSnell", "Nagelkerke")}.
#' @param include character vector of additional columns to append to the result.
#'   Any subset of \code{c("AIC", "BIC", "n")}, where \code{"n"} adds the
#'   number of observations used for each row.  Default: \code{"AIC"}.
#' @param digits  integer; number of decimal places used when printing
#'   (default \code{4L}).
#' @param \dots   currently unused.
#'
#' @return An object of class \code{c("RSQ.nestedLogit", "data.frame")} with one
#'   row per dichotomy plus a final \code{"Combined"} row, and columns for the
#'   model name, the requested pseudo-R² measures, and any additional statistics
#'   requested via \code{include}.  The \code{formula} used to fit the model and
#'   the \code{digits} argument are stored as attributes and used by the
#'   \code{print} method.
#'
#' @seealso \code{\link{nestedLogit}}, \code{\link[broom]{glance}},
#'   \code{\link[DescTools]{PseudoR2}}
#' @author Michael Friendly
#' @examples
#' data("Womenlf", package = "carData")
#' wlf.nested <- nestedLogit(partic ~ hincome + children,
#'   logits(work = dichotomy("not.work", c("parttime", "fulltime")),
#'          full = dichotomy("parttime", "fulltime")),
#'   data = Womenlf)
#'
#' # Default: McFadden, CoxSnell, Nagelkerke + AIC
#' RSQ(wlf.nested)
#'
#' # All measures, with AIC, BIC & n
#' RSQ(wlf.nested,
#'     which   = c("McFadden", "McFaddenAdj", "CoxSnell", "Nagelkerke", "Tjur"),
#'     include = c("AIC", "BIC", "n"))
#'
#' @importFrom utils capture.output
#' @importFrom stats AIC BIC formula fitted
#' @export
RSQ <- function(x, ...) UseMethod("RSQ")

#' @rdname RSQ
#' @export
RSQ.nestedLogit <- function(x,
                             which   = c("McFadden", "CoxSnell", "Nagelkerke"),
                             include = "AIC",
                             digits  = 4L,
                             ...) {
  which <- match.arg(which,
                     choices      = c("McFadden", "McFaddenAdj",
                                      "CoxSnell", "Nagelkerke", "Tjur"),
                     several.ok   = TRUE)
  include <- match.arg(include,
                       choices    = c("AIC", "BIC", "n"),
                       several.ok = TRUE)

  # glance.nestedLogit() gives one row per dichotomy with all ingredients:
  #   null.deviance, df.null, logLik, AIC, BIC, df.residual, nobs, ...
  gl        <- broom::glance(x)
  mod_names <- gl$response

  # --- Per-dichotomy rows ---
  rows <- vector("list", nrow(gl))
  for (i in seq_along(mod_names)) {
    L  <- gl$logLik[i]
    L0 <- -gl$null.deviance[i] / 2
    n  <- gl$nobs[i]
    k  <- gl$df.null[i] - gl$df.residual[i]   # non-intercept parameters
    m  <- models(x, mod_names[i])
    rows[[i]] <- .pseudo_r2(L, L0, n, k, which,
                             fitted_vals = fitted(m),
                             y           = m$y)
  }
  sub_df <- dplyr::bind_rows(rows)

  # --- Combined row ---
  # L_combined = sum of sub-model log-likelihoods (independence of dichotomies)
  # n_combined = full sample size (not sum of per-dichotomy nobs)
  L_combined  <- sum(gl$logLik)
  L0_combined <- sum(-gl$null.deviance / 2)
  n_combined  <- nrow(x$data)
  k_combined  <- sum(gl$df.null - gl$df.residual)

  combined_row <- .pseudo_r2(L_combined, L0_combined, n_combined, k_combined,
                              setdiff(which, "Tjur"))
  if ("Tjur" %in% which) combined_row$Tjur <- NA_real_
  # Restore column order to match sub_df
  combined_row <- combined_row[, names(sub_df), drop = FALSE]

  # --- Assemble ---
  result <- dplyr::bind_rows(sub_df, combined_row)
  result <- dplyr::bind_cols(
    data.frame(model = c(mod_names, "Combined"), stringsAsFactors = FALSE),
    result
  )

  # --- Optional extra columns ---
  if ("AIC" %in% include) result$AIC <- c(gl$AIC,  AIC(x))
  if ("BIC" %in% include) result$BIC <- c(gl$BIC,  BIC(x))
  if ("n"   %in% include) result$n   <- c(gl$nobs, n_combined)

  structure(result,
            class   = c("RSQ.nestedLogit", "data.frame"),
            formula = formula(x),
            which   = which,
            digits  = digits)
}

# Internal helper: compute pseudo-R² values from log-likelihood components.
# Not exported.
.pseudo_r2 <- function(L, L0, n, k, which, fitted_vals = NULL, y = NULL) {
  cs     <- 1 - exp(2 * (L0 - L) / n)
  cs_max <- 1 - exp(2 * L0 / n)
  result <- list()
  if ("McFadden"    %in% which) result$McFadden    <- 1 - L / L0
  if ("McFaddenAdj" %in% which) result$McFaddenAdj <- 1 - (L - k) / L0
  if ("CoxSnell"    %in% which) result$CoxSnell    <- cs
  if ("Nagelkerke"  %in% which) result$Nagelkerke  <- cs / cs_max
  if ("Tjur"        %in% which) {
    result$Tjur <- if (!is.null(fitted_vals) && !is.null(y))
      mean(fitted_vals[y == 1L]) - mean(fitted_vals[y == 0L])
    else NA_real_
  }
  as.data.frame(result)
}

#' @rdname RSQ
#' @export
print.RSQ.nestedLogit <- function(x, digits = attr(x, "digits"), ...) {
  cat("Pseudo R\u00b2 measures for nestedLogit model:\n")
  cat(" ", paste(deparse(attr(x, "formula")), collapse = " "), "\n\n")

  n_dichot <- nrow(x) - 1L

  # Round numeric columns before formatting
  out      <- as.data.frame(x)
  num_cols <- setdiff(names(out), "model")
  out[, num_cols] <- lapply(out[, num_cols, drop = FALSE], round, digits)

  # Capture formatted lines so we can insert a separator before Combined
  txt <- utils::capture.output(print(out, row.names = FALSE))
  # txt[1]              = column header line
  # txt[2:(1+n_dichot)] = one line per dichotomy
  # txt[2+n_dichot]     = Combined row
  cat(txt[1L], "\n")
  for (i in seq_len(n_dichot)) cat(txt[1L + i], "\n")
  cat(strrep("-", nchar(txt[1L])), "\n")
  cat(txt[2L + n_dichot], "\n")

  invisible(x)
}
