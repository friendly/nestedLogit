#' Pseudo-R² Measures for Nested Logit and Related Models
#'
#' @description
#' Computes pseudo-R² and related fit measures for a `"nestedLogit"` object
#' and related models for a polytomous response. For the `"nestedLogit"` case,
#' the result shows
#' one row per binary logit sub-model (dichotomy) and an additional
#' `"Combined"` row for the overall polytomous model.
#'
#' @details
#' `RSQ` is implemented as an S3 generic with methods for `"nestedLogit"`, as well as
#' `nnet::multinom()`, and `MASS::polr()` objects, which are other methods for
#' modeling a polytomous response variable.
#'
#' In contrast to standard, Gaussian linear models, where \eqn{R^2} has a uniformly simple interpretation as
#' "variance accounted for" by the model, and with different, yet _equivalent_ computational formulas,
#' there is no single commonly accepted measure for logistic regression models for a binary response or
#' a dichotomy among outcomes.
#'
#' The following measures are available via the `which` argument:
#' \describe{
#'   \item{`"McFadden"`}{1 - L/L\eqn{_0}, where L is the fitted model
#'     log-likelihood and L\eqn{_0} that of the null (intercept-only) model
#'     (McFadden, 1979).  Values of 0.1--0.3 indicate a reasonable fit in
#'     logistic regression.}
#'   \item{`"McFaddenAdj"`}{1 - (L - k)/L\eqn{_0}, where k is the number
#'     of non-intercept parameters; penalises model complexity (Hosmer &
#'     Lemeshow, 2000).}
#'   \item{`"CoxSnell"`}{1 - exp(2(L\eqn{_0} - L)/n); bounded strictly
#'     below 1 for discrete outcomes (Cox & Snell, 1989).}
#'   \item{`"Nagelkerke"`}{Cox-Snell divided by its theoretical maximum,
#'     rescaling to \[0, \1] (Nagelkerke, 1991).}
#'   \item{`"Tjur"`}{Mean fitted value for \eqn{y = 1} minus mean fitted
#'     value for \eqn{y = 0}; the coefficient of discrimination (Tjur, 2009).
#'     Per-dichotomy only (`NA` in the Combined row).}
#' }
#'
#' For the **Combined** row the log-likelihood is the sum of the sub-model
#' log-likelihoods (exploiting the independence of the nested dichotomies), and
#' \eqn{n} is `nrow(x$data)` --- the full sample size of the polytomous model ---
#' not the sum of per-dichotomy observation counts, which would double-count
#' observations that appear in more than one sub-model.
#'
#' A wider range of pseudo-R² measures for logistic-type models (`glm`,
#' `polr`, `multinom`, `vglm`) is available in
#' [DescTools::PseudoR2()], including the Efron (1978) and
#' McKelvey & Zavoina (1975) measures not implemented here.
#' For an accessible overview see \url{https://statisticalhorizons.com/r2logistic/}.
#'
#' @param x      a `"nestedLogit"` object.
#' @param which  character vector naming the pseudo-R² measures to compute.
#'   Any subset of `c("McFadden", "McFaddenAdj", "CoxSnell", "Nagelkerke", "Tjur")`,
#'   or `"ALL"` to include all of them.
#'   Default: `c("McFadden", "CoxSnell", "Nagelkerke")`.
#' @param include character vector of additional columns to append to the result.
#'   Any subset of `c("AIC", "BIC", "n")`, where `"n"` adds the
#'   number of observations used for each row, or `"ALL"` to include all of them.
#'   Default: `"AIC"`.
#' @param digits  integer; number of decimal places used when printing
#'   (default `3L`).
#' @param ...   currently unused.
#'
#' @return An object of class `c("RSQ.nestedLogit", "data.frame")` with one
#'   row per dichotomy plus a final `"Combined"` row, and columns
#'   `response` (the sub-model name), the requested pseudo-R² measures,
#'   and any additional statistics requested via `include`.
#'   The `formula`, object name, and `digits` are stored as attributes
#'   and used by the `print` method.
#'
#' @references
#' Cox, D. R., & Snell, E. J. (1989). *The Analysis of Binary Data* (2nd ed.).
#' Chapman and Hall.
#'
#' Efron, B. (1978). Regression and ANOVA with zero-one data: Measures of
#' residual variation. *Journal of the American Statistical Association*,
#' *73*(361), 113--121. \url{https://doi.org/10.2307/2286498}
#'
#' Hosmer, D. W., & Lemeshow, S. (2000). *Applied Logistic Regression*
#' (2nd ed.). Wiley. \url{https://doi.org/10.1002/0471722146}
#'
#' McFadden, D. (1979). Quantitative methods for analysing travel behaviour of
#' individuals: Some recent developments. In D. A. Hensher & P. R. Stopher
#' (Eds.), *Behavioural Travel Modelling* (pp. 279--318). Croom Helm.
#'
#' McKelvey, R. D., & Zavoina, W. (1975). A statistical model for the analysis
#' of ordinal level dependent variables. *Journal of Mathematical Sociology*,
#' *4*(1), 103--120. \url{https://doi.org/10.1080/0022250X.1975.9989847}
#'
#' Nagelkerke, N. J. D. (1991). A note on a general definition of the
#' coefficient of determination. *Biometrika*, *78*(3), 691--692.
#' \url{https://doi.org/10.1093/biomet/78.3.691}
#'
#' Tjur, T. (2009). Coefficients of determination in logistic regression
#' models --- a new proposal: The coefficient of discrimination.
#' *The American Statistician*, *63*(4), 366--372.
#' \url{https://doi.org/10.1198/tast.2009.08210}
#'
#' @seealso [nestedLogit()], [broom::glance()],
#'   [DescTools::PseudoR2()],
#'   [nnet::multinom()], [MASS::polr()]
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
#' # All measures and all extra columns
#' RSQ(wlf.nested, which = "ALL", include = "ALL")
#'
#' # Multinomial logit for comparison
#' if (requireNamespace("nnet", quietly = TRUE)) {
#'   wlf.multi <- nnet::multinom(partic ~ hincome + children, data = Womenlf,
#'                               trace = FALSE)
#'   RSQ(wlf.multi)
#' }
#'
#' # Proportional-odds model for comparison
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   wlf.polr <- MASS::polr(partic ~ hincome + children, data = Womenlf)
#'   RSQ(wlf.polr)
#' }
#'
#' @importFrom utils capture.output
#' @importFrom stats AIC BIC formula fitted model.frame model.response
#' @export
RSQ <- function(x, ...) UseMethod("RSQ")

#' @rdname RSQ
#' @export
RSQ.nestedLogit <- function(x,
                             which   = c("McFadden", "CoxSnell", "Nagelkerke"),
                             include = "AIC",
                             digits  = 3L,
                             ...) {
  obj_name    <- deparse(substitute(x))
  all_which   <- c("McFadden", "McFaddenAdj", "CoxSnell", "Nagelkerke", "Tjur")
  all_include <- c("AIC", "BIC", "n")
  if (identical(which,   "ALL")) which   <- all_which
  if (identical(include, "ALL")) include <- all_include
  which <- match.arg(which,   choices = all_which,   several.ok = TRUE)
  include <- match.arg(include, choices = all_include, several.ok = TRUE)

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
    data.frame(response = c(mod_names, "Combined"), stringsAsFactors = FALSE),
    result
  )

  # --- Optional extra columns ---
  if ("AIC" %in% include) result$AIC <- c(gl$AIC,  AIC(x))
  if ("BIC" %in% include) result$BIC <- c(gl$BIC,  BIC(x))
  if ("n"   %in% include) result$n   <- c(gl$nobs, n_combined)

  structure(result,
            class      = c("RSQ.nestedLogit", "data.frame"),
            formula    = formula(x),
            model.name = obj_name,
            which      = which,
            digits     = digits)
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
  cat(sprintf("Pseudo R\u00b2 measures for nestedLogit model %s:\n",
              attr(x, "model.name")))
  cat(" ", paste(deparse(attr(x, "formula")), collapse = " "), "\n\n")

  n_dichot <- nrow(x) - 1L

  # Round numeric columns before formatting
  out      <- as.data.frame(x)
  num_cols <- setdiff(names(out), "response")
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

#' @rdname RSQ
#' @importFrom stats logLik
#' @export
RSQ.multinom <- function(x,
                          which   = c("McFadden", "CoxSnell", "Nagelkerke"),
                          include = "AIC",
                          digits  = 3L,
                          ...) {
  obj_name    <- deparse(substitute(x))
  all_which   <- c("McFadden", "McFaddenAdj", "CoxSnell", "Nagelkerke", "Tjur")
  all_include <- c("AIC", "BIC", "n")
  if (identical(which,   "ALL")) which   <- all_which
  if (identical(include, "ALL")) include <- all_include
  which   <- match.arg(which,   choices = all_which,   several.ok = TRUE)
  include <- match.arg(include, choices = all_include, several.ok = TRUE)

  # --- Ingredients ---
  L  <- as.numeric(logLik(x))
  n  <- nrow(model.frame(x))

  # Null log-likelihood: intercept-only model predicts marginal proportions
  y   <- model.response(model.frame(x))
  n_j <- as.integer(table(y))
  n_j <- n_j[n_j > 0L]          # drop empty levels
  L0  <- sum(n_j * log(n_j / n))

  m <- length(n_j)               # number of response categories
  # Non-intercept parameters: total df minus one intercept per non-ref category
  k <- attr(logLik(x), "df") - (m - 1L)

  # Tjur is defined only for binary responses
  result <- .pseudo_r2(L, L0, n, k, setdiff(which, "Tjur"))
  if ("Tjur" %in% which) result$Tjur <- NA_real_
  result <- result[, intersect(all_which, names(result)), drop = FALSE]

  # Response variable name as the row label
  resp_name <- deparse(formula(x)[[2L]])
  result <- dplyr::bind_cols(
    data.frame(response = resp_name, stringsAsFactors = FALSE),
    result
  )

  # --- Optional extra columns ---
  if ("AIC" %in% include) result$AIC <- AIC(x)
  if ("BIC" %in% include) result$BIC <- BIC(x)
  if ("n"   %in% include) result$n   <- n

  structure(result,
            class      = c("RSQ.multinom", "data.frame"),
            formula    = formula(x),
            model.name = obj_name,
            which      = which,
            digits     = digits)
}


#' @rdname RSQ
#' @export
print.RSQ.multinom <- function(x, digits = attr(x, "digits"), ...) {
  cat(sprintf("Pseudo R\u00b2 measures for multinom model %s:\n",
              attr(x, "model.name")))
  cat(" ", paste(deparse(attr(x, "formula")), collapse = " "), "\n\n")

  out      <- as.data.frame(x)
  num_cols <- setdiff(names(out), "response")
  out[, num_cols] <- lapply(out[, num_cols, drop = FALSE], round, digits)
  print(out, row.names = FALSE)

  invisible(x)
}

#' @importFrom stats logLik
#' @rdname RSQ
#' @export
RSQ.polr <- function(x,
                     which   = c("McFadden", "CoxSnell", "Nagelkerke"),
                     include = "AIC",
                     digits  = 3L,
                     ...) {
  obj_name    <- deparse(substitute(x))
  all_which   <- c("McFadden", "McFaddenAdj", "CoxSnell", "Nagelkerke", "Tjur")
  all_include <- c("AIC", "BIC", "n")
  if (identical(which,   "ALL")) which   <- all_which
  if (identical(include, "ALL")) include <- all_include
  which   <- match.arg(which,   choices = all_which,   several.ok = TRUE)
  include <- match.arg(include, choices = all_include, several.ok = TRUE)

  L   <- as.numeric(logLik(x))
  n   <- nrow(model.frame(x))

  # Null log-likelihood: intercept-only model predicts marginal proportions
  y   <- model.response(model.frame(x))
  n_j <- as.integer(table(y))
  n_j <- n_j[n_j > 0L]
  L0  <- sum(n_j * log(n_j / n))

  # Non-intercept parameters: slopes only (thresholds/zeta are not predictors)
  k   <- length(x$coefficients)

  # Tjur is defined only for binary responses
  result <- .pseudo_r2(L, L0, n, k, setdiff(which, "Tjur"))
  if ("Tjur" %in% which) result$Tjur <- NA_real_
  result <- result[, intersect(all_which, names(result)), drop = FALSE]

  resp_name <- deparse(formula(x)[[2L]])
  result <- dplyr::bind_cols(
    data.frame(response = resp_name, stringsAsFactors = FALSE),
    result
  )

  if ("AIC" %in% include) result$AIC <- AIC(x)
  if ("BIC" %in% include) result$BIC <- BIC(x)
  if ("n"   %in% include) result$n   <- n

  structure(result,
            class      = c("RSQ.polr", "data.frame"),
            formula    = formula(x),
            model.name = obj_name,
            which      = which,
            digits     = digits)
}

#' @rdname RSQ
#' @export
print.RSQ.polr <- function(x, digits = attr(x, "digits"), ...) {
  cat(sprintf("Pseudo R\u00b2 measures for polr model %s:\n",
              attr(x, "model.name")))
  cat(" ", paste(deparse(attr(x, "formula")), collapse = " "), "\n\n")

  out      <- as.data.frame(x)
  num_cols <- setdiff(names(out), "response")
  out[, num_cols] <- lapply(out[, num_cols, drop = FALSE], round, digits)
  print(out, row.names = FALSE)

  invisible(x)
}
