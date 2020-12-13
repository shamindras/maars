#' Compute the White sandwich estimator of standard errors for OLS
#'
#' Compute the White sandwich estimator of standard errors for
#' ordinary least squares (OLS) regression, \insertCite{@see @white1980usinglsapproxunknownregfuncs and @white1980heteroskedasticconsistentcovest;textual}{maar}. For
#' more details
#' \insertCite{@see also @buja2019modelsasapproximationspart1 and @buja2019modelsasapproximationspart2;textual}{maar}.
#'
#' @param mod_fit An lm (OLS) object
#'
#' @return (tibble) White sandwich estimator of variance for OLS regression
#'
#' @export
#'
#' @importFrom Rdpack reprompt
#'
#' @references \insertAllCited{}
#'
#' @examples
#' \dontrun{
#' n <- 1e5
#' X <- stats::rnorm(n, 0, 1)
#' y <- 2 + X * 1 + stats::rnorm(n, 0, 10)
#' lm_fit <- stats::lm(y ~ X)
#' sandwich_qr_std_err <- comp_sandwich_qr_var(lm_fit)
#' }
comp_sandwich_qr_var <- function(mod_fit) {
  assertthat::assert_that(all("lm" == class(mod_fit)),
                          msg = glue::glue("lm_object must only be of class lm")
  )
  J_inv <- stats::summary.lm(mod_fit)$cov.unscaled
  X <- qr.X(mod_fit$qr)
  V <- t(X) %*% Matrix::Diagonal(x = stats::residuals(mod_fit)^2) %*% X

  out <- tibble::tibble(
    term = names(mod_fit$coefficients),
    estimate = mod_fit$coefficients,
    std.error = summary(mod_fit)[["coefficients"]][, "Std. Error"],
    t.stat = summary(mod_fit)[["coefficients"]][, "t value"],
    p.val = summary(mod_fit)[["coefficients"]][, "Pr(>|t|)"],
    std.error.sand = sqrt(diag(as.matrix(J_inv %*% V %*% J_inv)))
  ) %>%
    dplyr::mutate(
      t.stat.sand = estimate/std.error.sand,
      p.value.sand = 2*(1-sapply(abs(t.stat.sand), pnorm))
    )
  return(out)
}
