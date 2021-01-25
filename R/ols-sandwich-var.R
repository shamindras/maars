#' Compute the sandwich estimator of standard errors for OLS
#'
#' Compute the sandwich estimator of standard errors for
#' ordinary least squares (OLS) regression, \insertCite{@see @white1980usinglsapproxunknownregfuncs and @white1980heteroskedasticconsistentcovest;textual}{maars}. For more details
#' \insertCite{@see also @buja2019modelsasapproximationspart1 and @buja2019modelsasapproximationspart2;textual}{maars}.
#'
#' @details The function computes the sandwich estimator for the OLS regression
#'   passed in \code{mod_fit} and returns a tibble with summary information
#'   about the components of the model based on the sandwich standard errors.
#'
#' @param mod_fit A \code{\link[stats]{lm}} (OLS) object.
#'
#' @return A tibble containing the sandwich estimator of variance for OLS
#'   regression.
#'
#' @keywords internal
#'
#' @importFrom Rdpack reprompt
#' @importFrom rlang .data
#'
#' @references \insertAllCited{}
#'
#' @examples
#' \dontrun{
#' set.seed(674748)
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

std_error_sand <- sqrt(diag(as.matrix(J_inv %*% V %*% J_inv))) %>%
  tibble::enframe(
    x = .,
    name = "term",
    value = "std.error.sand"
  )

out <- mod_fit %>%
  broom::tidy(x = .) %>%
  dplyr::rename(
    .data = .,
    statistic = .data$statistic,
    p.value = .data$p.value
  ) %>%
  dplyr::left_join(
    x = .,
    y = std_error_sand,
    by = "term"
  ) %>%
  dplyr::mutate(
    .data = .,
    statistic.sand = .data$estimate / .data$std.error.sand,
    p.value.sand = 2 * (1 - sapply(
      abs(.data$statistic.sand),
      stats::pnorm
    ))
  )

return(out)
}
