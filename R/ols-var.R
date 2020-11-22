#' Compute the White sandwich estimator of standard errors for
#' ordinary least squares (OLS) regression. This is based on the series of
#' papers \insertCite{white1980usinglsapproxunknownregfuncs,white1980heteroskedasticconsistentcovest}{Rdpack}. For more details
#' \insertCite{@see also @buja2019modelsasapproximationspart1 and @buja2019modelsasapproximationspart2;textual}{Rdpack}
#'
#' @param lm_object (lm) : lm object
#'
#' @return (matrix) : White sandwich estimator of variance for OLS regression
#' @export
#'
#' @importFrom Rdpack reprompt
#' @references
#'     \insertAllCited{}
#' @examples
#' \dontrun{
#' n <- 1e5
#' X <- stats::rnorm(n, 0, 1)
#' y <- 2 + X * 1 + stats::rnorm(n, 0, 10)
#' lm_fit <- stats::lm(y ~ X)
#' sandwich_qr_var <- comp_sandwich_qr_var(lm_fit)
#' }
comp_sandwich_qr_var <- function(lm_object){
  J_inv <- summary.lm(lm_object)$cov.unscaled
  X <- qr.X(lm_object$qr)
  V <- t(X) %*% Matrix::Diagonal(x = residuals(lm_object)^2) %*% X
  return(unname(as.matrix(J_inv %*% V %*% J_inv)))
}
