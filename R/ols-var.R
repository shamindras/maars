#' Compute the White sandwich estimator for ordinary least squares (OLS) regression.
#'
#' @param lm_object (lm) : lm object
#'
#' @return (matrix) : White sandwich estimator of variance for OLS regression
#' @export
#'
#' @examples
comp_sandwich_qr_var <- function(lm_object){
  J_inv <- summary.lm(lm_object)$cov.unscaled
  X <- qr.X(lm_object$qr)
  V <- t(X) %*% Matrix::Diagonal(x = residuals(lm_object)^2) %*% X
  return(unname(as.matrix(J_inv %*% V %*% J_inv)))
}
