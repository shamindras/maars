#' Compute the White sandwich estimator for ordinary least squares (OLS) regression.
#'
#' @param lm_object (lm) : lm object
#'
#' @return (matrix) : White sandwich estimator of variance for OLS regression
#' @export
#'
#' @examples
comp_sandwich_qr_var <- function(lm_object){
  qR <- qr.R(lm_object$qr)
  J_inv <- chol2inv(qR)
  X <- qr.X(lm_object$qr)
  V <- t(X) %*% Matrix::Diagonal(x = lm_object$residuals^2) %*% X
  return(unname(as.matrix(J_inv %*% V %*% J_inv)))
}
