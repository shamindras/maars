#' Compute the White sandwich estimator for ordinary least squares (OLS) regression.
#'
#' @param X (matrix) : independent variables used in OLS fit
#' @param residuals (numeric) : residuals from OLS fit
#'
#' @return (matrix) : White sandwich estimator of variance for OLS regression
#' @export
#'
#' @examples
comp_sandwich_var <- function(X, residuals) {
  # TODO: need to check if X has intercept
  X <- cbind(1, X)
  J_inv <- solve(t(X) %*% X)
  V <- t(X) %*% diag(residuals^2) %*% X
  base::return(unname(J_inv %*% V %*% J_inv))
}


