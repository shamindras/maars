#' Calculate a single replication of the multiplier bootstrap for an OLS fitted dataset
#'
#' Calculate a single replication of the multiplier bootstrap for an OLS fitted dataset based on an \code{lm} fitted object in \code{R}.
#' This is given by the following expression:
#' \deqn{\frac{1}{n}\sum_{i=1}^{n} e_{i}\widehat{J}^{-1}X_{i}(Y_{i}-X_{i}^{T} \widehat{\beta})}
#'
#' @param n Number of observations (rows) of the underlying dataset.
#' In the given notation we assume our dataset has \eqn{n}
#' observations and \eqn{d} features (including an intercept)
#' @param J_inv_X_res A \eqn{d \times \d} matrix given by the
#' expression \eqn{\sum_{i=1}^{n}\widehat{J}^{-1}X_{i}(Y_{i}-X_{i}^{T} \widehat{\beta})}
#' @param e An \eqn{n \times 1} vector of mean zero, variance 1
#' random variables
#'
#' @return A tibble of the bootstrap standard errors
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Run an linear model (OLS) fit
#' set.seed(162632)
#' n <- 1e2
#' X <- stats::rnorm(n, 0, 1)
#' y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
#' lm_fit <- stats::lm(y ~ X)
#'
#' # Calculate the necessary required inputs for the bootstrap
#' J_inv <- summary.lm(lm_fit)$cov.unscaled
#' X <- qr.X(lm_fit$qr)
#' res <- residuals(lm_fit)
#' n <- length(res)
#' J_inv_X_res <-
#'    1:nrow(X) %>%
#'      purrr::map(~ t(J_inv %*% X[.x, ] * res[.x])) %>%
#'      do.call(rbind, .)
#'
#' # Generate a single random vector of length n, containing
#' # mean 0, variance 1 random variables
#' e <- rnorm(n, 0, 1)
#'
#' # Run a single replication of the multiplier bootstrap
#' mult_boot_single_rep <-
#' multiplier_single_bootstrap(n = n,
#'                             J_inv_X_res = J_inv_X_res,
#'                             e = e)
#' # Display the output
#' print(mult_boot)
#' }
multiplier_single_bootstrap <- function(n, J_inv_X_res, e) {
  out <- t(J_inv_X_res) %*% e / n
  return(out)
}

#' A wrapper to replicate the multiplier bootstrap calculation for an OLS fitted dataset
#'
#' The multiplier bootstrap calculated for a single replication
#' for an OLS fitted dataset is given by the following expression:
#' \deqn{\frac{1}{n}\sum_{i=1}^{n} e_{i}\widehat{J}^{-1}X_{i}(Y_{i}-X_{i}^{T} \widehat{\beta})}
#' This wrapper function calls on the helper function \code{\link{multiplier_single_bootstrap}}. See it's documentation
#' for how a single instance is run.
#'
#' @param lm_fit An lm (OLS) object
#' @param B The number of bootstrap replications
#'
#' @return A tibble of the bootstrap calculations
#' @export
#'
#' @examples
#' \dontrun{
#' # Simulate data from a linear model
#' set.seed(35542)
#' n <- 1e2
#' X <- stats::rnorm(n, 0, 1)
#' y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
#'
#' # Fit the linear model using OLS (ordinary least squares)
#' lm_fit <- stats::lm(y ~ X)
#'
#' # Run the multiplier bootstrap on the fitted (OLS) linear model
#' set.seed(162632)
#' multiplier_bootstrap(lm_fit, B = 15)
#' }
multiplier_bootstrap <- function(lm_fit, B = 100) {
  J_inv <- summary.lm(lm_fit)$cov.unscaled
  X <- qr.X(lm_fit$qr)
  res <- residuals(lm_fit)
  n <- length(res)
  J_inv_X_res <- 1:nrow(X) %>%
    purrr::map(~ t(J_inv %*% X[.x, ] * res[.x])) %>%
    do.call(rbind, .)

  # TODO: Check this change from list to matrix
  # e <- 1:B %>% purrr::map(~ rnorm(n, 0, 1))
  e <- matrix( rnorm(B*n,mean=0,sd=1), B, n)

  dist <- 1:B %>%
    purrr::map(~ multiplier_single_bootstrap(n, J_inv_X_res, e[.x, ])) %>%
    do.call(rbind, .)

  out <- tibble::tibble(
    iteration = rep(1:B, each = ncol(X)),
    term = rownames(dist),
    estimate = as.numeric(dist)
  )
  return(out)
}

multiplier_single_bootstrap2 <- function(n, J_inv_X_res, e) {
  out <- purrr::map2(
    .x = e,
    .y = J_inv_X_res,
    ~ .x * .y
  ) %>%
    do.call(rbind, .) %>%
    apply(., 2, mean)
  return(out)
}

multiplier_bootstrap2 <- function(lm_fit, B = 100) {
  J_inv <- summary.lm(lm_fit)$cov.unscaled
  X <- qr.X(lm_fit$qr)
  res <- residuals(lm_fit)
  n <- length(res)
  J_inv_X_res <- 1:nrow(X) %>% purrr::map(~ t(J_inv %*% X[.x, ] * res[.x]))

  # TODO: Check this change from list to matrix
  # e <- 1:B %>% purrr::map(~ rnorm(n, 0, 1))
  e <- matrix( rnorm(B*n,mean=0,sd=1), B, n)

  dist <- 1:B %>%
    purrr::map(~ multiplier_single_bootstrap2(n, J_inv_X_res, e[.x, ])) %>%
    do.call(rbind, .)

  out <- tibble::tibble(
    iteration = rep(1:B, ncol(X)),
    term = rep(colnames(X), each = B),
    estimate = as.numeric(dist)
  ) %>%
    dplyr::arrange(iteration, term)

  return(out)
}

#### EXAMPLE
# set.seed(162632)
# n <- 1e2
# X <- stats::rnorm(n, 0, 1)
# y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
# lm_fit <- stats::lm(y ~ X)
# set.seed(162632)
# multiplier_bootstrap(lm_fit, B = 15)
# set.seed(162632)
# multiplier_bootstrap2(lm_fit, B = 15)
