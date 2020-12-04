#' Generate Different Types of Multiplier Bootstrap Weights
#'
#' A Helper function for \code{\link[maar]{comp_multiplier_bootstrap_var}}
#' to generate different types of Multiplier Bootstrap
#' Weights. This section is inspired by the \code{weighttype} option in the
#' \code{Stata boottest} package.
#'
#' @param n The number of random Multiplier bootstrap weights to generate
#' @param weights_type The type of Multiplier bootstrap weights to generate.
#' Based on the \code{weighttype} option in the \code{Stata boottest} package,
#' this can only take the following five prespecified values
#' \code{"rademacher", "mammen", "webb", "std_gaussian", "gamma"}. A brief
#' description of each type is noted as below. The \code{"rademacher"} weights
#' are sampled from the two point Rademacher distribution which has takes values
#' \eqn{\{1, 1\}}, with equal probability. The \code{"mammen"} weights
#' are sampled from the two point Mammen distribution which has takes values
#' \eqn{\{\phi, 1 - \phi\}}, with probabilities
#' \eqn{\{\frac{\phi}{\sqrt{5}}, 1 - \frac{\phi}{\sqrt{5}}\}}, respectively. The
#' \code{"webb"} weights are sampled from the six point Webb distribution
#' which has takes values \eqn{\{\pm \sqrt{\frac{3}{2}}, \pm \sqrt{\frac{1}{2}}, \pm 1 \}},
#' with equal probability. The \code{"std_gaussian"} weights are sampled from
#' the standard Gaussian (normal) distribution with mean = \eqn{0},
#' and variance = \eqn{1}. Finally, the \code{"gamma"} weights are sampled from
#' the Gamma distribution with shape parameter = \eqn{4}, and scale parameter = \eqn{1}.
#' For more details on these weight types see \insertCite{@see @roodman2019fastwildinferencestataboottest;textual}{maar} for more details.
#'
#' @return A numeric vector of n (sampled with replacement) random Multiplier
#' bootstrap weights based on the specified Multiplier weights type
#'
#' @export
#'
#' @importFrom Rdpack reprompt
#'
#' @references \insertAllCited
#'
#' @examples
#' \dontrun{
#' # Number of Multiplier weights to generate
#' n <- 1000
#'
#' # Generate the different type of Multiplier Weights
#' rademacher_w <- gen_multiplier_bootstrap_weights(n = n,
#'                                                  weights_type = "rademacher")
#' mammen_w <- gen_multiplier_bootstrap_weights(n = n, weights_type = "mammen")
#' webb_w <- gen_multiplier_bootstrap_weights(n = n, weights_type = "webb")
#' std_gaussian_w <- gen_multiplier_bootstrap_weights(n = n,
#'                                                    weights_type = "std_gaussian")
#' gamma_w <- gen_multiplier_bootstrap_weights(n = n, weights_type = "gamma")
#' }
gen_multiplier_bootstrap_weights <- function(n, weights_type) {
  assertthat::assert_that(n == as.integer(n),
                          msg = glue::glue("n must be an integer"))
  assertthat::assert_that(n > 0,
                          msg = glue::glue("n must be positive"))
  assertthat::assert_that(length(weights_type) == 1,
                          msg = glue::glue("weights_type must only be a single value"))
  assertthat::assert_that(weights_type %in% c("rademacher", "mammen",
                                              "webb", "std_gaussian", "gamma"),
                          msg = glue::glue("weights_type must only be one of the following types c('rademacher', 'mammen', 'webb', 'std_gaussian', 'gamma')"))

  if (weights_type == "rademacher") {
    out <- sample(
      x = c(-1, 1),
      size = n,
      replace = TRUE,
      prob = rep(x = 1 / 2, times = 2)
    )
  } else if (weights_type == "mammen") {
    phi <- (1 + sqrt(5)) / 2 # Golden ratio
    out <- sample(
      x = c(1 - phi, phi),
      size = n,
      replace = TRUE,
      prob = c(phi / sqrt(5), 1 - phi / sqrt(5))
    )
  } else if (weights_type == "webb") {
    webb_neg_supp <- c(-sqrt(3 / 2), -1, -sqrt(1 / 2)) # Negative Webb weights
    out <- sample(
      x = c(webb_neg_supp, -webb_neg_supp),
      size = n,
      replace = TRUE,
      prob = rep(x = 1 / 6, times = 6)
    )
  } else if (weights_type == "std_gaussian") {
    out <- stats::rnorm(n = n, mean = 0, sd = 1)
  } else if (weights_type == "gamma") {
    out <- stats::rgamma(n = n, shape = 4, scale = 1 / 2)
  }
  return(out)
}

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
#' @param e Multiplier bootstrap weights. This is an \eqn{n \times 1}
#' vector of mean zero, variance 1 random variables
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
#' comp_multiplier_single_bootstrap_var(n = n,
#'                                      J_inv_X_res = J_inv_X_res,
#'                                      e = e)
#' # Display the output
#' print(mult_boot)
#' }
comp_multiplier_single_bootstrap_var <- function(n, J_inv_X_res, e) {
  out <- t(J_inv_X_res) %*% e / n
  return(out)
}

#' A wrapper to replicate the multiplier bootstrap calculation for an OLS fitted dataset
#'
#' The multiplier bootstrap calculated for a single replication
#' for an OLS fitted dataset is given by the following expression:
#' \deqn{\frac{1}{n}\sum_{i=1}^{n} e_{i}\widehat{J}^{-1}X_{i}(Y_{i}-X_{i}^{T} \widehat{\beta})}
#' This wrapper function calls on the helper function \code{\link{comp_multiplier_single_bootstrap_var}}. See it's documentation
#' for how a single instance is run.
#'
#' @param mod_fit An lm (OLS) object
#' @param B The number of bootstrap replications
#' @param weights_type The type of Multiplier bootstrap weights to generate.
#' Based on the \code{weighttype} option in the \code{Stata boottest} package,
#' this can only take the following five prespecified values
#' \code{"rademacher", "mammen", "webb", "std_gaussian", "gamma"}. For more
#' details see the documentation for
#' \code{\link[maar]{gen_multiplier_bootstrap_weights}}.
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
#' comp_multiplier_bootstrap_var(lm_fit, B = 15)
#' }
comp_multiplier_bootstrap_var <- function(mod_fit, B, weights_type) {
  assertthat::assert_that(all("lm" == class(mod_fit)),
                          msg = glue::glue("mod_fit must only be of class lm"))
  assertthat::assert_that(B == as.integer(B),
                          msg = glue::glue("B must be an integer e.g. 100, it is currently {B}"))
  assertthat::assert_that(B > 0,
                          msg = glue::glue("B must be positive e.g. 100, it is currently {B}"))
  # Get OLS related output
  betas <- stats::coef(mod_fit)
  J_inv <- stats::summary.lm(mod_fit)$cov.unscaled
  X <- qr.X(mod_fit$qr)
  res <- stats::residuals(mod_fit)
  n <- length(res)
  J_inv_X_res <- 1:nrow(X) %>%
                  purrr::map(~ t(J_inv %*% X[.x, ] * res[.x])) %>%
                  do.call(rbind, .)

  # Multiplier weights (mean 0, variance = 1)
  e <- matrix(data =
                maar::gen_multiplier_bootstrap_weights(n = B * n,
                                                       weights_type = weights_type),
              nrow = B,
              ncol = n)

  # Multiplier Bootstrap replications, B times
  boot_out <- 1:B %>%
                purrr::map(~ betas +
                             comp_multiplier_single_bootstrap_var(n, J_inv_X_res,
                                                         e[.x, ])) %>%
                purrr::map(~ tibble::tibble(term = rownames(.x),
                                            estimate = .x[, 1]))

  # Consolidate output in a nested tibble
  out <- tibble::tibble("B" = 1:B) %>% dplyr::mutate("boot_out" = boot_out)

  return(out)
}
