#' Generate Different Types of multiplier bootstrap weights
#'
#' \code{comp_boot_mul_wgt} is a Helper function for
#' \code{\link{comp_boot_mul}} to generate different types
#' of multiplier bootstrap weights. This section is inspired by the
#' \code{weighttype} option in the Stata \code{boottest} package.
#'
#' @param n The number of random multiplier bootstrap weights to generate.
#' @param weights_type The type of multiplier bootstrap weights to generate.
#'   Based on the \code{weighttype} option in the \code{Stata boottest} package,
#'   this can only take the following five pre-specified values
#'   \code{"rademacher", "mammen", "webb", "std_gaussian", "gamma"}.
#'   A brief description of each type is as follows. The \code{"rademacher"}
#'   weights are sampled from the two point Rademacher distribution which
#'   takes values \eqn{\{1, 1\}}, with equal probability. The \code{"mammen"}
#'   weights are sampled from the two point Mammen distribution which has takes
#'   values \eqn{\{\phi, 1 - \phi\}}, with probabilities
#'   \eqn{\{\frac{\phi}{\sqrt{5}}, 1 - \frac{\phi}{\sqrt{5}}\}}, respectively.
#'   The \code{"webb"} weights are sampled from the six point Webb distribution
#'   which has takes values
#'   \eqn{\{\pm \sqrt{\frac{3}{2}}, \pm \sqrt{\frac{1}{2}}, \pm 1 \}},
#'   with equal probability. The \code{"std_gaussian"} weights are sampled from
#'   the standard Gaussian (normal) distribution with mean = \eqn{0},
#'   and variance = \eqn{1}. Finally, the \code{"gamma"} weights are sampled
#'   from the Gamma distribution with shape parameter = \eqn{4}, and
#'   scale parameter = \eqn{1}. For more details on these weight types see
#'   \insertCite{@see @roodman2019fastwildinferencestataboottest;textual}{maars}.
#'
#' @return A numeric vector of n (sampled with replacement) random multiplier
#'   bootstrap weights based on the specified multiplier weights type.
#'
#' @keywords internal
#'
#' @importFrom Rdpack reprompt
#' @importFrom rlang .data
#'
#' @references \insertAllCited
#'
#' @examples
#' \dontrun{
#' set.seed(824908)
#' # Number of multiplier weights to generate
#' n <- 1000
#'
#' # Generate the different type of multiplier weights
#' rademacher_w <- comp_boot_mul_wgt(n = n,
#'                                                  weights_type = "rademacher")
#' mammen_w <- comp_boot_mul_wgt(n = n, weights_type = "mammen")
#' webb_w <- comp_boot_mul_wgt(n = n, weights_type = "webb")
#' std_gaussian_w <- comp_boot_mul_wgt(n = n,
#'                                                    weights_type = "std_gaussian")
#' gamma_w <- comp_boot_mul_wgt(n = n, weights_type = "gamma")
#' }
comp_boot_mul_wgt <- function(n, weights_type) {
  check_fn_args(n = n)

  assertthat::assert_that(
    is.character(weights_type)
    && weights_type %in% c(
        "rademacher", "mammen", "webb",
        "std_gaussian", "gamma"
      ),
    msg = glue::glue("weights_type must only be one of the following types ['rademacher', 'mammen', 'webb', 'std_gaussian', 'gamma']")
  )
  assertthat::assert_that(length(weights_type) == 1,
    msg = glue::glue("weights_type must only be a single value")
  )

  if (weights_type == "rademacher") {
    out <- sample(
      x = c(-1, 1),
      size = n,
      replace = TRUE#,
     # prob = rep(x = 1 / 2, times = 2)
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

#' Calculate a single replication of the multiplier bootstrap for an OLS
#' fitted dataset
#'
#' \code{comp_boot_mul_ind} calculates a single
#' replication of the multiplier bootstrap for an OLS fitted dataset
#' based on an \code{\link[stats]{lm}} fitted object in \code{R}.
#' This is given by the following expression:
#' \deqn{\frac{1}{n}\sum_{i=1}^{n} e_{i}\widehat{J}^{-1}X_{i}(Y_{i}-X_{i}^{T} \widehat{\beta})}.
#'
#' @param n Number of observations (rows) of the underlying dataset.
#'   In the given notation we assume our dataset has \eqn{n}
#'   observations and \eqn{d} features (including an intercept)
#' @param J_inv_X_res A \eqn{d \times \d} matrix given by the
#'   expression
#'   \eqn{\sum_{i=1}^{n}\widehat{J}^{-1}X_{i}(Y_{i}-X_{i}^{T} \widehat{\beta})}.
#' @param e multiplier bootstrap weights. This is an \eqn{n \times 1}
#'   vector of mean zero, variance 1 random variables.
#'
#' @return A tibble of the bootstrap standard errors.
#'
#' @keywords internal
#'
#' @importFrom rlang .data
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
#' comp_boot_mul_ind(n = n,
#'                                      J_inv_X_res = J_inv_X_res,
#'                                      e = e)
#' # Display the output
#' print(mult_boot)
#' }
comp_boot_mul_ind <- function(J_inv_X_res, e) {
  out <- t(J_inv_X_res) %*% e #/ n
  return(out)
}

#' A wrapper to replicate the multiplier bootstrap calculation for an OLS fitted dataset
#'
#' The multiplier bootstrap calculated for a single replication
#' for an OLS fitted dataset is given by the following expression:
#' \deqn{\frac{1}{n}\sum_{i=1}^{n} e_{i}\widehat{J}^{-1}X_{i}(Y_{i}-X_{i}^{T} \widehat{\beta})}
#' This wrapper function calls on the helper function \code{\link{comp_boot_mul_ind}}. See its documentation
#' for how a single instance is run.
#'
#' @param mod_fit An \code{\link[stats]{lm}} (OLS) object.
#' @param B The number of bootstrap replications.
#' @param weights_type The type of multiplier bootstrap weights to generate.
#'   Based on the \code{weighttype} option in the Stata \code{boottest} package,
#'   this can only take the following five prespecified values
#'   \code{"rademacher", "mammen", "webb", "std_gaussian", "gamma"}.
#'   For more details see the documentation for
#'   \code{\link{comp_boot_mul_wgt}}. The default value is "rademacher".
#'
#' @return A list containing the following elements.
#'   \code{var_type}: The type of estimator for the variance of the coefficients
#'   estimates. An abbreviated string representing the
#'   type of the estimator of the variance  (\code{var_type_abb}).
#'   \code{var_summary}: A tibble containing the summary statistics for the model:
#'   terms (\code{term}), standard errors (\code{std.error}),
#'   statistics (\code{statistic}), p-values (\code{p.values}). The format
#'   of the tibble is exactly identical to the one generated by
#'   \code{\link[broom]{tidy}}, but the standard errors and p-values are computed
#'   via the bootstrap.
#'   \code{var_assumptions}: The assumptions under which the estimator of the
#'   variance is consistent.
#'   \code{cov_mat}: The covariance matrix of the coefficients estimates.
#'   \code{boot_out}: A tibble of the model's coefficients estimated (\code{term} and
#'   \code{estimate}) on the bootstrapped datasets,
#'   the size of the original dataset (\code{n}), and the number of the
#'   bootstrap repetition (\code{b}). In case of empirical bootstrap, it will
#'   also contain the size of each bootstrapped dataset (\code{m}).
#'
#' @keywords internal
#'
#' @importFrom rlang .data
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
#' comp_boot_mul(lm_fit, B = 15, weights_type = 'std_gaussian')
#' }
comp_boot_mul <- function(mod_fit, B, weights_type = "rademacher") {
  assertthat::assert_that(all("lm" == class(mod_fit)),
                          msg = glue::glue("mod_fit must only be of class lm"))
  check_fn_args(B=B)
  # Get OLS related output
  betas <- stats::coef(mod_fit)
  J_inv <- stats::summary.lm(mod_fit)$cov.unscaled
  X <- qr.X(mod_fit$qr)
  res <- stats::residuals(mod_fit)
  n <- length(res)
  J_inv_X_res <- 1:nrow(X) %>%
                  purrr::map(~ t(J_inv %*% X[.x, ] * res[.x])) %>%
                  do.call(rbind, .)

  # # multiplier weights (mean 0, variance = 1)
  e <- comp_boot_mul_wgt(n = B * n, weights_type = weights_type)
  boot_out <- purrr::map(
    1:B,
    ~ betas + comp_boot_mul_ind(J_inv_X_res = J_inv_X_res,
                                e = e[seq(n * (.x - 1) + 1, .x * n)])
  ) %>%
    do.call(rbind, .)

  # convert to tibble
  boot_out <- tibble::tibble(
    b = rep(1:B, each = length(betas)),
    term = rownames(boot_out),
    estimate = boot_out[, 1]
  ) %>%
    # consolidate tibble
    tidyr::nest(boot_out = c(term, estimate))

  summary_boot <- get_summary(
    mod_fit = mod_fit,
    boot_out = boot_out,
    boot_type = "mul"
  )

  out <- list(
    var_type = "boot_mul",
    var_type_abb = "mul",
    var_summary = summary_boot,
    var_assumptions = "The observations need to be independent.",
    cov_mat = NULL,
    boot_out = boot_out
  )

  return(out)
}
