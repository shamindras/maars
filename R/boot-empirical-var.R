#' Draw bootstrap samples from the data
#'
#' Draw bootstrap samples from the data.
#'
#' @param data Tibble or data frame containing the dataset to be sampled from
#' @param B Bootstrap repetitions or number of bootstrap samples to be drawn
#' @param m Number of observations to be sampled with replacement from the dataset
#' for each bootstrap repetition
#'
#' @return A tibble of the bootstrap samples
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Draw B=100 bootstrap samples of size m=5
#' set.seed(162632)
#' n <- 100
#' boot <- bootstrap_samples(data.frame(y = rnorm(n,0,1), x = rnorm(n,0,1)), B = 100, m = 5)
#'
#' # Display the output
#' print(boot)
#' }
comp_empirical_bootstrap_samples <- function(data,
                              B = 100,
                              m = NULL) {
  n <- nrow(data)
  if (missing(m)) {
    m <- n
  }

  indices <- purrr::map(rep(n, B), sample, replace = TRUE, size = m)

  out <- tibble::tibble(
    b = as.integer(paste0(1:length(indices))),
    data = purrr::map(indices, ~ data[.x, ])
  )
  return(out)
}



#' Fit OLS or GLM on the data
#'
#' Fit OLS or GLM on the data
#'
#' @param mod_fit An object of class "lm" or "glm" to fit on the data. This object
#' should contain the formula, the data, and, in case of "glm", the family
#' @param data A tibble or data frame containing the data set on which the model
#' will be trained
#' @param weights A character corresponding to the name of the weights
#' feature name in the data
#'
#' @return A tibble containing the estimated coefficients of the model
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Estimate OLS from a bootstraped data set
#' n <- 1e3
#' X <- stats::rnorm(n, 0, 1)
#' y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
#' lm_fit <- stats::lm(y ~ X)
#' boot <- comp_empirical_bootstrap_samples(model.frame(lm_fit))$data[[1]]
#' mod <- comp_cond_model(lm_fit, boot)
#'
#' # Display the output
#' print(mod)
#'
#' #' # Estimate OLS from a data set with weights
#' n <- 1e3
#' X <- stats::rnorm(n, 0, 1)
#' y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
#' mod_fit <- stats::lm(y ~ X)
#' df <- tibble::tibble(y = y, X = X, weights = 1:length(X))
#' mod <- comp_cond_model(mod_fit, df, 'weights')
#'
#' # Display the output
#' print(mod)
#' }
comp_cond_model <- function(mod_fit, data, weights=NULL){
  if (all("lm" == class(mod_fit))) {
    if(is.null(weights)){
      out <- stats::lm(formula = stats::formula(mod_fit), data = data)
    } else{
      out <- stats::lm(formula = stats::formula(mod_fit), data = data, weights = weights)
    }
  } else {
    if(is.null(weights)){
      out <- stats::glm(formula = stats::formula(mod_fit), data = data, family = stats::family(mod_fit))
    } else{
      out <- stats::glm(formula = stats::formula(mod_fit), data = data, family = stats::family(mod_fit), weights = weights)
    }
  }
  out <- tibble::tibble(term = names(stats::coef(out)), estimate = stats::coef(out))
  return(out)
}


#' A wrapper for the empirical bootstrap of a fitted OLS or GLM models
#'
#' The empirical bootstrap consists of fitting the chosen statistical model
#' onto B bootstrap versions of size m of the data set
#'
#' @param mod_fit An object of class "lm" or "glm" to fit on the data. This object
#' should contain the formula, the data, and, in case of "glm", the family
#' @param B Bootstrap repetitions or number of bootstrap samples to be drawn
#' @param m Number of observations to be sampled with replacement from the dataset
#' for each bootstrap repetition
#'
#' @return A tibble of the model's coefficients estimated on the bootstrapped data sets
#' @export
#'
#' @examples
#' \dontrun{
#' # Obtain estimates of the coefficients on bootstrapped versions of the data set
#' set.seed(35542)
#' n <- 1e3
#' X <- stats::rnorm(n, 0, 1)
#' y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
#' lm_fit <- stats::lm(y ~ X)
#' out <- comp_empirical_bootstrap(lm_fit, B = 100, m = 300)
#'
#' print(out)
#' }
comp_empirical_bootstrap <- function(mod_fit, B = 100, m = NULL) {
  assertthat::assert_that(all("lm" == class(mod_fit)) | any("glm" == class(mod_fit)),
    msg = glue::glue("mod_fit must only be of class lm or glm"))
  assertthat::assert_that(B == as.integer(B),
    msg = glue::glue("B must be an integer e.g. 100, it is currently {B}"))
  assertthat::assert_that(B > 0,
    msg = glue::glue("B must be positive e.g. 100, it is currently {B}"))
  if(!is.null(m)){
    assertthat::assert_that(m == as.integer(m),
                            msg = glue::glue("m must be an integer e.g. 100, it is currently {m}"))
    assertthat::assert_that(m > 0,
                            msg = glue::glue("m must be positive e.g. 100, it is currently {m}"))

  }

  data <- stats::model.frame(mod_fit)
  if (is.null(m) | missing(m)) {
    m <- nrow(data)
  }


  boot_samples <- comp_empirical_bootstrap_samples(data, B, m)

  boot_out <- boot_samples %>%
    tibble::add_column(m = m) %>%
    tibble::add_column(n = nrow(data)) %>%
    dplyr::mutate(boot_out =
                    purrr::map(data,
                               ~ comp_cond_model(mod_fit = mod_fit, data = .))) %>%
    dplyr::select(b, m, n, boot_out)

  return(boot_out)
}

#' Confidence intervals for regression models estimates on bootstrapped data sets
#' via percentile bootstrap
#'
#' Confidence intervals for regression models estimates on bootstrapped data sets
#' via percentile bootstrap. The functions takes in set of bootstrap estimates,
#' a series of probabilities for the quantiles, and some "grouping" terms.
#' It returns the corresponding quantiles.
#'
#' @param boot_out A tibble of the model's coefficients estimated on the bootstrapped data sets
#' @param probs Numeric vector containing the probabilities of the corresponding quantiles
#' @param group_vars A vector of characters with the variables used to form the groups
#'
#' @return A tibble containing the quantiles (x) and the probabilities (q) for  group_vars
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get confidence interval for OLS estimates via bootstrap
#' n <- 1e3
#' X1 <- stats::rnorm(n, 0, 1)
#' X2 <- stats::rnorm(n, 0, 3)
#' y <- 2 + X1 + X2*0.3 + stats::rnorm(n, 0, 1)
#' df <- tibble::tibble(y = y, X1 = X1, X2 = X2, n_obs = 1:length(X1))
#' mod_fit <- stats::lm(y ~ X1+X2, df)
#' boot_out <- comp_empirical_bootstrap(mod_fit)
#' conf_int <- comp_conf_int_boot(boot_out, c(0.05,0.95)) %>%
#' tidyr::pivot_wider(names_from = q, values_from = x)
#'
#' # Display the output
#' print(conf_int)
#' }
comp_conf_int_bootstrap <- function(boot_out, probs = c(0.025, 0.975), group_vars = "term") {
  out <- boot_out %>%
    tidyr::unnest(boot_out) %>%
    dplyr::group_by_at(group_vars) %>%
    dplyr::summarise(x = quantile(sqrt(m/n)*estimate, probs = probs), q = probs, .groups = "keep")
  return(out)
}


#' Normal QQ plot of the terms in an output of the bootstrap function
#'
#' This function produces a normal QQPlot for each term
#' in the output of the bootstrap function.
#' This function is a wrapper for the ggplot2::stat_qq function.
#'
#' @param boot_out A tibble of the model's coefficients estimated on the bootstrapped data sets
#'
#' @return A ggplot2 object containing normal QQ plot for each term
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Obtain normal QQ plot of the
#' n <- 1e3
#' X1 <- stats::rnorm(n, 0, 1)
#' X2 <- stats::rnorm(n, 0, 3)
#' y <- 2 + X1 + X2*0.3 + stats::rnorm(n, 0, 1)
#' df <- tibble::tibble(y = y, X1 = X1, X2 = X2, n_obs = 1:length(X1))
#' mod_fit <- stats::lm(y ~ X1+X2, df)
#' boot_out <- comp_empirical_bootstrap(mod_fit)
#'
#' # Display the output
#' qqnorm_bootstrap(boot_out)
#' }
qqnorm_bootstrap <- function(boot_out) {
  out <- boot_out %>%
    tidyr::unnest(boot_out) %>%
    ggplot2::ggplot(ggplot2::aes(sample = estimate)) +
    ggplot2::stat_qq() + ggplot2::stat_qq_line() +
    ggplot2::facet_wrap(~ term, ncol = 3, scales = 'free_y') +
    ggplot2::labs(x = 'Theoretical quantiles', y = 'Sample quantiles') +
    ggplot2::theme_bw()
  return(out)
}










