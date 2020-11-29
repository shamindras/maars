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
    B = paste0(1:length(indices)),
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
#' mod <- comp_empirical_bootstrap_cond_model(lm_fit, boot)
#'
#' # Display the output
#' print(mod)
#' }
comp_empirical_bootstrap_cond_model <- function(mod_fit, data) {
  if (all("lm" == class(mod_fit))) {
    out <- stats::lm(formula = stats::formula(mod_fit), data = data)
  } else {
    out <- stats::glm(formula = stats::formula(mod_fit), data = data, family = stats::family(mod_fit))
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
    msg = glue::glue("lm_fit must only be of class lm"))
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
    dplyr::mutate(boot_out = purrr::map(data, ~ comp_empirical_bootstrap_cond_model(mod_fit = mod_fit, data = .))) %>%
    dplyr::select(B, boot_out)

  return(boot_out)
}










