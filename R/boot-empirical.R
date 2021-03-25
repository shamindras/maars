#' Draw bootstrap sample from the data
#'
#' \code{comp_boot_emp_sample} draws one bootstrap samples
#' (i.e. sampling observations with replacement) from the input
#' \code{data}. It returns a list containing the indices of the
#' \code{m} observations that have been sampled (\code{indices}) and
#' the bootstrapped dataset (\code{data}).
#'
#' @param data A tibble or data frame containing the dataset to be
#'   sampled from.
#' @param B Bootstrap repetitions or number of bootstrap samples to
#'   be drawn.
#' @param m Number of observations to be sampled with replacement from
#'   the original dataset for each bootstrap repetition.
#'
#' @return A tibble containing the number of bootstrap iteration, the
#'   bootstrap samples (\code{data}),
#'   and the indices of the observations (linked to the original sample) in the
#'   sample (\code{indices}).
#'
#' @keywords internal
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # Draw B=100 bootstrap samples of size m=5
#' set.seed(162632)
#' n <- 100
#' boot <- comp_boot_emp_samples(
#'   data.frame(
#'     y = stats::rnorm(n, 0, 1),
#'     x = stats::rnorm(n, 0, 1)
#'   ),
#'   B = 100,
#'   m = 5
#' )
#'
#' # Display the output
#' print(boot)
#' }
comp_boot_emp_samples <- function(data,
                                  B = 1,
                                  m = NULL) {
  n <- nrow(data)
  if (is.null(m)) {
    m <- n
  }
  # this is a simplified version of rsample because rsample currently
  # does not allow for sampling m!=n observations from the data
  indices <- purrr::map(rep(n, B), sample, replace = TRUE, size = m)

  out <- tibble::tibble(
    b = as.integer(paste0(1:length(indices))),
    data = purrr::map(indices, ~ dplyr::slice(data, .x)),
    indices = indices
  )

  return(out)
}


#' Fit OLS or GLM on the data
#'
#' \code{fit_reg} fits an OLS or GLM on the input dataset.
#'
#' @details The model specification obtained from \code{mod_fit}, of
#'   class \code{\link[stats]{lm}} or \code{\link[stats]{glm}}, is
#'   fitted on \code{data}. The user can choose to fit a weighted
#'   regression using the argument \code{weights}.
#'
#' @param mod_fit An object of class \code{\link[stats]{lm}} or
#'   \code{\link[stats]{glm}} to fit on the data. This object should contain
#'   the formula, the data, and, in case of \code{\link[stats]{glm}}, the
#'   glm family.
#' @param data A tibble or data frame containing the dataset on which the
#'   model will be trained.
#' @param weights A character corresponding to the name of the weights feature
#'   name in the data.
#'
#' @return A tibble containing the estimated coefficients (\code{term}) of
#'   the regressors (\code{term}).
#'
#' @keywords internal
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' set.seed(457468)
#' # Estimate ols from a bootstraped dataset
#' n <- 1e3
#' X <- stats::rnorm(n, 0, 1)
#' y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
#' lm_fit <- stats::lm(y ~ X)
#' boot <- comp_boot_emp_samples(model.frame(lm_fit))$data[[1]]
#' mod <- fit_reg(lm_fit, boot)
#'
#' # Display the output
#' print(mod)
#'
#' #' # Estimate OLS from a dataset with weights
#' n <- 1e3
#' X <- stats::rnorm(n, 0, 1)
#' y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
#' mod_fit <- lm(y ~ X)
#'
#' # fit weighted regression
#' reg_df <- tibble::tibble(y = y, X = X, weights = 1:length(X))
#' mod <- fit_reg(mod_fit, reg_df, "weights")
#' print(mod)
#'
#' # fit unweighted regression
#' mod <- fit_reg(mod_fit, reg_df)
#' print(mod)
#' # compare this output with the output from lm
#' coef(lm(y ~ X, reg_df))
#' }
fit_reg <- function(mod_fit, data, weights = NULL) {
  if (all("lm" == class(mod_fit))) {
    if (is.null(weights)) {
      out <- stats::lm(
        formula = stats::formula(mod_fit),
        data = data
      )
    } else {
      out <- stats::lm(
        formula = stats::formula(mod_fit),
        data = data,
        weights = weights
      )
    }
  } else {
    if (is.null(weights)) {
      out <- stats::glm(
        formula = stats::formula(mod_fit),
        data = data,
        family = stats::family(mod_fit)
      )
    } else {
      out <- stats::glm(
        formula = stats::formula(mod_fit),
        data = data,
        family = stats::family(mod_fit),
        weights = weights
      )
    }
  }

  out <- tibble::tibble(
    term = names(stats::coef(out)),
    estimate = stats::coef(out)
  )
  return(out)
}


#' A wrapper for the empirical bootstrap of a fitted OLS or GLM regression model
#'
#' \code{comp_boot_emp} is a wrapper for the empirical bootstrap of
#' a fitted \code{\link[stats]{lm}} or \code{\link[stats]{glm}} model.
#'
#' @details The empirical bootstrap consists of fitting the chosen statistical
#'   model (\code{mod_fit}) onto \code{B} bootstrap versions of size \code{m}
#'   of the dataset.
#'
#' @param mod_fit An object of class \code{\link[stats]{lm}} or
#'   \code{\link[stats]{glm}} to fit on the data. This object should contain
#'   the formula, the data, and, in case of \code{\link[stats]{glm}},
#'   the family.
#' @param B Bootstrap repetitions or number of bootstrap samples to be drawn.
#' @param m Number of observations to be sampled with replacement from the
#'   dataset for each bootstrap repetition.
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
#' # Obtain estimates of the coefficients on bootstrapped versions of the dataset
#' set.seed(35542)
#' n <- 1e3
#' X <- stats::rnorm(n, 0, 1)
#' y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
#' lm_fit <- stats::lm(y ~ X)
#' out <- comp_boot_emp(lm_fit, B = 100, m = 1000)
#'
#' print(out)
#' }
comp_boot_emp <- function(mod_fit, B = 100, m = NULL) {
  assertthat::assert_that(all("lm" == class(mod_fit)) | any("glm" == class(mod_fit)),
                          msg = glue::glue("mod_fit must only be of class lm or glm")
  )
  check_fn_args(B = B, m = m)

  data <- stats::model.frame(mod_fit)
  n <- nrow(data)
  if (is.null(m)) {
    m <- n
  }

  boot_out <- lapply(
    1:B,
    function(x) {
      fit_reg(
        mod_fit = mod_fit,
        data = comp_boot_emp_samples(data, B = 1, m)$data[[1]]
      )
    }
  )

  cov_mat <- boot_out %>%
    purrr::map(~ .x %>% dplyr::pull(estimate)) %>%
    dplyr::bind_rows(data = ., .id = NULL) %>%
    stats::cov(x = .) * m / n

  boot_out <- boot_out %>%
    dplyr::bind_rows(.id = "b") %>%
    tidyr::nest(.data = .,
                boot_out = c(.data$estimate, .data$term)) %>%
    tibble::add_column(m = m, n = n)

  summary_boot <- get_boot_summary(
    mod_fit = mod_fit,
    boot_out = boot_out,
    boot_type = "emp"
  )

  out <- list(
    var_type = "boot_emp",
    var_type_abb = "emp",
    var_summary = summary_boot,
    var_assumptions = c(
      glue::glue("Observations are assumed to be independent",
                 .sep = " "
      ),
      glue::glue("Parameters: B = {B}, m = {m}, n = {n}")
    ),
    cov_mat = cov_mat,
    boot_out = boot_out
  )

  return(out)
}

#' Confidence intervals for on bootstrapped datasets via percentile bootstrap
#'
#' \code{comp_ci_boot} produces confidence intervals for regression
#' models estimates on bootstrapped datasets, via percentile bootstrap.
#'
#' @details `comp_ci_boot` takes in a set of bootstrap estimates
#'   (\code{boot_out}), a series of probabilities for the quantiles
#'   (\code{probs}), and optionally some "grouping" terms (\code{group_vars}).
#'   It returns the corresponding quantiles.
#'
#' @param boot_out A tibble of the model's coefficients estimated (\code{term}
#'   and \code{estimate}) on the bootstrapped datasets, the size of each
#'   bootstrapped dataset (\code{m}), the size of the original dataset
#'   (\code{n}), and the number of the bootstrap repetition (\code{b}).
#' @param probs A numeric vector containing the probabilities of the quantiles
#'   to be computed.
#' @param group_vars A vector of characters including the variables used
#'   to form the groups.
#'
#' @return A tibble containing the quantiles (\code{x}) and the
#'   probabilities (\code{q}) for each group as specified by \code{group_vars}.
#'
#' @keywords internal
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # Get confidence interval for OLS estimates via bootstrap
#' n <- 1e3
#' X1 <- stats::rnorm(n, 0, 1)
#' X2 <- stats::rnorm(n, 0, 3)
#' y <- 2 + X1 + X2 * 0.3 + stats::rnorm(n, 0, 1)
#' reg_df <- tibble::tibble(y = y, X1 = X1, X2 = X2, n_obs = 1:length(X1))
#' mod_fit <- stats::lm(y ~ X1 + X2, reg_df)
#' boot_out <- comp_boot_emp(mod_fit)
#' conf_int <- comp_ci_boot(boot_out, c(0.05, 0.95)) %>%
#'   tidyr::pivot_wider(names_from = q, values_from = x)
#'
#' # Display the output
#' print(conf_int)
#' }
comp_ci_boot <- function(boot_out, probs = c(0.025, 0.975),
                         group_vars = "term") {
  out <- boot_out %>%
    tidyr::unnest(boot_out) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(.data = .,
                     x = stats::quantile(sqrt(.data$m / .data$n) * (.data$estimate - mean(.data$estimate)) + mean(.data$estimate),
                                         probs = probs
                     ),
                     q = probs,
                     .groups = "keep"
    )
  return(out)
}
