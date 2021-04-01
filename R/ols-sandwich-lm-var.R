#' Compute the sandwich estimator of standard errors for OLS
#'
#' Compute the sandwich estimator of standard errors for
#' ordinary least squares (OLS) regression, \insertCite{@see @white1980usinglsapproxunknownregfuncs and @white1980heteroskedasticconsistentcovest;textual}{maars}. For more details
#' \insertCite{@see also @buja2019modelsasapproximationspart1 and @buja2019modelsasapproximationspart2;textual}{maars}.
#'
#' @details The function computes the sandwich estimator for the OLS regression
#'   passed in \code{mod_fit} and returns a tibble with summary information
#'   about the components of the model based on the sandwich standard errors.
#'
#' @param mod_fit A \code{\link[stats]{lm}} (OLS) object.
#'
#' @return A list containing the following elements: the type of estimator of
#'   of the variance  (\code{var_type}); An abbreviated string representing the
#'   type of the estimator of the variance  (\code{var_type_abb}); the summary
#'   statistics of \code{mod_fit} based on this estimator of the variance
#'   (e.g., standard errors and p-values) (\code{var_summary}); the assumptions
#'   under which the estimator of the variance is consistent
#'   (\code{var_assumptions}); the covariance matrix for the coefficients
#'   estimates (\code{cov_mat}).
#'
#' @keywords internal
#'
#' @importFrom Rdpack reprompt
#' @importFrom rlang .data
#'
#' @references \insertAllCited{}
#'
#' @examples
#' \dontrun{
#' set.seed(674748)
#' n <- 1e5
#' X <- stats::rnorm(n, 0, 1)
#' y <- 2 + X * 1 + stats::rnorm(n, 0, 10)
#' lm_fit <- stats::lm(y ~ X)
#' sandwich_qr_std_err <- comp_sand_var(lm_fit)
#' }
comp_sand_var <- function(mod_fit) {
  assertthat::assert_that(all("lm" == class(mod_fit)),
                          msg = glue::glue("lm_object must only be of class lm")
  )
  J_inv <- stats::summary.lm(mod_fit)$cov.unscaled
  X <- stats::model.matrix(mod_fit)
  res <- stats::residuals(mod_fit)
  meat <- crossprod(x = res * X)
  cov_mat <- as.matrix(J_inv %*% meat %*% J_inv)

  std_error_sand <- sqrt(diag(cov_mat)) %>%
    tibble::enframe(
      x = .,
      name = "term",
      value = "std.error"
    )

  summary_sand <- mod_fit %>%
    broom::tidy(x = .) %>%
    dplyr::select(.data = .,
                  .data$term,
                  .data$estimate) %>%
    dplyr::left_join(
      x = .,
      y = std_error_sand,
      by = "term"
    ) %>%
    dplyr::mutate(
      .data = .,
      statistic = .data$estimate / .data$std.error,
      p.value = 2 * (1 - sapply(
        abs(.data$statistic),
        stats::pnorm
      ))
    )

  out <- list(var_type = "sand",
              var_type_abb = "sand",
              var_summary =  summary_sand,
              var_assumptions = "Observations are assumed to be independent",
              cov_mat = cov_mat)

  return(out)
}

#' Get a list output summary for \code{\link[stats]{lm}}, under well specified
#' linear modeling assumptions
#'
#' Get a list output summary for \code{\link[stats]{lm}} that is consistent
#' for use in \code{\link{comp_mms_var}}. This assumes that the linear model is
#' fitted under well specified assumptions. That is, the data is assumed to be
#' generated directly from the linear model under OLS assumptions.
#'
#' @param mod_fit A \code{\link[stats]{lm}} (OLS) object.
#'
#' @return A list containing the following elements: the type of estimator of
#'   of the variance  (\code{var_type}); the summary statistics of \code{mod_fit}
#'   based on this estimator of the variance (e.g., standard errors and p-values)
#'   (\code{var_summary}); the assumptions under which the estimator of the
#'   variance is consistent (\code{var_assumptions}); the covariance matrix for
#'   the coefficients estimates (\code{cov_mat}).
#'
#' @keywords internal
#'
#' @importFrom Rdpack reprompt
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' set.seed(674748)
#' n <- 1e5
#' X <- stats::rnorm(n, 0, 1)
#' y <- 2 + X * 1 + stats::rnorm(n, 0, 10)
#' lm_fit <- stats::lm(y ~ X)
#' sandwich_qr_std_err <- comp_sand_var(lm_fit)
#' }
comp_lm_var <- function(mod_fit) {
  assertthat::assert_that(all("lm" == class(mod_fit)),
                          msg = glue::glue("lm_object must only be of class lm")
  )

  summary_lm <- mod_fit %>%
    broom::tidy(x = .)

  # Add detailed assumptions for the well-specified linear model
  # This is to design the specific string at the bottom of the
  # standard out put of the summary.lm with F-statistics, p-value etc
  lmg <- mod_fit %>% broom::glance(x = .)
  lmg_v <- lmg %>%
    as.numeric(x = .) %>%
    purrr::set_names(x = ., nm = names(x = lmg))

  # Default output to be with 4 digits
  FMT_NUM_DIGITS <- 4
  assumptions_lm <-
    c(
      glue::glue("Observations are assumed to be independent"),
      glue::glue("Residuals are assumed to be homoscedastic"),
      glue::glue("Linearity of the conditional expectation is assumed")
    )

  out <- list(
    var_type = "well_specified",
    var_type_abb = "lm",
    var_summary = summary_lm,
    var_assumptions = assumptions_lm,
    cov_mat = NULL
  )

  return(out)
}
