#' Get a list output summary for \code{\link[stats]{lm}}, under well specified
#' linear modeling assumptions
#'
#' Get a list output summary for \code{\link[stats]{lm}} that is consistent
#' for use in \code{\link{comp_var}}. This assumes that the linear model is
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

  out <- list(
    var_type = "well_specified",
    var_type_abb = "lm",
    var_summary = summary_lm,
    var_assumptions = "All OLS assumptions",
    cov_mat = NULL
  )

  return(out)
}
