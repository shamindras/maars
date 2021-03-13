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
      glue::glue("The model must be well specified"),
      glue::glue("The observations are assumed to be independent",
        "and",
        "identically distributed (i.i.d)",
        .sep = " "
      ),
      glue::glue("Residual standard error:",
        "{formatC(signif(lmg_v[['sigma']], digits = FMT_NUM_DIGITS))}",
        "on",
        "{lmg_v[['df.residual']]}",
        "degrees of freedom",
        .sep = " "
      ),
      glue::glue("Multiple R-squared:",
        "{formatC(lmg_v[['r.squared']], digits = FMT_NUM_DIGITS)},",
        "Adjusted R-squared:",
        "{formatC(lmg_v[['adj.r.squared']], digits = FMT_NUM_DIGITS)}",
        .sep = " "
      ),
      glue::glue("F-statistic:",
        "{formatC(lmg_v[['statistic']], digits = FMT_NUM_DIGITS)}",
        "on",
        "{lmg_v[['df']]} and {lmg_v[['df.residual']]} DF,",
        "p-value:",
        "{format.pval(pf(lmg_v[['statistic']],
                       lmg_v[['df']],
                       lmg_v[['df.residual']],
                       lower.tail = FALSE))}",
        .sep = " "
      )
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
