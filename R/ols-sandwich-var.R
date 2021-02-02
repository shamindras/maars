#' get_summary computes the statistics based on boot_out and boot_type
#'
#' @param mod_fit
#' @param boot_out
#' @param boot_type
#'
#' @keywords internal
#'
#' @return
get_summary <- function(mod_fit, boot_out, boot_type) {
  assertthat::assert_that(all("lm" == class(mod_fit)) | any("glm" == class(mod_fit)),
                          msg = glue::glue("mod_fit must only be of class lm or glm"))
  assertthat::assert_that("tbl_df" %in% class(boot_out),
                          msg = glue::glue("boot_out must only be of class tibble"))
  assertthat::assert_that(boot_type %in% c("emp", "mul", "res"),
                          msg = glue::glue("boot_out must only be a character taking values either 'emp' or 'mul'"))

  if(boot_type %in% c("mul", "res")){
    boot_out <- boot_out %>% dplyr::mutate(m = nrow(model.frame(mod_fit)), n = m)
  }

  # Get model's estimate
  mod_out <- broom::tidy(x = mod_fit) %>%
    dplyr::rename(.data = .,
                  statistic = .data$statistic,
                  p.value = .data$p.value)
  mod_out_sel <- mod_out %>%
    dplyr::select(.data = .,
                  .data$term,
                  .data$estimate)

  # Define colnames into separate variables, dplyr::rename does not like
  # this to be defined on the fly. TODO: Investigate a more elegant approach
  std.error.boot.colname <- stringr::str_c("std.error.boot", boot_type, sep = ".")
  p.value.boot.colname <- stringr::str_c("p.value.boot", boot_type, sep = ".")
  statistic.boot.colname <-  stringr::str_c("statistic.boot", boot_type, sep = ".")

  out <- boot_out %>%
    tidyr::unnest(data = .,
                  .data$boot_out) %>%
    dplyr::rename(.data = .,
                  estimate.boot = .data$estimate) %>%
    dplyr::left_join(x = .,
                     y = mod_out %>%
                       dplyr::select(.data = .,
                                     .data$term, .data$estimate),
                     by = "term") %>%
    dplyr::group_by(.data = .,
                    .data$term,
                    .data$estimate) %>%
    dplyr::summarise(
      .data = .,
      std.error.boot = stats::sd(sqrt(.data$m / .data$n) * (.data$estimate.boot - mean(.data$estimate.boot))),
      p.value.boot = mean(abs(.data$estimate - .data$estimate.boot) > abs(.data$estimate)),
      .groups = 'keep'
    ) %>%
    dplyr::mutate(.data = .,
                  statistic.boot = .data$estimate / .data$std.error.boot)  %>%
    dplyr::left_join(x = .,
                     y = mod_out %>% dplyr::select(.data = .,
                                                   -.data$estimate),
                     by = "term") %>%
    # dplyr::left_join(mod_out, by = "term") %>%
    dplyr::select(.data = .,
                  .data$term,
                  .data$estimate,
                  .data$std.error,
                  .data$statistic,
                  .data$p.value,
                  .data$std.error.boot,
                  .data$statistic.boot,
                  .data$p.value.boot) %>%
    purrr::set_names(x = .,
                     nm = c("term", "estimate",
                            "std.error", "statistic", "p.value",
                            std.error.boot.colname,
                            statistic.boot.colname,
                            p.value.boot.colname)) %>%
    dplyr::arrange(.data = ., .data$term)
  return(out)
}

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
#' of the variance  (\code{var_type}); the summary statistics of \code{mod_fit} based on
#' this estimator of the variance (e.g., standard errors and p-values)
#' (\code{var_summary}); the assumptions under which the estimator of the
#' variance is consistent (\code{var_assumptions}); the covariance matrix for
#' the coefficients estimates (\code{cov_mat}).
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
  X <- qr.X(mod_fit$qr)
  V <- t(X) %*% Matrix::Diagonal(x = stats::residuals(mod_fit)^2) %*% X

  std_error_sand <- sqrt(diag(as.matrix(J_inv %*% V %*% J_inv))) %>%
    tibble::enframe(
      x = .,
      name = "term",
      value = "std.error.sand"
    )

  summary_sand <- mod_fit %>%
    broom::tidy(x = .) %>%
    dplyr::select(term, estimate) %>%
    dplyr::left_join(
      x = .,
      y = std_error_sand,
      by = "term"
    ) %>%
    dplyr::mutate(
      .data = .,
      statistic = .data$estimate / .data$std.error.sand,
      p.value = 2 * (1 - sapply(
        abs(.data$statistic),
        stats::pnorm
      ))
    )

  out <- list(var_type = "var_sand",
                   var_summary =  summary_sand,
                   var_assumptions = "The observations need to be i.i.d.",
                   cov_mat = V)

  return(out)
}
