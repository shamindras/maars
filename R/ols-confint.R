#' Get the tidy variance summary and confidence intervals for the coefficients of
#' an object of class "maars_lm"
#'
#' Get the tidy variance summary and confidence intervals from a fitted OLS \code{maars_lm, lm}
#' class object
#'
#' @param mod_fit (maars_lm, lm) A fitted OLS \code{maars_lm, lm} class object
#' @param level (double) : numeric value between 0 and 1 indicating the
#'   confidence level (e.g., 0.95)
#' @param sand (logical) : \code{TRUE} if sandwich estimator output is required,
#'   \code{FALSE} to exclude this output from the request
#' @param boot_emp (logical) : \code{TRUE} if empirical bootstrap standard error
#'   output is required, \code{FALSE} to exclude this output from the request
#' @param boot_res (logical) : \code{TRUE} if residual bootstrap standard error
#'   output is required, \code{FALSE} to exclude this output from the request
#' @param boot_mul (logical) : \code{TRUE} if multiplier bootstrap standard error
#'   output is required, \code{FALSE} to exclude this output from the request
#' @param well_specified (logical) : \code{TRUE} if lm standard errors
#'   (well specified) output is required, \code{FALSE} to exclude this output
#'   from the request
#'
#' @return (tibble) : Combined standard error and confidence intervals
#'   summary from a fitted OLS \code{maars_lm, lm} class object
#'
#' @details The function generates the same output as \code{\link{get_boot_summary}},
#'   but also includes lower and upper confidence intervals for the regression
#'   coefficients according to the significance level specified in the "level"
#'   argument.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(1243434)
#'
#' # generate data
#' n <- 1e3
#' X_1 <- stats::rnorm(n, 0, 1)
#' X_2 <- stats::rnorm(n, 10, 20)
#' eps <- stats::rnorm(n, 0, 1)
#'
#' # OLS data and model
#' y <- 2 + X_1 * 1 + X_2 * 5 + eps
#' lm_fit <- stats::lm(y ~ X_1 + X_2)
#'
#' # Empirical Bootstrap check
#' set.seed(454354534)
#' comp_var1 <- comp_var(mod_fit = lm_fit, boot_emp = list(B = 20, m = 200),
#'                     boot_res = list(B = 30),
#'                     boot_mul = NULL)
#'
#' # Return the summary with the confidence intervals
#' get_confint(mod_fit = comp_var1, sand = TRUE,
#'                      boot_emp = TRUE, boot_res = TRUE, boot_mul = FALSE,
#'                      well_specified = TRUE)
#' }
get_confint <- function(mod_fit,
                        level = 0.95,
                        sand = TRUE,
                        boot_emp = FALSE,
                        boot_mul = FALSE,
                        boot_res = FALSE,
                        well_specified = FALSE) {
  req_var_nms <- check_fn_args_summary(
    mod_fit = mod_fit,
    sand = sand,
    boot_emp = boot_emp,
    boot_res = boot_res,
    boot_mul = boot_mul,
    well_specified = well_specified
  )

  assertthat::assert_that(level > 0 & level < 1,
    msg = glue::glue("level must be between 0 and 1")
  )

  out_comp_mms_var_filt <- req_var_nms %>%
      purrr::map(.x = ., ~purrr::pluck(mod_fit$var, .x))

  out_comp_mms_var_filt <- out_comp_mms_var_filt %>%
      purrr::modify_in(.x = ., list(1,'var_summary'),
                       .f = ~ .x %>%
                        dplyr::mutate(
          conf.low = estimate + qt((1-level)/2, mod_fit$df.residual) * std.error,
          conf.high = estimate + qt(1 - (1-level)/2, mod_fit$df.residual) * std.error
      ))

  out_comp_mms_var_filt_mod <- out_comp_mms_var_filt %>%
      purrr::map(.x = ., .f = ~get_var_tidy_summary_ind(comp_mms_var_dat = .x))

  out <- out_comp_mms_var_filt_mod %>%
      purrr::reduce(.x = ., dplyr::left_join, by = c("term", "estimate"))

  return(out)
}
