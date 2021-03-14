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
#' @param tidy (logical) : \code{TRUE} if user requires a
#'   \code{\link[broom]{tidy}} style variance summary, \code{FALSE} if the user
#'   requires the compressed variance summary joined by the common
#'   "term", "estimate" values for the OLS fit. In this output each variance
#'   type has the required standard error term as a separate column with the
#'   variance type abbreviation as the suffix. That is, for sandwich estimator
#'   the 'p.value' column is renamed to 'p.value.sand'.

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
#' # TODO: Add here
#' }
get_confint3 <- function(mod_fit,
                         level = 0.95,
                         sand = TRUE,
                         boot_emp = FALSE,
                         boot_mul = FALSE,
                         boot_res = FALSE,
                         well_specified = FALSE,
                         tidy = TRUE) {

  # Get the variance types the user has requested. This performs assertion
  # Checking, so if there is no error it will return the required names,
  # else it will throw an error
  req_var_nms <- check_fn_args_summary(
    mod_fit = mod_fit,
    sand = sand,
    boot_emp = boot_emp,
    boot_res = boot_res,
    boot_mul = boot_mul,
    well_specified = well_specified
  )

  # Check alpha level is valid
  assertthat::assert_that(level > 0 & level < 1,
                          msg = glue::glue("level must be between 0 and 1")
  )

  # Filter the comp_mms_var output from the fitted maars_lm object for the
  # requested variance types from the user
  comp_var_ind_filt <- req_var_nms %>%
    purrr::map(.x = ., ~ purrr::pluck(mod_fit$var, .x))

  # Modified summary table with the broom standard column names and
  # variance type column
  all_summary_mod <- comp_var_ind_filt %>%
    purrr::map(.x = ., .f = ~ fetch_mms_comp_var_attr(
      comp_var_ind = .x,
      req_type = "var_summary_mod"
    ))

  all_var_type_abb <- comp_var_ind_filt %>%
    purrr::map(.x = ., .f = ~ fetch_mms_comp_var_attr(
      comp_var_ind = .x,
      req_type = "var_type_abb"
    ))

  # Get fitted OLS residual degrees of freedom
  df_residual <- mod_fit[["df.residual"]]

  all_summary_confint <- all_summary_mod %>%
    purrr::map(
      .x = .,
      .f = ~ dplyr::mutate(
        .data = .,
        conf.low = .data$estimate +
          qt((1 - level) / 2, df_residual) * .data$std.error,
        conf.high = .data$estimate +
          qt(1 - (1 - level) / 2, df_residual) * .data$std.error
      )
    ) %>%
    # TODO: Perhaps delete this line?
    purrr::set_names(x = ., nm = req_var_nms) %>%
    purrr::map_df(.x = ., .f = ~ dplyr::relocate(
      .data = .x,
      .data$var_type_abb,
      .after = dplyr::last_col()
    ))

  # Produce the tidy summary by default
  all_summary_confint_tidy <- all_summary_confint %>%
    tidyr::pivot_longer(data = .,
                        cols = -c("term", "estimate", "var_type_abb"),
                        names_to = "stat_type",
                        values_to = "stat_val")

  # Obtain the required variance summary tibble
  if (tidy) {
    # Keep the tidy summary
    out <- all_summary_confint_tidy
  } else {
    # Overwrite tidy summary with wider (non-tidy) summary. This uses
    # the previously created tidy summary
    out <- all_summary_confint_tidy %>%
      tidyr::pivot_wider(data = .,
                         names_from = c(stat_type, var_type_abb),
                         names_glue = "{stat_type}.{var_type_abb}",
                         values_from = stat_val)
  }

  return(out)
}
