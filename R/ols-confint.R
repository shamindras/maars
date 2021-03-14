# FUNCTIONS: get_confint2 - delete after get_confint3 after review  ----

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
      purrr::map(.x = ., .f = ~get_summary_ind(comp_mms_var_dat = .x))

  out <- out_comp_mms_var_filt_mod %>%
      purrr::reduce(.x = ., dplyr::left_join, by = c("term", "estimate"))

  return(out)
}

# FUNCTIONS: get_confint2 - delete after get_confint3 after review  ----

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
get_confint2 <- function(mod_fit,
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
    purrr::set_names(x = ., nm = req_var_nms)

  # Obtain the required variance summary tibble
  if (tidy) {
    # Tidy summary
    all_summary_confint_bind <- all_summary_confint %>%
      purrr::map_df(.x = ., .f = ~ dplyr::relocate(
        .data = .x,
        .data$var_type_abb,
        .after = dplyr::last_col()
      ))

    # Produce the tidy summary by default
    out <- all_summary_confint_bind %>%
      tidyr::pivot_longer(data = .,
                          cols = -c("term", "estimate", "var_type_abb"),
                          names_to = "stat_type",
                          values_to = "stat_val")
  } else {
    # Modified summary table with the abbreviated variance type as a suffix
    # This is then left_joined by the common columns to produce a single
    # table
    # TODO: If we did the tidy summary above, we can just get this using
    #       a pivot wider in a clever way
    out <- all_summary_confint %>%
      purrr::map2(
        .x = .,
        .y = all_var_type_abb,
        .f = ~ get_mms_rnm_cols_suff(
          var_summary = dplyr::select(.x, -var_type_abb),
          var_type_abb = .y
        )
      ) %>%
      purrr::reduce(.x = ., dplyr::left_join, by = c("term", "estimate"))
  }

  return(out)
}

# FUNCTIONS: get_confint3 ----
# TODO: delete get_confint, get_confint2 after review

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
