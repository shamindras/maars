# Comp var examples ----
devtools::load_all()
set.seed(1243434)

# generate modeling data ----
n <- 1e3
X_1 <- stats::rnorm(n, 0, 1)
X_2 <- stats::rnorm(n, 10, 20)
eps <- stats::rnorm(n, 0, 100)

# Let's generate data and fit a well-specified OLS data and model ----
y <- 2 + X_1 * 1 + X_2 * 5 + eps
lm_fit <- stats::lm(y ~ X_1 + X_2)
summary(lm_fit)

# mms_fit1: Fit our first maars_lm object ----
# Here are essentially retaining our OLS fitted estimates but adding on
# revised model-misspecified standard errors for: [emp, mul], we did not
# run [res] bootstrap errors. Sandwich and well specified OLS errors get
# run by default
set.seed(454354534)
mms_fit1 <- comp_var(
  mod_fit = lm_fit,
  boot_emp = list(B = 50, m = 200),
  boot_res = list(B = 70),
  boot_mul = list(B = 60)
)

# mms_fit2: Let's run an example where some variances are not run ----
set.seed(454354534)
mms_fit2 <- comp_var(
  mod_fit = lm_fit,
  boot_emp = list(B = 50, m = 200),
  boot_res = NULL,
  boot_mul = NULL
)

# print - let's test out the print method for both objects ----
print(mms_fit1)
# print(mms_fit2) # TODO: This is failing, need to check it out

# summary - let's test out the summary method for both objects ----
summary(mms_fit1, boot_emp = TRUE)

# Try some interesting cases
summary(object = mms_fit2) # Just sand = TRUE
summary(object = mms_fit2, boot_emp = TRUE) # sand and emp both TRUE
summary(object = mms_fit2, boot_emp = TRUE, boot_mul = TRUE) # Error, with helpful warning
summary(object = mms_fit2, boot_emp = TRUE, boot_mul = TRUE, boot_res = TRUE) # Error, with helpful warning

# assumptions - let's test out the assumptions method for both objects ----
get_assumptions2(mod_fit = mms_fit2, boot_emp = TRUE)

#' Adds a variance type abbreviation suffix to variance summary tibbles
#'
#' @param var_summary TODO: Add later
#' @param var_type_abb TODO: Add later
#'
#' @return (\code{tibble}) : TODO Add later
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # TODO: Add here
#' }
get_mms_rnm_cols_suff <- function(var_summary, var_type_abb) {

  # Append suffix to variable names
  cols_common <- c("term", "estimate")
  cols_with_suffix <- setdiff(colnames(var_summary), cols_common) %>%
    stringr::str_c(., var_type_abb, sep = ".")
  cols_renamed <- c(cols_common, cols_with_suffix)

  # Apply new names
  var_summary <- var_summary %>%
    dplyr::rename_with(~cols_renamed, dplyr::everything())

  return(var_summary)
}

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
    out <- all_summary_confint %>%
      purrr::map_df(.x = ., .f = ~ dplyr::relocate(
        .data = .x,
        .data$var_type_abb,
        .after = dplyr::last_col()
      ))
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
          var_summary = .x,
          var_type_abb = .y
        )
      ) %>%
      purrr::reduce(.x = ., dplyr::left_join, by = c("term", "estimate"))
  }

  return(out)
}

# TODO: This isn't tidy - not sure why pivot_longer is not working on this
confint_out <- get_confint2(mod_fit = mms_fit2,
                             level = 0.95,
                             sand = TRUE,
                             boot_emp = TRUE,
                             boot_mul = FALSE,
                             boot_res = FALSE,
                             well_specified = FALSE,
                             tidy = TRUE)
# TODO: Check the names
confint_tidy <- confint_out %>%
  tidyr::pivot_longer(data = .,
                      cols = -c("term", "estimate", "var_type_abb"),
                      names_to = "stat_type",
                      values_to = "stat_val")
confint_tidy

confint_tidy_wider <- confint_tidy %>%
  tidyr::pivot_wider(data = .,
                     names_from = c(stat_type, var_type_abb),
                     names_glue = "{stat_type}.{var_type_abb}",
                     values_from = stat_val
  )
confint_tidy_wider
