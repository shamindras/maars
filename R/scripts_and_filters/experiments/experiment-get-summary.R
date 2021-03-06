# Internal function
fetch_tidy_summary_assumptions <- function(mod_fit, sand = TRUE,
                        boot_emp = FALSE, boot_res = FALSE, boot_mul = FALSE,
                        well_specified = FALSE) {

  # Assertion checking for mod_fit is of class "maars_lm", "lm"
  assertthat::assert_that(all(c("maars_lm", "lm") == class(mod_fit)),
                          msg = glue::glue("mod_fit must only be of class: ['maars_lm', 'lm']")
  )

  # Input variance parameter names
  var_param_nms <- c("sand", "boot_emp", "boot_res", "boot_mul", "well_specified")
  # Get the combined variance related parameter values
  var_param_vals <- purrr::map(.x = var_param_nms, ~get(x = .x))
  # Rename the list values to correspond to the input names
  names(var_param_vals) <- var_param_nms
  # var_param_vals <- c(sand, boot_emp, boot_res, boot_mul, well_specified)

    # Check all input parameters, other than mod_fit are of class logical
  assertthat::assert_that(
    all(purrr::map_lgl(.x = var_param_vals, ~is.logical(.x))),
    msg = glue::glue(
      "All input variance parameters: [sand, boot_emp, boot_res, boot_mul, well_specified]",
      "must be of class logical",
      .sep = " "
    ))

  # browser()

  # Now that we have assertion checked all variance input parameters are of
  # class logical, let's convert them into a logical vector (not list) to
  # make future assertion checking easier
  var_param_vals_lgl <- var_param_vals %>% purrr::map_lgl(.x = ., ~ .x)

  # Get the comp_var output from the fitted maars_lm object
  out_comp_var <- mod_fit$var
  out_comp_var_nms <- names(out_comp_var)
  # Get the abbreviated names i.e. remove the "var_" prefix to align with summary
  # inputs
  out_comp_var_nms_abb <- out_comp_var_nms %>%
    stringr::str_replace_all(string = ., pattern = "var_", "")

    # If all parameters are FALSE, return a warning, and the table containing
    # only the sandwich variance
    if(!any(var_param_vals_lgl)){
        warning(glue::glue("You have passed in FALSE for all input variance parameters:",
                "\n[sand, boot_emp, boot_res, boot_mul, well_specified].",
                "\n\nReturning the default sandwich variance estimator...\n",
                .sep = " "))
      # Process the comp_var list and get the non-NULL valued variance lists
      #
      summary_out <- out_comp_var %>% purrr::pluck("var_sand") %>% purrr::map(.x = ., ~magic_summary(.x))
      assumptions_out <- out_comp_var %>% purrr::pluck("var_sand") %>% purrr::map(.x = ., ~magic_assumptions(.x))
      confint_out <- out_comp_var %>% purrr::pluck("var_sand") %>% purrr::map(.x = ., ~magic_assumptions(.x))
      out <- list(summary_out = summary_out,
                  assumptions_out = assumptions_out,
                  confint_out = confint_out)
      # Assertion check that we are not missing

      # and get the column modified
    }

    # If all parameters are true, we should also return the standard lm
  if(all(var_param_vals_lgl)){
    # Assertion handling to be added, if they pass in all TRUE but maarslm doesn't have all the errors e.g. res = NULL
    # We exit gracefully here with an assertion error
    # lm %>% broom::tidy() %>% left_join()
    # out_comp_var contains all of the things we are asking for
    # Process the comp_var list and get the non-NULL valued variance lists
    summary_out <- out_comp_var %>% purrr::map(.x = ., ~magic_summary(.x))
    assumptions_out <- out_comp_var %>% purrr::map(.x = ., ~magic_assumptions(.x))
    confint_out <- out_comp_var %>% purrr::map(.x = ., ~magic_assumptions(.x))
    out <- list(summary_out = summary_out,
                assumptions_out = assumptions_out,
                confint_out = confint_out)
  }


  # Assertion check that the
  # out_comp_var contains all of the things we are asking for
  # Process the comp_var list and get the non-NULL valued variance lists
  summary_out <- out_comp_var %>% purrr::map(.x = ., ~magic_summary(.x))
  assumptions_out <- out_comp_var %>% purrr::map(.x = ., ~magic_assumptions(.x))
  confint_out <- out_comp_var %>% purrr::map(.x = ., ~magic_assumptions(.x))
  out <- list(summary_out = summary_out,
              assumptions_out = assumptions_out,
              confint_out = confint_out)

}

# Comp var examples ----
devtools::load_all()
set.seed(1243434)

# generate data
n <- 1e3
X_1 <- stats::rnorm(n, 0, 1)
X_2 <- stats::rnorm(n, 10, 20)
eps <- stats::rnorm(n, 0, 1)

# OLS data and model
y <- 2 + X_1 * 1 + X_2 * 5 + eps
lm_fit <- stats::lm(y ~ X_1 + X_2)

# DEFINE common column names - these stay the same across all
# reported error types
COMMON_ERR_STAT_COLNAMES <- c("term", "estimate")

# Empirical Bootstrap check
set.seed(454354534)
mss_var1 <- mss_var(mod_fit = lm_fit, boot_emp = list(B = 100, m = 200),
                    boot_res = list(B = 100),
                    boot_mul = list(B = 10, weights_type = 'rademacher'))
# names(mss_var1)
# class(mss_var1)

out_test_summ1 <- get_tidy_summary(mod_fit = mss_var1, sand = TRUE,
                                   boot_emp = FALSE, boot_res = FALSE,
                                   boot_mul = FALSE,
                                   well_specified = FALSE)


# Create maars_lm object
la_fit <- la_housing_data %>%
  lm(y ~ x + z) %>%
  maars::mss_var(mod_fit = .,
                 sand = TRUE,
                 boot_emp = list(B = 100, m = 10),
                 boot_mul = list(B = 10, weights_type = 'rademacher'),
                 boot_res = NULL)


# If they want the tidy summary
la_fit %>%
  maars::get_summary(maars_lm_fit, emp = ...) %>%
  dplyr::mutate(add ratio_columns) %>%
  dplyr::relocate(do operations) %>%
  knitr::kable(format the table here)

# If they want assumptions only
la_fit %>% maars::get_assumptions(maars_lm_fit, emp = ...)

# If they want assumptions only
la_fit %>% maars::get_confint(maars_lm_fit, emp = ..., quantile = c(0.05, 0.95))

# Behind the scenes we call fetch for BOTH...


get_summary # User facing, per our spec it outputs only the tibble of std errors - what maar_lm.summary
get_assumptions # User facing, per our spec it outputs only the tibble of std errors - what maar_lm.summary
get_confint # User facing

fetch_summary_assumptions # Internal, returns a list of 2 things. A tidy tibble of all left_join std. errors, and character vector of assumptions
