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
comp_var1 <- comp_var(mod_fit = lm_fit, boot_emp = list(B = 20, m = 200),
                    boot_res = list(B = 30),
                    boot_mul = NULL)

# This returns everything but boot_mul, since we didn't run it in the original
# original maars_lm model
get_var_tidy_summary(mod_fit = comp_var1, sand = TRUE,
                     boot_emp = TRUE, boot_res = TRUE, boot_mul = FALSE,
                     well_specified = TRUE)

# This returns an error since we require the tidy summary with boot_mul, but
# we have not run it in the original maars_lm model
get_var_tidy_summary(mod_fit = comp_var1, sand = TRUE,
                     boot_emp = TRUE, boot_res = TRUE, boot_mul = TRUE,
                     well_specified = TRUE)

# We are passing in all FALSE values, should return the sandwich estimator
# with a warning message
get_var_tidy_summary(mod_fit = comp_var1, sand = FALSE,
                     boot_emp = FALSE, boot_res = FALSE, boot_mul = FALSE,
                     well_specified = FALSE)

# res_sand_warn <- check_fn_args_summary(mod_fit = comp_var1, sand = FALSE,
#                                        boot_emp = FALSE, boot_res = FALSE, boot_mul = FALSE,
#                                        well_specified = FALSE)
# res_sand_warn
#
# res_sand_err <- check_fn_args_summary(mod_fit = comp_var1, sand = FALSE,
#                                       boot_emp = FALSE, boot_res = TRUE, boot_mul = FALSE,
#                                       well_specified = FALSE)
# res_sand_err
#
# res_sand_val <- check_fn_args_summary(mod_fit = comp_var1, sand = TRUE,
#                                       boot_emp = TRUE, boot_res = FALSE, boot_mul = FALSE,
#                                       well_specified = TRUE)
# res_sand_val
#
# testthat::expect_error(
# res_sand_err <- check_fn_args_summary(mod_fit = comp_var1, sand = FALSE,
#                                       boot_emp = FALSE, boot_res = TRUE, boot_mul = FALSE,
#                                       well_specified = FALSE)
# )
