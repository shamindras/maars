set.seed(1243434)

# generate data
n <- 1e3
X <- stats::rnorm(n, 0, 1)
# OLS data and model
y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
lm_fit <- stats::lm(y ~ X)

# Empirical Bootstrap check
set.seed(454354534)
boot_out_emp1 <- comp_boot_emp(lm_fit, B = 1e4, m = 600)
set.seed(454354534)
boot_out_emp_comp1 <- comp_var(mod_fit = lm_fit,
                               boot_emp = list(B = 1e4, m = 600),
                               boot_res = NULL,
                               boot_mul = NULL)

expect_equal(
    boot_out_emp_comp1[["var_boot_emp"]][["var_summary"]][, "std.error"],
    boot_out_emp1[["var_summary"]][, "std.error"], tol = 1e-7)

# Multiplier Bootstrap check
set.seed(454354534)
boot_out_mul1 <- comp_boot_mul(lm_fit, B = 1e4, weights_type = "rademacher")
set.seed(454354534)
boot_out_mul_comp1 <- comp_var(mod_fit = lm_fit,
                               boot_emp = NULL,
                               boot_res = NULL,
                               boot_mul = list(B = 1e4, weights_type = "rademacher"))

expect_equal(
    boot_out_mul_comp1[["var_boot_mul"]][["var_summary"]][, "std.error"],
    boot_out_mul1[["var_summary"]][, "std.error"], tol = 1e-7)
