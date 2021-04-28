# Test Subsampling Bootstrap i.e. sampling WITHOUT replacement for each of the B
# replications
set.seed(1234)
n <- 1e3
X <- stats::rnorm(n, 0, 1)
y <- 2 + X * 1 + stats::rnorm(n, 0, 10)
lm_fit <- stats::lm(y ~ X)
# MAX_DIFF_low_precision <- 1e-4
# MAX_DIFF_high_precision <- 1e-7

lm_fit_summ <- broom::tidy(lm_fit)
mod_fit0 <- comp_var(mod_fit = lm_fit,
                     boot_emp = NULL,
                     boot_sub = list(B = 1e3, m = n),
                     boot_res = NULL,
                     boot_mul = NULL)
mod_fit1 <- comp_var(mod_fit = lm_fit,
                     boot_emp = NULL,
                     boot_sub = list(B = 1e3, m = n/2),
                     boot_res = NULL,
                     boot_mul = NULL)
mod_fit2 <- comp_var(mod_fit = lm_fit,
                     boot_emp = NULL,
                     boot_sub = list(B = 1e3, m = floor(sqrt(n))),
                     boot_res = NULL,
                     boot_mul = NULL)
print("m = n")
mod_fit0 %>%
    get_summary(mod_fit = ., boot_sub = TRUE, sand = TRUE) %>%
    tidyr::pivot_wider(names_from = stat_type, values_from = stat_val)
print("half sampling")
mod_fit1 %>%
    get_summary(mod_fit = ., boot_sub = TRUE, sand = TRUE) %>%
    tidyr::pivot_wider(names_from = stat_type, values_from = stat_val)
print("m = sqrt(n)")
mod_fit2 %>%
    get_summary(mod_fit = ., boot_sub = TRUE) %>%
    tidyr::pivot_wider(names_from = stat_type, values_from = stat_val)

lm_fit_summ

test_that("Test case where m > n for subsampling on OLS", {
    # m = n + 1 i.e. m > n
    expect_error(comp_var(mod_fit = lm_fit,
                          boot_emp = NULL,
                          boot_sub = list(B = 1e3, m = n + 1),
                          boot_res = NULL,
                          boot_mul = NULL))

    # Add valid empirical bootstrap, but failing subsampling with
    # m = n + 1 i.e. m > n
    expect_error(comp_var(mod_fit = lm_fit,
                          boot_emp = list(B = 1e3, m = n + 1),
                          boot_sub = list(B = 1e3, m = n + 1),
                          boot_res = NULL,
                          boot_mul = NULL))
})

