set.seed(454354534)
n <- 1e3
X_1 <- stats::rnorm(n, 0, 1)
X_2 <- stats::rnorm(n, 10, 20)
eps <- stats::rnorm(n, 0, 1)

# Let's generate data and fit a well-specified OLS data and model ----
y <- 2 + X_1 * 1 + X_2 * 5 + eps
lm_fit <- stats::lm(y ~ X_1 + X_2)
summary(lm_fit)

# Fit our first maars_lm object i.e. comp_var1
comp_var1 <- comp_var(mod_fit = lm_fit,
                      boot_emp = list(B = 50, m = 200),
                      boot_res = NULL,
                      boot_mul = list(B = 60))


testthat::test_that("Check print method on maars object prints to the console", {
    # Check for plot that does not exist is not returned
    expect_output(print(comp_var1))
})


testthat::test_that("Check assertions in plot method are handled correctly", {
    # Check for plot that does not exist is not returned
    expect_error(plot(comp_var(mod_fit = lm_fit), which = 8))
    # Check which is outside of range
    expect_error(plot(comp_var(mod_fit = lm_fit), which = 9))
    expect_error(plot(comp_var(mod_fit = lm_fit), which = 0))
})


# TODO: Fix this by using https://github.com/r-lib/vdiffr
# testthat::test_that("Check plots are printed to the console", {
#     expect_plot(plot(comp_var1, which = 1))
#     expect_output(plot(comp_var1, which = c(1,8)))
#     expect_output(plot(comp_var1, which = c(8)))
#     expect_output(plot(comp_var1, which = 1:8))
#     expect_output(plot(comp_var1))
# })
