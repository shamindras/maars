set.seed(1243434)

# generate data
n <- 1e3
X <- stats::rnorm(n, 0, 1)
# OLS data and model
y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
lm_fit <- stats::lm(y ~ X)
# glm data and model
y <- dplyr::if_else(runif(n, 0, 1) < 1 / (1 + exp(-y)), 1, 0)
glm_fit <- stats::glm(y ~ X, family = binomial())



# test_that("test estimate variance from empirical bootstrap matches estimate of variance from stats::lm", {
#     boot_out <- comp_boot_emp(lm_fit, B = 1e3)
#     boot_summary <- comp_var(mod_fit = lm_fit, boot_out = boot_out, boot_type = 'emp')
#     expect_equal(boot_summary$std.error.boot.emp, boot_summary$std.error, tol = 1e-2)
#     expect_equal(boot_summary$statistic.boot.emp, boot_summary$statistic, tol = 1e-1)
#     expect_equal(boot_summary$p.value.boot.emp, boot_summary$p.value, tol = 1e-2)
# })
#
#
# test_that("test estimate variance from empirical bootstrap for different values of m match", {
#     boot_out <- comp_boot_emp(lm_fit, B = 1e4, m = 50)
#     boot_summary_50 <- comp_var(mod_fit = lm_fit, boot_out = boot_out, boot_type = 'emp')
#     boot_out <- comp_boot_emp(lm_fit, B = 1e4)
#     boot_summary <- comp_var(mod_fit = lm_fit, boot_out = boot_out, boot_type = 'emp')
#     expect_equal(boot_summary_50$std.error.boot.emp, boot_summary$std.error, tol = 1e-3)
#     expect_equal(boot_summary_50$std.error.boot.emp, boot_summary$std.error, tol = 1e-1)
#     expect_equal(boot_summary_50$std.error.boot.emp, boot_summary$std.error, tol = 1e-2)
# })
#
#
# test_that("test estimate of variance from empirical bootstrap matches estimate of variance from stats::glm", {
#     boot_out <- comp_boot_emp(glm_fit, B = 1e3)
#     boot_summary <- comp_var(mod_fit = glm_fit, boot_out = boot_out, boot_type = 'emp')
#     expect_equal(boot_summary$std.error.boot.emp, boot_summary$std.error, tol = 1e-2)
#     expect_equal(boot_summary$statistic.boot.emp, boot_summary$statistic, tol = 1e-1)
#     expect_equal(boot_summary$p.value.boot.emp, boot_summary$p.value, tol = 1e-2)
# })
#
#
#
# test_that("test estimate of variance from multiplier bootstrap matches estimate of variance from stats::lm", {
#     boot_out <- comp_boot_mul(lm_fit, B = 1e4, weights_type = 'std_gaussian')
#     boot_summary <- comp_var(mod_fit = lm_fit, boot_out = boot_out, boot_type = 'mult')
#     expect_equal(boot_summary$std.error.boot.mult, boot_summary$std.error, tol = 1e-2)
#     expect_equal(boot_summary$statistic.boot.mult, boot_summary$statistic, tol = 1e-1)
#     expect_equal(boot_summary$p.value.boot.mult, boot_summary$p.value, tol = 1e-2)
# })
#
#
# test_that("test estimate variance from residual bootstrap matches estimate of variance from stats::lm", {
#     boot_out <- comp_boot_res(mod_fit = lm_fit, B = 1e3)
#     boot_summary <- comp_var(mod_fit = lm_fit, boot_out = boot_out, boot_type = 'res')
#     expect_equal(boot_summary$std.error.boot.res, boot_summary$std.error, tol = 1e-2)
#     expect_equal(boot_summary$statistic.boot.res, boot_summary$statistic, tol = 1e-1)
#     expect_equal(boot_summary$p.value.boot.res, boot_summary$p.value, tol = 1e-2)
# })
#
#
#
#
#
#
#
#
#
#
#
#
