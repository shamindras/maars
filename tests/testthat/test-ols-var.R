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

test_that("Empirical Bootstrap input list assertion checking explicitly", {
    # valid since the named arguments are B, m
    testthat::expect_true(check_fn_args_comp_var_boot(inp_list = list(B = 10, m = 100), boot_type = "boot_emp"))
    # valid since the named arguments are B, m with m = NULL
    testthat::expect_true(check_fn_args_comp_var_boot(inp_list = list(B = 10, m = NULL), boot_type = "boot_emp"))
    # valid since the named arguments are B only, so m = NULL here by assumption
    testthat::expect_true(check_fn_args_comp_var_boot(inp_list = list(B = 10), boot_type = "boot_emp"))
    # valid since the named arguments are B, m
    testthat::expect_true(check_fn_args_comp_var_boot(inp_list = list(B = 0, m = 100), boot_type = "boot_emp"))
    # valid since the named arguments are B, m
    testthat::expect_true(check_fn_args_comp_var_boot(inp_list = list(B = 10, m = 0), boot_type = "boot_emp"))
    # valid since the named arguments are B, m
    testthat::expect_true(check_fn_args_comp_var_boot(inp_list = list(B = 10, m = -1), boot_type = "boot_emp"))
    # valid since the named arguments are B, m
    testthat::expect_true(check_fn_args_comp_var_boot(inp_list = list(B = -10, m = -1), boot_type = "boot_emp"))
    # invalid due to extra z parameter
    testthat::expect_error(check_fn_args_comp_var_boot(inp_list = list(B = 200, m = 400, z = 2300), boot_type = "boot_emp"))
    # invalid since the named arguments are b, m i.e. not B, m
    testthat::expect_error(check_fn_args_comp_var_boot(inp_list = list(b = 200, m = 400), boot_type = "boot_emp"))
    # invalid since the named arguments are b, M i.e. not B, m
    testthat::expect_error(check_fn_args_comp_var_boot(inp_list = list(b = 200, M = 400), boot_type = "boot_emp"))
    # invalid since the named arguments are B, M i.e. not B, m
    testthat::expect_error(check_fn_args_comp_var_boot(inp_list = list(B = 200, M = 400), boot_type = "boot_emp"))
    testthat::expect_error(check_fn_args_comp_var_boot(inp_list = list(B = 200, m = 400, z = NULL), boot_type = "boot_emp"))
})

test_that("Residual Bootstrap input list assertion checking explicitly", {
    # invalid since the named arguments are B, m i.e. not just B
    testthat::expect_error(check_fn_args_comp_var_boot(inp_list = list(B = 10, m = 100), boot_type = "boot_res"))
    # invalid since the named arguments are B, m i.e. not just B
    testthat::expect_error(check_fn_args_comp_var_boot(inp_list = list(B = 10, m = NULL), boot_type = "boot_res"))
    # valid since the named arguments are B only
    testthat::expect_true(check_fn_args_comp_var_boot(inp_list = list(B = 10), boot_type = "boot_res"))
    # invalid since the named arguments are B, m i.e. not just B
    testthat::expect_error(check_fn_args_comp_var_boot(inp_list = list(B = 0, m = 100), boot_type = "boot_res"))
    # invalid since the named arguments are B, m i.e. not just B
    testthat::expect_error(check_fn_args_comp_var_boot(inp_list = list(B = 10, m = 0), boot_type = "boot_res"))
    # invalid since the named arguments are B, m i.e. not just B
    testthat::expect_error(check_fn_args_comp_var_boot(inp_list = list(B = 10, m = -1), boot_type = "boot_res"))
    # valid since the named arguments are B, m
    testthat::expect_error(check_fn_args_comp_var_boot(inp_list = list(B = -10, m = -1), boot_type = "boot_res"))
    # invalid since the named arguments are B, m, z i.e. not just B
    testthat::expect_error(check_fn_args_comp_var_boot(inp_list = list(B = 200, m = 400, z = 2300), boot_type = "boot_res"))
    # invalid since the named arguments are b, m i.e. not just B
    testthat::expect_error(check_fn_args_comp_var_boot(inp_list = list(b = 200, m = 400), boot_type = "boot_res"))
    # invalid since the named arguments are b, M i.e. not just B
    testthat::expect_error(check_fn_args_comp_var_boot(inp_list = list(b = 200, M = 400), boot_type = "boot_res"))
    # invalid since the named arguments are B, M i.e. not just B
    testthat::expect_error(check_fn_args_comp_var_boot(inp_list = list(B = 200, M = 400), boot_type = "boot_res"))
    # invalid since the named arguments are B, m, z i.e. not just B
    testthat::expect_error(check_fn_args_comp_var_boot(inp_list = list(B = 200, m = 400, z = NULL), boot_type = "boot_res"))
    # invalid since the named arguments are b i.e. not B
    testthat::expect_error(check_fn_args_comp_var_boot(inp_list = list(b = 200), boot_type = "boot_res"))
    # invalid since the named arguments are all NULL
    testthat::expect_error(check_fn_args_comp_var_boot(inp_list = list(B = NULL), boot_type = "boot_res"))
})

test_that("Multiplier Bootstrap input list assertion checking explicitly", {
    # valid since the named arguments are B, weights_type
    testthat::expect_true(check_fn_args_comp_var_boot(inp_list = list(B = 10, weights_type = "rademacher"), boot_type = "boot_mul"))
    # valid since the named arguments are B, weights_type i.e. weights_type is NULL
    testthat::expect_true(check_fn_args_comp_var_boot(inp_list = list(B = 10, weights_type = NULL), boot_type = "boot_mul"))
    # valid since the named arguments are B only, so weights_type = NULL here by assumption
    testthat::expect_true(check_fn_args_comp_var_boot(inp_list = list(B = 10), boot_type = "boot_mul"))
    # valid since the named arguments are B, m
    testthat::expect_true(check_fn_args_comp_var_boot(inp_list = list(B = 0, weights_type = 100), boot_type = "boot_mul"))
    # valid since the named arguments are B, weights_type
    testthat::expect_true(check_fn_args_comp_var_boot(inp_list = list(B = 10, weights_type = 0), boot_type = "boot_mul"))
    # valid since the named arguments are B, weights_type
    testthat::expect_true(check_fn_args_comp_var_boot(inp_list = list(B = 10, weights_type = -1), boot_type = "boot_mul"))
    # valid since the named arguments are B, weights_type
    testthat::expect_true(check_fn_args_comp_var_boot(inp_list = list(B = -10, weights_type = -1), boot_type = "boot_mul"))
    # invalid due to extra z parameter
    testthat::expect_error(check_fn_args_comp_var_boot(inp_list = list(B = 200, weights_type = 400, z = 2300), boot_type = "boot_mul"))
    # invalid since the named arguments are b, weights_type i.e. not B, weights_type
    testthat::expect_error(check_fn_args_comp_var_boot(inp_list = list(b = 200, weights_type = 400), boot_type = "boot_mul"))
    # invalid since the named arguments are b, WEIGHTS_TYPE i.e. not B, weights_type
    testthat::expect_error(check_fn_args_comp_var_boot(inp_list = list(b = 200, WEIGHTS_TYPE = 400), boot_type = "boot_mul"))
    # invalid since the named arguments are B, WEIGHTS_TYPE i.e. not B, weights_type
    testthat::expect_error(check_fn_args_comp_var_boot(inp_list = list(B = 200, WEIGHTS_TYPE = 400), boot_type = "boot_mul"))
    testthat::expect_error(check_fn_args_comp_var_boot(inp_list = list(B = 200, weights_type = 400, z = NULL), boot_type = "boot_mul"))
})

# test_that("test estimate variance from empirical bootstrap matches estimate of variance from stats::lm", {
#      boot_out <- comp_boot_emp(lm_fit, B = 1e3)
#      boot_summary <- comp_var(mod_fit = lm_fit, boot_out = boot_out, boot_type = 'emp')
#      expect_equal(boot_summary$std.error.boot.emp, boot_summary$std.error, tol = 1e-2)
#      expect_equal(boot_summary$statistic.boot.emp, boot_summary$statistic, tol = 1e-1)
#      expect_equal(boot_summary$p.value.boot.emp, boot_summary$p.value, tol = 1e-2)
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
