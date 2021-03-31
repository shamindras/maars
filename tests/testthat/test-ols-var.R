set.seed(1243434)

# thanks to Arun K. for providing the following function
gen_reg_data <- function(n,
                         gamma){ # gamma ~ "degree of misspecification"
    beta0 <- 1
    beta1 <- 2
    X <- runif(n, 0, 10)
    Y <- beta0 + beta1*X + gamma*X^{1.7} + exp(gamma*X)*rnorm(n)
    data <- tibble::tibble(X = X, Y = Y)
    return(data)
}

# generate regression data on which we'll fit a correctly specified reg. model
n <- 1e3
df <- gen_reg_data(n = n, gamma = 0)
lm_fit <- stats::lm(Y ~ X, data = df)


# test error handling and assertion checking ----
test_that("Empirical Bootstrap input list assertion checking explicitly", {
    # valid since the named arguments are B, m
    testthat::expect_true(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 10, m = 100), boot_type = "boot_emp"))
    # valid since the named arguments are B, m with m = NULL
    testthat::expect_true(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 10, m = NULL), boot_type = "boot_emp"))
    # valid since the named arguments are B only, so m = NULL here by assumption
    testthat::expect_true(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 10), boot_type = "boot_emp"))
    # valid since the named arguments are B, m
    testthat::expect_true(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 0, m = 100), boot_type = "boot_emp"))
    # valid since the named arguments are B, m
    testthat::expect_true(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 10, m = 0), boot_type = "boot_emp"))
    # valid since the named arguments are B, m
    testthat::expect_true(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 10, m = -1), boot_type = "boot_emp"))
    # valid since the named arguments are B, m
    testthat::expect_true(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = -10, m = -1), boot_type = "boot_emp"))
    # invalid due to extra z parameter
    testthat::expect_error(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 200, m = 400, z = 2300), boot_type = "boot_emp"))
    # invalid since the named arguments are b, m i.e. not B, m
    testthat::expect_error(check_fn_args_comp_mms_var_boot_ind(inp_list = list(b = 200, m = 400), boot_type = "boot_emp"))
    # invalid since the named arguments are b, M i.e. not B, m
    testthat::expect_error(check_fn_args_comp_mms_var_boot_ind(inp_list = list(b = 200, M = 400), boot_type = "boot_emp"))
    # invalid since the named arguments are B, M i.e. not B, m
    testthat::expect_error(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 200, M = 400), boot_type = "boot_emp"))
    testthat::expect_error(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 200, m = 400, z = NULL), boot_type = "boot_emp"))
})

test_that("Residual Bootstrap input list assertion checking explicitly", {
    # invalid since the named arguments are B, m i.e. not just B
    testthat::expect_error(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 10, m = 100), boot_type = "boot_res"))
    # invalid since the named arguments are B, m i.e. not just B
    testthat::expect_error(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 10, m = NULL), boot_type = "boot_res"))
    # valid since the named arguments are B only
    testthat::expect_true(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 10), boot_type = "boot_res"))
    # invalid since the named arguments are B, m i.e. not just B
    testthat::expect_error(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 0, m = 100), boot_type = "boot_res"))
    # invalid since the named arguments are B, m i.e. not just B
    testthat::expect_error(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 10, m = 0), boot_type = "boot_res"))
    # invalid since the named arguments are B, m i.e. not just B
    testthat::expect_error(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 10, m = -1), boot_type = "boot_res"))
    # valid since the named arguments are B, m
    testthat::expect_error(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = -10, m = -1), boot_type = "boot_res"))
    # invalid since the named arguments are B, m, z i.e. not just B
    testthat::expect_error(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 200, m = 400, z = 2300), boot_type = "boot_res"))
    # invalid since the named arguments are b, m i.e. not just B
    testthat::expect_error(check_fn_args_comp_mms_var_boot_ind(inp_list = list(b = 200, m = 400), boot_type = "boot_res"))
    # invalid since the named arguments are b, M i.e. not just B
    testthat::expect_error(check_fn_args_comp_mms_var_boot_ind(inp_list = list(b = 200, M = 400), boot_type = "boot_res"))
    # invalid since the named arguments are B, M i.e. not just B
    testthat::expect_error(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 200, M = 400), boot_type = "boot_res"))
    # invalid since the named arguments are B, m, z i.e. not just B
    testthat::expect_error(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 200, m = 400, z = NULL), boot_type = "boot_res"))
    # invalid since the named arguments are b i.e. not B
    testthat::expect_error(check_fn_args_comp_mms_var_boot_ind(inp_list = list(b = 200), boot_type = "boot_res"))
    # invalid since the named arguments are all NULL
    testthat::expect_error(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = NULL), boot_type = "boot_res"))
})

test_that("Multiplier Bootstrap input list assertion checking explicitly", {
    # valid since the named arguments are B, weights_type
    testthat::expect_true(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 10, weights_type = "rademacher"), boot_type = "boot_mul"))
    # valid since the named arguments are B, weights_type i.e. weights_type is NULL
    testthat::expect_true(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 10, weights_type = NULL), boot_type = "boot_mul"))
    # valid since the named arguments are B only, so weights_type = NULL here by assumption
    testthat::expect_true(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 10), boot_type = "boot_mul"))
    # valid since the named arguments are B, m
    testthat::expect_true(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 0, weights_type = 100), boot_type = "boot_mul"))
    # valid since the named arguments are B, weights_type
    testthat::expect_true(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 10, weights_type = 0), boot_type = "boot_mul"))
    # valid since the named arguments are B, weights_type
    testthat::expect_true(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 10, weights_type = -1), boot_type = "boot_mul"))
    # valid since the named arguments are B, weights_type
    testthat::expect_true(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = -10, weights_type = -1), boot_type = "boot_mul"))
    # invalid due to extra z parameter
    testthat::expect_error(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 200, weights_type = 400, z = 2300), boot_type = "boot_mul"))
    # invalid since the named arguments are b, weights_type i.e. not B, weights_type
    testthat::expect_error(check_fn_args_comp_mms_var_boot_ind(inp_list = list(b = 200, weights_type = 400), boot_type = "boot_mul"))
    # invalid since the named arguments are b, WEIGHTS_TYPE i.e. not B, weights_type
    testthat::expect_error(check_fn_args_comp_mms_var_boot_ind(inp_list = list(b = 200, WEIGHTS_TYPE = 400), boot_type = "boot_mul"))
    # invalid since the named arguments are B, WEIGHTS_TYPE i.e. not B, weights_type
    testthat::expect_error(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 200, WEIGHTS_TYPE = 400), boot_type = "boot_mul"))
    testthat::expect_error(check_fn_args_comp_mms_var_boot_ind(inp_list = list(B = 200, weights_type = 400, z = NULL), boot_type = "boot_mul"))
})

test_that("test empirical bootstrap assertion checking in comp_mms_var", {
    # invalid due to extra z parameter
    testthat::expect_error(comp_mms_var(mod_fit = lm_fit,
                                    boot_emp = list(B = 200, m = 400, z = 2300),
                                    boot_res = NULL,
                                    boot_mul = NULL))
    # invalid since the named arguments are b, m i.e. not B, m
    testthat::expect_error(comp_mms_var(mod_fit = lm_fit,
                                    boot_emp = list(b = 200, m = 400),
                                    boot_res = NULL,
                                    boot_mul = list(B = 1e4,
                                                    weights_type = "rademacher")))
    # invalid since the named arguments are b, M i.e. not B, m
    testthat::expect_error(comp_mms_var(mod_fit = lm_fit,
                                    boot_emp = list(b = 200, M = 400),
                                    boot_res = NULL,
                                    boot_mul = NULL))
    # invalid since the named arguments are B, M i.e. not B, m
    testthat::expect_error(comp_mms_var(mod_fit = lm_fit,
                                    boot_emp = list(B = 200, M = 20),
                                    boot_res = NULL,
                                    boot_mul = NULL))
    testthat::expect_error(comp_mms_var(mod_fit = lm_fit,
                                    boot_emp = list(B = 200, m = 400, z = NULL),
                                    boot_res = NULL,
                                    boot_mul = NULL))
})


test_that("test residual bootstrap assertion checking in comp_mms_var", {

    # invalid since the named arguments are B, m i.e. not just B
    testthat::expect_error(comp_mms_var(mod_fit = lm_fit,
                                    boot_emp = NULL,
                                    boot_res = list(B = 10, m = 100),
                                    boot_mul = NULL))
    # invalid since the named arguments are B, m i.e. not just B
    testthat::expect_error(comp_mms_var(mod_fit = lm_fit,
                                    boot_emp = NULL,
                                    boot_res = list(B = 10, m = NULL),
                                    boot_mul = NULL))
    # invalid since the named arguments are B, m i.e. not just B
    testthat::expect_error(comp_mms_var(mod_fit = lm_fit,
                                    boot_emp = NULL,
                                    boot_res = list(B = 0, m = 100),
                                    boot_mul = NULL))
    # invalid since the named arguments are B, m i.e. not just B
    testthat::expect_error(comp_mms_var(mod_fit = lm_fit,
                                    boot_emp = NULL,
                                    boot_res = list(B = 10, m = 0),
                                    boot_mul = NULL))
    # invalid since the named arguments are B, m i.e. not just B
    testthat::expect_error(comp_mms_var(mod_fit = lm_fit,
                                    boot_emp = NULL,
                                    boot_res = list(B = 10, m = -1),
                                    boot_mul = NULL))
    # valid since the named arguments are B, m
    testthat::expect_error(comp_mms_var(mod_fit = lm_fit,
                                    boot_emp = NULL,
                                    boot_res = list(B = -10, m = -1),
                                    boot_mul = NULL))
    # invalid since the named arguments are B, m, z i.e. not just B
    testthat::expect_error(comp_mms_var(mod_fit = lm_fit,
                                    boot_emp = NULL,
                                    boot_res = list(B = 200, m = 400, z = 2300),
                                    boot_mul = NULL))
    # invalid since the named arguments are b, m i.e. not just B
    testthat::expect_error(comp_mms_var(mod_fit = lm_fit,
                                    boot_emp = NULL,
                                    boot_res = list(b = 200, m = 400),
                                    boot_mul = NULL))
    # invalid since the named arguments are b, M i.e. not just B
    testthat::expect_error(comp_mms_var(mod_fit = lm_fit,
                                    boot_emp = NULL,
                                    boot_res = list(b = 200, M = 400),
                                    boot_mul = NULL))
    # invalid since the named arguments are B, M i.e. not just B
    testthat::expect_error(comp_mms_var(mod_fit = lm_fit,
                                    boot_emp = NULL,
                                    boot_res = list(B = 200, M = 400),
                                    boot_mul = NULL))
    # invalid since the named arguments are B, m, z i.e. not just B
    testthat::expect_error(comp_mms_var(mod_fit = lm_fit,
                                    boot_emp = NULL,
                                    boot_res = list(B = 200, m = 400, z = NULL),
                                    boot_mul = NULL))
    # invalid since the named arguments are b i.e. not B
    testthat::expect_error(comp_mms_var(mod_fit = lm_fit,
                                    boot_emp = NULL,
                                    boot_res = list(b = 200),
                                    boot_mul = NULL))
    # invalid since the named arguments are all NULL
    testthat::expect_error(comp_mms_var(mod_fit = lm_fit,
                                    boot_emp = NULL,
                                    boot_res = list(B = NULL),
                                    boot_mul = NULL))
})


test_that("test multiplier bootstrap assertion checking in comp_mms_var", {
    # invalid due to extra z parameter
    testthat::expect_error(comp_mms_var(mod_fit = lm_fit,
                                    boot_emp = list(B = 200, m = 400),
                                    boot_res = NULL,
                                    boot_mul = list(B = 200,
                                                    weights_type = 400,
                                                    z = 2300)))
    # invalid since the named arguments are b, weights_type i.e. not B, weights_type
    testthat::expect_error(comp_mms_var(mod_fit = lm_fit,
                                    boot_emp = list(B = 200),
                                    boot_res = NULL,
                                    boot_mul = list(b = 200, weights_type = 400)))
    # invalid since the named arguments are b, WEIGHTS_TYPE i.e. not B, weights_type
    testthat::expect_error(comp_mms_var(mod_fit = lm_fit,
                                    boot_emp = list(B = 200, M = 20),
                                    boot_res = NULL,
                                    boot_mul = list(b = 200, WEIGHTS_TYPE = 400)))
    # invalid since the named arguments are B, WEIGHTS_TYPE i.e. not B, weights_type
    testthat::expect_error(comp_mms_var(mod_fit = lm_fit,
                                    boot_emp = list(B = 200, m = 400),
                                    boot_res = NULL,
                                    boot_mul = list(B = 200, WEIGHTS_TYPE = 400)))
})







# test output of comp_mms_var matches output of its functions ----


test_that("test estimate variance from empirical bootstrap matches comp_mms_var", {
    # Empirical Bootstrap check
    set.seed(454354534)
    boot_out_emp1 <- comp_boot_emp(lm_fit, B = 1e2, m = 600)
    set.seed(454354534)
    boot_out_emp_comp1 <- comp_mms_var(mod_fit = lm_fit,
                                       boot_emp = list(B = 1e2, m = 600),
                                       boot_res = NULL,
                                       boot_mul = NULL)
    expect_equal(
        boot_out_emp_comp1[["var_boot_emp"]][["cov_mat"]],
        boot_out_emp1[["cov_mat"]], tol = 1e-5)
    expect_equal(
        boot_out_emp_comp1[["var_boot_emp"]][["cov_mat"]],
        boot_out_emp1[["cov_mat"]], tol = 1e-5)
})

test_that("test estimate variance from residual bootstrap matches comp_mms_var", {
    # Residual Bootstrap check
    set.seed(67242)
    boot_out_res1 <- comp_boot_res(lm_fit, B = 1e2)
    set.seed(67242)
    boot_out_res_comp1 <- comp_mms_var(mod_fit = lm_fit,
                                       boot_emp = NULL,
                                       boot_res = list(B = 1e2),
                                       boot_mul = NULL)
    expect_equal(
        boot_out_res1[["var_boot_res"]][["cov_mat"]],
        boot_out_res_comp1[["cov_mat"]], tol = 1e-4)
})

test_that("test estimate variance from multiplier bootstrap matches comp_mms_var", {
    # Multiplier Bootstrap check
    set.seed(542525)
    boot_out_mul1 <- comp_boot_mul(lm_fit, B = 1e2, weights_type = "rademacher")
    set.seed(542525)
    boot_out_mul_comp1 <- comp_mms_var(mod_fit = lm_fit,
                                       boot_emp = NULL,
                                       boot_res = NULL,
                                       boot_mul = list(B = 1e2,
                                                       weights_type = "rademacher"))
    expect_equal(
        boot_out_mul1[["var_boot_res"]][["cov_mat"]],
        boot_out_mul_comp1[["cov_mat"]], tol = 1e-4)
})



# test whether our variance estimators agree ----
test_that("test estimated covariance matrices from multiplier and empirical bootstraps match in case of misspecified models", {

    set.seed(1342)
    # generate misspecified model
    df <- gen_reg_data(100, gamma = 0.1)
    lm_fit <- lm(Y ~ X, data = df)

    # Multiplier Bootstrap
    boot_out_mul <- comp_boot_mul(lm_fit, B = 1e4, weights_type = "rademacher")
    # Empirical bootstrap
    boot_out_emp <- comp_boot_emp(lm_fit, B = 1e4)

    # check covariance matrix
    expect_equal(
        boot_out_mul[["cov_mat"]],
        boot_out_emp[["cov_mat"]],
        tol = 1e-2)
    expect_equal(
        boot_out_mul[["var_summary"]],
        boot_out_emp[["var_summary"]],
        tol = 1e-1)
})




