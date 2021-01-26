# Multiplier Bootstrap Utilities -----------------------------------------------
base::source(here::here("R", "ols-multiplier-boot-var.R"))
# devtools::load_all()
# base::source("../../R/ols-multiplier-boot-var.R")
base::source(here::here("tests", "testthat", "test-utils.R"))

# Create OLS linear regression simulated data
set.seed(1246426)
n <- 1e5
X <- stats::rnorm(n, 0, 1)
y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
lm_fit <- stats::lm(y ~ X)
MAX_DIFF_high_precision <- 1e-10
WEIGHTS_TYPE <- "std_gaussian"

test_that("Check matrix and purrr multiplier bootstrap implmentations", {
    set.seed(5444243)
    mult_boot_1 <- comp_multiplier_bootstrap_var(mod_fit = lm_fit, B = 3,
                                                 weights_type = WEIGHTS_TYPE)
    set.seed(5444243)
    mult_boot_2 <- comp_multiplier_bootstrap_purrr_var(mod_fit = lm_fit, B = 3,
                                                       weights_type = WEIGHTS_TYPE)
    testthat::expect_equal(object = mult_boot_1,
                           expected = mult_boot_2,
                           tolerance = MAX_DIFF_high_precision)
})

test_that("Check matrix and purrr multiplier bootstrap implmentations, 30 replications", {
    set.seed(1626323)
    mult_boot_1 <- comp_multiplier_bootstrap_var(mod_fit = lm_fit, B = 30,
                                                 weights_type = WEIGHTS_TYPE)
    set.seed(1626323)
    mult_boot_2 <- comp_multiplier_bootstrap_purrr_var(mod_fit = lm_fit, B = 30,
                                                       weights_type = WEIGHTS_TYPE)
    testthat::expect_equal(object = mult_boot_1,
                           expected = mult_boot_2,
                           tolerance = MAX_DIFF_high_precision)
})

testthat::test_that("Check assertions are handled correctly", {
    # Check that B is an integer
    expect_error(comp_multiplier_bootstrap_var(mod_fit = lm_fit, B = 3.5,
                                               weights_type = WEIGHTS_TYPE))
    # Check that B is positive
    expect_error(comp_multiplier_bootstrap_var(mod_fit = lm_fit, B = -1,
                                               weights_type = WEIGHTS_TYPE))
    # Check that lm_fit is an object of class "lm"
    expect_error(comp_multiplier_bootstrap_var(mod_fit = mtcars, B = -1,
                                               weights_type = WEIGHTS_TYPE))
})


