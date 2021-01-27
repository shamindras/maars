# Multiplier Bootstrap Utilities -----------------------------------------------

#' This is the \code{purrr} implementation of
#' \code{\link{multiplier_single_bootstrap}}. It should be slower
#' than the matrix implementation \code{\link{multiplier_single_bootstrap}}
comp_multiplier_single_bootstrap_purrr_var <- function(n, J_inv_X_res, e) {
    out <- purrr::map2(
        .x = e,
        .y = J_inv_X_res,
        ~ .x * .y
    ) %>%
        do.call(rbind, .) %>%
        #apply(., 2, mean) %>%
        apply(., 2, sum) %>%
        as.matrix(x = .)
    return(out)
}

#' This is the wrapper for the purrr implementation of the equivalent
#' \code{\link{multiplier_single_bootstrap}} function. It should be slower
#' than the matrix implementation \code{\link{comp_boot_mul}}
comp_multiplier_bootstrap_purrr_var <- function(mod_fit, B = 100, weights_type) {
    # Get OLS related output
    betas <- stats::coef(mod_fit)
    J_inv <- stats::summary.lm(mod_fit)$cov.unscaled
    X <- qr.X(mod_fit$qr)
    res <- stats::residuals(mod_fit)
    n <- length(res)
    J_inv_X_res <- 1:nrow(X) %>%
        purrr::map(~ t(J_inv %*% X[.x, ] * res[.x]))

    # Multiplier weights (mean 0, variance = 1)
    e <- matrix(data =
                    comp_boot_mul_wgt(n = B * n,
                                                     weights_type = weights_type),
                nrow = B,
                ncol = n)
    # e <- matrix(data = rnorm(n = B * n, mean = 0, sd = 1), nrow = B, ncol = n)

    # Multiplier Bootstrap replications, B times
    boot_out <- 1:B %>%
        purrr::map(~ betas +
                       comp_multiplier_single_bootstrap_purrr_var(n = n,
                                                                  J_inv_X_res = J_inv_X_res,
                                                                  e = e[.x, ])) %>%
        purrr::map(~ tibble::tibble(term = rownames(.x),
                                    estimate = .x[, 1]))

    # Consolidate output in a nested tibble
    out <- tibble::tibble("B" = 1:B) %>% dplyr::mutate("boot_out" = boot_out)

    return(out)
}

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
    mult_boot_1 <- comp_boot_mul(mod_fit = lm_fit, B = 3,
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
    mult_boot_1 <- comp_boot_mul(mod_fit = lm_fit, B = 30,
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
    expect_error(comp_boot_mul(mod_fit = lm_fit, B = 3.5,
                                               weights_type = WEIGHTS_TYPE))
    # Check that B is positive
    expect_error(comp_boot_mul(mod_fit = lm_fit, B = -1,
                                               weights_type = WEIGHTS_TYPE))
    # Check that lm_fit is an object of class "lm"
    expect_error(comp_boot_mul(mod_fit = mtcars, B = -1,
                                               weights_type = WEIGHTS_TYPE))
})


