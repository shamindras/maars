# Multiplier Bootstrap Utilities -----------------------------------------------

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
    e <- comp_boot_mul_wgt(n = B * n, weights_type = weights_type)
    # e <- matrix(data = rnorm(n = B * n, mean = 0, sd = 1), nrow = B, ncol = n)

    # Multiplier Bootstrap replications, B times
    boot_out <- 1:B %>%
        purrr::map(~ betas +
                       comp_multiplier_single_bootstrap_purrr_var(n = n,
                                                                  J_inv_X_res = J_inv_X_res,
                                                                  e = e[seq(n * (.x - 1) + 1, .x * n)])) %>%
        purrr::map(~ tibble::tibble(term = rownames(.x),
                                    estimate = .x[, 1]))

    # Consolidate output in a nested tibble
    out <- tibble::tibble("b" = 1:B) %>% dplyr::mutate("boot_out" = boot_out)

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
                                                 weights_type = WEIGHTS_TYPE)$boot_out
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
                                                 weights_type = WEIGHTS_TYPE)$boot_out
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
                                               weights_type = WEIGHTS_TYPE)$boot_out)
    # Check that B is positive
    expect_error(comp_boot_mul(mod_fit = lm_fit, B = -1,
                                               weights_type = WEIGHTS_TYPE)$boot_out)
    # Check that lm_fit is an object of class "lm"
    expect_error(comp_boot_mul(mod_fit = mtcars, B = -1,
                                               weights_type = WEIGHTS_TYPE)$boot_out)
})



test_that("test std errors from our multiplier bootstrap match those from the sandwich package for misspecified model", {

    # ols
    n <- 1e2
    X <- rnorm(n, 0, 4)
    y <- 2*X + 3 + X^3 + rnorm(n, 0, 2)
    lm_fit <- lm(y ~ X)
    var_mulboot <- comp_boot_mul(mod_fit = lm_fit, B = 1e5,
                                 weights_type = 'rademacher')$var_summary$std.error^2
    # get estimate from sandwich package
    var_sandwichpkg <- sandwich::vcovBS(lm_fit, cluster = NULL, R = 1e5, type = "wild-rademacher")
    expect_equal(var_mulboot,
                 diag(var_sandwichpkg) %>% unname(), tol = 1e-2)

})



test_that("test statistics from our multiplier
          bootstrap matches that from the sandwich package for misspecified model", {

              # generate misspecified model
              df <- gen_reg_data(100, gamma = 0.3)
              lm_fit <- lm(Y ~ X, data = df)

              var_mulboot <- comp_boot_mul(lm_fit, B = 1e5)
              # get estimate from sandwich package
              var_sandwichpkg <- sandwich::vcovBS(lm_fit,
                                                  type = "wild",
                                                  cluster = NULL,
                                                  R = 1e5)
              # covariance matrices
              expect_equal(var_mulboot$cov_mat,
                           var_sandwichpkg, tol = 1e-2)
              # statistic
              expect_equal(
                  var_mulboot$var_summary$statistic,
                  broom::tidy(lmtest::coeftest(lm_fit, vcov = var_sandwichpkg))$statistic,
                  tol = 1e-2
              )
              # error
              expect_equal(
                  var_mulboot$var_summary$std.error,
                  broom::tidy(lmtest::coeftest(lm_fit, vcov = var_sandwichpkg))$std.error,
                  tol = 1e-2
              )
              # p-values
              expect_equal(
                  var_mulboot$var_summary$p.value,
                  broom::tidy(lmtest::coeftest(lm_fit, vcov = var_sandwichpkg))$p.value,
                  tol = 1e-2
              )
          })





