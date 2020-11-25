set.seed(1246426)
n <- 1e2
X <- stats::rnorm(n, 0, 1)
y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
lm_fit <- stats::lm(y ~ X)
MAX_DIFF_low_precision <- 1e-5
MAX_DIFF_high_precision <- 1e-7

multiplier_single_bootstrap2 <- function(n, J_inv_X_res, e) {
    out <- purrr::map2(
        .x = e,
        .y = J_inv_X_res,
        ~ .x * .y
    ) %>%
        do.call(rbind, .) %>%
        apply(., 2, mean)
    return(out)
}

multiplier_bootstrap2 <- function(lm_fit, B = 100) {
    J_inv <- summary.lm(lm_fit)$cov.unscaled
    X <- qr.X(lm_fit$qr)
    res <- residuals(lm_fit)
    n <- length(res)
    J_inv_X_res <- 1:nrow(X) %>% purrr::map(~ t(J_inv %*% X[.x, ] * res[.x]))

    e <- matrix( rnorm(B*n,mean=0,sd=1), B, n)

    dist <- 1:B %>%
        purrr::map(~ multiplier_single_bootstrap2(n, J_inv_X_res, e[.x, ])) %>%
        do.call(rbind, .)

    out <- tibble::tibble(
        iteration = rep(1:B, ncol(X)),
        term = rep(colnames(X), each = B),
        estimate = as.numeric(dist)
    ) %>%
        dplyr::arrange(iteration, term)

    return(out)
}

# test_that("Check multiplier bootstrap implementations", {
#     set.seed(162632)
#     mult_boot_1 <- multiplier_bootstrap(lm_fit, B = 15) %>% tidyr::unnest(cols = boot_out)
#     set.seed(162632)
#     mult_boot_2 <- multiplier_bootstrap2(lm_fit, B = 15)
#     mult_boot_1
#     mult_boot_2
#     testthat::expect_equal(object = mult_boot_1,
#                            expected = mult_boot_2,
#                            MAX_DIFF_low_precision)
# })
