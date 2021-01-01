# Multiplier Bootstrap ---------------------------------------------------------

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
        apply(., 2, mean) %>%
        as.matrix(x = .)
    return(out)
}

#' This is the wrapper for the purrr implementation of the equivalent
#' \code{\link{multiplier_single_bootstrap}} function. It should be slower
#' than the matrix implementation \code{\link{comp_multiplier_bootstrap_var}}
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
                    maar::gen_multiplier_bootstrap_weights(n = B * n,
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
