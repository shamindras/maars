set.seed(674748)
n <- 1e5
X <- stats::rnorm(n, 0, 1)
y <- 2 + X * 1 + stats::rnorm(n, 0, 10)
mod_fit <- stats::lm(y ~ X)

J_inv <- stats::summary.lm(mod_fit)$cov.unscaled
X <- qr.X(mod_fit$qr)
V <- t(X) %*% Matrix::Diagonal(x = stats::residuals(mod_fit)^2) %*% X

std_error_sand <- sqrt(diag(as.matrix(J_inv %*% V %*% J_inv))) %>%
    tibble::enframe(
        x = .,
        name = "term",
        value = "std.error"
    )

out <- mod_fit %>%
    broom::tidy(x = .) %>%
    dplyr::select(term, estimate) %>%
    dplyr::left_join(
        x = .,
        y = std_error_sand,
        by = "term"
    ) %>%
    dplyr::mutate(
        .data = .,
        statistic = .data$estimate / .data$std.error,
        p.value = 2 * (1 - sapply(
            abs(.data$statistic),
            stats::pnorm
        ))
    )

out_list <- list(var_type = "se_sand",
                 var_summary =  out,
                 var_assumptions = c("All observations as assumed to be i.i.d",
                                     "Assumption 2",
                                     "Assumption 3"),
                 cov_mat = V)
