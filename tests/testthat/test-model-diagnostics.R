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


test_that("error handling in computation of grid", {
    expect_error(gen_grid_cent_rwgt(X, grid_method = 'regular', n_grid=-1))
    expect_error(gen_grid_cent_rwgt(X, grid_method = 'regular', n_grid=0))
    expect_error(gen_grid_cent_rwgt(X, grid_method = 'regular', n_grid=1.1))
})


test_that("computation of grid centers for regular grid", {
    expect_equal(
        seq(min(X), max(X),length=100),
        gen_grid_cent_rwgt(X, grid_method = 'regular', n_grid=100)
        )
})


test_that("standard errors in reweighting with extreme center has large variance", {
    data <- model.frame(glm_fit)
    # with reweighting
    boot_samples <- comp_boot_emp_samples(data = data %>% tibble::add_column(n_obs = 1:nrow(data), B = 1000, m = nrow(data)))
    coef_rwgt_boot <- suppressWarnings(diag_fit_reg_rwgt_single(glm_fit, 'X', boot_samples, c(-5)))
    # without reweighting
    coef_boot <- comp_boot_emp(glm_fit, B = 1e3)$boot_out %>% tidyr::unnest(cols = boot_out)
    expect_true(
       all(coef_boot %>% dplyr::group_by(term) %>% dplyr::summarise(std.error = sd(estimate)) %>% dplyr::pull(std.error) <
        coef_rwgt_boot %>% dplyr::group_by(term) %>% dplyr::summarise(std.error = sd(estimate)) %>% dplyr::pull(std.error))
    )
})


test_that("error handling in the function for the reweighting of the coefficients", {
    expect_error(diag_fit_reg_rwgt(lm_fit, 'X_fake'))
    expect_error(diag_fit_reg_rwgt(lm_fit, 'X', n_grid = 11.2))
    expect_error(diag_fit_reg_rwgt(lm_fit, 'X', grid_centers = 10))
})
