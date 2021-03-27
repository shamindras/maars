set.seed(1243434)

# generate data
n <- 1e3
X <- stats::rnorm(n, 0, 1)
# OLS data and model
y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
lm_fit <- stats::lm(y ~ X)


test_that("test sample mean of coefficients estimated via bootstrap matches the original coefficients", {
  boot_out <- comp_boot_res(lm_fit, B = 1e3)$boot_out %>% tidyr::unnest(cols = boot_out)
  expect_equal(boot_out %>% dplyr::group_by(term) %>%
    dplyr::summarize(mean = mean(estimate)) %>% dplyr::pull(mean) %>% unname(),
    unname(stats::coef(lm_fit)), tol = 1e-2)
})

test_that('variance is 0 when the model is well-specified and sample size is large',{
    n <- 1e4
    X <- stats::rnorm(n, 0, 1)
    # OLS data and model
    y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
    lm_fit <- stats::lm(y ~ X)
    boot_out <- comp_boot_res(lm_fit, B = 1e3)$boot_out %>% tidyr::unnest(cols = boot_out)
    expect_equal(boot_out %>% dplyr::group_by(term) %>%
                     dplyr::summarize(sd_estimate = sd(estimate)) %>%
                   dplyr::pull(sd_estimate) %>% unname(),
                 c(0,0), tol = 1e-2)
})




