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


test_that("draw from dataset with one observation works returns that observation", {
  boot <- bootstrap_samples(data = data.frame(y = 1, x = 2), B = 100, m = 10) %>% tidyr::unnest(cols = data)

  expect_equal(boot$y, rep(1, 100 * 10))
  expect_equal(boot$x, rep(2, 100 * 10))
})


test_that("sample mean of bootstraped data sets equals asymptotic mean", {
  boot <- bootstrap_samples(data = data.frame(y = 1, x = 2), B = 1e3, m = 1) %>% tidyr::unnest(cols = data)
  expect_equal(mean(boot$y), 1, tolerance = 1e-5)
  expect_equal(mean(boot$x), 2, tolerance = 1e-5)

  boot <- bootstrap_samples(data = data.frame(y = 1, x = 2), B = 1, m = 1e3) %>% tidyr::unnest(cols = data)
  expect_equal(mean(boot$y), 1, tolerance = 1e-5)
  expect_equal(mean(boot$x), 2, tolerance = 1e-5)
})


test_that("lm and glm fitted with conditional_model return the same outputs as the original models", {
  # ols
  mod_fit <- conditional_model(lm_fit, model.frame(lm_fit))
  expect_equal(mod_fit %>% dplyr::pull(estimate) %>% unname(), broom::tidy(lm_fit) %>% dplyr::pull(estimate))

  # glm
  mod_fit <- conditional_model(glm_fit, model.frame(glm_fit))
  expect_equal(mod_fit %>% dplyr::pull(estimate) %>% unname(), broom::tidy(glm_fit) %>% dplyr::pull(estimate))
})



test_that("test sample mean of coefficients estimated via bootstrap matches the original coefficients", {
  # ols
  boot_out <- empirical_bootstrap(lm_fit, B = 1e3) %>% tidyr::unnest(cols = boot_out)
  expect_equal(boot_out %>% group_by(term) %>% summarise(mean = mean(estimate)) %>% pull(mean) %>% unname(), unname(coef(lm_fit)), tol = 1e-2)

  # glm
  boot_out <- empirical_bootstrap(glm_fit, B = 1e3) %>% tidyr::unnest(cols = boot_out)
  expect_equal(boot_out %>% group_by(term) %>% summarise(mean = mean(estimate)) %>% pull(mean) %>% unname(), unname(coef(glm_fit)), tol = 1e-2)
})



