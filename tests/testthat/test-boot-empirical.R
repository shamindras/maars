set.seed(1243434)

# generate data
n <- 1e3
X <- stats::rnorm(n, 0, 1)
# OLS data and model
y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
lm_fit <- stats::lm(y ~ X)


test_that("draw from dataset with one observation works returns that observation", {
  boot <- comp_boot_emp_samples(data = data.frame(y = 1, x = 2), B = 100, m = 10) %>% tidyr::unnest(cols = data)
  expect_equal(boot$y, rep(1, 100 * 10))
  expect_equal(boot$x, rep(2, 100 * 10))
})


test_that("sample mean of bootstraped data sets equals asymptotic mean", {
  boot <- comp_boot_emp_samples(data = data.frame(y = 1, x = 2), B = 1e3, m = 1) %>% tidyr::unnest(cols = data)
  expect_equal(mean(boot$y), 1, tolerance = 1e-5)
  expect_equal(mean(boot$x), 2, tolerance = 1e-5)

  boot <- comp_boot_emp_samples(data = data.frame(y = 1, x = 2), B = 1, m = 1e3) %>% tidyr::unnest(cols = data)
  expect_equal(mean(boot$y), 1, tolerance = 1e-5)
  expect_equal(mean(boot$x), 2, tolerance = 1e-5)
})


test_that("model fitted with conditional_model return the same outputs as the original models", {
  # ols
  mod_fit <- fit_reg(lm_fit, stats::model.frame(lm_fit))
  expect_equal(mod_fit %>% dplyr::pull(estimate) %>% unname(),
               broom::tidy(lm_fit) %>% dplyr::pull(estimate))
})


test_that("test sample mean of coefficients estimated via bootstrap matches the original coefficients", {
  # ols
  boot_out <- comp_boot_emp(lm_fit, B = 1e3)$boot_out %>% tidyr::unnest(cols = boot_out)
  expect_equal(boot_out %>% dplyr::group_by(term) %>% dplyr::summarise(mean = mean(estimate)) %>% dplyr::pull(mean) %>% unname(),
               unname(stats::coef(lm_fit)), tol = 1e-2)
})


test_that("test std errors from our empirical bootstrap match those from the sandwich package for well-specified model", {
  # ols
  var_empboot <- comp_boot_emp(lm_fit, B = 1e3)$var_summary$std.error^2
  # get estimate from sandwich package
  var_sandwichpkg <- sandwich::vcovBS(lm_fit, cluster = NULL, R = 1e3)
  expect_equal(var_empboot,
               diag(var_sandwichpkg) %>% unname(), tol = 1e-2)
})


# test_that("test std errors from our empirical bootstrap match those from the sandwich package for misspecified model", {
#   # ols
#   n <- 1e2
#   X <- rnorm(n, 0, 4)
#   y <- 2*X + 3 + X^3 + rnorm(n, 0, 2)
#   lm_fit <- lm(y ~ X)
#   var_empboot <- comp_boot_emp(lm_fit, B = 1e4)$var_summary$std.error^2
#   # get estimate from sandwich package
#   var_sandwichpkg <- sandwich::vcovBS(lm_fit, cluster = NULL, R = 1e4, type = "xy")
#   expect_equal(var_empboot,
#                diag(var_sandwichpkg) %>% unname(), tol = 1e-2)
# })


test_that("test bootstrap confidence intervals via percentile method for different values of m matches for stats::lm", {
  # ols
  boot_out <- comp_boot_emp(lm_fit, B = 1e3)$boot_out
  ci <- comp_ci_boot(comp_boot_emp(lm_fit, B = 1e3)$boot_out)
  boot_50 <- comp_boot_emp(lm_fit, B = 1e4, m = 50)$boot_out
  ci_50 <- comp_ci_boot(boot_50)
  expect_equal(ci, ci_50, tol = 1e-2)
})








