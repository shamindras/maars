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

# generate regression data on which we'll fit a correctly specified reg. model
n <- 1e3
df <- gen_reg_data(n = n, gamma = 0)
lm_fit <- stats::lm(Y ~ X, data = df)


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
  expect_equal(boot_out %>% dplyr::group_by(term) %>%
                 dplyr::summarize(mean = mean(estimate)) %>%
                 dplyr::pull(mean) %>% unname(),
               unname(stats::coef(lm_fit)), tol = 1e-2)
})


test_that("test covariance matrix from our empirical bootstrap matches that
          from the sandwich package for well-specified model", {

  var_empboot <- comp_boot_emp(lm_fit, B = 1e4)$cov_mat
  # get estimate from sandwich package
  var_sandwichpkg <- sandwich::vcovBS(lm_fit, cluster = NULL, R = 1e4)

  expect_equal(var_empboot,
               var_sandwichpkg, tol = 1e-3)
})

test_that("test statistics from our empirical
          bootstrap matches that from the sandwich package for misspecified model", {

  # generate misspecified model
  df <- gen_reg_data(100, gamma = 0.3)
  lm_fit <- lm(Y ~ X, data = df)

  var_empboot <- comp_boot_emp(lm_fit, B = 5e4)
  # get estimate from sandwich package
  var_sandwichpkg <- sandwich::vcovBS(lm_fit,
                                      type = "xy",
                                      cluster = NULL,
                                      R = 1e5)
  # covariance matrices
  expect_equal(var_empboot$cov_mat,
               var_sandwichpkg, tol = 1e-2)
  # statistic
  expect_equal(
    var_empboot$var_summary$statistic,
    broom::tidy(lmtest::coeftest(lm_fit, vcov = var_sandwichpkg))$statistic,
    tol = 1e-2
  )
  # error
  expect_equal(
    var_empboot$var_summary$std.error,
    broom::tidy(lmtest::coeftest(lm_fit, vcov = var_sandwichpkg))$std.error,
    tol = 1e-2
  )
  # p-values
  expect_equal(
    var_empboot$var_summary$p.value,
    broom::tidy(lmtest::coeftest(lm_fit, vcov = var_sandwichpkg))$p.value,
    tol = 1e-2
  )
})


test_that("test bootstrap confidence intervals via percentile method for different values of m matches for stats::lm", {
  # ols
  boot_out <- comp_boot_emp(lm_fit, B = 1e3)$boot_out
  ci <- comp_ci_boot(comp_boot_emp(lm_fit, B = 1e3)$boot_out)
  boot_50 <- comp_boot_emp(lm_fit, B = 1e4, m = 50)$boot_out
  ci_50 <- comp_ci_boot(boot_50)
  expect_equal(ci, ci_50, tol = 1e-2)
})








