set.seed(1243434)

# Define global variables
MAX_DIFF_high_precision <- 1e-7

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


# Test Empirical Bootstrap i.e. sampling WITH replacement for each of the B
# replications

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

vcvoc
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


# Test Subsampling Bootstrap i.e. sampling WITHOUT replacement for each of the B
# replications

# Generate data from a well specified linear model
n <- 1e3
X <- stats::rnorm(n, 0, 1)
y <- 2 + X * 1 + stats::rnorm(n, 0, 10)
lm_fit <- stats::lm(y ~ X)

test_that("Test case where m > n for subsampling on OLS", {
  # m = n + 1 i.e. m > n
  expect_error(comp_var(mod_fit = lm_fit,
                        boot_emp = NULL,
                        boot_sub = list(B = 1e3, m = n + 1),
                        boot_res = NULL,
                        boot_mul = NULL))

  # Add valid empirical bootstrap, but failing subsampling with
  # m = n + 1 i.e. m > n
  expect_error(comp_var(mod_fit = lm_fit,
                        boot_emp = list(B = 1e3, m = n + 1),
                        boot_sub = list(B = 1e3, m = n + 1),
                        boot_res = NULL,
                        boot_mul = NULL))
})

test_that("Test m = n for subsampling and compare error to well specified OLS fit", {
  lm_fit_summ <- broom::tidy(lm_fit)
  mod_fit0 <- comp_var(mod_fit = lm_fit,
                       boot_emp = NULL,
                       boot_sub = list(B = 1e3, m = n),
                       boot_res = NULL,
                       boot_mul = NULL)

  mod_fit0_summ <- mod_fit0 %>%
    get_summary(mod_fit = ., boot_sub = TRUE) %>%
    tidyr::pivot_wider(names_from = stat_type, values_from = stat_val)

  # Check that the standard errors for subsampling in the case m = n are close
  # to zero, for the well specified model
  mod_fit0_serr <- mod_fit0_summ %>% dplyr::pull(std.error)
  expect_equal(mod_fit0_serr[1], 0, tol = MAX_DIFF_high_precision)
  expect_equal(mod_fit0_serr[2], 0, tol = MAX_DIFF_high_precision)

  # Check that we have computed the correct estimate values
  est_vals <- mod_fit0_summ %>%
                dplyr::select(term, estimate) %>%
                dplyr::rename(estimate_sub = estimate) %>%
                dplyr::left_join(lm_fit_summ, by = "term") %>%
                dplyr::select(term, estimate_sub, estimate) %>%
    dplyr::rename(estimate_ols = estimate) %>%
    dplyr::mutate(abs_estimate_diff = abs(estimate_sub - estimate_ols)) %>%
    dplyr::pull(abs_estimate_diff)
  expect_equal(est_vals[1], 0, tol = MAX_DIFF_high_precision)
  expect_equal(est_vals[2], 0, tol = MAX_DIFF_high_precision)
})

