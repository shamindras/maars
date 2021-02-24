set.seed(1246426)

n <- 1e5
X <- stats::rnorm(n, 0, 1)
y <- 2 + X * 1 + stats::rnorm(n, 0, 10)
lm_fit <- stats::lm(y ~ X)
sandwich_qr_std_err <- comp_sand_var(lm_fit)$var_summary$std.error
MAX_DIFF_low_precision <- 1e-4
MAX_DIFF_high_precision <- 1e-7

test_that("lm and sandwhich standard errors", {
  expect_true(
    max(abs(unname(sqrt(diag(vcov(lm_fit)))) - sandwich_qr_std_err)) < MAX_DIFF_low_precision
  )
})

test_that("sandwich standard errors from our estimator and estimator from Sandwich pkg", {
  sandwich_sandpkg_std_err <- sqrt(diag(sandwich::sandwich(lm_fit)))
  expect_true(
    max(abs(sandwich_sandpkg_std_err - sandwich_qr_std_err)) < MAX_DIFF_high_precision
  )
})

test_that("sandwich variance from estimator via qr and lm", {
  n <- 1e4
  X <- stats::rnorm(n, 0, 1)
  y <- 2 + X * 1 + stats::rnorm(n, 0, 10)
  lm_fit <- stats::lm(y ~ X)
  sandwich_qr_std_err <- comp_sand_var(lm_fit)$var_summary$std.error
  sandwich_lm_var_term <- coef(lm(diag(lm_fit$residuals) ~ 0 + qr.X(lm_fit$qr)))
  sandwich_lm_var <- sandwich_lm_var_term %*% t(sandwich_lm_var_term)
  sandwich_lm_std_err <- sqrt(diag(sandwich_lm_var))
  expect_true(
    max(abs(c(sandwich_lm_std_err) - c(sandwich_qr_std_err))) < MAX_DIFF_high_precision
  )
})

testthat::test_that("Check assertions are handled correctly", {
  # Check that lm_fit is an object of class "lm"
  expect_error(comp_sand_var(mtcars))
})

