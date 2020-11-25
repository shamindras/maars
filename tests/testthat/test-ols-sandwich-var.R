set.seed(1246426)

n <- 1e5
X <- stats::rnorm(n, 0, 1)
y <- 2 + X * 1 + stats::rnorm(n, 0, 10)
lm_fit <- stats::lm(y ~ X)
sandwich_qr_var <- comp_sandwich_qr_var(lm_fit)
MAX_DIFF_low_precision <- 1e-5
MAX_DIFF_high_precision <- 1e-7

test_that("lm and sandwhich variance", {
  expect_true(
    max(abs(unname(diag(vcov(lm_fit))) - diag(sandwich_qr_var))) < MAX_DIFF_low_precision
  )
})

test_that("sandwich variance from our estimator and estimator from Sandwich pkg", {
  sandwich_sandpkg_var <- sandwich::sandwich(lm_fit)
  expect_true(
    max(abs(c(sandwich_sandpkg_var) - c(sandwich_qr_var))) < MAX_DIFF_high_precision
  )
})

test_that("sandwich variance from estimator via qr and lm", {
  n <- 1e4
  X <- stats::rnorm(n, 0, 1)
  y <- 2 + X * 1 + stats::rnorm(n, 0, 10)
  lm_fit <- stats::lm(y ~ X)
  sandwich_qr_var <- comp_sandwich_qr_var(lm_fit)
  sandwich_lm_var_term <- coef(lm(diag(lm_fit$residuals) ~ 0 + qr.X(lm_fit$qr)))
  sandwich_lm_var <- sandwich_lm_var_term %*% t(sandwich_lm_var_term)
  expect_true(
    max(abs(c(sandwich_lm_var) - c(sandwich_qr_var))) < MAX_DIFF_high_precision
  )
})

testthat::test_that("Check assertions are handled correctly", {
  # Check that lm_fit is an object of class "lm"
  expect_error(comp_sandwich_qr_var(lm_object = mtcars))
})
