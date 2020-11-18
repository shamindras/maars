set.seed(1)
test_that("lm and sandwhich variance - 4 dec.pl.", {
  n <- 1e5
  X <- stats::rnorm(n, 0, 1)
  y <- 2 + X * 1 + stats::rnorm(n, 0, 10)
  lm_fit <- stats::lm(y ~ X)
  sandwich_qr_var <- comp_sandwich_qr_var(lm_fit)
  expect_equal(
    round(broom::tidy(lm_fit)$std.error, 4),
    round(sqrt(diag(sandwich_qr_var)), 4)
  )
})

test_that("sandwich variance from our estimator and estimator from Sandwich pkg - 4 dec.pl.", {
  n <- 1e5
  X <- stats::rnorm(n, 0, 1)
  y <- 2 + X * 1 + stats::rnorm(n, 0, 10)
  lm_fit <- stats::lm(y ~ X)
  sandwich_sandpkg_var <- sandwich::sandwich(lm_fit)
  sandwich_qr_var <- comp_sandwich_qr_var(lm_fit)
  expect_equal(
    c(round(sandwich_sandpkg_var, 4)),
    c(round(sandwich_qr_var, 4))
  )
})
