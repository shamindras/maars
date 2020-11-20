set.seed(1246426)

n <- 1e5
X <- stats::rnorm(n, 0, 1)
y <- 2 + X * 1 + stats::rnorm(n, 0, 10)
lm_fit <- stats::lm(y ~ X)
sandwich_qr_var <- comp_sandwich_qr_var(lm_fit)
NUM_DEC_PL <- 5

test_that("lm and sandwhich variance", {
  expect_equal(
    round(unname(diag(vcov(lm_fit))), NUM_DEC_PL),
    round(diag(sandwich_qr_var), NUM_DEC_PL)
  )
})

test_that("sandwich variance from our estimator and estimator from Sandwich pkg", {
  sandwich_sandpkg_var <- sandwich::sandwich(lm_fit)
  expect_equal(
    c(round(sandwich_sandpkg_var, NUM_DEC_PL)),
    c(round(sandwich_qr_var, NUM_DEC_PL))
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
  expect_equal(
    c(round(sandwich_lm_var, NUM_DEC_PL)),
    c(round(sandwich_qr_var, NUM_DEC_PL))
  )
})


test_that("sandwich variance from estimator via qr and solve", {
  n <- 2e4
  X <- stats::rnorm(n, 0, 1)
  y <- 2 + X * 1 + stats::rnorm(n, 0, 10)
  lm_fit <- stats::lm(y ~ X)
  sandwich_qr_var <- comp_sandwich_qr_var(lm_fit)
  sandwich_solve_var_bread <- solve(t(cbind(1, X)) %*% cbind(1, X))
  sandwich_solve_var <- sandwich_solve_var_bread %*% t(cbind(1, X)) %*% diag(lm_fit$residuals^2) %*% cbind(1, X) %*% sandwich_solve_var_bread
  expect_equal(
    c(round(sandwich_solve_var, NUM_DEC_PL)),
    c(round(sandwich_qr_var, NUM_DEC_PL))
  )
})
