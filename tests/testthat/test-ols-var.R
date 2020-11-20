set.seed(1246426)

n <- 1e5
X <- stats::rnorm(n, 0, 1)
y <- 2 + X * 1 + stats::rnorm(n, 0, 10)
lm_fit <- stats::lm(y ~ X)
sandwich_qr_var <- comp_sandwich_qr_var(lm_fit)
NUM_DEC_PL <- 4

test_that("lm and sandwhich variance", {
  expect_equal(
    round(broom::tidy(lm_fit)$std.error, NUM_DEC_PL),
    round(sqrt(diag(sandwich_qr_var)), NUM_DEC_PL)
  )
})

test_that("sandwich variance from our estimator and estimator from Sandwich pkg", {
  sandwich_sandpkg_var <- sandwich::sandwich(lm_fit)
  expect_equal(
    c(round(sandwich_sandpkg_var, NUM_DEC_PL)),
    c(round(sandwich_qr_var, NUM_DEC_PL))
  )
})
