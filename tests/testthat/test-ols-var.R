test_that("lm and sandwhich variance - 4 dec.pl.", {
    n = 2e4
    X <- rnorm(n, 0, 1)
    y <- 2 + X * 1 + rnorm(n, 0, 10)
    lm_fit = lm(y ~ X)
    sandwich_var = comp_sandwich_var(X, lm_fit$residuals)
  expect_equal(round(broom::tidy(lm_fit)$std.error,3),
               round(sqrt(diag(sandwich_var)),3))
})

