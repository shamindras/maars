set.seed(168465)

# Number of decimal places to round the variance estimators
NUM_DEC_PL <- 7

# Function to create the sample linear regression simulated data
create_lm_fit <- function(n, p) {
    Sigma <- diag(p)
    betas <- seq.int(from = 1, by = 1, length.out = p)
    X <- MASS::mvrnorm(n = n, rep(0, p), Sigma)
    y <- 2 + X %*% betas + stats::rnorm(n, 0, 10)
    return(stats::lm(y ~ X))
}

results <- bench::press(
    n = seq.int(from = 1000, by = 1000, length.out = 5),
    p = seq.int(from = 1, by = 1, length.out = 5),
    {
        lm_fit <- create_lm_fit(n, p)
        bench::mark(
            min_iterations = 50,
            qr_var = unname(round(comp_sandwich_qr_var(lm_fit),
                                  NUM_DEC_PL)),
            sandwich_sandpkg_var = unname(round(sandwich::sandwich(lm_fit),
                                                NUM_DEC_PL))
        )
    }
)
