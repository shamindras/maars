# Setup Libraries ---------------------------------------------------------
library(tidyverse)
library(glue)
devtools::document()
devtools::load_all()

# Define GLOBAL variables -------------------------------------------------
set.seed(35426)
NUM_COVG_REPS <- 1000 # Number of coverage replications
N <- 1e3 # Number of observed samples
D <- 5 # Number of covariates (excluding intercept)
DOF <- 3 # t-distribution degrees of freedom

# True beta values INCLUDING intercept i.e. (D + 1) values with first
# value for intercept
BETA_VALS <- rep(x = 1, times = D + 1)

# Generate replication data + model fit -----------------------------------
# Helper function to generate a single replication dataset and `lm` fit
gen_ind_tdist_mod_fit <- function(n, d, dof, betas){
    # Get the design matrix without intercept
    # This is currently generating data from a t-distn with D deg. of freedom
    X_noint <- matrix(data = stats::rt(n = n * d, df = dof), nrow = n, ncol = d)

    # Add an intercept column of 1's
    intcpt_col <- matrix(data = rep(x = 1, times = n), nrow = n, ncol = 1)

    # Get the design matrix WITH intercept column of 1's
    X <- cbind(intcpt_col, X_noint)

    # Get the true response variable generated from this model
    # In this case we define all betas to be of the same value i.e. 1
    eps <- matrix(data = rnorm(n = n, mean = 0, sd = 1), nrow = n, ncol = 1)
    Y <- X %*% betas + eps

    # Join on design matrix without intercept term, since lm fits this manually
    gen_dat_mat <- cbind(Y, X_noint)
    colnames(gen_dat_mat) <- c("y", glue::glue("x{1:D}"))
    gen_dat <- tibble::as_tibble(x = gen_dat_mat)

    # Return fitted linear model
    return(stats::lm(formula = y ~ ., data = gen_dat))
}

# Test out a single fit
# set.seed(454526)
# lm_fit1 <- gen_ind_tdist_mod_fit(n = N, d = D, dof = DOF, betas = BETA_VALS)
# summary(lm_fit1)

# Generate all fitted models on our replication datasets
# Note: This could be generated on the fly as well, but here we will reuse this
#       multiple times, so more efficient to generate this up front
set.seed(35542)
replication_mod_fits <- map(1:NUM_COVG_REPS,
                            ~gen_ind_tdist_mod_fit(n = N, d = D, dof = DOF,
                                                   betas = BETA_VALS))
length(replication_mod_fits)
replication_mod_fits[[1]]
replication_mod_fits[[1]] %>% broom::tidy(x = .)
