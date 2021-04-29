# Define GLOBAL variables -------------------------------------------------
set.seed(35426)
N <- 1e5 # Number of observed samples
D <- 5 # Number of covariates (excluding intercept)
DOF <- 3 # t-distribution degrees of freedom

# True beta values INCLUDING intercept i.e. (D + 1) values with first
# value for intercept
BETA_VALS <- rep(x = 1, times = D + 1)


# Simulate Data from underlying model -------------------------------------

# * Design matrix ---------------------------------------------------------
# Get the design matrix without intercept
# This is currently generating data from a t-distn with D deg. of freedom
X_noint <- matrix(data = rt(n = N * D, df = DOF), nrow = N, ncol = D)

# This must be of dimension (N * D)
dim(X_noint)

# Add an intercept column of 1's
intcpt_col <- matrix(data = rep(x = 1, times = N), nrow = N, ncol = 1)

# Get the design matrix WITH intercept column of 1's
X <- cbind(intcpt_col, X_noint)

# This must be of dimension (N * (D + 1))
dim(X)

# * Response term ---------------------------------------------------------
# Get the true response variable generated from this model
# In this case we define all betas to be of the same value i.e. 1
betas <- matrix(data = rep(x = 1, times = D + 1), nrow = D + 1, ncol = 1)
eps <- matrix(data = rnorm(n = N, mean = 0, sd = 1), nrow = N, ncol = 1)
Y <- X %*% betas + eps

# Get tibble of response and covariates from this true generating process
col_names <- c("y", glue::glue("x{1:D}"))

# Join on design matrix without intercept term, since lm fits this manually
gen_dat_mat <- cbind(Y, X_noint)
colnames(gen_dat_mat) <- col_names
gen_dat <- tibble::as_tibble(x = gen_dat_mat)

# Fit linear model (OLS) to the generated data ----------------------------
lm_fit <- stats::lm(formula = y ~ ., data = gen_dat)
lm_fit
summary(lm_fit)
