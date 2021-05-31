#' title: "Vignette 3.1"
#' date: "May 8, 2021"
#' output: github_document
#' ---

#+ setup, include=FALSE
# knitr::opts_chunk$set(collapse = TRUE)

# Setup Libraries ---------------------------------------------------------
#' Let us load the required libraries
library(tidyverse)
# library(furrr)
library(glue)
library(cli)
library(broom)
# library(progressr)

#+ setup, include=FALSE
devtools::document()
devtools::load_all()

# effect of increasing B in (n-out-of-n) empirical / multiplier /
# (sqrt(n) out of n) subsampling bootstrap samples on coverage when
# compared to sandwich

# Global variables --------------------------------------------------------
set.seed(1241)
NUM_COVG_REPS <- 5000

# Generate replication data + model fit -----------------------------------
# Helper function to generate a single replication dataset and `lm` fit

# Arun's original function, with RF/SS modifications
# gen_ind_mod_fit <- function(n) {
#   x1 <- runif(n, -1, 1)
#   x2 <- runif(n, -1, 1)
#   e1 <- runif(n, -1, 1)
#   e2 <- runif(n, -1, 1)
#   x3 <- 0.2 * x1 + 0.2 * (x2 + 2)^2 + 0.2 * e1
#   x4 <- 0.1 + 0.1 * (x1 + x2) + 0.3 * (x1 + 1.5)^2 + 0.2 * e2
#   x5 <- rbinom(n, 1, exp(x1) / {
#     1 + exp(x1)
#   })
#   x6 <- rbinom(n, 1, exp(x2) / {
#     1 + exp(x2)
#   })
#   x <- cbind(x1, x2, x3, x4, x5, x6)
#   beta0 <- c(1.3, -1.3, 1, -0.5, 0.5, -0.5) / sqrt(5.13)
#   y <- abs(x %*% beta0) + rnorm(n)
#   mod_data <- as_tibble(x) %>% add_column(y = as.vector(y))
#   return(lm(y ~ ., data = mod_data))
# }

# Arun's original function, with very minor modifications
gen_ind_mod_fit <- function(n) {
  nsamp <- n
  x1 <- runif(nsamp, -1, 1)
  x2 <- runif(nsamp, -1, 1)
  e1 <- runif(nsamp, -1, 1)
  e2 <- runif(nsamp, -1, 1)
  x3 <- 0.2 * x1 + 0.2 * (x2 + 2)^2 + 0.2 * e1
  x4 <- 0.1 + 0.1 * (x1 + x2) + 0.3 * (x1 + 1.5)^2 + 0.2 * e2
  x5 <- rbinom(nsamp, 1, exp(x1) / {
    1 + exp(x1)
  })
  x6 <- rbinom(nsamp, 1, exp(x2) / {
    1 + exp(x2)
  })
  x <- cbind(x1, x2, x3, x4, x5, x6)
  beta0 <- c(1.3, -1.3, 1, -0.5, 0.5, -0.5) / sqrt(5.13)
  m0 <- function(t) {
    return(abs(t))
  }
  y <- m0(x %*% beta0) + rnorm(nsamp)
  data <- cbind(y, x)
  colnames(data) <- c("y", str_c("x", 1:6))
  mod_data <- as_tibble(data)
  return(lm(y ~ ., data = mod_data))
}

# Generate all fitted models on our replication datasets
# Note: This could be generated on the fly as well, but here we will reuse this
#       multiple times, so more efficient to generate this up front

# Population parameters, run on large n e.g. n = 1e6
cli_h1('Obtain population projection parameter...')
df_mod <- gen_ind_mod_fit(1e7)
df_mod

# Get the broom output
df_mod_summ <- tidy(df_mod) %>%
  select(term, estimate) %>%
  rename(true_param = estimate)

# Create grid sequences -----------------------------------------------
# Generate the grid of all the variance calculation parameters
# This time run it without the filtering
# grid_B <- seq(2, 100, by = 2)
grid_n <- c(20, 50, 100, 200, 500, 1000)

grid_params <- crossing(covg = 1:NUM_COVG_REPS, n = grid_n) %>%
  # crossing(B = grid_B) %>%
  select(covg, n) %>%
  mutate(lm_fit = map(.x = n, .f = ~gen_ind_mod_fit(n = .x)),
         conf_int_out = map(.x = lm_fit,
                            .f = ~tidy(.x, conf.int = TRUE,
                                       conf.level = 0.95))) %>%
  unnest(conf_int_out)

all_confint_coverage <- grid_params %>%
  left_join(df_mod_summ, by = "term") %>%
  mutate(var_type_abb = "lm") %>%
  mutate(coverage_ind = ifelse(conf.low <= true_param &
                                 conf.high >= true_param, 1, 0)) %>%
  # group_by(term, B, n, var_type_abb) %>%
  group_by(term, n, var_type_abb) %>%
  summarize(
    coverage = mean(coverage_ind),
    avg_width = mean(conf.high - conf.low)
  )

all_confint_coverage %>% ungroup(x = .) %>% group_by(term) %>% group_split()

# Let's manually check x2
true_proj_param_x2 <- df_mod_summ %>%
  filter(term == "x2") %>%
  pull(true_param)
grid_params %>%
  filter(term == "x2") %>%
  mutate(var_type_abb = "lm",
         conf.low.is.lower = (conf.low <= true_proj_param_x2),
         conf.high.is.higher = (conf.high >= true_proj_param_x2)) %>%
  mutate(coverage_ind = ifelse(conf.low.is.lower &
                                 conf.high.is.higher, 1, 0)) %>%
  # group_by(term, B, n, var_type_abb) %>%
  group_by(term, n, var_type_abb) %>%
  summarize(
    coverage = mean(coverage_ind),
    avg_width = mean(conf.high - conf.low)
  )

