#' title: "Vignette 3.1"
#' date: "May 8, 2021"
#' output: github_document
#' ---

#+ setup, include=FALSE
# knitr::opts_chunk$set(collapse = TRUE)

# Setup Libraries ---------------------------------------------------------
#' Let us load the required libraries
library(tidyverse)
library(furrr)
library(glue)
library(cli)
library(broom)
library(vroom)
library(progressr)

#+ setup, include=FALSE
devtools::document()
devtools::load_all()


# effect of increasing B in (n-out-of-n) empirical / multiplier /
# (sqrt(n) out of n) subsampling bootstrap samples on coverage when
# compared to sandwich

# Global variables --------------------------------------------------------
set.seed(1241)
NUM_COVG_REPS <- 5 * 1e3 # Number of coverage replications

# Generate replication data + model fit -----------------------------------
# Helper function to generate a single replication dataset and `lm` fit
gen_ind_mod_fit <- function(n) {
  x1 <- runif(n, -1, 1)
  x2 <- runif(n, -1, 1)
  e1 <- runif(n, -1, 1)
  e2 <- runif(n, -1, 1)
  x3 <- 0.2 * x1 + 0.2 * (x2 + 2)^2 + 0.2 * e1
  x4 <- 0.1 + 0.1 * (x1 + x2) + 0.3 * (x1 + 1.5)^2 + 0.2 * e2
  x5 <- rbinom(n, 1, exp(x1) / {
    1 + exp(x1)
  })
  x6 <- rbinom(n, 1, exp(x2) / {
    1 + exp(x2)
  })
  x <- cbind(x1, x2, x3, x4, x5, x6)
  beta0 <- c(1.3, -1.3, 1, -0.5, 0.5, -0.5) / sqrt(5.13)
  y <- abs(x %*% beta0) + rnorm(n)
  mod_data <- as_tibble(x) %>% add_column(y = as.vector(y))
  return(lm(y ~ ., data = mod_data))
}

# Generate all fitted models on our replication datasets
# Note: This could be generated on the fly as well, but here we will reuse this
#       multiple times, so more efficient to generate this up front

# Create grid sequences -----------------------------------------------
# Generate the grid of all the variance calculation parameters
# This time run it without the filtering
grid_B <- c(seq(5, 40, by = 5), c(seq(45, 120, by = 10)))
grid_n <- 500
#grid_n <- c(20, 50, 100, 150)#, 500) %>% rev()
# grid_B <- seq(2,10, by = 2)
# grid_n <- seq(20,40,length = 2)

grid_params <- crossing(covg = 1:NUM_COVG_REPS, n = grid_n) %>%
    #mutate(lm_fit = map(n, ~gen_ind_mod_fit(n = .x))) %>%
    crossing(B = grid_B) %>%
    select(covg, n, B)
grid_params

length(grid_B) * length(grid_n) * NUM_COVG_REPS - nrow(grid_params)

# We can get the confint for a single replication
get_ind_confint <- function(covg, n, B){
    # TODO: Remove the print message later
    #cli_text(glue::glue("Running coverage replication index: {covg}..."))
    #cli_text(glue::glue("n: {n}..."))
    #cli_text(glue::glue("B: {B}..."))
  #pb()
    confint_fit <- comp_var(mod_fit = gen_ind_mod_fit(n = n),
                            boot_emp = list(B = B),
                            boot_sub = list(B = B, m = floor(sqrt(n))),
                            boot_mul = list(B = B, weights_type = "rademacher")) %>%
        get_confint(mod_fit = ., level = 0.95)
    return(confint_fit)
}

# Use furrr to run in parallel
# https://github.com/DavisVaughan/furrr#example
plan(multicore, workers = 25)

cli_h1('Confint replications...')
#with_progress({
  #pb <- progressor(steps = nrow(grid_params))
confint_replications <- grid_params %>%
    mutate(out_confint = future_pmap(.l = .,
                                     .f = get_ind_confint,
                                     .options = furrr_options(seed = TRUE),
                                     .progress = TRUE))
#})

# Check the output
head(confint_replications)


# Get combined confidence intervals ---------------------------------------
cli_h1('Process confidence interval data')
all_confint <- confint_replications %>%
    unnest(out_confint) %>%
    filter(stat_type == "conf.low" | stat_type == "conf.high") %>%
    pivot_wider(names_from = stat_type, values_from = stat_val)

# Population parameters, run on large n e.g. n = 1e6
cli_h1('Obtain population projection parameter...')
df_mod <- gen_ind_mod_fit(1e7)
df_mod

# Get the broom output
df_mod_summ <- tidy(df_mod) %>% select(term, estimate) %>% rename(true_param = estimate)

sd_bootstrap <- function(x, B = 1e3, alpha = 0.95){
  #len_x <- length(x)
  #out_sample <- 1:B %>%
  #  future_map_dbl(~ mean(sample(x, size = len_x, replace = TRUE)),
  #                 .options = furrr_options(seed = TRUE))
  #quants <- quantile(out, c((1 - alpha)/2, alpha + (1 - alpha)/2))
  #return(sd(out_sample))
  return(sd(x))
}

cli_h1('Confint coverage and width plots...')

all_confint_coverage <- all_confint %>%
    left_join(df_mod_summ, by = "term") %>%
    mutate(coverage_ind = ifelse(conf.low <= true_param &
                                   conf.high >= true_param, 1, 0)) %>%
    group_by(term, B, n, var_type_abb) %>%
    summarize(
        coverage = mean(coverage_ind),
        avg_width = mean(conf.high - conf.low),
        std.error.avg_width = sd(conf.high - conf.low)/sqrt(n()),
        std.error.coverage = sd(coverage_ind)/sqrt(n())
    )

vroom_write(x = all_confint_coverage, path = here::here("R/scripts_and_filters/experiments/confidence3.1.csv"))

all_confint_coverage <- all_confint_coverage %>%
  filter(term == 'x1')

# Coverage seems to be working here i.e. most of the values are in the 86-100% range
summary(all_confint_coverage$coverage)
hist(all_confint_coverage$coverage, breaks = 10, xlim = c(0, 1))

all_confint_plt <- all_confint_coverage %>%
    mutate(
      n_text = as.factor(glue("n = {n}"))
    ) %>%
    ggplot(aes(x = B, y = coverage, fill = var_type_abb, col = var_type_abb)) +
    geom_point() +
    geom_line() +
  geom_ribbon(aes(ymin = coverage-1.96*std.error.coverage,
                  ymax = coverage+1.96*std.error.coverage), alpha = 0.3) +
    facet_grid(~ n_text) +
    labs(y = "Coverage") +
    theme_bw() +
    geom_hline(yintercept = 0.95, linetype = "dashed")
all_confint_plt

# Save the plot
# Note: It may be good to pre-run this script and read it in our paper, rather
#       than knit the pdf with this time consuming run
ggsave(filename = here::here("R/scripts_and_filters/experiments/experiment-vignette3.1_coverage.png"), plot = all_confint_plt)
# Plot interactively using plotly
#plotly::ggplotly(p = all_confint_plt)

all_avgwidth_plt <- all_confint_coverage %>%
  mutate(n_text = as.factor(glue("n = {n}"))) %>%
  ggplot(aes(x = B, y = avg_width, fill = var_type_abb, col = var_type_abb)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = avg_width-1.96*std.error.avg_width,
                  ymax = avg_width+1.96*std.error.avg_width), alpha = 0.3) +
  facet_grid(~ n_text) +
  labs(y = "Coverage") +
  theme_bw()
all_avgwidth_plt
# Save the plot
# Note: It may be good to pre-run this script and read it in our paper, rather
#       than knit the pdf with this time consuming run
ggsave(filename = here::here("R/scripts_and_filters/experiments/experiment-vignette3.1_width.png"), plot = all_avgwidth_plt)
# Plot interactively using plotly
#plotly::ggplotly(p = all_confint_plt)


