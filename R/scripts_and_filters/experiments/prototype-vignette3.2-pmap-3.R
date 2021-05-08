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

#+ setup, include=FALSE
devtools::document()
devtools::load_all()

# effect of increasing B in (n-out-of-n) empirical / multiplier /
# (sqrt(n) out of n) subsampling bootstrap samples on coverage when
# compared to sandwich

# Global variables --------------------------------------------------------
set.seed(1241)
NUM_COVG_REPS <- 10 # Number of coverage replications

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
grid_B <- seq(2, 14, by = 2)
grid_n <- c(20, 50, 100)

grid_params <- crossing(covg = 1:NUM_COVG_REPS, n = grid_n) %>%
    mutate(lm_fit = map(n, ~gen_ind_mod_fit(n = .x))) %>%
    crossing(B = grid_B) %>%
    select(covg, n, B, m_sub, lm_fit)
grid_params

length(grid_B) * length(grid_n) * NUM_COVG_REPS - nrow(grid_params)

# We can get the confint for a single replication
get_ind_confint <- function(covg, n, B, lm_fit){
    # TODO: Remove the print message later
    cli_text(glue::glue("Running coverage replication index: {covg}..."))
    cli_text(glue::glue("n: {n}..."))
    cli_text(glue::glue("B: {B}..."))
    confint_fit <- comp_var(mod_fit = lm_fit,
                            boot_emp = list(B = B),
                            boot_sub = list(B = B, m = floor(sqrt(n))),
                            boot_mul = list(B = B, weights_type = "rademacher")) %>%
        get_confint(mod_fit = ., level = 0.95,
                    sand = TRUE, boot_emp = TRUE, boot_sub = TRUE,
                    boot_mul = TRUE, boot_res = FALSE)
    return(confint_fit)
}

# Use furrr to run in parallel
# https://github.com/DavisVaughan/furrr#example
plan(multicore, workers = 50)

# TODO: replace pmap with future_pmap
system.time(confint_replications <- grid_params %>%
    mutate(out_confint = pmap(.l = ., .f = get_ind_confint)))

# Check the output
head(confint_replications)

# Get combined confidence intervals ---------------------------------------
all_confint <- confint_replications %>%
    unnest(out_confint) %>%
    filter(stat_type == "conf.low" | stat_type == "conf.high") %>%
    pivot_wider(names_from = stat_type, values_from = stat_val)

# Population parameters, run on large n e.g. n = 1e6
df_mod <- gen_ind_mod_fit(1e6)
df_mod

# Get the broom output
df_mod_summ <- tidy(df_mod) %>% select(term, estimate) %>% rename(true_param = estimate)

all_confint_coverage <- all_confint %>%
    left_join(df_mod_summ, by = "term") %>%
    mutate(coverage_ind = ifelse(conf.low <= true_param &
                                   conf.high >= true_param, 1, 0)) %>%
    group_by(term, B, n, var_type_abb) %>%
    summarize(
        coverage = mean(coverage_ind),
        avg_width = mean(conf.high - conf.low)
    ) %>%
    filter(term == 'x1')

# Coverage seems to be working here i.e. most of the values are in the 86-100% range
summary(all_confint_coverage$coverage)
hist(all_confint_coverage$coverage, breaks = 10, xlim = c(0, 1))

# TODO: Check if we should be plotting avg_width?
# Should this be coverage instead?
all_confint_plt <- all_confint_coverage %>%
    mutate(var_type_abb = as.factor(var_type_abb),
           std.error.coverage = sqrt(coverage*(1-coverage)/NUM_COVG_REPS),
           n_text = as.factor(glue("n = {n}"))) %>%
    filter(var_type_abb != 'sand') %>%
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
plotly::ggplotly(p = all_confint_plt)

all_avgwidth_plt <- all_confint_coverage %>%
    filter(!var_type_abb %in% c('sand', 'lm')) %>%
  all_confint_coverage %>%
  mutate(var_type_abb = as.factor(var_type_abb),
         std.error.avgwidth = sqrt(coverage*(1-coverage)/NUM_COVG_REPS),
         n_text = as.factor(glue("n = {n}"))) %>%
  filter(var_type_abb != 'sand') %>%
  ggplot(aes(x = B, y = coverage, fill = var_type_abb, col = var_type_abb)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = coverage-1.96*std.error.coverage,
                  ymax = coverage+1.96*std.error.coverage), alpha = 0.3) +
  facet_grid(~ n_text) +
  labs(y = "Coverage") +
  theme_bw() +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
    geom_hline(yintercept =  (all_confint_coverage %>% filter(var_type_abb == 'sand') %>% pull(avg_width))[1], linetype = 'dashed')
all_avgwidth_plt
# Save the plot
# Note: It may be good to pre-run this script and read it in our paper, rather
#       than knit the pdf with this time consuming run
ggsave(filename = here::here("R/scripts_and_filters/experiments/experiment-vignette3.1_width.png"), plot = all_avgwidth_plt)
# Plot interactively using plotly
plotly::ggplotly(p = all_confint_plt)


