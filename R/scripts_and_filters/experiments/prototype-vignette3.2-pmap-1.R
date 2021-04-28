# Setup Libraries ---------------------------------------------------------
library(tidyverse)
#library(progress)
library(glue)
library(furrr)

devtools::document()
devtools::load_all()

# Global variables --------------------------------------------------------
NUM_COVG_REPS <- 500 # Number of coverage replications
N <- 200

# Generate replication data + model fit -----------------------------------
# Helper function to generate a single replication dataset and `lm` fit
gen_ind_mod_fit <- function(n){
    # X <- stats::rnorm(n, 0, 1)
    # y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
    beta0 <- 1
    beta1 <- 2
    gamma <- 0.1
    x <- runif(n, 0, 10)
    y <- beta0 + beta1*x + gamma*x^{1.7} + exp(gamma*x)*rnorm(n)
    lm_fit <- stats::lm(y ~ x)
    return(lm_fit)
}

# Generate all fitted models on our replication datasets
# Note: This could be generated on the fly as well, but here we will reuse this
#       multiple times, so more efficient to generate this up front
set.seed(35542)
replication_mod_fits <- map(1:NUM_COVG_REPS, ~gen_ind_mod_fit(n = N))
length(replication_mod_fits)
replication_mod_fits[[1]]

# Create grid sequences -----------------------------------------------
# Generate the grid of all the variance calculation parameters
# This time run it without the filtering
# grid_B <- seq(50, 100, by = 50)
# grid_m <- seq(from = 40, to = 120, by = 40)
grid_B <- seq(100, 500, by = 100)
grid_m <- c(floor(N**(1/3)), 50, 100, 200, 500, 1e3, 1e4, 4e4) # cleaner version of grid_m
#grid_m <- c(floor(seq(N**(1/3), to = N, length = 3)), (N^2)/4, (N^2)/2, N^2)
length(grid_B) * length(grid_m)

grid_params <- tidyr::crossing(
    # B = seq(50, 100, by = 50),
    # m = seq(from = 40, to = 120, by = 40)
    B = grid_B,
    m = grid_m
) %>%
    dplyr::mutate(
        boot_emp = map2(B, m, ~ list(B = .x, m = .y)),
        boot_sub = purrr::map(seq_along(boot_emp), ~NULL),
        boot_mul = purrr::map(seq_along(boot_mul), ~NULL),
        boot_res = purrr::map(seq_along(boot_res), ~NULL))

grid_params$boot_emp[[1]]
grid_params$boot_emp[[2]]
grid_params$boot_mul[[1]]
grid_params$boot_sub[[1]]

# Now cross join on all of the replications to this dataset
# This intentionally contains some duplication redundancy, to make downstream
# code a single application of pmap!
out_all <- tidyr::crossing(replication_mod_fits, grid_params) %>%
    rename(mod_fit = replication_mod_fits) %>%
    rownames_to_column(var = "covg_rep_idx") %>%
    select(covg_rep_idx, B, m, everything())
nrow(out_all)
# Quickly examine our table
head(out_all)
out_all$boot_sub[[1]]
lobstr::obj_size(out_all)*1e-6

# We can get the confint for a single replication
get_ind_confint <- function(covg_rep_idx, B, m, mod_fit, boot_emp, boot_sub, boot_mul, boot_res){
    # TODO: Remove the print message later
    print(glue::glue("Running coverage replication index: {covg_rep_idx}...\n"))
    print(glue::glue("B: {B}...\n"))
    print(glue::glue("m: {m}...\n"))
    # TODO: Perhaps we don't need a boot_res column i.e. can just set it
    #       manually to NULL below
    confint_fit <- comp_var(mod_fit = mod_fit, boot_emp = boot_emp,
                            boot_sub = boot_sub, boot_mul = boot_mul,
                            boot_res = boot_res) %>%
        get_confint(mod_fit = ., level = 0.95,
                    sand = TRUE, boot_emp = TRUE, boot_sub = TRUE,
                    boot_mul = TRUE, boot_res = FALSE) %>%
        mutate(covg_rep_idx = covg_rep_idx, B = B, m = m) %>%
        select(covg_rep_idx, B, m, everything())
    return(confint_fit)
}

# Use furrr to run in parallel
# https://github.com/DavisVaughan/furrr#example
plan(multicore, workers = 50)

# ORIGINAL CODE - using purrr not furrr
# system.time(confint_replications <- out_all %>%
#                 mutate(confint_fit = purrr::pmap(.l = ., .f = get_ind_confint)))

# Run the confidence interval replications
system.time(confint_replications <- out_all %>%
                mutate(confint_fit = furrr::future_pmap(.l = ., .f = get_ind_confint)))

# Check the output
head(confint_replications)
# confint_replications$confint_fit[[1]] %>% View()

# Get combined confidence intervals ---------------------------------------
all_confint <- confint_replications %>%
    pull(confint_fit) %>%
    purrr::map_dfr(.f = ~.x) %>%
    # Add plotting code here
    filter(stat_type == "conf.low" |
               stat_type == "conf.high") %>%
    pivot_wider(names_from = stat_type, values_from = stat_val)

df_mod <- gen_ind_mod_fit(1e8)

all_confint_coverage <- all_confint %>%
    inner_join(tibble(
        term = c("(Intercept)", "x"),
        value_par = c(coef(df_mod)[1], coef(df_mod)[2])
    ),
    by = "term"
    ) %>%
    mutate(covers_c = ifelse(conf.low <= value_par & conf.high >= value_par, 1, 0)) %>%
    group_by(term, B, m, var_type_abb) %>%
    summarize(
        coverage = mean(covers_c),
        avg_width = mean(conf.high - conf.low)
    ) %>%
    filter(var_type_abb != 'lm') %>%
    filter(term != '(Intercept)')

# Coverage seems to be working here i.e. most of the values are in the 86-100% range
summary(all_confint_coverage$coverage)
hist(all_confint_coverage$coverage, breaks = 10, xlim = c(0, 1))

# TODO: Check if we should be plotting avg_width?
# Should this be coverage instead?
all_confint_plt <- all_confint_coverage %>%
    mutate(var_type_abb = as.factor(var_type_abb),
           B = as.factor(B),
           m = as.factor(m))  %>%
    filter(var_type_abb != 'sand') %>%
    ggplot(aes(m, coverage)) +
    geom_col() +
    facet_grid(B~var_type_abb) +
    labs(y = "Coverage") +
    theme_bw() +
    geom_hline(yintercept = (all_confint_coverage %>% filter(var_type_abb == 'sand') %>% pull(coverage))[1], linetype = "dashed")
all_confint_plt
# all_confint_plt <- all_confint_coverage %>%
#     filter(var_type_abb != 'lm') %>%
#     filter(term != '(Intercept)') %>%
#     mutate(var_type_abb = as.factor(var_type_abb),
#            B = as.factor(B)
#            ) %>%
#     ggplot(aes(m, coverage, col = B)) +
#     geom_point() +
#     #geom_path() +
#     ggplot2::scale_x_log10() +
#     ylim(0.75,1) +
#     #facet_grid~ var_type_abb) +
#     labs(y = "Coverage") +
#     geom_hline(yintercept = (all_confint_coverage %>% filter(var_type_abb == 'sand') %>% pull(coverage))[1], linetype = "dashed") +
#     theme_bw()
all_confint_plt
# Save the plot
# Note: It may be good to pre-run this script and read it in our paper, rather
#       than knit the pdf with this time consuming run
ggsave(filename = here::here("R/scripts_and_filters/experiments/experiment-vignette3.1_coverage.png"), plot = all_confint_plt)
# Plot interactively using plotly
plotly::ggplotly(p = all_confint_plt)


all_avgwidth_plt <- all_confint_coverage %>%
    filter(var_type_abb != 'lm') %>%
    filter(term != '(Intercept)') %>%
    filter(var_type_abb != 'sand') %>%
    mutate(var_type_abb = as.factor(var_type_abb),
           B = as.factor(B),
           m = as.factor(m)) %>%
    ggplot(aes(m, avg_width)) +
    geom_col() +
    facet_grid(B ~ var_type_abb) +
    labs(y = "Average width of confidence inverval") +
    theme_bw() +
    geom_hline(yintercept =  (all_confint_coverage %>% filter(var_type_abb == 'sand') %>% pull(avg_width))[1], linetype = 'dashed')
all_avgwidth_plt
# Save the plot
# Note: It may be good to pre-run this script and read it in our paper, rather
#       than knit the pdf with this time consuming run
ggsave(filename = here::here("R/scripts_and_filters/experiments/experiment-vignette3.1_width.png"), plot = all_avgwidth_plt)
# Plot interactively using plotly
plotly::ggplotly(p = all_confint_plt)


