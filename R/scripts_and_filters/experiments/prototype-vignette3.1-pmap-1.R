# Setup Libraries ---------------------------------------------------------
library(tidyverse)
library(progress)
library(glue)

devtools::document()
devtools::load_all()

# Global variables --------------------------------------------------------
NUM_COVG_REPS <- 300 # Number of coverage replications
N <- 1000

# Generate replication data + model fit -----------------------------------
# Helper function to generate a single replication dataset and `lm` fit
gen_ind_mod_fit <- function(n){
    X <- stats::rnorm(n, 0, 1)
    y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
    lm_fit <- stats::lm(y ~ X)
    return(lm_fit)
}

# Generate all fitted models on our replication datasets
# Note: This could be generated on the fly as well, but here we will reuse this
#       multiple times, so more efficient to generate this up front
set.seed(35542)
replication_mod_fits <- map(1:NUM_COVG_REPS, .f = ~gen_ind_mod_fit(n = N))

# Create grid sequences -----------------------------------------------
# Generate the grid of all the variance calculation parameters
grid_params <-
  tibble(
    B = seq(from = 20, to = 100, by = 20), m = N,
    boot_emp = map(B, ~ list(B = .x, m = N)),
    boot_sub = map(B, ~ list(B = .x, m = floor(sqrt(N)))),
    boot_mul = map(B, ~ list(B = .x, weights_type = "rademacher")),
    boot_res = purrr::map(seq_along(boot_mul), ~NULL)
  )

# Now cross join on all of the replications to this dataset
# This intentionally contains some duplication redundancy, to make downstream
# code a single application of pmap!
out_all <- tidyr::crossing(replication_mod_fits, grid_params) %>%
    rename(mod_fit = replication_mod_fits) %>%
    rownames_to_column(var = "covg_rep_idx") %>%
    select(covg_rep_idx, B, m, everything())

# Quickly examine our table
head(out_all)
out_all$boot_sub[[1]]

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

# Run the confidence interval replications
system.time(confint_replications <- out_all %>%
    mutate(confint_fit = purrr::pmap(.l = ., .f = get_ind_confint)))

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

all_confint_coverage <- all_confint %>%
    inner_join(tibble(
        term = c("(Intercept)", "X"),
        value_par = c(2, 1)
    ),
    by = "term"
    ) %>%
    mutate(covers_c = ifelse(conf.low <= value_par & conf.high >= value_par, 1, 0)) %>%
    group_by(term, B, m, var_type_abb) %>%
    summarize(
        coverage = mean(covers_c),
        avg_width = mean(conf.high - conf.low)
    )

# Coverage seems to be working here i.e. most of the values are in the 86-100% range
summary(all_confint_coverage$coverage)
hist(all_confint_coverage$coverage, breaks = 10, xlim = c(0, 1))

# TODO: Check if we should be plotting avg_width?
# Should this be coverage instead?
all_confint_plt <- all_confint_coverage %>%
    # filter(var_type_abb == "emp") %>%
    mutate(var_type_abb = as.factor(var_type_abb),
           B = as.factor(B)) %>%
    ggplot(aes(B, coverage)) +
    geom_col() +
    facet_grid(term ~ var_type_abb) +
    labs(title = glue::glue("m = {N}"),
         y = "Coverage") +
    geom_hline(yintercept = 0.95, linetype = "dashed")
all_confint_plt
# Save the plot
# Note: It may be good to pre-run this script and read it in our paper, rather
#       than knit the pdf with this time consuming run
# ggsave(filename = here::here("R/scripts_and_filters/experiments/experiment-vignette3.1-SS-2.png"), plot = all_confint_plt)
# Plot interactively using plotly
plotly::ggplotly(p = all_confint_plt)
