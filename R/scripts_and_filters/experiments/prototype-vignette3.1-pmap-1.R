# Setup Libraries ---------------------------------------------------------
library(tidyverse)
library(progress)
library(glue)

devtools::document()
devtools::load_all()

# Global variables --------------------------------------------------------
NUM_COVG_REPS <- 10 # Number of coverage replications
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
replication_mod_fits <- purrr::map(.x = 1:NUM_COVG_REPS,
                                   .f = ~gen_ind_mod_fit(n = N))

# Create grid sequences ---------------------------------------------------
# B parameter grid for bootstrap (empirical/multiplier/subsampling)
grid_B <- seq(from = 20, to = 100, by = 20)

# Empirical bootstrap parameters grid i.e. n:n bootstrap, with replacement
# TODO: Later switch to having `replace = TRUE` explicitly
grid_boot_emp_n_n <- purrr::map(.x = grid_B, .f = ~list(B = .x, m = N))
# grid_boot_emp_n_n <- purrr::map(.x = grid_B, .f = ~list(B = .x, m = N, replace = TRUE))

# Empirical bootstrap parameters grid i.e. sqrt(n):n bootstrap, subsampling (without replacement)
grid_boot_emp_sqrtn_n <- purrr::map(.x = grid_B, .f = ~list(B = .x, m = N))

# Multiplier bootstrap parameters grid
grid_boot_mul <- map(.x = grid_B, .f = ~list(B = .x, weights_type = "rademacher"))

# grid_boot_res <- list(rep(x = NA, times = length(grid_boot_mul)))
# grid_boot_res <- seq_along(along.with = grid_boot_mul) %>%
#     purrr::map(.x = ., .f = ~ NULL)
grid_boot_res <- purrr::map(.x = seq_along(grid_boot_mul), .f = ~ NULL)

out_crossing <- tibble::tibble(B = grid_B,
                               m = N,
                               boot_emp = grid_boot_emp_n_n,
                               boot_sub = grid_boot_emp_sqrtn_n,
                               boot_mul = grid_boot_mul,
                               boot_res = grid_boot_res)
out_crossing
out_crossing$boot_res

# Let's try running pmap inside the mutate statement for a single replication
# This pmap does not seem to run the required boot_emp, boot_sub, boot_mul, boot_res correctly
lm_fit <- gen_ind_mod_fit(n = N)
test_pmap1 <- out_crossing %>%
    dplyr::mutate(out_fit = purrr::pmap(.l = .,
                                        .f = ~comp_var(mod_fit = lm_fit)))
print(test_pmap1$out_fit[[1]], boot_emp = TRUE)
test_pmap1$out_fit[[1]]$var

# Let's try running pmap outside the mutate statement for a single replication
# Let's try running it outside the mutate statement
test_pmap2 <- purrr::pmap(.l = out_crossing, .f = ~comp_var(mod_fit = lm_fit))
test_pmap2[[1]]$var

# Let's try doing a cross join on all models with the data
length(replication_mod_fits)

out_all <- tidyr::crossing(replication_mod_fits, out_crossing) %>%
    dplyr::rename(mod_fit = replication_mod_fits) %>%
    tibble::rownames_to_column(var = "covg_rep_idx") %>%
    dplyr::select(covg_rep_idx, B, m, dplyr::everything())
colnames(out_all) %>% cat(sep = ", ")


fin_out <- out_all %>%
    dplyr::select(-covg_rep_idx, -B, -m) %>%
    dplyr::mutate(out_fit = purrr::pmap(.l = .,
                                        .f = ~comp_var(mod_fit = lm_fit)))

get_ind_confint <- function(covg_rep_idx, B, m, mod_fit, boot_emp, boot_sub, boot_mul, boot_res){
    # TODO: Remove the print message later
    print(glue::glue("Running coverage replication index: {covg_rep_idx}...\n"))
    print(glue::glue("B: {B}...\n"))
    print(glue::glue("m: {m}...\n"))
    confint_fit <- comp_var(mod_fit = mod_fit,
                            boot_emp = boot_emp,
                            boot_sub = boot_sub,
                            boot_mul = boot_mul,
                            boot_res = boot_res) %>%
        get_confint(mod_fit = .,
                    level = 0.95,
                    boot_emp = TRUE,
                    boot_sub = TRUE,
                    boot_mul = TRUE,
                    boot_res = FALSE) %>%
        dplyr::mutate(
            covg_rep_idx = covg_rep_idx,
            B = B,
            m = m
        ) %>%
        dplyr::select(covg_rep_idx, B, m, dplyr::everything())
    return(confint_fit)
}

confint_replications <- out_all %>%
    dplyr::mutate(confint_fit = purrr::pmap(.l = .,
                                            .f = get_ind_confint))

# Check the output
head(confint_replications)
# confint_replications$confint_fit[[1]] %>% View()

# Get combined confidence intervals ---------------------------------------
all_confint <- confint_replications %>%
    dplyr::pull(confint_fit) %>%
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
# all_confint_plt <- all_confint_coverage %>%
#     mutate(title = as.factor(glue::glue("m = {m}"))) %>%
#     ggplot(aes(B, coverage)) +
#     geom_col() +
#     facet_grid(term ~ title) +
#     labs(y = "Coverage") +
#     geom_hline(yintercept = 0.95, linetype = "dashed")
# all_confint_plt
# Save the plot
# Note: It may be good to pre-run this script and read it in our paper, rather
#       than knit the pdf with this time consuming run
# ggsave(filename = here::here("R/scripts_and_filters/experiments/experiment-vignette3.1-SS-2.png"), plot = all_confint_plt)
# Plot interactively using plotly
# plotly::ggplotly(p = all_confint_plt)
