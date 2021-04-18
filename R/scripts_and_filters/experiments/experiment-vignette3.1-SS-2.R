# Setup Libraries ---------------------------------------------------------
library(tidyverse)
library(progress)
library(glue)

devtools::document()
devtools::load_all()

# Global variables --------------------------------------------------------
set.seed(35542)
NUM_COVG_REPS <- 200 # Number of coverage replications
NUM_OBS <- 1000

# Create grid sequences ---------------------------------------------------
# For empirical bootstrap parameters i.e. B*m
# Create individual sequences of grid values to cross join
# TODO: Need to add replace = {TRUE/FALSE} values once we add subsampling
out_crossing <- purrr::cross2(
  .x = seq(from = 20, to = 100, by = 20),
  .y = seq(from = 250, to = 1000, by = 250),
  .filter = `==`
) %>%
  purrr::map(purrr::set_names, c("B", "m"))

# Check output for one of the lists we have constructed
out_crossing[[10]]
out_crossing[[10]]$B
out_crossing[[10]]$m

#' Given fitted linear model i.e. `mod_fit`
#' For an individual coverage replication index i.e. `covg_rep_idx`
#' For an individual [B, m] empirical bootstrap combination i.e. `boot_emp`
#' Get the empirical bootstrap confidence interval for the fitted parameters
fit_ind_repl_ind_boot_emp <- function(covg_rep_idx, mod_fit, boot_emp) {
  confint_fit <- comp_var(mod_fit = mod_fit, boot_emp = boot_emp) %>%
    get_confint(mod_fit = ., level = 0.95, boot_emp = TRUE) %>%
    dplyr::mutate(
      covg_rep_idx = covg_rep_idx,
      B = boot_emp[["B"]],
      m = boot_emp[["m"]]
    ) %>%
    dplyr::select(covg_rep_idx, B, m, dplyr::everything())
  return(confint_fit)
}

fit_ind_repl_all_boot_emp <- function(covg_rep_idx, out_crossing) {
  # TODO: Remove the print message later
  print(glue::glue("Running coverage replication index: {covg_rep_idx}...\n"))

  # For each coverage replication generate the data once
  # For all B*m combinations
  n <- NUM_OBS
  X <- stats::rnorm(n, 0, 1)
  y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
  lm_fit <- stats::lm(y ~ X)

  # Using the single data generated for each coverage replication
  # Get the empirical bootstrap confidence interval for each individual B*m
  # combination, and combine all such combination output into a single
  # tibble
  out <- out_crossing %>%
    purrr::map_dfr(
      .x = .,
      .f = ~ fit_ind_repl_ind_boot_emp(
        covg_rep_idx = covg_rep_idx,
        mod_fit = lm_fit,
        boot_emp = .x
      )
    )
  return(out)
}

fit_all_repl_all_boot_emp <- function(num_reps, out_crossing) {
  out <- 1:num_reps %>%
    purrr::map_dfr(
      .x = .,
      .f = ~ fit_ind_repl_all_boot_emp(
        covg_rep_idx = .x,
        out_crossing = out_crossing
      )
    )
  return(out)
}

# Compute confidence intervals for N replications -------------------------
system.time(confint_replications <- fit_all_repl_all_boot_emp(
  num_reps = NUM_COVG_REPS,
  out_crossing = out_crossing
) %>%
  dplyr::arrange(B, m, covg_rep_idx) %>%
  dplyr::mutate(
    term = forcats::as_factor(term),
    B = forcats::as_factor(B),
    m = forcats::as_factor(m)
  ))

# Check the output
head(confint_replications)

# Get combined confidence intervals ---------------------------------------
all_confint <- confint_replications %>%
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
  mutate(title = as.factor(glue::glue("m = {m}"))) %>%
  ggplot(aes(B, coverage)) +
  geom_col() +
  facet_grid(term ~ title) +
  labs(y = "Coverage") +
  geom_hline(yintercept = 0.95, linetype = "dashed")
all_confint_plt
# Save the plot
# Note: It may be good to pre-run this script and read it in our paper, rather
#       than knit the pdf with this time consuming run
ggsave(filename = here::here("R/scripts_and_filters/experiments/experiment-vignette3.1-SS-2.png"), plot = all_confint_plt)
# Plot interactively using plotly
# plotly::ggplotly(p = all_confint_plt)

all_confint_coverage %>%
  dplyr::filter(B == 100, m == 250)
