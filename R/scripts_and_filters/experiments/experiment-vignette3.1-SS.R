# Setup Libraries ---------------------------------------------------------
library(tidyverse)
library(progress)

devtools::document()
devtools::load_all()

# Global variables --------------------------------------------------------
set.seed(35542)
NUM_COVG_REPS <- 10 # Number of coverage replications

# Setup progress bar ------------------------------------------------------
pb <- progress_bar$new(
  format = "running replications [:bar] :elapsedfull",
  total = NUM_COVG_REPS, clear = FALSE, width = 60
)

# Run lm fitted model -----------------------------------------------------
# Fit the linear model using OLS (ordinary least squares)
fit_confint_boot_emp <- function(covg_rep_idx, boot_emp) {
  n <- 500
  X <- stats::rnorm(n, 0, 1)
  y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
  lm_fit <- stats::lm(y ~ X)
  confint_fit <- comp_var(mod_fit = lm_fit, boot_emp = boot_emp) %>%
    get_confint(mod_fit = ., level = 0.95, boot_emp = TRUE) %>%
    dplyr::mutate(
      covg_rep_idx = covg_rep_idx,
      B = boot_emp[["B"]],
      m = boot_emp[["m"]]
    ) %>%
    dplyr::select(covg_rep_idx, B, m, dplyr::everything())
  return(confint_fit)
}

# Wrapper function to
wrap_fit_confint_boot_emp <- function(num_reps, boot_emp) {
  pb$tick()
  out <- 1:num_reps %>%
    purrr::imap_dfr(.x = ., .f = ~ fit_confint_boot_emp(
      covg_rep_idx = .y,
      boot_emp = boot_emp
    ))
  return(out)
}

# Test a single fit
fit_confint_boot_emp(covg_rep_idx = 1, boot_emp = list(B = 20, m = 200))

# Test replications of a single fit
wrap_fit_confint_boot_emp(num_reps = 5, boot_emp = list(B = 20, m = 200))

# Create grid sequences ---------------------------------------------------
# For empirical bootstrap i.e. B, m,
# Create individual sequences of grid values to cross join
# TODO: Need to add replace = {TRUE/FALSE} values once we add subsampling

# TODO: This version is using rowwise, we can simplify this with map2 below
# out_crossing <- tidyr::crossing(B = seq(from = 5, to = 35, by = 10),
#                                 m = seq(from = 40, to = 120, by = 40)) %>%
#   dplyr::rowwise(data = .) %>%
#   dplyr::mutate(boot_emp = list(tibble::lst("B" = B,
#                                             "m" = switch(!is.na(m), m, NULL))))

out_crossing <- tidyr::crossing(
  B = seq(from = 5, to = 35, by = 10),
  m = seq(from = 40, to = 120, by = 40)
) %>%
  dplyr::mutate(boot_emp = purrr::map2(
    .x = B, .y = m,
    .f = ~ list(B = .x, m = .y)
  ))

# Check output for one of the lists we have constructed
out_crossing[8, 3]$boot_emp

# Compute maars_lm objects for B, m ---------------------------------------
# TODO: This is not required, we can remove this later
#       Interesting to just get used to mutate syntax
# out_crossing_norpl_comp <-
#   out_crossing %>%
#   dplyr::mutate(.data = .,
#                 cmp_var = purrr::imap(
#                   .x = boot_emp,
#                   .f = ~ fit_confint_boot_emp(covg_rep_idx = .y, boot_emp = .x)
#                 ))

# Compute confidence intervals for N replications -------------------------
confint_replications <-
  out_crossing %>%
  dplyr::mutate(
    .data = .,
    cmp_var = purrr::imap(
      .x = boot_emp,
      .f = ~ wrap_fit_confint_boot_emp(
        num_reps = NUM_COVG_REPS,
        boot_emp = .x
      )
    )
  )

# Check the output
head(confint_replications)

# Get combined confidence intervals ---------------------------------------
confint_replications %>%
  dplyr::pull(cmp_var) %>%
  purrr::map_dfr(.x = ., .f = ~.x)
  # Add plotting code here

# TODO: Discuss
# 1. Can we filter the tibble from `fit_confint_boot_emp` to only keep the
#    output for confidence interval coverage
# 2. Can we mutate the tibble from `fit_confint_boot_emp` to perform the
#    relevant confidence interval calculations e.g. length, or should we
#    do this in confint_replications?
# 3. The code is really slow as the the NUM_COVG_REPS increases
#    (this is expected). Either we reduce n, or the size of our B * m grid nrows
#    or the NUM_COVG_REPS? We should find a reasonable combination that
#    allows the key ideas to be illustrated and for the code to be more
#    efficiently run
