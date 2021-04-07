# Setup Libraries ---------------------------------------------------------

library(tidyverse)
devtools::load_all()

# Run lm fitted model -----------------------------------------------------

set.seed(35542)
n <- 1e2
X <- stats::rnorm(n, 0, 1)
y <- 2 + X * 1 + stats::rnorm(n, 0, 1)

# Fit the linear model using OLS (ordinary least squares)
mod_fit <- stats::lm(y ~ X)

# Create grid sequences ---------------------------------------------------

# For empirical bootstrap i.e. B, m,
# Create individual sequences of grid values to cross join
# B <- seq(from = 50, to = 250, by = 50)
# m <- seq(from = 100, to = 300, by = 100)
B <- seq(from = 5, to = 35, by = 10)
m <- seq(from = 40, to = 120, by = 40)

# Allow NULL values for m. However, vectors can't take NULL values so we
# set add NA values instead. We will coerce these to NULL values at the time
# the list is constructed
m <- c(NA, m)

# Don't call it replace, so function replace function is not overidden
replc <- c(TRUE, FALSE)

# Inspect the individual grid values
B
m
replc

# TODO: Need to construct just NULL value
out_crossing <- tidyr::crossing(B, m, replc) %>%
  # dplyr::arrange(.data = ., B, m) %>%
  dplyr::rename("replace" = replc) %>%
  dplyr::rowwise(data = .) %>%
  dplyr::mutate(boot_emp = list(tibble::lst(
    "B" = B,
    "m" = switch(!is.na(m),
      m,
      NULL
    ),
    # "m" = if(!is.na(m)){
    #     m
    # }else{
    #     NULL
    # },
    "replace" = replace
  )))

# Seems to work, but would be nice to have
head(out_crossing, 3)
# out_crossing %>% dplyr::glimpse()
# out_crossing %>% View()

# Check an individual element is a list with 3 elements
out_crossing[8, 4]$boot_emp

# Get the cartesian product without any replace variables, since we haven't
# yet implemented subsampling
out_crossing_norpl <-
  tidyr::crossing(B, m) %>%
  dplyr::rowwise(data = .) %>%
  dplyr::mutate(boot_emp = list(tibble::lst(
    "B" = B,
    "m" = switch(!is.na(m),
      m,
      NULL
    )
  )))

# Run the map across the empirical bootstrap models. Really we should
# just put this in a mutate statement
boot_emp_crossing <-
  out_crossing_norpl %>%
  dplyr::pull(boot_emp) %>%
  purrr::map(
    .x = .,
    .f = ~ comp_var(mod_fit = mod_fit, boot_emp = .x)
  )
