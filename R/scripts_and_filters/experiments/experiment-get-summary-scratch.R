library(tidyverse)
cool_var1 <- 16
cool_var2 <- 26
cool_var3 <- 36
cool_var4 <- 46
cool_var5 <- 56
cool_var6 <- 66

my_cool_var_names <- c("cool_var1", "cool_var2", "cool_var3",
                       "cool_var4", "cool_var5", "cool_var6")
purrr::map(.x = my_cool_var_names, ~get(x = .x))

my_cool_func <- function(cool_var1, cool_var2, cool_var3,
                         cool_var4, cool_var5, cool_var6){
    cool_var_names <- c("cool_var1", "cool_var2", "cool_var3",
                        "cool_var4", "cool_var5", "cool_var6")
    out <- purrr::map(.x = cool_var_names, ~get(x = .x))
    names(out) <- cool_var_names
    base::return(out)
}

out_cool_func <- my_cool_func(cool_var1 = 18, cool_var2 = 28, cool_var3 = 38,
                              cool_var4 = 48, cool_var5 = 58, cool_var6 = NULL)
out_cool_func

out_cool_func %>% purrr::compact(.x = .)
out_cool_func %>% purrr::compact(.x = .) %>% names(x = .)
out_cool_func %>% names(x = .)

out_cool_func2 <- my_cool_func(cool_var1 = TRUE, cool_var2 = FALSE, cool_var3 = FALSE,
                               cool_var4 = FALSE, cool_var5 = TRUE, cool_var6 = NULL)
out_cool_func2
out_cool_func2 %>% purrr::map_lgl(.x = ., ~is.logical(.x)) %>% all()


out_cool_func3 <- my_cool_func(cool_var1 = TRUE, cool_var2 = FALSE, cool_var3 = FALSE,
                               cool_var4 = FALSE, cool_var5 = TRUE, cool_var6 = TRUE)
out_cool_func3
out_cool_func3 %>% purrr::map_lgl(.x = ., ~is.logical(.x)) %>% all()


# out_cool_func %>% purrr::pluck("cool_var1")
# out_cool_func %>% purrr::pluck()


# Comp var examples ----
devtools::load_all()
set.seed(1243434)

# generate data
n <- 1e3
X_1 <- stats::rnorm(n, 0, 1)
X_2 <- stats::rnorm(n, 10, 20)
eps <- stats::rnorm(n, 0, 1)

# OLS data and model
y <- 2 + X_1 * 1 + X_2 * 5 + eps
lm_fit <- stats::lm(y ~ X_1 + X_2)

# DEFINE common column names - these stay the same across all
# reported error types
COMMON_ERR_STAT_COLNAMES <- c("term", "estimate")

# Empirical Bootstrap check
set.seed(454354534)
comp_var1 <- comp_var(
    mod_fit = lm_fit,
    boot_emp = list(B = 100, m = 200),
    boot_res = list(B = 100),
    boot_mul = list(B = 10, weights_type = 'rademacher')
)
comp_var1
names(comp_var1)

# Get the standard error data for a single variance type ----
# Standard Error type name from comp_var
se_type_name <- "var_well_specified"

# Extract the standard error data for the specified error type
se_dat <- purrr::pluck(.x = comp_var1, se_type_name)

# Check that this is a list
class(se_dat)

# Get the abbreviated name
se_type_name_abb <- purrr::pluck(.x = se_dat, "var_type_abb")
se_type_name_abb

se_dat_var_summ <- se_dat %>% purrr::pluck("var_summary")

# Get all names except for term, estimate
err_comm_stat_names <- se_dat_var_summ %>%
    base::colnames(x = .)

# Get the column names that should have the var type suffix added
err_stat_names_mod <- setdiff(err_comm_stat_names,
                              COMMON_ERR_STAT_COLNAMES)

# Create the modified column names with the var type suffix added
err_stat_names_mod <- err_stat_names_mod %>%
    stringr::str_c(se_type_name_abb, sep = ".")
err_fin_stat_names <- c(COMMON_ERR_STAT_COLNAMES,
                        err_stat_names_mod)
err_fin_stat_names

# Apply the modified column names with the var type suffix added on
# the standard error dataset
se_dat_var_summ <- se_dat_var_summ %>%
    dplyr::rename_with(~ err_fin_stat_names, dplyr::everything())

# Modify the original var summary object for the specified var type
purrr::pluck(comp_var1,
             se_type_name, "var_summary") <- se_dat_var_summ


# Check that we did indeed permanently set the value for the original
# data
purrr::pluck(comp_var1, se_type_name, "var_summary")
comp_var1

# We can just return the se_dat_var_summ here
# Rename the elements to be the same names as the original list

# Question for Riccardo - why are these sandwich matrices so different? Are we outputting the incorrect covariance matrix?
comp_var1[[1]]
sandwich::sandwich(x = lm_fit)
# sandwich::vcovHC(x = lm_fit)
class(lm_fit)

all(c("maars_lm", "lm") == c("maars_lm", "lm"))
