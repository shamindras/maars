# Setup Libraries ---------------------------------------------------------
library(tidyverse)
devtools::document()
devtools::load_all()

# Prototype individual comp_var function ----------------------------------
proto_get_mms_comp_var_ind <- function(var_type_abb,
                                       summary_tbl = NULL,
                                       cov_mat = NULL,
                                       B = NULL,
                                       m = NULL,
                                       n = NULL,
                                       weights_type = NULL,
                                       boot_out = NULL) {

  # Get the title only for the specific variance type
  var_title <- dplyr::case_when(
    var_type_abb == "lm" ~ "Well Specified Model",
    var_type_abb == "sand" ~ "Sandwich",
    var_type_abb == "emp" ~ "Empirical Bootstrap",
    var_type_abb == "sub" ~ "Subsampling",
    var_type_abb == "res" ~ "Residual Bootstrap",
    var_type_abb == "mul" ~ "Multiplier Bootstrap"
  ) %>%
    glue::as_glue(x = .)

  # Get the title only for the specific variance type
  var_type <- dplyr::case_when(
    var_type_abb == "lm" ~ "well_specified",
    var_type_abb == "sand" ~ "sand",
    var_type_abb == "emp" ~ "boot_emp",
    var_type_abb == "sub" ~ "boot_sub",
    var_type_abb == "res" ~ "boot_res",
    var_type_abb == "mul" ~ "boot_mul"
  ) %>%
    glue::as_glue(x = .)

  # Get the emoji only for the specific variance type
  var_emoji <- dplyr::case_when(
    var_type_abb == "lm" ~ "\U1F4C9\U1F4C8",
    var_type_abb == "sand" ~ "\U1F969\U1F35E", # \U1F96A
    var_type_abb == "emp" ~ "\U1F9EE\U1F45F",
    var_type_abb == "sub" ~ "\U1F9EE\U1F45F",
    var_type_abb == "res" ~ "\U2696\U1F45F",
    var_type_abb == "mul" ~ "\U274C\U1F45F" # \U2716
  ) %>%
    glue::as_glue(x = .)

  # Get the combined emoji: title for the specific variance type
  var_emoji_title <- glue::glue("{var_emoji}: {var_title}:")

  # Get the variance type abbreviation as a glue string
  var_type_abb <- glue::glue("{var_type_abb}")

  # Get the assumptions vector only for the specific variance type
  var_assumptions <- switch(var_type_abb,
    "lm" = {
      c(
        glue::glue("Observations are assumed to be independent"),
        glue::glue("Residuals are assumed to be homoscedastic"),
        glue::glue("Linearity of the conditional expectation is assumed")
      )
    },
    "sand" = {
      c(
        glue::glue("Observations are assumed to be independent")
      )
    },
    "emp" = {
      c(
        glue::glue("Observations are assumed to be independent",
                   .sep = " "
        ),
        glue::glue("Parameters: B = {B}, m = {m}, n = {n}")
      )
    },
    "sub" = {
      c(
        glue::glue("Observations are assumed to be independent",
                   .sep = " "
        ),
        glue::glue("Parameters: B = {B}, m = {m}, n = {n}")
      )
    },
    "res" = {
      c(
        glue::glue("Observations are assumed to be independent"),
        glue::glue("Residuals are assumed to be homoscedastic"),
        glue::glue("Linearity of the conditional expectation is assumed")
      )
    },
    "mul" = {
      c(
        glue::glue("Observations are assumed to be independent",
                   .sep = " "
        ),
        glue::glue("Parameters: B = {B}, weights = {weights_type}")
      )
    }
  )

  # Construct comp_var list only for the specific variance type
  out_list <- list(
    "var_type" = var_type,
    "var_type_abb" = var_type_abb,
    # "var_emoji" = var_emoji,
    # "var_title" = var_title,
    # "var_emoji_title" = var_emoji_title,
    "var_summary" = summary_tbl,
    "var_assumptions" = var_assumptions,
    "cov_mat" = cov_mat,
    boot_out = boot_out
  )

  return(out_list)
}

# Run basic checks --------------------------------------------------------
# Variables as they would be passed in from individual functions
B <- 150
m <- 60
n <- 1000
cov_mat <- diag(3)

# Run the get_assumptions
proto_get_mms_comp_var_ind(
  var_type_abb = "emp",
  summary_tbl = NULL, # TODO: Should be passed in, no NULLS
  cov_mat = cov_mat,
  B = B,
  m = m,
  n = 1000,
  weights_type = NULL,
  boot_out = NULL
)

# Check internal function applied correctly -------------------------------
# A demo on simple linear regression!
# Comp var examples ----
set.seed(1243434)

# generate modeling data ----
n <- 1e3
x <- stats::rnorm(n, 0, 1)
y <- 2 + x * 1 + 1 * x^{2} + exp(0.2 * abs(x)) * rnorm(n)
lm_fit <- stats::lm(y ~ x)

# Let's get another set of estimates, now also asking for residual and multiplier
# bootstraps.
set.seed(454354534)
mms_fit2 <- comp_var(
  mod_fit = lm_fit,
  boot_emp = list(B = 50, m = 200),
  boot_sub = list(B = 100, m = 100),
  boot_res = list(B = 70),
  boot_mul = list(B = 60))
mms_fit2

# Multiplier/Residual should contain a non-NULL boot_out
mms_fit2$var$var_boot_mul
mms_fit2$var$var_boot_res

# Empirical/Subsampling should contain a NULL boot_out
mms_fit2$var$var_boot_emp
mms_fit2$var$var_boot_sub

# Check that get assumptions is working correctly
mms_fit2 %>% get_assumptions(mod_fit = .)
