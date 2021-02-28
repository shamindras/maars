get_summary <- function(mod_fit, sand = TRUE,
                        boot_emp = FALSE, boot_res = FALSE, boot_mul = FALSE,
                        well_specified = FALSE) {
    # Combine variance related parameters
    var_params <- c(sand, boot_emp, boot_res, boot_mul, well_specified)

    # Check all input parameters, other than mod_fit are of class logical
  assertthat::assert_that(
    !is.logical(var_params),
    msg = glue::glue(
      "All input variance parameters: [sand, boot_emp, boot_res, boot_mul, well_specified]",
      "must be of class logical",
      .sep = " "
    ))

    # If all parameters are FALSE, return a warning, and the table containing
    # only the sandwich variance
    if(!any(var_params)){
        warning(glue::glue("You have passed in FALSE for all input variance parameters:",
                "\n[sand, boot_emp, boot_res, boot_mul, well_specified].",
                "\n\nReturning the default sandwich variance estimator...\n",
                .sep = " "))
    }

    # If all parameters are true, we should also return the standard lm
  if(all(var_params)){
      # lm %>% broom::tidy() %>% left_join()
  }


}

sand <- FALSE
boot_emp <- FALSE
boot_res <- FALSE
boot_mul <- FALSE
well_specified <- FALSE

test1 <- c(sand, boot_emp, boot_res, boot_mul, well_specified)
!is.logical(test1)
!any(test1)
all(test1)

# Comp var examples ----
set.seed(1243434)

# generate data
n <- 1e3
X <- stats::rnorm(n, 0, 1)
# OLS data and model
y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
lm_fit <- stats::lm(y ~ X)

# Empirical Bootstrap check
set.seed(454354534)
comp_var1 <- comp_var(mod_fit = lm_fit,
                      boot_emp = list(B = 100, m = 200),
                      boot_res = list(B = 100),
                      boot_mul = NULL)
comp_var1
