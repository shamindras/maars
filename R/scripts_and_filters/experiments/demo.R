# A demo on simple linear regression!
# Comp var examples ----
devtools::load_all()
set.seed(1243434)

# generate modeling data ----
n <- 1e3
x <- stats::rnorm(n, 0, 1)
y <- 2 + x * 1 + 1 * x^{2} + exp(0.2 * x) * rnorm(n)

# let's first look at the data: Let's go the "ggplot" way!
# This is what we like to call the "modern" statistician (as we will
# repeatedly argue next)
ggplot2::ggplot(data = tibble::tibble(x = x, y = y),
                ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() +
    ggplot2::theme_bw()

# Fitting OLS ----
# We are highly trained statisticians and we decide to fit an lm model...
# which is obviously misspecified
lm_fit <- stats::lm(y ~ x)

# Inspecting lm ----
# There are several ways in which we can inspect the output of "lm"
# We can either call the print method on the object
print(lm_fit)
# or get more detailed information by calling summary
summary(lm_fit)

# Inspecting lm with the (modern) tidy toolkit  ----
# An analogous way to inspect the lm object, which the statistician
# might prefer, is to leverage the functions provided by the "broom" package!
# This package follows the philosophy of the "tidyverse", a way to interact
# and visualize objects in R that we also follow as a design principle in maars.
# For example, we can obtain a "tidy" version of the coefficients estimates,
# standard errors etc by running
broom::tidy(lm_fit)
# and more information on the lm object via
broom::augment(lm_fit)
broom::glance(lm_fit)

### TALK ABOUT NUMBERS!!!!!!!!!!!

# Fit our first maars_lm object ----
# Here we return an object of class ["maars_lm", "lm"], which contains all the
# attributes contained in lm, plus additional estimates of the variance.
# Currently, comp_var supports the computations of standard errors based on
# (i) sandwich, (ii) m-out-of-n empirical bootstrap, (iii) multiplier bootstrap,
# and (iv) residual bootstrap. Sandwich is always returned by default.
# Let's now get the variance estimates based on empirical bootstrap via
set.seed(454354534)
mms_fit0 <- comp_var(mod_fit = lm_fit)
class(mms_fit0)
print(mms_fit0)
summary(mms_fit0)

set.seed(454354534)
mms_fit1 <- comp_var(
    mod_fit = lm_fit,
    boot_emp = list(B = 100, m = 50))
# Take a look at the class of the object
class(mms_fit1)

# Let's get another set of estimates, now also asking for residual and multiplier
# bootstraps.
set.seed(454354534)
mms_fit2 <- comp_var(
    mod_fit = lm_fit,
    boot_emp = list(B = 50, m = 200),
    boot_res = list(B = 70),
    boot_mul = list(B = 60)
)

# Inspecting the maars_lm object ----
# print - let's test out the print method for both objects ----
print(mms_fit1)
# you can see that we return the same output as print(lm), but we also
# print the assumptions and parameters used in each of the variance
# estimators used in the function. Let's now look at the second maars_lm object
print(mms_fit2)

# summary - let's test out the summary method for both objects ----
# If we call print on summary on a maars_lm object. or equivalently
# run the following line of code, the several estimates of the variance
# are shown, together with the corresponding assumptions
#summary(mms_fit0)
#summary(mms_fit1, boot_emp = TRUE)
summary(mms_fit2, boot_emp = TRUE, boot_mul = TRUE, boo_res = TRUE)
# we also store summary in a list!
summ_out <- summary(mms_fit1)

# Let's try out some interesting cases
# In this case we'll just obtain the summaries for sandwich and lm
summary(mms_fit2)
# and here only sadnwich and empirical bootstrap (but not the others)
summary(mms_fit2, boot_emp = TRUE)

# Let's see whether summary can handle the user's mistakes
# Here we request multiplier bootstrap even though it's not available
summary(mms_fit1, boot_emp = TRUE, boot_mul = TRUE)
# and here we ask for residual bootstrap, which is not available either
summary(mms_fit1, boot_res = TRUE)
# In both cases, these summaries return helpful warnings


# extract assumptions ----
# The output of the assumptions will be revised in the (very) near future
get_assumptions(mod_fit = mms_fit2, boot_emp = TRUE)

# extract summary in tidy format ----
# We can also extract the variance estimates (and other statistics) in a tidy
# format. Here it is!
summ_out <- get_summary(mod_fit = mms_fit2, boot_emp = TRUE)
summ_out

# The output cna be reformatted in a more readable way:
summ_out %>%
    dplyr::filter(stat_type == 'std.error') %>%
    tidyr::pivot_wider(names_from = var_type_abb, values_from = stat_val)

# Compute confidence intervals ----
# Let's now compute confidence intervals. We can call the confint method on
# a maars_lm object
confint(mms_fit2, level = 0.99, boot_emp = TRUE, boot_mul = TRUE)
confint(mms_fit2, level = 0.8, boot_emp = TRUE, boot_mul = TRUE)

# Alternatively we can
mms_fit2_conf <- get_confint(mod_fit = mms_fit2, level = 0.95,
                             sand = TRUE, boot_emp = TRUE)
mms_fit2_conf

# Manually get the wider (non-tidy) version - for vignette Table 1 like output
mms_fit2_conf %>%
    # add
    tidyr::pivot_wider(data = .,
                       names_from = c(stat_type, var_type_abb),
                       names_glue = "{stat_type}.{var_type_abb}",
                       values_from = stat_val)


# Plots for maars_lm object ----

# We may want to inspect our fitted model via graphical tools. For this reason,
# by calling the plot method on a maars_lm object, we return the six
# "typical" lm plots and two additional plots, including (i) the confidence intervals
# based on the different types of standard errors that are computed and (ii) the
# QQ norm of the bootstrap coefficients estimates, when they are available.
# Let's first look at plot()
plot(mms_fit1)

# We can also return the plots in a list by calling
mms_fit1_plots <- get_plot(mms_fit1)
# Let's first look at the six lm plots
patchwork::wrap_plots(1:6 %>%
                         purrr::map(~ purrr::pluck(mms_fit1_plots, .)),
                     ncol = 3)
# and then at the two additional plots
purrr::pluck(mms_fit1_plots, 7)
purrr::pluck(mms_fit1_plots, 8)


# Experimental feature: Model diagnostics ----

# We can run the model diagnostics proposed by
# Models as Approximations II: A Model-Free Theory of Parametric Regression
# (Buja et al, 2019)

# We first need to estimate the estimates under reweighting. We can do with
coef_rwgt <- diag_fit_reg_rwgt(lm_fit,
                  terms_to_rwgt = 'x', # regressors under reweighting
                  B = 100, # number of bootstrap repetitions
                  m = NULL, # for m-out-of-n empirical bootstrap
                  grid_centers = NULL, # reweighting centers for each regressor
                  # to be reweighted (optional)
                  grid_method = "quantiles", # method to construct the grid (only
                  # if grid_centers is NULL, otherwise it is ignored)
                  n_grid = 9) # number of reweighting centers on the grid, if
                  # grid_centers is not NULL

# Let's first obtain the "focal slope" model diagnostic. We show how the
# estimates of one coefficient vary under reweighting of one regressor.
# This graphical tool provides insights into the interactions between the
# regressor specified in term_chosen and all other regressors.
# We can then obtain an estimate of the
diag_foc_slope(mod_fit = lm_fit,
               coef_rwgt = coef_rwgt,
               term_chosen = 'x')

# Let's then get the "nonlinearity detection" model diagnostic. We show how
# each coefficient's estimate varies with the reweighting of its own regressor
# This graphical tool provides insights into the marginal nonlinear behavior of
# response surfaces for each of the regressors. Let's get it via...
diag_nl_detect(mod_fit = lm_fit,
               coef_rwgt = coef_rwgt)
# Obviously it's identical the one above! It's not very interesting here.

# Obtain the "focal reweighting variable" model diagnostic.
# This function shows how the estimates of all coefficients vary under
# reweighting of only one regressor specified in term_chosen
diag_foc_rwgt(mod_fit = lm_fit,
              coef_rwgt = coef_rwgt,
              term_chosen = 'x')

# ... See the documentation for more information on these plots!
?diag_foc_rwgt

