# Comp var examples ----
devtools::load_all()
set.seed(1243434)

# generate data ----
n <- 1e3
X_1 <- stats::rnorm(n, 0, 1)
X_2 <- stats::rnorm(n, 10, 20)
eps <- stats::rnorm(n, 0, 1)

# Let's generate data and fit a well-specified OLS data and model ----
y <- 2 + X_1 * 1 + X_2 * 5 + eps
lm_fit <- stats::lm(y ~ X_1 + X_2)
summary(lm_fit)

# Fit our first maars_lm object i.e. comp_var1 ----
# Here are essentially retaining our OLS fitted estimates but adding on
# revised model-misspecified standard errors for: [emp, mul], we did not
# run [res] bootstrap errors. Sandwich and well specified OLS errors get
# run by default
set.seed(454354534)
comp_var1 <- comp_var(mod_fit = lm_fit,
                      boot_emp = list(B = 50, m = 200),
                      boot_res = NULL,
                      boot_mul = list(B = 60))

# summary
print(comp_var1)
summary(comp_var1)

# SUMMARY: comp_var1 tidy and printed summary examples ----
# This returns everything but boot_mul, since we didn't run it in the original
# original maars_lm model
get_summary(mod_fit = comp_var1, sand = TRUE,
            boot_emp = TRUE, boot_res = FALSE, boot_mul = FALSE,
            well_specified = TRUE)
# CONFINT: comp_var1 lm plot examples ----
get_confint(mod_fit = comp_var1, level = 0.95, sand = TRUE,
            boot_emp = TRUE, boot_res = FALSE, boot_mul = FALSE,
            well_specified = TRUE)

# Let's run the printed summary!
# TODO: This is not currently working! Minor fix mod_fit vs. object
#summary(object = comp_var1)

# This returns an error since we require the tidy summary with boot_res, but
# we have not run it in the original maars_lm model
summary(mod_fit = comp_var1, sand = TRUE,
        boot_emp = TRUE, boot_res = TRUE, boot_mul = TRUE,
        well_specified = TRUE)
get_summary(mod_fit = comp_var1, sand = TRUE,
            boot_emp = TRUE, boot_res = TRUE, boot_mul = TRUE,
            well_specified = TRUE)
get_confint(mod_fit = comp_var1, level = 0.95, sand = TRUE,
            boot_emp = TRUE, boot_res = TRUE, boot_mul = TRUE,
            well_specified = TRUE)

# We are passing in all FALSE values, should return the sandwich estimator
# with a warning message
summary(mod_fit = comp_var1, sand = FALSE,
        boot_emp = FALSE, boot_res = FALSE, boot_mul = FALSE,
        well_specified = FALSE)
get_summary(mod_fit = comp_var1, sand = FALSE,
            boot_emp = FALSE, boot_res = FALSE, boot_mul = FALSE,
            well_specified = FALSE)

# summary(object = comp_var1, sand = FALSE,
#         boot_emp = FALSE, boot_res = FALSE, boot_mul = FALSE,
#         well_specified = FALSE)

# PLOT: comp_var1 lm plot examples ----
# Currently we produce ggplot2 themed diagnostic plots for the OLS lm
# object.

# Let's plot the lm object default plot method, so that we can compare
plot(lm_fit)

# Let's compare this to our implemented plot method for the maars_lm object
# As observed, the first default lm plot displays automatically, but all
# other default plots are displayed in order
plot(comp_var1)

