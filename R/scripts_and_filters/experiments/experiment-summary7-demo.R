# Comp var examples ----
devtools::load_all()
set.seed(1243434)

# generate modeling data ----
n <- 1e3
X_1 <- stats::rnorm(n, 0, 1)
X_2 <- stats::rnorm(n, 10, 20)
eps <- stats::rnorm(n, 0, 100)

# Let's generate data and fit a well-specified OLS data and model ----
y <- 2 + X_1 * 1 + X_2 * 5 + eps
lm_fit <- stats::lm(y ~ X_1 + X_2)
summary(lm_fit)

# mms_fit1: Fit our first maars_lm object ----
# Here are essentially retaining our OLS fitted estimates but adding on
# revised model-misspecified standard errors for: [emp, mul], we did not
# run [res] bootstrap errors. Sandwich and well specified OLS errors get
# run by default
set.seed(454354534)
mms_fit1 <- comp_var(
    mod_fit = lm_fit,
    boot_emp = list(B = 50, m = 200),
    boot_res = list(B = 70),
    boot_mul = list(B = 60)
)

# mms_fit2: Let's run an example where some variances are not run ----
set.seed(454354534)
mms_fit2 <- comp_var(
    mod_fit = lm_fit,
    boot_emp = list(B = 50, m = 200),
    boot_res = NULL,
    boot_mul = NULL
)

# print - let's test out the print method for both objects ----
print(mms_fit1)
# print(mms_fit2) # TODO: This is failing, need to check it out

# summary - let's test out the summary method for both objects ----
summary(mms_fit1, boot_emp = TRUE)

# Try some interesting cases
summary(object = mms_fit2) # Just sand = TRUE
summary(object = mms_fit2, boot_emp = TRUE) # sand and emp both TRUE
summary(object = mms_fit2, boot_emp = TRUE, boot_mul = TRUE) # Error, with helpful warning
summary(object = mms_fit2, boot_emp = TRUE, boot_mul = TRUE, boot_res = TRUE) # Error, with helpful warning

# assumptions - let's test out the assumptions method for both objects ----
get_assumptions2(mod_fit = mms_fit2, boot_emp = TRUE)

# Check get_confint2 against get_confint3 ----
# get_confint2 tidy version
confint2_out_tidy <- get_confint2(mod_fit = mms_fit2,
                                  level = 0.95,
                                  sand = TRUE,
                                  boot_emp = TRUE,
                                  boot_mul = FALSE,
                                  boot_res = FALSE,
                                  well_specified = FALSE,
                                  tidy = TRUE)

# Manually get the wider (non-tidy) version
confint2_out_tidy_wider <- confint2_out_tidy %>%
    tidyr::pivot_wider(data = .,
                       names_from = c(stat_type, var_type_abb),
                       names_glue = "{stat_type}.{var_type_abb}",
                       values_from = stat_val)

# get_confint2 wider (non-tidy) version
confint2_out_wider <- get_confint2(mod_fit = mms_fit2,
                                  level = 0.95,
                                  sand = TRUE,
                                  boot_emp = TRUE,
                                  boot_mul = FALSE,
                                  boot_res = FALSE,
                                  well_specified = FALSE,
                                  tidy = FALSE)
confint2_out_wider
# confint2_out_wider %>% dplyr::glimpse()

# Check: confint2 manual wider check
# It fails, but it is just a column ordering issue
identical(x = confint2_out_tidy_wider, y = confint2_out_wider, )
confint2_out_wider %>% dplyr::glimpse()
confint2_out_tidy_wider %>% dplyr::glimpse()
colnames(confint2_out_wider) == colnames(confint2_out_tidy_wider)

# get_confint3 tidy version
confint3_out_tidy <- get_confint3(mod_fit = mms_fit2,
                                  level = 0.95,
                                  sand = TRUE,
                                  boot_emp = TRUE,
                                  boot_mul = FALSE,
                                  boot_res = FALSE,
                                  well_specified = FALSE,
                                  tidy = TRUE)
confint3_out_tidy

# Manually get the wider (non-tidy) version
confint3_out_tidy_wider <- confint3_out_tidy %>%
    tidyr::pivot_wider(data = .,
                       names_from = c(stat_type, var_type_abb),
                       names_glue = "{stat_type}.{var_type_abb}",
                       values_from = stat_val)

# get_confint3 wider (non-tidy) version
confint3_out_wider <- get_confint3(mod_fit = mms_fit2,
                                   level = 0.95,
                                   sand = TRUE,
                                   boot_emp = TRUE,
                                   boot_mul = FALSE,
                                   boot_res = FALSE,
                                   well_specified = FALSE,
                                   tidy = FALSE)
confint3_out_wider

# Check: confint3 manual wider check
# It fails, but it is just a column ordering issue
identical(x = confint3_out_tidy_wider, y = confint3_out_wider)
confint3_out_wider %>% dplyr::glimpse()
confint3_out_tidy_wider %>% dplyr::glimpse()
colnames(confint3_out_wider) == colnames(confint3_out_tidy_wider)

# Main difference is that we output more columns
# But this is an easy thing to correct for using dplyr::select in a
# clean way
# TODO: Discuss which columns we should output for confint

# Check confint3 to the original
get_confint(mod_fit = mms_fit2,
            level = 0.95,
            sand = TRUE,
            boot_emp = TRUE,
            boot_mul = FALSE,
            boot_res = FALSE,
            well_specified = FALSE) %>%
    dplyr::glimpse()

# Compare this to
confint3_out_wider %>% dplyr::glimpse()

# CONFINT: Test out confint functionality ----
confint(mms_fit2)
confint(mms_fit2, boot_emp = TRUE)
confint(mms_fit2, boot_emp = TRUE, boot_mul = TRUE)
print(mms_fit1)
