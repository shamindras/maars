rm(list = ls())
# A demo on simple linear regression!
# Comp var examples ----
devtools::load_all()
set.seed(1243434)

# generate modeling data ----
n <- 1e3
x <- stats::rnorm(n, 0, 1)
y <- 2 + x * 1 + 1 * x^{2} + exp(0.2 * abs(x)) * rnorm(n)
lm_fit <- stats::lm(y ~ x)

mms_fit0 <- comp_var(mod_fit = lm_fit,
                     boot_emp = list(B = 100),
                     boot_mul = list(B = 100),
                     boot_res = list(B = 100))

##
# all NULL: return everything available
summary(mms_fit0, sand = NULL,
        boot_mul = NULL, boot_emp = NULL,
        boot_res = NULL, well_specified = NULL)
# one NULL, all other FALSE: return NULL
summary(mms_fit0, sand = NULL,
        boot_mul = FALSE, boot_emp = FALSE,
        boot_res = FALSE, well_specified = FALSE)
# two NULLs, all other FALSE: return NULLs
summary(mms_fit0, sand = NULL,
        boot_mul = NULL, boot_emp = FALSE,
        boot_res = FALSE, well_specified = FALSE)
# one TRUE, all other NULL: return TRUE only
summary(mms_fit0, sand = TRUE,
        boot_mul = NULL, boot_emp = NULL,
        boot_res = NULL, well_specified = NULL)
# two TRUE, all other NULL: return TRUEs only
summary(mms_fit0, sand = TRUE,
        boot_mul = TRUE, boot_emp = NULL,
        boot_res = NULL, well_specified = NULL)
# all TRUE
summary(mms_fit0, sand = TRUE,
        boot_mul = TRUE, boot_emp = TRUE,
        boot_res = TRUE, well_specified = TRUE)
# two TRUE, one FALSE: return TRUEs only
summary(mms_fit0, sand = TRUE,
        boot_mul = TRUE, boot_emp = FALSE,
        boot_res = FALSE, well_specified = NULL)




mms_fit0 <- comp_var(mod_fit = lm_fit,
                     boot_emp = list(B = 100),
                     boot_mul = list(B = 100))

##
# all NULL: return everything available
summary(mms_fit0, sand = NULL,
        boot_mul = NULL, boot_emp = NULL,
        boot_res = NULL, well_specified = NULL)
# one NULL, all other FALSE: return NULL
summary(mms_fit0, sand = NULL,
        boot_mul = FALSE, boot_emp = FALSE,
        boot_res = FALSE, well_specified = FALSE)
# two NULLs, all other FALSE: return NULLs
summary(mms_fit0, sand = NULL,
        boot_mul = NULL, boot_emp = FALSE,
        boot_res = FALSE, well_specified = FALSE)
# one TRUE, all other NULL: return TRUE only
summary(mms_fit0, sand = TRUE,
        boot_mul = NULL, boot_emp = NULL,
        boot_res = NULL, well_specified = NULL)
# two TRUE, all other NULL: return TRUEs only
summary(mms_fit0, sand = TRUE,
        boot_mul = TRUE, boot_emp = NULL,
        boot_res = NULL, well_specified = NULL)
# two TRUE, all other NULL: error
summary(mms_fit0, sand = TRUE,
        boot_mul = NULL, boot_emp = NULL,
        boot_res = TRUE, well_specified = NULL)
# all TRUE: error
summary(mms_fit0, sand = TRUE,
        boot_mul = TRUE, boot_emp = TRUE,
        boot_res = TRUE, well_specified = TRUE)
# two TRUE, one FALSE: return TRUEs only
summary(mms_fit0, sand = TRUE,
        boot_mul = TRUE, boot_emp = FALSE,
        boot_res = FALSE, well_specified = NULL)

