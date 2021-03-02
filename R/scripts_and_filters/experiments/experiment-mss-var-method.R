library(here)
source(here('R', 'mss-var.R'))
# ------------------
set.seed(1243434)

# generate data
n <- 1e3
X <- stats::rnorm(n, 0, 1)
# OLS data and model
y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
lm_fit <- stats::lm(y ~ X)
mod_fit <- mss_var(lm_fit, boot_emp = list(B=10))

class(mod_fit)
print(mod_fit)
summary(mod_fit, sand = TRUE)
summary(mod_fit, sand = FALSE, boot_emp = TRUE)
get_summary2(mod_fit, boot_emp = TRUE)


# recompute variance on the maars_lm object
mod_fit <- mss_var(mod_fit)
summary(mod_fit)
summary(mod_fit, sand=TRUE)

# create object of class maars_lm
class(ml)
ml <- as.maars(lm_fit)
class(ml)

