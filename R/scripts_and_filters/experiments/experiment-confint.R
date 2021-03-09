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
comp_var1 <- comp_var(mod_fit = lm_fit, boot_emp = list(B = 20, m = 200),
                    boot_res = list(B = 30),
                    boot_mul = NULL)

broom::tidy(lm_fit, conf.int = TRUE)

ci <- get_confint(mod_fit = comp_var1,
            level = 0.95,
            sand = TRUE,
            boot_emp = TRUE) %>%
    dplyr::select(conf.low.sand, conf.high.sand)
ci

get_confint(mod_fit = comp_var1,
                  level = 0.1,
                  sand = TRUE,
                  boot_emp = TRUE) %>%
    dplyr::select(conf.low.sand, conf.high.sand)

get_var_tidy_summary(mod_fit = comp_var1, sand = TRUE,
                     boot_emp = TRUE, boot_res = TRUE, boot_mul = FALSE,
                     well_specified = TRUE)

