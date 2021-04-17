devtools::load_all()

library(maars)
library(tidyverse)

# generate covariates
n_obs <- 1e3
X <- runif(n_obs, min = 0, max = 10)

# generate Y's for a grid of gamma values
gamma_values <- c(0, 0.1, 0.5, 1, 1.5, 2, 2.5)
beta_0 <=
# beta_val <- 2
# inter_val <- 1
# Y_by_gamma <- gamma_values %>%
#     map(~ inter_val + beta_val*X + .*X^{1.7} + exp(0.5*X)*rnorm(n_obs)) %>%
#     setNames(gamma_values)

# Attempt at non-gaussian i.e. gamma noise
# Y_by_gamma <- gamma_values %>%
#     map(~ inter_val + beta_val*X + rgamma(n_obs, 1, 1)) %>%
#     setNames(gamma_values)

# Attempt at heteroscedasticity with gamma noise
# # Y_i = \beta X_i + eps_i, eps_i ~N(0, \sigma_i^{2})
# Y_by_gamma <- gamma_values %>%
#     map(~ inter_val + beta_val*X + sapply(sqrt(X), rnorm, n = n_obs, mean = 0)) %>%
#     setNames(gamma_values)

gamma_values <- c(0, 0.1, 0.3, 0.5)
Y_by_gamma <- gamma_values %>%
    map(~ inter_val + beta_val*X + .*X^2 + rnorm(n_obs)) %>%
    setNames(gamma_values)

# plot the data - need to add the title gamma = value
Y_by_gamma %>%
    imap( ~ tibble(X = X, Y = .x, gamma = .y) %>%
             ggplot(., aes(X,Y)) +
             geom_point() +
             labs(title = expression(gamma))
             + geom_smooth(method='lm', formula = y ~ x)
             ) %>%
    patchwork::wrap_plots(., ncol = 3, nrow = 2)

# regress each of the Y's vectors on X
modfit_by_gamma <- Y_by_gamma %>%
    map(~ lm(.~X))

# compute variance estimates for each of the fitted models
varest_by_gamma <- modfit_by_gamma %>%
    map( ~ comp_var(mod_fit = .,
                    boot_emp = list(B = 100),
                    boot_mul = list(B = 100, weights_type = "rademacher"),
                    boot_res = list(B = 100)))

# obtain confidence intervals
ci_by_gamma <- varest_by_gamma %>%
    imap_dfr(~ get_confint(.) %>%
            filter(stat_type == "conf.low" | stat_type == "conf.high") %>%
            pivot_wider(names_from = stat_type, values_from = stat_val),
            .id = "gamma")


# plot confidence intervals for beta
ci_by_gamma %>%
    filter(term == 'X') %>%
    ggplot(aes(x = gamma, y = estimate)) +
    geom_point(position = position_dodge(width = 0.5)) +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    geom_errorbar(aes(
        ymin = conf.low,
        ymax = conf.high),
        position = position_dodge(width = 0.5),
        width = 0.1
    )  +
    geom_hline(yintercept = beta_val, col = 'red', linetype = 'dashed') +
    facet_wrap(~ var_type_abb, ncol = 3)


# ----

# old (to be deleted) -----
# std_errs <- varest_by_gamma %>%
#     map(~ get_summary(.) %>%
#             filter(stat_type == 'std.error'))
# varmat <- varest_by_gamma %>%
#     map(~ pluck(., 'var'))
#
#
sand_sp <- modfit_by_gamma %>%
    map_dfr( ~ sandwich::sandwich(.) %>%
             diag() %>% sqrt(),
             .id = 'gamma') %>%
    setNames(c('gamma', 'intercept', 'X'))
empboot_sp <- modfit_by_gamma %>%
    map_dfr( ~ sandwich::vcovBS(., type = 'xy') %>%
                 diag() %>% sqrt(),
             .id = 'gamma') %>%
    setNames(c('gamma', 'intercept', 'X'))

empboot_sp %>% select(X) %>%
    bind_cols(sand_sp %>% select(X))


