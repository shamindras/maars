devtools::load_all()

library(maars)
library(tidyverse)

# population model ----


# data generating mechanism:
# X~U(0,10) and Y~beta0+beta1*X+beta2*X^2+epsilon, where epsilon ~ N(0,1)

# set parameters of true models
beta0 <- 1
beta1 <- 2
beta2_grid <- c(0, 0.1, 0.3, 0.5)


# note that the parameters of the linear pojection are
get_proj_params <- function(beta0, beta1, beta2) {
  first_term <- solve(matrix(c(1, 5, 5, 100 / 3), ncol = 2))
  second_term <- beta0 * c(1, 5) + beta1 * c(5, 100 / 3) + beta2 * c(100 / 3, 250)
  proj_beta <- first_term %*% second_term
  return(tibble(c0 = proj_beta[1], c1 = proj_beta[2]))
}
C_beta2 <- beta2_grid %>%
  imap_dfr(~ bind_cols(
    tibble(beta2 = .x),
    get_proj_params(beta0, beta1, .x)
  ))
C_beta2 <- C_beta2 %>%
  pivot_longer(cols = c(c0, c1), names_to = "par", values_to = "value_par")

# fit linear model on only one dataset ----

# generate covariates - rewrite using the function
n_obs <- 1e2
# generate data
X <- runif(n_obs, 0, 10)
Y_beta2 <- beta2_grid %>%
  map(~ beta0 + beta1 * X + .x * X^2 + rnorm(n_obs, 0, 1)) %>%
  setNames(beta2_grid)

# plot the data
Y_beta2 %>%
  imap(~ tibble(X = X, Y = .x, beta2 = .y) %>%
    ggplot(., aes(X, Y)) +
    geom_point() +
    labs(title = expression(beta2 ~ "=")) # TODO: Fix this
    +
    geom_smooth(method = "lm", formula = y ~ x)) %>%
  patchwork::wrap_plots(., ncol = 3, nrow = 2)




# obtain coverage ----

n_obs <- 5e2

# function to obtain regression data
get_data <- function(beta2_grid, n_obs) {
  X <- runif(n_obs, 0, 10)
  Y_beta2 <- map(
    beta2_grid,
    ~ beta0 + beta1 * X + .x * X^2 + rnorm(n_obs, 0, 1)
  )

  out <- tibble(
    beta2 = beta2_grid,
    regdata = map(Y_beta2, ~ tibble(Y = ., X = X))
  )

  return(out)
}


# obtain maars objects with variance estimates
# for (to add) replications of the data
var_maars_all <- 1:500 %>%
  map_dfr(~ get_data(beta2_grid) %>%
    mutate(var_maars = map(
      regdata,
      ~ comp_var(
        mod_fit = lm(Y ~ X, data = .x),
        boot_emp = list(B = 100),
        boot_mul = list(B = 100, weights_type = "rademacher"),
        boot_res = list(B = 100)
      )
    )),
  .id = "iter"
  )


# obtain confidence intervals and plot coverage ----

# obtain confidence intervals for each estimate
ci_all <- var_maars_all %>%
  mutate(ci = map(var_maars, ~ get_confint(.) %>%
    filter(stat_type == "conf.low" |
      stat_type == "conf.high") %>%
    pivot_wider(names_from = stat_type, values_from = stat_val) %>%
    mutate(term = case_when(
      term == "(Intercept)" ~ "c0",
      term == "X" ~ "c1"
    ))))

glimpse(ci_all)

# obtain average width of the interval
avg_width_beta2 <- ci_all %>%
  unnest(ci) %>%
  group_by(term, beta2, var_type_abb) %>%
  summarise(avg_width = mean(conf.high - conf.low))
avg_width_beta2

# TODO: add "beta2=" to the panels titles
avg_width_beta2 %>%
  ggplot(aes(
    factor(var_type_abb, levels = c("lm", "res", "sand", "emp", "mul")),
    avg_width
  )) +
  geom_col() +
  facet_grid(term ~ beta2, scales = "free_y") +
  labs(x = "Type of estimator", y = "Average width of confidence intervals")


# obtain coverage
coverage_proj <- ci_all %>%
  unnest(ci) %>%
  inner_join(C_beta2, by = c("beta2", "term" = "par")) %>%
  mutate(covers_c = ifelse(conf.low <= value_par & conf.high >= value_par, 1, 0)) %>%
  group_by(term, beta2, var_type_abb) %>%
  summarise(coverage = mean(covers_c))

coverage_proj

# plot coverage
coverage_proj %>%
  ggplot(aes(
    x = factor(var_type_abb,
      levels = c("lm", "res", "sand", "emp", "mul")
    ),
    y = coverage,
    fill = factor(var_type_abb,
      levels = c("lm", "res", "sand", "emp", "mul")
    )
  )) +
  geom_col() +
  facet_grid(term ~ beta2, scales = "free_y") +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(x = "", y = "Coverage", fill = "Estimator") +
  geom_hline(yintercept = 0.95, linetype = "dashed")






