#' title: "Vignette 3.1"
#' date: "May 8, 2021"
#' output: github_document
#' ---

#+ setup, include=FALSE
# knitr::opts_chunk$set(collapse = TRUE)


library(tidyverse)
library(furrr)
library(glue)
library(cli)
library(broom)
library(vroom)

devtools::document()
devtools::load_all()

plan(multicore, workers = 25)

gen_ind_mod_fit <- function(n) {
    x <- runif(n, 0, 10)
    y <- 3 * x^2 + rnorm(n, mean = 0, sd = sqrt(100))
    return(lm(y ~ x))
}

get_ind_confint <- function(covg, n, B){
    ind_confint <- gen_ind_mod_fit(n = n) %>%
        comp_var(mod_fit = .,
                            boot_emp = list(B = B),
                            boot_sub = list(B = B, m = floor(sqrt(n))),
                            boot_mul = list(B = B, weights_type = "rademacher")) %>%
        get_confint(level = 0.95)
    return(ind_confint)
}


NUM_COVG_REPS <- 5 * 1e3 # Number of coverage replications
grid_n <- 500
grid_B <- c(seq(4, 50, by = 2),
            seq(60, 100, by = 10))
grid_params <- crossing(covg = 1:NUM_COVG_REPS,
                        n = grid_n) %>%
    crossing(B = grid_B)

confint_replications <- grid_params %>%
    mutate(out_confint = future_pmap(.l = .,
                                     .f = get_ind_confint,
                                     .options = furrr_options(seed = TRUE),
                                     .progress = TRUE))

all_confint <- confint_replications %>%
    unnest(out_confint) %>%
    filter(stat_type == "conf.low" | stat_type == "conf.high") %>%
    pivot_wider(names_from = stat_type, values_from = stat_val) %>%
    select(n, B, term, var_type_abb, conf.low, conf.high)

proj_par <- gen_ind_mod_fit(1e7) %>%
    tidy() %>%
    select(term, estimate)

#
all_confint_coverage <- all_confint %>%
    inner_join(proj_par, by = 'term') %>%
    mutate(ind_coverage = ifelse(conf.low <= estimate &
                                     conf.high >= estimate, 1, 0))

# summarise sandwich and lm
cw <- all_confint_coverage %>%
    filter(var_type_abb %in% c('sand', 'lm')) %>%
    group_by(term, var_type_abb) %>%
    summarise(
        coverage = mean(ind_coverage),
        avg_width = mean(conf.high - conf.low),
        std.error.avg_width = sd(conf.high - conf.low)/sqrt(n()),
        std.error.coverage = sd(ind_coverage)/sqrt(n())
    )

# summarise emp, mult, sub
cw_x_B <- all_confint_coverage %>%
    filter(var_type_abb %in% c('emp', 'mult', 'sub')) %>%
    group_by(term, B, var_type_abb) %>%
    summarise(
        coverage = mean(ind_coverage),
        avg_width = mean(conf.high - conf.low),
        std.error.avg_width = sd(conf.high - conf.low)/sqrt(n()),
        std.error.coverage = sd(ind_coverage)/sqrt(n())
    )

cw_all <- cw %>%
    bind_rows(cw_x_B)


# plots ----
confint_plt <- cw_all %>%
    ggplot(aes(x = B, y = coverage, fill = var_type_abb, col = var_type_abb)) +
    geom_point() +
    geom_line() +
    geom_ribbon(aes(ymin = coverage-1.96*std.error.coverage,
                    ymax = coverage+1.96*std.error.coverage), alpha = 0.3) +
    labs(y = "Coverage") +
    theme_bw() +
    facet_wrap(~ term, scales = 'free') +
    geom_hline(yintercept = 0.95, linetype = "dashed")
confint_plt


avgwidth_plt <- cw_all %>%
    mutate(n_text = as.factor(glue("n = {n}"))) %>%
    ggplot(aes(x = B, y = avg_width, fill = var_type_abb, col = var_type_abb)) +
    geom_point() +
    geom_line() +
    geom_ribbon(aes(ymin = avg_width-1.96*std.error.avg_width,
                    ymax = avg_width+1.96*std.error.avg_width), alpha = 0.3) +
    facet_grid(~ n_text) +
    labs(y = "Coverage") +
    facet_wrap(~ term, scales = 'free') +
    theme_bw()
avgwidth_plt


