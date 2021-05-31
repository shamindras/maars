# Setup Libraries --------------------------------------------------------
#library(furrr)
#library(progressr)
library(tidyverse)
#library(progress)
library(glue)
#library(parallel)

#devtools::document()
devtools::load_all()
pbo = pbapply::pboptions(type="txt")

# Global variables --------------------------------------------------------
NUM_COVG_REPS <- 1e3 # Number of coverage replications
N <- 1e5

# Generate replication data + model fit -----------------------------------
# Helper function to generate a single replication dataset and `lm` fit
gen_ind_mod_fit <- function(n){
    # X <- stats::rnorm(n, 0, 1)
    # y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
    d <- 1e2#0.5*1e2
    X <- matrix(rt(d*n,df=3), nrow = n)
    beta <- rep(1,d)
    y <- X%*%beta + rnorm(n, 0, 10)
    lm_fit <- stats::lm(y ~ 0+X)
    return(lm_fit)
}

# generate data
#replication_mod_fits <- map(1:NUM_COVG_REPS, ~gen_ind_mod_fit(n = N))

grid_B <- 2:50

#mc <- getOption("mc.cores", 10)
comp_ci <- function(iter, grid_B, n){
    #print(iter)
    #browser()
    mod_fit <- gen_ind_mod_fit(n = n)
    out <- grid_B %>%
        map_dfr(~ comp_var(mod_fit = mod_fit,
                           boot_emp = list(B = ., m = floor(n^0.6))#,
                           #boot_sub = list(B = ., m = floor(sqrt(n))),
                           #boot_mul = list(B = .)
        ) %>%
            get_confint(mod_fit = ., level = 0.95),
        .id = 'B'
        )
    return(out)
}

out <- pbapply::pblapply(
    #lapply(
    X = as.list(1:NUM_COVG_REPS),
    FUN = comp_ci,
    grid_B = grid_B,
    n = N
    ,cl = 10
    #mc.cores = getOption("cores", 50)
)

out <- out %>%
    bind_rows(.)



# plan(multicore, workers = 1)
# with_progress({
#     p <- progressor(steps = NUM_COVG_REPS)
#     # for each
#     out <- 1:NUM_COVG_REPS %>%
#         future_map( ~ comp_ci(., grid_B),
#                     seed = TRUE
#                     #,progress = TRUE
#         )
# })


# comp_ci <- function(mod_fit, grid_B){
#     print('Iteration')
#     out <- grid_B %>%
#         map_dfr(~ comp_var(mod_fit = mod_fit,
#                            boot_emp = list(B = .),
#                            boot_sub = list(B = ., m = sqrt(N)),
#                            boot_mul = list(B = .)) %>%
#                     get_confint(mod_fit = ., level = 0.95),
#                 .id = 'B'
#         )
#     return(out)
# }
#
#
# with_progress({
#     p <- progressor(steps = length(replication_mod_fits))
# # for each
#     out <- replication_mod_fits %>%
#         future_map( ~ comp_ci(., grid_B),
#                 seed = TRUE
#                 #,progress = TRUE
#                 )
# })





confint_replications <- out
# Get combined confidence intervals ---------------------------------------
all_confint <- confint_replications %>%
    filter(stat_type == "conf.low" |
               stat_type == "conf.high") %>%
    pivot_wider(names_from = stat_type, values_from = stat_val)

write_csv(x = all_confint, file = here::here("R/scripts_and_filters/experiments/confidence3.2.csv"))

#df_mod <- gen_ind_mod_fit(1e8)

all_confint_coverage <- all_confint %>%
    inner_join(tibble(
        term = c("X1"),
        value_par = c(1)
    ),
    by = "term"
    ) %>%
    mutate(covers_c = ifelse(conf.low <= value_par & conf.high >= value_par, 1, 0)) %>%
    group_by(term, B, var_type_abb) %>%
    summarize(
        coverage = mean(covers_c),
        avg_width = mean(conf.high - conf.low)
    ) %>%
    filter(var_type_abb != 'lm') %>%
    filter(term != '(Intercept)')

# Coverage seems to be working here i.e. most of the values are in the 86-100% range
#summary(all_confint_coverage$coverage)
#hist(all_confint_coverage$coverage, breaks = 10, xlim = c(0, 1))

all_confint_plt <- all_confint_coverage %>%
    mutate(B = as.numeric(B)) %>%
    #filter(var_type_abb != 'sand') %>%
    ggplot(aes(B, coverage, col = var_type_abb)) +
    geom_line() +
    labs(y = "Coverage") +
    theme_bw() +
    ylim(0,1)
all_confint_plt

# TODO: Check if we should be plotting avg_width?
# Should this be coverage instead?
# all_confint_plt <- all_confint_coverage %>%
#     mutate(var_type_abb = as.factor(var_type_abb),
#            B = as.factor(B),
#            m = as.factor(m))  %>%
#     filter(var_type_abb != 'sand') %>%
#     ggplot(aes(m, coverage)) +
#     geom_col() +
#     facet_grid(B~var_type_abb) +
#     labs(y = "Coverage") +
#     theme_bw() +
#     geom_hline(yintercept = (all_confint_coverage %>% filter(var_type_abb == 'sand') %>% pull(coverage))[1], linetype = "dashed")
# all_confint_plt
# all_confint_plt <- all_confint_coverage %>%
#     filter(var_type_abb != 'lm') %>%
#     filter(term != '(Intercept)') %>%
#     mutate(var_type_abb = as.factor(var_type_abb),
#            B = as.factor(B)
#            ) %>%
#     ggplot(aes(m, coverage, col = B)) +
#     geom_point() +
#     #geom_path() +
#     ggplot2::scale_x_log10() +
#     ylim(0.75,1) +
#     #facet_grid~ var_type_abb) +
#     labs(y = "Coverage") +
#     geom_hline(yintercept = (all_confint_coverage %>% filter(var_type_abb == 'sand') %>% pull(coverage))[1], linetype = "dashed") +
#     theme_bw()
#all_confint_plt
# Save the plot
# Note: It may be good to pre-run this script and read it in our paper, rather
#       than knit the pdf with this time consuming run
ggsave(filename = here::here("R/scripts_and_filters/experiments/experiment-vignette3.1_coverage.png"), plot = all_confint_plt)
# Plot interactively using plotly
#plotly::ggplotly(p = all_confint_plt)


all_avgwidth_plt <- all_confint_coverage %>%
    mutate(B = as.numeric(B)) %>%
    ggplot(aes(B, avg_width, col = var_type_abb)) +
    geom_line() +
    labs(y = "Average width of confidence inverval") +
    theme_bw()
all_avgwidth_plt
# Save the plot
# Note: It may be good to pre-run this script and read it in our paper, rather
#       than knit the pdf with this time consuming run
ggsave(filename = here::here("R/scripts_and_filters/experiments/experiment-vignette3.1_width.png"), plot = all_avgwidth_plt)
# Plot interactively using plotly
#plotly::ggplotly(p = all_confint_plt)





