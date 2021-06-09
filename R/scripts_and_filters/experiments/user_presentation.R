# Set seed ----
set.seed(14352)

# Load standard libraries ----
library(tidyverse)
# library(maars) # Uncomment line

# Load development maars code ----
devtools::document()
devtools::load_all()

# fit model ----
n <- 1e3
x <- runif(n, -10, 10)
y <- 2 * x + sapply(sqrt(abs(x)), rnorm, n = 1, mean = 0)
df <- data.frame(x = x, y = y)

# plot the data sample
df %>%
    ggplot(aes(x, y)) +
    geom_point() +
    theme_bw()

# get
mod_fit <- lm(y ~ 0 + x, data = df) %>%
    comp_var()

get_confint(mod_fit, level = 0.95,
            sand = FALSE,
            well_specified = TRUE) %>%
    dplyr::filter(.data = .,
                  .data$stat_type == "conf.low" | .data$stat_type == "conf.high") %>%
    tidyr::pivot_wider(names_from = stat_type,
                       values_from = stat_val) %>%
    dplyr::mutate(.data = .,
                  Type = fetch_mms_emoji_title(var_type_abb = .data$var_type_abb,
                                               title_type = "title")) %>%
    ggplot2::ggplot(data = .,
                    mapping = ggplot2::aes(x = .data$Type,
                                           y = .data$estimate,
                                           col = .data$Type)) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.5)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank()) +
    ggplot2::labs(
        x = "",
        y = "Estimate",
        title = "95% confidence intervals for coefficients") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_errorbar(ggplot2::aes(
        ymin = conf.low,
        ymax = conf.high),
        position = ggplot2::position_dodge(width = 0.5),
        width = 0.1
    ) +
    ggplot2::facet_wrap(~term, ncol = 3, scales = "free_y") +
    #ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)) +
    ggplot2::guides(col = ggplot2::guide_legend(title = "Type of standard error")) +
    ylim(1.9, 2.1) +
    geom_hline(yintercept = 2,
               linetype = 'dashed',
               col = 'black')


# get coverage for well specified model over 10000 replications ------
conf <- c()
for(i in 1:1e4){
    x <- runif(n,-10,10)
    y <- 2 * x + unlist(lapply(sqrt(abs(x)), rnorm, n = 1, mean = 0))
    lm_fit <- lm(y ~ 0 + x) %>%
        confint()
    conf <- rbind(conf, lm_fit)
}

low <- if_else(conf[,1] -  2 < 0, 1 , 0)
high <- if_else(conf[,2] -  2 > 0, 1 , 0)

covers <- pmin(low, high)
mean(covers)

# Plot the coverage for lm() model, compare to 95% coverage
data.frame(x = 'well-specified model', coverage = mean(covers)) %>%
    ggplot(aes(x = x, y = coverage)) +
    geom_col() +
    theme_bw() +
    geom_hline(linetype = 'dashed', col = 'black', yintercept = 0.95) +
    xlab('') +
    scale_y_continuous('Coverage', breaks = seq(0,1,by=0.25),
                       labels = paste0(seq(0,1,by=0.25)*100, '%'),
                       limits = c(0,1)) +
    ggtitle('Coverage for coefficient')


# maars fits on sample data ----
lm_fit <- lm(y ~ 0 + x)
confint(lm_fit)
summary(lm_fit)

# Fit the required standard variance
maars_fit <- lm_fit %>%
    comp_var(boot_emp = list(B = 1e3),
             boot_mul = list(B = 1e3),
             boot_res = list(B = 1e3))

print(maars_fit)
summary(maars_fit)
get_summary(maars_fit)

# maars_plots <- maars_fit %>%
#     get_plot()
#
# maars_plots %>%
#     pluck('p7') +
#     ylim(1.75, 2.1) +
#     geom_hline(yintercept = 2,
#                linetype = 'dashed',
#                col = 'black')

maars_fit %>%
    get_plot() %>%
    pluck('p7') +
    ylim(1.75, 2.1) +
    geom_hline(yintercept = 2,
               linetype = 'dashed',
               col = 'black')


