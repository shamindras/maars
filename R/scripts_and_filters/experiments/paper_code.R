set.seed(132423)
devtools::install_github("shamindras/maars", force = TRUE)

library(tidyverse)
library(maars)

# load LA county homeless data into dataframe
la_county <- get(data("la_county",
  package = "maars"
))

# fit OLS on LA county data
mod_fit <- lm(
  formula = StreetTotal ~ .,
  data = la_county
)

# summary of fitted model
summary(mod_fit)

# obtain maars with obj. w/ sandwich variance
maars_var_sand <- mod_fit %>%
  comp_var()

# obtain maars obj. w/ emp. boot. variance
maars_var_emp <- mod_fit %>%
  comp_var(boot_emp = list(B = 1e3,
                                  m = 505))

# obtain maars obj. w/ mul. boot. variance
maars_var_mul <- mod_fit %>%
  comp_var(boot_mul = list(B = 1e3,
    weights_type = "rademacher"))

# obtain maars obj. w/ subsampling variance
maars_var_sub <- mod_fit %>%
  comp_var(boot_sub = list(B = 1e3, m = 200))

# obtain maars obj. w/ res. boot. variance
maars_var_res <- mod_fit %>%
  comp_var(boot_res = list(B = 1e3))

# obtain maars obj. w/ all methods
maars_var <- mod_fit %>%
  comp_var(
    boot_emp = list(B = 1e3, m = 505),
    boot_mul = list(B = 1e3),
    boot_sub = list(B = 1e3, m = 200),
    boot_res = list(B = 1e3))

# print obj.
print(maars_var)

# summarise obj.
summary(maars_var)

#
get_summary(maars_var)

# obtain diagnostic plots
maars_plots <- maars:::get_plot(maars_var)

# obtain confidence intervals
p7 <- get_confint(maars_var, level = 0.95,
                  sand = TRUE,
                  boot_emp = TRUE,
                  boot_mul = TRUE,
                  boot_res = TRUE,
                  well_specified = TRUE) %>%
    dplyr::filter(.data = ., term != '(Intercept)') %>%
    dplyr::filter(.data = .,
                  .data$stat_type == "conf.low" | .data$stat_type == "conf.high") %>%
    tidyr::pivot_wider(names_from = stat_type,
                       values_from = stat_val) %>%
    dplyr::mutate(.data = .,
                  Type = maars:::fetch_mms_emoji_title(var_type_abb = .data$var_type_abb,
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
    NULL
p7 +
  ggtitle('') +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  theme(legend.position="bottom", legend.box="vertical",
        legend.text=element_text(size=12),
        legend.title=element_blank(),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        strip.text.x = element_text(size = 12)) +
  NULL

# QQnorm
boot_names <- c('var_boot_emp', 'var_boot_mul', 'var_boot_res')
boot_vars <- boot_names %>% purrr::map(~ purrr::pluck(maars_var, 'var', .)) %>%
    purrr::keep(~ !is.null(.x))
if(length(boot_vars)>0){
    p8 <- maars:::diag_boot_qqn(boot_out = purrr::pluck(boot_vars, 1, 'boot_out') %>%
                                    unnest(boot_out) %>%
                                    filter(term != '(Intercept)') %>%
                                    nest(boot_out = c(estimate, term)),
                        boot_type = purrr::pluck(boot_vars, 1, 'var_type_abb')) +
        NULL
} else{
    p8 <- NULL
}
p8 +
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        strip.text.x = element_text(size = 12)) +
  ggtitle('') +
  NULL




# regression based on reweighting of regressors
coef_rwgt <- mod_fit %>% maars::diag_fit_reg_rwgt(
  B = 300, terms_to_rwgt = names(la_county)[-1])

# focal slope for "PercVacant"
mod_fit %>% diag_foc_slope(coef_rwgt,'PercVacant')
# nonlinearity detection
mod_fit %>% diag_nl_detect(coef_rwgt)
# focal reweighting var. for "PercVacant"
mod_fit %>% diag_foc_rwgt(coef_rwgt,'PercVacant')

# focal slope for "PercVacant" withotu intercept
mod_fit %>%
  diag_foc_slope(coef_rwgt %>% filter(term_rwgt != '(Intercept)'), 'PercVacant') +
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        strip.text.x = element_text(size = 12))
