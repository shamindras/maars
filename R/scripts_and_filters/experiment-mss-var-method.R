summarize_lm_style <- function(mod_fit, type_var = 'sand', digits) {

    out_summ <- mod_fit[['var']][[paste0('var_', type_var)]]$var_summary

    out_summ <- out_summ %>%
        dplyr::mutate(sig = dplyr::case_when(
            p.value <= 0.001 ~ "***",
            p.value <= 0.01 ~ "**",
            p.value <= 0.05 ~ "*",
            TRUE ~ "."
        ))
    colnames(out_summ) <- c(
        'Term', 'Estimate', paste0('Std. Error ', type_var),
        't value', 'Pr(>|t|)', 'Significance'
    )

    cat("\nCall:\n", # S has ' ' instead of '\n'
        stringr::str_c(rlang::expr_deparse(x = mod_fit$call),
                       sep = "\n",
                       collapse = "\n"
        ),
        "\n\n",
        sep = ""
    )
    cat('Coefficients:\n')
    print.data.frame(out_summ, row.names = FALSE, digits = digits)
    cat("\n---\n")
    cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ‘ ’ 1")
}


mss_var <- function(mod_fit, boot_emp = NULL, boot_res = NULL, boot_mul = NULL){
    # the following condition will need to be revised once we introduce glm
    if(all(c('maars_lm', 'lm') %in% class(mod_fit))) attr(mod_fit, 'class') <- 'lm'
    var_est <- comp_var(mod_fit, boot_emp, boot_res, boot_mul)
    mod_fit$var <- var_est
    attr(mod_fit, 'class') <- c('maars_lm', 'lm')
    mod_fit
}


print.maars_lm <- function(x,...){
    # this function is not really needed, but let's keep it for now
    NextMethod('print')
}

summary.maars_lm <- function(x, sand=TRUE, boot_emp=FALSE, boot_res=FALSE, boot_mul=FALSE,
                             well_specified=FALSE,...) {
    if(well_specified){NextMethod('summary')}else{
        out_summ <- list(
            sand = ifelse(sand, 'sand', FALSE),
            boot_emp = ifelse(boot_emp, 'boot_emp', FALSE),
            boot_res = ifelse(boot_res, 'boot_res', FALSE),
            boot_mul = ifelse(boot_mul, 'boot_mul', FALSE)
        ) %>%
            purrr::keep( ~ !isFALSE(.x)) %>%
            purrr::map(~ mod_fit[['var']][[paste0('var_', .x)]]$var_summary %>%
                           dplyr::mutate(sig = dplyr::case_when(
                               p.value <= 0.001 ~ "***",
                               p.value <= 0.01 ~ "**",
                               p.value <= 0.05 ~ "*",
                               TRUE ~ "."
                           )) %>%
                           stats::setNames(c('Term',
                                             'Estimate',
                                             paste0('t value ', .x),
                                             paste0('Std. Error ', .x),
                                             paste0('Pr(>|t|) ', .x),
                                             paste0('Significance ', .x)))
            ) %>%
            purrr::reduce(dplyr::left_join, by = c('Term', 'Estimate')) %>%
            dplyr::relocate(Term, Estimate, matches('Std. Error'),
                            matches('t value'), matches('Pr(>|t|)'),
                            matches('Significance'))

        cat("\nCall:\n", # S has ' ' instead of '\n'
            stringr::str_c(rlang::expr_deparse(x = mod_fit$call),
                           sep = "\n",
                           collapse = "\n"
            ),
            "\n\n",
            sep = ""
        )
        cat('Coefficients:\n')
        print.data.frame(out_summ, row.names = FALSE, digits = 2)
        cat("\n---\n")
        cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ‘ ’ 1")
    }
}

summary.maars_lm2 <- function(x, type_var = 'sand',...) {
    # need to add some limitations for type_var (e.g., needs to be = boot_emp,
    # boot_res, etc)
    out_summ <- as.list(type_var) %>% stats::setNames(type_var) %>%
        purrr::map(~ mod_fit[['var']][[paste0('var_', .x)]]$var_summary %>%
                       dplyr::mutate(sig = dplyr::case_when(
                           p.value <= 0.001 ~ "***",
                           p.value <= 0.01 ~ "**",
                           p.value <= 0.05 ~ "*",
                           TRUE ~ "."
                       )) %>%
                       stats::setNames(c('Term',
                                         'Estimate',
                                         paste0('t value ', .x),
                                         paste0('Std. Error ', .x),
                                        paste0('Pr(>|t|) ', .x),
                                        paste0('Significance ', .x)))
                   ) %>%
    purrr::reduce(dplyr::left_join, by = c('Term', 'Estimate')) %>%
        dplyr::relocate(Term, Estimate, matches('Std. Error'),
                        matches('t value'), matches('Pr(>|t|)'),
                        matches('Significance'))

    cat("\nCall:\n", # S has ' ' instead of '\n'
        stringr::str_c(rlang::expr_deparse(x = mod_fit$call),
                       sep = "\n",
                       collapse = "\n"
        ),
        "\n\n",
        sep = ""
    )
    cat('Coefficients:\n')
    print.data.frame(out_summ, row.names = FALSE, digits = 2)
    cat("\n---\n")
    cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ‘ ’ 1")
}

as.maars <- function(x, ...){
    UseMethod('as.maars', x)
}

as.maars.lm <- function(x, ...){
    attr(x, 'class') <- c('maars_lm', 'lm')
    return(x)
}


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
summary(mod_fit, type_var = c('sand', 'boot_emp'))


# recompute variance on the maars_lm object
mod_fit <- mss_var(mod_fit)
summary(mod_fit)

# create object of class maars_lm
ml <- as.maars(lm_fit)
class(ml)

