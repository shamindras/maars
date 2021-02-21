summarize_lm_style <- function(mod_fit, digits) {

    if('var' %in% names(mod_fit)){
        out_summ <- mod_fit[['var']]$var_sand$var_summary
    } else{
        out_summ <- broom::tidy(mod_fit)
    }

    out_summ <- out_summ %>%
        dplyr::mutate(sig = dplyr::case_when(
            p.value <= 0.001 ~ "***",
            p.value <= 0.01 ~ "**",
            p.value <= 0.05 ~ "*",
            TRUE ~ "."
        )) %>%
        dplyr::rename(
            'Term' = term,
            "Estimate" = estimate,
            "Std. Error" = std.error,
            "t value" = statistic,
            "Pr(>|t|)" = p.value,
            "Significance" = sig
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

summary.maars_lm <- function(x,...){
    summarize_lm_style(mod_fit = x, digits = 2)
}

as.maars <- function(x, ...){
    UseMethod('as.maars')
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
mod_fit <- stats::lm(y ~ X)
mod_fit <- mss_var(mod_fit)

class(mod_fit)
print(mod_fit)
summary(mod_fit)


# recompute variance on the maars_lm object
mod_fit <- mss_var(mod_fit)
summary(mod_fit)

# create object of class maars_lm
ml <- as.maars(lm_fit)
class(ml)

