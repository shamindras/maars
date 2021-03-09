


get_assumptions <- function(mod_fit,
                            sand = sand,
                            boot_emp = boot_emp,
                            boot_res = boot_res,
                            boot_mul = boot_mul,
                            well_specified = well_specified,
                            as_tibble = TRUE) {
    req_var_nms <- check_fn_args_summary(mod_fit = mod_fit, sand = sand,
                                         boot_emp = boot_emp, boot_res = boot_res,
                                         boot_mul = boot_mul,
                                         well_specified = well_specified)

    assumptions <- req_var_nms %>%
        purrr::map(.x = ., ~paste0(purrr::pluck(mod_fit$var, .x, 'var_type_abb'),
                                   ': ',
                                   purrr::pluck(mod_fit$var, .x, 'var_assumptions')))

    assumptions <- unlist(assumptions)

    return(assumptions)
}



summary.maars_lm <- function(object,
                             sand = TRUE,
                             boot_emp = FALSE,
                             boot_res = FALSE,
                             boot_mul = FALSE,
                             well_specified = FALSE,
                             digits = 3, ...) {

    ## Generate main table (coefficients estimates etc)
    out_summ <- get_var_tidy_summary(mod_fit = object,
                                     sand = sand,
                                     boot_emp = boot_emp,
                                     boot_res = boot_res,
                                     boot_mul = boot_mul,
                                     well_specified = well_specified
    )
    # add asterisks for statistical significance in a separate tibble
    significance_ast <- out_summ %>%
        dplyr::select(dplyr::starts_with('p.value')) %>%
        dplyr::mutate(dplyr::across(dplyr::everything(),
                                    ~dplyr::case_when(round(.x,3) == 0.000 ~ '***',
                                                      .x < 0.001 ~ '**',
                                                      .x < 0.01 ~ '*',
                                                      .x < 0.05 ~ '.',
                                                      TRUE ~ ' '))) %>%
        stats::setNames(paste0('signif.', stringr::str_sub(names(.), start=9)))
    # join asterisks with out_summ
    out_summ <- out_summ %>% dplyr::bind_cols(significance_ast)
    # reorder columns
    cols_prefix_order <- c('term', 'estimate', 'std.error',
                           'statistic', 'p.value', 'signif')
    out_summ <- out_summ[,cols_prefix_order %>%
                             purrr::map(~ names(out_summ)[
                                 grepl(., names(out_summ))]) %>%
                             unlist()]
    # format p values
    out_summ <- out_summ %>%
        dplyr::mutate(dplyr::across(
            dplyr::starts_with("p.value"),
            ~ format.pval(.x, digits = 1)
        ))


    ## Get assumptions
    assumptions <- get_assumptions(mod_fit = object,
                                   sand = sand, boot_emp = boot_emp,
                                   boot_res = boot_res,
                                   boot_mul = boot_mul,
                                   well_specified = well_specified
    )

    ## NOW DO SOME CRAZY STUFF THAT WE'LL GET RID OF ONCE WE RETURN LISTS
    # create list with assumptions
    assumptions_split <- stringr::str_split(assumptions, ':')
    assumptions <- purrr::map(assumptions_split, ~.x[2])
    names(assumptions) <- purrr::map(assumptions_split, ~.x[1])
    #
    out_summ <- names(assumptions) %>%
        purrr::map(
            ~ out_summ[,c('term', 'estimate',
                                  colnames(out_summ)[grepl(.x, colnames(out_summ))])])
    names(out_summ) <- names(assumptions)
    #

    cat("\nCall:\n",
        stringr::str_c(rlang::expr_deparse(x = object$call),
                       sep = "\n",
                       collapse = "\n"
        ),
        "\n\n",
        sep = ""
    )

    # old way - change with purrr?
    for(est in names(assumptions)){
        cat("\n---\n")
        cat(est, '\n')
        print.data.frame(out_summ[[est]],
                         row.names = FALSE, digits = digits, right = TRUE)
        cat('\nAssumption:')
        cat(assumptions[[est]])
        cat("\n---\n")
    }

    #cat("Coefficients:\n")
    #print.data.frame(out_summ, row.names = FALSE, digits = digits, right = TRUE)
    cat("\n---\n")
    cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")

    summ_lm <- summary.lm(object)
    cat(
        "\n\nResidual standard error:",
        formatC(signif(summ_lm$sigma, digits = digits)), "on",
        object$df.residual, "degrees of freedom"
    )
    cat(
        "\nMultiple R-squared:", formatC(summ_lm$r.squared, digits = digits),
        ",\tAdjusted R-squared:", formatC(summ_lm$adj.r.squared, digits = digits)
    )
    cat(
        "\nF-statistic:", summ_lm$fstatistic[1L],
        " on ", summ_lm$fstatistic[2L], " and ", summ_lm$fstatistic[3L],
        "DF,  p-value:",
        format.pval(pf(summ_lm$fstatistic[1L],
                       summ_lm$fstatistic[2L],
                       summ_lm$fstatistic[3L],
                       lower.tail = FALSE
        ))
    )

    #cat("\n---\nAssumptions:\n")
    # cat(paste0(utf8::utf8_format('\U001F3AF'),
    #            assumptions) %>% paste0(., collapse = '\n'))
}



# Comp var examples ----
suppressWarnings(devtools::load_all())
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
mss_var1 <- comp_var(mod_fit = lm_fit, boot_emp = list(B = 20, m = 200),
                    boot_res = list(B = 30),
                    boot_mul = NULL)

summary(mss_var1, boot_emp = TRUE)



