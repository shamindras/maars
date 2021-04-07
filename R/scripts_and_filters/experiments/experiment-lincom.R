# A demo on simple linear regression!
# Comp var examples ----
devtools::load_all()
set.seed(1243434)

# generate modeling data ----
n <- 1e3
x <- stats::rnorm(n, 0, 1)
y <- 2 + x * 1 + 1 * x^{2} + exp(0.2 * abs(x)) * rnorm(n)

lm_fit <- lm(y ~ x)

mms_var <- comp_var(lm_fit)


lincom <- function(coef_est, R, r, cov_mat){
    browser()


    # look at page 58 of Wooldridge
    # check that rank of R is equal to the length of r and that nrow(R)<=ncol(R)

    chisq_stats <- as.vector(t(R %*% coef_est - r) %*%
        solve(R %*% cov_mat %*% t(R),
              R %*% coef_est - r))

   R_rank <- nrow(R)
   # think about this
   out <- tibble::tibble(
       chisq.stat = chisq_stats,
       p.value = 1 - pchisq(chisq_stats, df = R_rank)
   )
   return(out)
}

# look at Stata
# https://www.stata.com/manuals/rlincom.pdf


get_lincomtest <- function(mod_fit, R,
                   r,
                   sand = NULL,
                   boot_emp = NULL,
                   boot_res = NULL,
                   boot_mul = NULL,
                   well_specified = NULL
                   ){

    req_var_nms <- check_fn_args_summary(
        mod_fit = mod_fit,
        sand = sand,
        boot_emp = boot_emp,
        boot_res = boot_res,
        boot_mul = boot_mul,
        well_specified = well_specified
    )

    # extract each of the variance estimators
    comp_var_ind_filt <- purrr::map(
        .x = req_var_nms,
        .f = ~ purrr::pluck(mod_fit, 'var', .x)
    ) %>%
        # fix this
        setNames(stringr::str_replace(req_var_nms, 'var_', ''))

    comp_var_ind_filt %>%
        purrr::map_dfr(~ lincom(
            coef_est = purrr::pluck(.x, 'var_summary') %>% dplyr::pull(estimate),
            R = R,
            r = r,
            cov_mat = purrr::pluck(.x, 'cov_mat')),
            .id = 'var_type_abb')

}


get_lincomtest(mms_var, R = matrix(c(0,1,1,0), ncol = 2), r = c(0,0))


# check lincom vs function from car package
lincom(coef_est = coef(lm_fit),
       R = matrix(c(0,1), ncol = 2),
       r = 0,
       cov_mat = vcov(lm_fit))
car::linearHypothesis(lm_fit, c(0,1), test=c("Chisq"))



lmtest::coeftest(lm_fit, sandwich::sandwich(lm_fit))
car::linearHypothesis(lm_fit, c("(Intercept) = 0", "x = 1"), test = "Chisq")
car::linearHypothesis(lm_fit, c(0,1), test = "Chisq")
car::linearHypothesis(lm_fit, matrix(c(1,1,-1,2), ncol = 2), test = "Chisq")
car::linearHypothesis(lm_fit, c("(Intercept) = 0", "z = 1"), test = "Chisq")

# https://github.com/cran/car/blob/master/R/linearHypothesis.R
