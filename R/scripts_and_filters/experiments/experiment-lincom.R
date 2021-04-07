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

    t_stat <- t(R %*% coef_est -r) %*%
        solve(R %*% cov_mat %*% t(R),
              R %*% coef_est - r)

   # think about this
}



lincom <- function(mod_fit, R, r,
                   sand = NULL,
                   boot_emp = NULL,
                   boot_res = NULL,
                   boot_mul = NULL,
                   well_specified = NULL
                   ){

    browser()
    req_var_nms <- check_fn_args_summary(
        mod_fit = mod_fit,
        sand = sand,
        boot_emp = boot_emp,
        boot_res = boot_res,
        boot_mul = boot_mul,
        well_specified = well_specified
    )


    comp_var_ind_filt <- purrr::map(
        .x = req_var_nms,
        .f = ~ purrr::pluck(mod_fit, 'var', .x)
    )


    comp_var_ind_filt %>%
        purrr::map(~ mms_lincom(
            coef_est = purrr::pluck(.x, 'var_summary') %>% dplyr::pull(estimate),
            R = R,
            r = r,
            purrr::pluck(.x, 'cov_mat')))

}


lincom(mms_var, 1)

lmtest::coeftest(lm_fit, sandwich::sandwich(lm_fit))
car::linearHypothesis(lm_fit, c("(Intercept) = 0", "x = 1"), test = "Chisq")
car::linearHypothesis(lm_fit, c(0,1), test = "Chisq")
car::linearHypothesis(lm_fit, matrix(c(1,1,-1,2), ncol = 2), test = "Chisq")
car::linearHypothesis(lm_fit, c("(Intercept) = 0", "z = 1"), test = "Chisq")

# https://github.com/cran/car/blob/master/R/linearHypothesis.R
