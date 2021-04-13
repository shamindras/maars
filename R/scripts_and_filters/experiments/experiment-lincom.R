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


# introduce option for F-test for residual bootstrap & well specified

lincom <- function(coef_est, R, r, cov_mat){
    #browser()
    coef_est <- as.vector(coef_est)
    #browser()
    r <- matrix(as.vector(r), ncol = 1)
    if(is.vector(R)) R <- matrix(R, nrow = 1)
    # look at page 58 of Wooldridge
    # check that rank of R is equal to the length of r and that nrow(R)<=ncol(R)
    assertthat::assert_that(is.matrix(R) || is.vector(R),
                           msg = glue::glue("R must be a vector or a matrix."))
    assertthat::assert_that(is.vector(R) || (is.matrix(R) &&
                                                 (nrow(R)<= ncol(R) &
                                Matrix::rankMatrix(R)<=ncol(R))),
                            msg = glue::glue("The rank of R must be equal to the number of rows.")
    )
    assertthat::assert_that(nrow(r) == nrow(R),
                            msg = glue::glue("r and R must be compatible.")
    )
    # check also coefficients
    # assertthat::assert_that((is.vector(r) && (length(r) == nrow(R))) ||
    #                              (is.matrix(r) && (ncol(r) == 1 & nrow(r) == nrow(R))),
    #                          msg = glue::glue("r and R must be compatible")
    # )
    # assertthat::assert_that((is.vector(coef_est) && (length(coef_est) == ncol(R))) ||
    #                              (is.matrix(coef_est) && (ncol(coef_est) == 1 & nrow(coef_est) == ncol(R))),
    #                          msg = glue::glue("coef_est and R must be compatible")
    # )

    chisq_stats <- as.vector(t(R %*% coef_est - r) %*%
        solve(R %*% cov_mat %*% t(R),
              R %*% coef_est - r))

   R_rank <- Matrix::rankMatrix(R)
   # think about this
   out <- tibble::tibble(
       #type.stat = 'chisq',
       #statistic = chisq_stats,
       chisq = chisq_stats,
       df = R_rank,
       p.value = 1 - pchisq(chisq_stats, df = R_rank)
   )
   return(out)
}

# look at Stata
# https://www.stata.com/manuals/rlincom.pdf
# https://www.stata.com/manuals/rtest.pdf page 21


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

    out <- comp_var_ind_filt %>%
        purrr::map_dfr(~ lincom(
            coef_est = purrr::pluck(.x, 'var_summary') %>% dplyr::pull(estimate),
            R = R,
            r = r,
            cov_mat = purrr::pluck(.x, 'cov_mat')),
            .id = 'var_type_abb')

    return(out)
}


# test linear combination of coefficients
get_lincomtest(mms_var,
               R = matrix(c(1,0,0,1),ncol = 2),
               r = c(0,0))
# test


# check lincom vs function from car package
lincom(coef_est = coef(lm_fit),
       R = matrix(c(0,1), ncol = 2),
       r = 0,
       cov_mat = vcov(lm_fit))
lincom(coef_est = coef(lm_fit),
       R = matrix(c(0,1,1,2), ncol = 2),
       r = c(0,1),
       cov_mat = vcov(lm_fit))

# get p-value for each coefficient
diag(length(coef(lm_fit))) %>%
    apply(., 2,
          function(x) lincom(coef_est = coef(lm_fit), R = x, r = c(0), cov_mat = vcov(lm_fit)))





car::linearHypothesis(lm_fit, c(0,1,0), test=c("Chisq"))
lmtest::coeftest(lm_fit, sandwich::sandwich(lm_fit))
lmtest::waldtest(lm_fit)
car::linearHypothesis(lm_fit, c("(Intercept) = 0", "x = 1"), test = "Chisq")
car::linearHypothesis(lm_fit, c(0,1), test = "Chisq")
car::linearHypothesis(lm_fit, matrix(c(1,1,-1,2), ncol = 2), test = "Chisq")
car::linearHypothesis(lm_fit, c("(Intercept) = 0", "z = 1"), test = "Chisq")

# https://github.com/cran/car/blob/master/R/linearHypothesis.R
