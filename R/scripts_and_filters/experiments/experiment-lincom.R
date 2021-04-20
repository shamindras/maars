# A demo on simple linear regression!
# Comp var examples ----
devtools::load_all()
set.seed(1243434)

# TODO: print the model that we are testing

# generate modeling data ----
n <- 1e3
x <- stats::rnorm(n, 0, 1)
y <- 2 + x * 1 + 1 * x^{
  2
} + exp(0.2 * abs(x)) * rnorm(n)

lm_fit <- lm(y ~ x)

mms_var <- comp_var(lm_fit)


# test R
# test individual coefficients
# look at Stata
# https://www.stata.com/manuals/rlincom.pdf
# https://www.stata.com/manuals/rtest.pdf page 21

# test: test whether one or more coefficients are equal to zero
# x1 x2 OR x1=x2
#
# introduce option for F-test for residual bootstrap & well specified

# rename lincom -> test_lincom OR test_wald
lincom <- function(coef_est, R, r, cov_mat) {

  # if R is a vector, convert it into a matrix
  if (is.vector(R)) R <- matrix(R, nrow = 1)

  assertthat::assert_that(is.matrix(R) || is.vector(R),
    msg = glue::glue("R must be a vector or a matrix.")
  )
  assertthat::assert_that((is.vector(R) & length(R) == length(r))
  || (is.matrix(R) &&
      (nrow(R) <= ncol(R) &
        Matrix::rankMatrix(R) == nrow(R))),
  msg = glue::glue("The rank of R must be equal to the number of its rows.")
  )
  assertthat::assert_that(length(r) == nrow(R),
    msg = glue::glue("r and R must be compatible.")
  )

  # reference: page 58 of Wooldridge
  chisq_stats <- as.vector(t(R %*% coef_est - r) %*%
    solve(
      R %*% cov_mat %*% t(R),
      R %*% coef_est - r
    ))

  R_rank <- nrow(R)

  out <- tibble::tibble(
    # type.stat = 'chisq',
    # statistic = chisq_stats,
    chisq = chisq_stats,
    df = R_rank,
    p.value = 1 - pchisq(chisq_stats, df = R_rank)
  )
  return(out)
}


get_lincomtest <- function(mod_fit,
                           R,
                           r,
                           sand = NULL,
                           boot_emp = NULL,
                           boot_res = NULL,
                           boot_mul = NULL,
                           well_specified = NULL) {
  req_var_nms <- check_fn_args_summary(
    mod_fit = mod_fit,
    sand = sand,
    boot_emp = boot_emp,
    boot_res = boot_res,
    boot_mul = boot_mul,
    well_specified = well_specified
  )

  # extract each of the variance estimates
  comp_var_ind_filt <- purrr::map(
    .x = req_var_nms,
    .f = ~ purrr::pluck(mod_fit, "var", .x)
  ) %>%
    # fix this
    setNames(stringr::str_replace(req_var_nms, "var_", ""))

  out <- comp_var_ind_filt %>%
    purrr::map_dfr(~ lincom(
      coef_est = purrr::pluck(.x, "var_summary") %>% dplyr::pull(estimate),
      R = R,
      r = r,
      cov_mat = purrr::pluck(.x, "cov_mat")
    ),
    .id = "var_type_abb"
    )

  return(out)
}


# test linear combination of coefficients
get_lincomtest(mms_var,
  R = matrix(c(1, 0, 0, 1), ncol = 2),
  r = c(0, 0)
)

get_lincomtest(mms_var,
               R = matrix(c(1, 0, 0, 0), ncol = 2),
               r = c(0, 0)
)

get_lincomtest(mms_var,
  R = matrix(c(1, 0), ncol = 2),
  r = 0
)


# check lincom vs function from car package
lincom(
  coef_est = coef(lm_fit),
  R = matrix(c(0, 1), ncol = 2),
  r = 0,
  cov_mat = vcov(lm_fit)
)
lincom(
  coef_est = coef(lm_fit),
  R = matrix(c(0, 1, 1, 2), ncol = 2),
  r = c(0, 1),
  cov_mat = vcov(lm_fit)
)

# get p-value for each coefficient
diag(length(coef(lm_fit))) %>%
  apply(
    ., 2,
    function(x) lincom(coef_est = coef(lm_fit), R = x, r = c(0), cov_mat = vcov(lm_fit))
  )


x <- car::linearHypothesis(lm_fit, c(0, 1, 0), test = c("Chisq"))
lmtest::coeftest(lm_fit, sandwich::sandwich(lm_fit))
lmtest::waldtest(lm_fit)
x <- car::linearHypothesis(lm_fit, c("(Intercept) = 0", "x = 1"), test = "Chisq")
car::linearHypothesis(lm_fit, c(0, 1), test = "Chisq")
car::linearHypothesis(lm_fit, matrix(c(1, 1, -1, 2), ncol = 2), test = "Chisq")
car::linearHypothesis(lm_fit, c("(Intercept) = 0", "z = 1"), test = "Chisq")


# anova
anova(lm_fit)
aov(y ~ x, data = tibble::tibble(x = x, y = y))
# https://github.com/cran/car/blob/master/R/linearHypothesis.R


