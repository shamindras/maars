# A demo on simple linear regression!
# Comp var examples ----
devtools::load_all()
set.seed(1243434)

# TODO: print the model that we are testing

# generate modeling data ----
n <- 1e3
x <- stats::rnorm(n, 0, 1)
xd <- sample(c("a", "b"), replace = TRUE, size = n)
y <- 2 + x * 1 + rnorm(n) # + 1 * x^{2} + 0.5*ifelse(xd=='a', 1, 0) + exp(0.2 * abs(x)) * rnorm(n)

lm_fit <- lm(y ~ x)

mms_var <- comp_var(lm_fit)


# test R
# test individual coefficients
# look at Stata
# https://www.stata.com/manuals/rlincom.pdf
# https://www.stata.com/manuals/rtest.pdf page 21



# rename lincom -> test_lincom OR test_wald
# call it wald test
# add possibility to do Bonferroni correction?
test_mms_lincom <- function(coef_est, R, r, cov_mat) {
  # add "alt" option

  coef_est <- as.matrix(coef_est)
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
    chisq = chisq_stats,
    df = R_rank,
    p.value = 1 - pchisq(chisq_stats, df = R_rank)
  )

  return(out)
}


test_lincom <- function(mod_fit,
                           R,
                           r,
                           sand = NULL,
                           boot_emp = NULL,
                           boot_sub = NULL,
                           boot_res = NULL,
                           boot_mul = NULL,
                           well_specified = NULL) {
  req_var_nms <- check_fn_args_summary(
    mod_fit = mod_fit,
    sand = sand,
    boot_emp = boot_emp,
    boot_sub = boot_sub,
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
    purrr::map_dfr(.f = ~ test_mms_lincom(
      # for coef_est, should we use the estimates from
      coef_est = purrr::pluck(.x, "var_summary") %>% dplyr::pull(estimate),
      R = R,
      r = r,
      cov_mat = purrr::pluck(.x, "cov_mat")
    ),
    .id = "var_type_abb"
    )
  class(out) <- c("test.maars_lm", class(out))
  return(out)
}

print.test.maars <- function(x,  ...) {
  x %>%
    dplyr::mutate(
      sig = stats::symnum(.data$p.value,
        corr = FALSE, na = FALSE,
        legend = FALSE,
        cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
        symbols = c("***", "**", "*", ".", " ")
      ),
      p.value = format.pval(.data$p.value)
    ) %>%
    print.data.frame(x = .)
}


test_wald <- function(mod_fit,
                      alt = NULL,
                      sand = NULL,
                      boot_emp = NULL,
                      boot_sub = NULL,
                      boot_res = NULL,
                      boot_mul = NULL,
                      well_specified = NULL) {
  req_var_nms <- check_fn_args_summary(
    mod_fit = mod_fit,
    sand = sand,
    boot_emp = boot_emp,
    boot_sub = boot_sub,
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

  out <- purrr::map_dfr(
    .x = comp_var_ind_filt,
    .f = ~ test_wald_indcoef(
      coef_est = purrr::pluck(.x, "var_summary") %>%
        dplyr::pull(estimate),
      cov_mat = purrr::pluck(.x, "cov_mat")
      ),
    .id = 'var_type_abb')
  class(out) <- c("test.maars", class(out))
  return(out)
}

test_wald_indcoef <- function(coef_est, cov_mat){

  d <- length(coef_est)
  R_iter <- diag(d)

  wald_stats <- purrr::map_dfr(.x = 1:d,
                               .f = ~ test_mms_lincom(
                                 coef_est = coef_est,
                                 R = matrix(R_iter[.x,], ncol = d),
                                 r = 0,
                                 cov_mat = cov_mat
                               )) %>%
  dplyr::select(chisq, p.value)

  out <- tibble::tibble(
    term = colnames(cov_mat),
    estimate = coef_est,
    std.error = sqrt(diag(cov_mat)),
    statistic = sqrt(wald_stats$chisq),
    p.value = wald_stats$p.value
  )

  out
}



# test linear combination of coefficients
test_lincom(mms_var,
  R = matrix(c(1, 0, 0, 1), ncol = 2),
  r = c(0, 0)
)
# show erorr handling
test_lincom(mms_var,
  R = matrix(c(1, 0, 0, 0), ncol = 2),
  r = c(0, 0)
)
# show error handling
test_lincom(mms_var,
  R = matrix(c(1, 0), ncol = 1),
  r = 0
)


# check lincom vs function from car package
test_lincom(
  mms_var,
  R = matrix(c(0, 1, 1, 1), ncol = 2),
  r = c(0,0)
)
# lincom(
#   coef_est = coef(lm_fit),
#   R = matrix(c(0, 1, 1, 1), ncol = 2),
#   r = c(0,0),
#   cov_mat = vcov(lm_fit)
# )
car::linearHypothesis(lm_fit,
                      matrix(c(0, 1, 1, 1), ncol = 2),
                      test = c("Chisq"))
(coef(lm_fit)[2]/(sqrt(diag(vcov(lm_fit)))[2]))^2
# should we add additional output?


# our function vs lmtest
test_wald(mms_var)
lmtest::coeftest(lm_fit, sandwich::sandwich(lm_fit))
# this function will be used within all the individual variance functions
# leveraged by comp_var
# how to do the one-sided test???






# other experiments ----


lincom(
  coef_est = coef(lm_fit),
  R = matrix(c(0, 1), ncol = 2),
  r = 0,
  cov_mat = vcov(lm_fit)
)



lincom(
  coef_est = coef(lm_fit),
  R = matrix(c(0, 1, 1, 2, 23, 3), ncol = 3),
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

# try anova with as.factor

mm <- sample(c(1, 2), replace = TRUE, size = 12)
size <- c(3, 4, 5, 6, 4, 5, 6, 7, 7, 8, 9, 10)
pop <- c("A", "A", "A", "A", "B", "B", "B", "B", "C", "C", "C", "C")
gg <- sample(c("c", "d", "f", "g"), replace = TRUE, size = 12)
anova(lm(size ~ pop + gg + mm))
aov.model <- aov(size ~ pop + gg + mm)
summary(aov.model)
