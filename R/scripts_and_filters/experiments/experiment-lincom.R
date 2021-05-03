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


## questions:
# for test_lincom, we report the chi2 stat. Instead, for test_wald,
# we report the z stat.



test_wald <- function(coef_est, R, r, cov_mat,
                            alt = 'two.sided',
                            stat_type = 'chi2'
                            ) {

  # if R is a vector, convert it into a matrix
  coef_est <- as.matrix(coef_est)
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
    msg = glue::glue("The dimensions of r and R must be compatible.")
  )
  assertthat::assert_that(alt %in% c('two.sided', 'greater', 'less'),
                          msg = glue::glue('"alt" needs to be either ',
                                           '"two.sided", "greater", or "less"')
  )
  if(alt != 'two.sided'){
    assertthat::assert_that(nrow(R) == 1,
                            msg = glue::glue("R must have only one row when ",
                                             "the alternative hypothesis is not ",
                                             "two sided.")
    )
  }

  # compute chi square stats
  # reference: page 58 of Wooldridge
  chisq_stat <- as.vector(t(R %*% coef_est - r) %*%
    solve(
      R %*% cov_mat %*% t(R),
      R %*% coef_est - r
    ))

  # rank of R
  R_rank <- nrow(R)

  # see how computation is handled in stata
  # https://www.stata.com/support/faqs/statistics/one-sided-tests-for-coefficients/
  # https://www.stata.com/manuals15/rztest.pdf
  # compute p-value

  if(alt == 'two.sided' & stat_type == 'chi2'){
      p_value <- 1 - pchisq(chisq_stat, df = R_rank)
  } else{
    z_stat <- sqrt(chisq_stat) * sign(R %*% coef_est - r)
    if(alt == 'greater'){
      p_value <- 1 - pnorm(z_stat)[,1]
    } else if(alt == 'less'){
      p_value <- pnorm(z_stat)[,1]
    } else{
      p_value <- 2 * (1 - pnorm(abs(z_stat))[,1])
    }
  }

  # store output
  out <- tibble::tibble(
    stat.type = ifelse(stat_type == 'chi2', 'chi2', 'z'),
    statistic = ifelse(stat_type == 'chi2', chisq_stat, z_stat),
    #df = ifelse(alt == 'two.sided', R_rank, NA),
    p.value = p_value
  )

  if(stat_type == 'chi2'){
    out <- out %>%
      tibble::add_column(.data = ., df = R_rank, .before = 'p.value')
  }

  return(out)
}


test_lincom <- function(mod_fit,
                           R,
                           r,
                           alt = 'two.sided',
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
    purrr::map_dfr(.f = ~ test_wald(
      coef_est = purrr::pluck(.x, "var_summary") %>% dplyr::pull(estimate),
      R = R,
      r = r,
      cov_mat = purrr::pluck(.x, "cov_mat"),
      alt = alt
    ),
    .id = "var_type_abb"
    )
  class(out) <- c("test.maars_lm", class(out))
  return(out)
}


test_coef <- function(mod_fit,
                      alt = 'two.sided',
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
    .f = ~ test_mms_coef(
      coef_est = purrr::pluck(.x, "var_summary") %>%
        dplyr::pull(estimate),
      cov_mat = purrr::pluck(.x, "cov_mat"),
      alt = alt
      ),
    .id = 'var_type_abb')
  class(out) <- c("test.maars_lm", class(out))
  return(out)
}

# internal function for testing
test_mms_coef <- function(coef_est, cov_mat, alt = 'two.sided'){

  d <- length(coef_est) # dimension
  R_iter <- diag(d)

  wald_stats <- purrr::map_dfr(.x = 1:d,
                               .f = ~ test_wald(
                                 coef_est = coef_est,
                                 R = matrix(R_iter[.x,], ncol = d),
                                 r = 0,
                                 cov_mat = cov_mat,
                                 alt = alt,
                                 stat_type = 'z'
                               )) %>%
  dplyr::select(statistic, p.value)

  out <- tibble::tibble(
    term = colnames(cov_mat),
    estimate = coef_est,
    std.error = sqrt(diag(cov_mat)),
    statistic = wald_stats$statistic,
    p.value = wald_stats$p.value
  )

  out
}

print.test.maars_lm <- function(x,  ...) {
  x <- x %>%
    dplyr::mutate(
      sig = stats::symnum(.data$p.value,
                          corr = FALSE, na = FALSE,
                          legend = FALSE,
                          cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                          symbols = c("***", "**", "*", ".", " ")
      ),
      p.value = format.pval(.data$p.value, digits = 4)
    )
  colnames(x)[colnames(x)=='sig'] <- ''

  print.data.frame(x = x)
}


## test linear combination of coefficients
# returns chi2 stat
test_lincom(mms_var,
  R = matrix(c(1, 0, 0, 1), ncol = 2),
  r = c(0, 0)
)
# test linear combination and one-sided alternative
# returns z stat
test_lincom(mms_var,
            R = matrix(c(1, 0), ncol = 2),
            r = 0,
            alt = 'greater'
)
# test linear combination and one-sided alternative
# returns z stat
test_lincom(mms_var,
            R = matrix(c(1, 0), ncol = 2),
            r = 0,
            alt = 'less'
)

## test individual coefficients
# returns z stats but does not show the type of stat
test_coef(mms_var)


## error handling
# R has rank 1 instead of 2
test_lincom(mms_var,
  R = matrix(c(1, 0, 0, 0), ncol = 2),
  r = c(0, 0)
)
# dimensions of R and r do not match
test_lincom(mms_var,
  R = matrix(c(1, 0), ncol = 2),
  r = c(0,0)
)
# alternative hypothesis is nonsense
test_lincom(mms_var,
            R = matrix(c(1, 0), ncol = 2),
            r = 0,
            alt = 'hi'
)




## tests
# test lincom vs similar function from car package
test_that("chi2 and p-value from car package and our package coincide", {
  for(i in 1:100){
    n_constraints <- sample(1:5, 1)
    R <- matrix(rnorm(2*n_constraints), ncol = 2)
    if(Matrix::rankMatrix(R) == nrow(R)){
      mms_stats <- test_lincom(mms_var,
                               R = R,
                               r = rep(0, n_constraints))
      car_stats <- car::linearHypothesis(lm_fit,
                                         R,
                                         test = c("Chisq"))
      expect_equal(car_stats$Chisq[2], mms_stats$statistic[1], tol = 1e-2)
      expect_equal(car_stats$`Pr(>Chisq)`[2], mms_stats$p.value[1], tol = 1e-2)
    }
  }
})


test_that('z-stats and p-values from sandwich package and our package coincide', {
  # two-sided test
  mms_stats <- test_coef(mms_var, alt = 'two.sided', sand = TRUE)
  lmtest_stats <- (lmtest::coeftest(lm_fit,
                                   sandwich::sandwich(lm_fit),
                                   alternative = 'two.sided') %>%
    capture.output())[5:6] %>%
    stringr::str_squish(.) %>%
    stringr::str_split(., ' ') %>%
    purrr::map(.f = ~ .x %>% purrr::pluck(4)) %>%
    unlist() %>% as.numeric()
  expect_equal(lmtest_stats, mms_stats$statistic[1:2], tol = 1e-3)
})





