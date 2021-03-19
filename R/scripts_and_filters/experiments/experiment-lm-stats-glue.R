# Comp var examples ----
devtools::load_all()
set.seed(1243434)

# generate modeling data ----
n <- 1e3
X_1 <- stats::rnorm(n, 0, 1)
X_2 <- stats::rnorm(n, 10, 20)
eps <- stats::rnorm(n, 0, 100)

# Let's generate data and fit a well-specified OLS data and model ----
y <- 2 + X_1 * 1 + X_2 * 5 + eps
lm_fit <- stats::lm(y ~ X_1 + X_2)
summary(lm_fit)

lmg <- lm_fit %>% broom::glance(x = .)
lmg_v <- lm_fit_glance %>%
  as.numeric(x = .) %>%
  purrr::set_names(x = ., nm = names(x = lmg))

FMT_NUM_DIGITS <- 4
assumptions_lm <-
  c(
    glue::glue("Residual standard error:",
      "{formatC(signif(lmg_v[['sigma']], digits = FMT_NUM_DIGITS))}",
      "on",
      "{lmg_v[['df.residual']]}",
      "degrees of freedom",
      .sep = " "
    ),
    glue::glue("Multiple R-squared:",
      "{formatC(lmg_v[['r.squared']], digits = FMT_NUM_DIGITS)},",
      "Adjusted R-squared:",
      "{formatC(lmg_v[['adj.r.squared']], digits = FMT_NUM_DIGITS)}",
      .sep = " "
    ),
    glue::glue("F-statistic:",
      "{formatC(lmg_v[['statistic']], digits = FMT_NUM_DIGITS)}",
      "on",
      "{lmg_v[['df']]} and {lmg_v[['df.residual']]} DF,",
      "p-value:",
      "{format.pval(pf(lmg_v[['statistic']],
                       lmg_v[['df']],
                       lmg_v[['df.residual']],
                       lower.tail = FALSE))}",
      .sep = " "
    )
  )
# summary(lm_fit)
glue::glue_collapse(x = assumptions_lm, sep = "\n")
