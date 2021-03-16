#' Get a list of standard diagnostic plots for \code{\link[stats]{lm}},
#' in the \code{link[ggplot2]} package style.
#'
#' @param mod_fit (maars_lm, lm) A fitted OLS \code{maars_lm, lm} class object
#'
#' @return A list of standard diagnostic plots for OLS in the
#'   \code{link[ggplot2]} package style.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' set.seed(1243434)
#'
#' # generate data
#' n <- 1e3
#' X_1 <- stats::rnorm(n, 0, 1)
#' X_2 <- stats::rnorm(n, 10, 20)
#' eps <- stats::rnorm(n, 0, 1)
#'
#' # OLS data and model
#' y <- 2 + X_1 * 1 + X_2 * 5 + eps
#' lm_fit <- stats::lm(y ~ X_1 + X_2)
#' mms_fit <- lm_fit %>% comp_var(
#'   mod_fit = .,
#'   boot_emp = list(B = 100, m = 50),
#'   boot_res = list(B = 50)
#' )
#'
#' # Produce and sequentially display diagnostic plots
#' plot(mms_fit)
#' }
get_ols_diag_plots <- function(mod_fit) {
  # Assertion checking for mod_fit is of class "maars_lm", "lm"
  assertthat::assert_that(all(c("maars_lm", "lm") == class(mod_fit)),
    msg = glue::glue("mod_fit must only be of class: ['maars_lm', 'lm']")
  )

  # Compute values from OLS fit which will be use across several plots
  cooks_distance <- stats::cooks.distance(model = mms_fit)
  hat <- stats::lm.influence(mod_fit)$hat
  fitted_values <- mod_fit$fitted.values

  # Compute the standardized residuals from the model in both vector and
  # tibble format
  std_resid <- rstandard(mod_fit)
  std_resid_tbl <- std_resid %>%
    tibble::enframe(x = ., name = NULL, value = "std_residuals")

  # Fitted vs. Residuals
  p1 <- ggplot2::ggplot(
    mod_fit,
    ggplot2::aes(x = fitted_values, mod_fit$residuals)
  ) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::stat_smooth(formula = y ~ x, method = "loess", na.rm = TRUE) +
    ggplot2::geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
    ggplot2::labs(
      title = "Residuals vs. Fitted",
      x = "Fitted values",
      y = "Residuals"
    ) +
    ggplot2::theme_bw()

  # Scale-Location Plots
  p2 <- ggplot2::ggplot(
    data = std_resid_tbl,
    ggplot2::aes(sample = std_residuals)
  ) +
    ggplot2::stat_qq(shape = 1, size = 3) + # open circles
    ggplot2::stat_qq_line() +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = "Normal Q-Q", # plot title
      x = "Theoretical Quantiles", # x-axis label
      y = "Standardized Residuals"
    ) +
    ggplot2::theme_bw()

  # Fitted vs. Standardized Residuals
  p3 <- ggplot2::ggplot(
    mod_fit,
    ggplot2::aes(x = fitted_values, sqrt(abs(std_resid)))
  ) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::stat_smooth(formula = y ~ x, method = "loess", na.rm = TRUE) +
    ggplot2::labs(
      title = "Scale-Location",
      x = "Fitted values",
      y = expression(sqrt("|Standardized residuals|"))
    ) +
    ggplot2::theme_bw()

  # Cook’s distance vs. observation number
  p4 <- ggplot2::ggplot(
    mod_fit,
    ggplot2::aes(
      x = seq_along(cooks_distance),
      y = cooks_distance
    )
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      position = "identity"
    ) +
    ggplot2::labs(
      title = "Cook's Distance",
      x = "Obs. Number",
      y = "Cook's Distance"
    ) +
    ggplot2::scale_size_continuous("Cook's Distance", range = c(0, 4)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    NULL

  # Residual vs. Leverage Plot
  p5 <- ggplot2::ggplot(
    mod_fit,
    ggplot2::aes(x = stats::lm.influence(mod_fit)$hat, std_resid)
  ) +
    ggplot2::geom_point(ggplot2::aes(size = stats::cooks.distance(mod_fit)),
      na.rm = TRUE
    ) +
    ggplot2::stat_smooth(formula = y ~ x, method = "loess", na.rm = TRUE) +
    ggplot2::labs(
      title = "Residual vs. Leverage Plot",
      x = "Leverage",
      y = "Standardized Residuals"
    ) +
    ggplot2::scale_size_continuous("Cook's Distance", range = c(1, 5)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")

  # Cook’s distance against (leverage)/(1 – leverage)
  p6 <- ggplot2::ggplot(
    mod_fit,
    ggplot2::aes(x = hat, y = cooks_distance)
  ) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::geom_smooth(formula = y ~ x, method = "loess", na.rm = TRUE) +
    ggplot2::labs(
      title = "Cook's dist vs Leverage hii/(1-hii)",
      x = "Leverage hii",
      y = "Cook's Distance"
    ) +
    ggplot2::geom_abline(
      slope = seq(0, 3, 0.5),
      color = "gray",
      linetype = "dashed"
    ) +
    ggplot2::theme_bw() +
    NULL

  # QQnorm of the estimates of the regression coefficients based on
  # one of the bootstraps available.
  # First try with empirical. If not available, use multiplier. If not available,
  # then use residual.
  boot_names <- c('var_boot_emp', 'var_boot_mul', 'var_boot_res')
  boot_vars <- boot_names %>% purrr::map(~ purrr::pluck(mod_fit, 'var', .)) %>%
    purrr::keep(~ !is.null(.x))
  if(length(boot_out)>0){
    p8 <- diag_boot_qqn(boot_out = purrr::pluck(boot_vars, 1, 'boot_out'),
                        boot_type = purrr::pluck(boot_vars, 1, 'var_type_abb'))
  } else{
    p7 <- NULL
  }

  out <- list(p1 = p1, p2 = p2, p3 = p3, p4 = p4, p5 = p5, p6 = p6, p7 = p7)
  base::return(out)
}


#' Normal QQ plot of the terms in an output of the bootstrap function
#'
#' \code{diag_boot_qqn} produces a normal QQPlot for each regressors included
#' in the estimates on the bootstrapped datasets. This function is a wrapper
#' for the \code{\link[ggplot2]{stat_qq}} function.
#'
#' @param boot_out A tibble of the model's coefficients estimated (\code{term}
#'   and \code{estimate}) on the bootstrapped datasets, the size of each
#'   bootstrapped dataset (\code{m}), the size of the original dataset
#'   (\code{n}), and the number of the bootstrap repetition (\code{b}).
#' @param boot_type (\code{character}) : The (abbreviate) type of boostrap
#'   estimates to use for the plot
#'
#' @return A ggplot2 object containing normal QQ plot for each regressor in
#'   \code{boot_out}. Each panel corresponds to a different coefficient,
#'   whose name appears in the panel's titles.
#'
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # Obtain normal QQ plot of the
#' n <- 1e3
#' X1 <- stats::rnorm(n, 0, 1)
#' X2 <- stats::rnorm(n, 0, 3)
#' y <- 2 + X1 + X2 * 0.3 + stats::rnorm(n, 0, 1)
#' df <- tibble::tibble(y = y, X1 = X1, X2 = X2, n_obs = 1:length(X1))
#'
#' # Fit a linear model (OLS) to the data
#' mod_fit <- stats::lm(y ~ X1 + X2, df)
#' mms_fit <- comp_var(mod_fit, boot_emp = list(B = 50))
#' boot_var <- purrr::pluck(mms_fit, 'var', 'var_boot_emp')
#' # Display the output
#' diag_boot_qqn(purrr::pluck(boot_var, 'boot_out'), boot_type = 'emp')
#' }
diag_boot_qqn <- function(boot_out, boot_type) {

  boot_title <- fetch_mms_emoji_title(
    var_type_abb = boot_type,
    title_type = 'title')

  out <- boot_out %>%
    tidyr::unnest(
      data = .,
      .data$boot_out
    ) %>%
    ggplot2::ggplot(
      data = .,
      ggplot2::aes(sample = .data$estimate)
    ) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line() +
    ggplot2::facet_wrap(~term, ncol = 3, scales = "free_y") +
    ggplot2::labs(
      title = glue::glue('Normal Q-Q plot - {boot_title}'),
      x = "Theoretical quantiles",
      y = "Sample quantiles"
    ) +
    ggplot2::theme_bw()
  return(out)
}


