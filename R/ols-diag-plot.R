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

  # Compute the standardized residuals from the model in both vector and
  # tibble format
  std_resid <- rstandard(mod_fit)
  std_resid_tbl <- std_resid %>%
    tibble::enframe(x = ., name = NULL, value = "std_residuals")

  # Fitted vs. Residuals
  p1 <- ggplot2::ggplot(
    mod_fit,
    ggplot2::aes(x = mod_fit$fitted.values, mod_fit$residuals)
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
    ggplot2::aes(x = mod_fit$fitted.values, sqrt(abs(std_resid)))
  ) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::stat_smooth(formula = y ~ x, method = "loess", na.rm = TRUE) +
    ggplot2::labs(
      title = "Scale-Location",
      x = "Fitted values",
      y = expression(sqrt("|Standardized residuals|"))
    ) +
    ggplot2::theme_bw()

  # Residual vs. Leverage Plot
  p4 <- ggplot2::ggplot(
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

  out <- list(p1 = p1, p2 = p2, p3 = p3, p4 = p4)
  base::return(out)
}
