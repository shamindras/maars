#' Get a list of standard diagnostic plots for \code{\link[stats]{lm}},
#' in the \code{link[ggplot2]} package style.
#'
#' @param mod_fit A \code{\link[stats]{lm}} (OLS) object.
#'
#' @return A list of standard diagnostic plots for OLS in the
#'   \code{link[ggplot2]} package style.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'
#' }
get_lm_diag_plots <- function(mod_fit){
    # Assertion checking for mod_fit is of class "maars_lm", "lm"
    # TODO: Switch to stronger assertion checking for maars_lm soon
    # assertthat::assert_that(all(c("maars_lm", "lm") == class(mod_fit)),
    #                         msg = glue::glue("mod_fit must only be of class: ['maars_lm', 'lm']")
    # )

    # TODO: Remove and replace this weaker assertion checking just for "lm"
    #       with assertion checking for maars_lm, lm objects
    assertthat::assert_that(all("lm" == class(mod_fit)),
                            msg = glue::glue("lm_object must only be of class lm")
    )

    # Compute the standardized residuals from the model
    std_resid <- rstandard(mod_fit)
    std_resid_tbl <- std_resid %>%
        tibble::enframe(x = ., name = NULL, value = "std_residuals")

    # Fitted vs. Residuals
    p1 <- ggplot2::ggplot(
        mod_fit,
        aes(x = mod_fit$fitted.values, mod_fit$residuals)
    ) +
        ggplot2::geom_point(na.rm = TRUE) +
        ggplot2::stat_smooth(formula = y ~ x, method = "loess", na.rm = TRUE) +
        ggplot2::geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
        ggplot2::labs(
            title = "Residuals vs. Fitted",
            x = "Fitted values",
            y = "Residuals"
        ) +
        ggplot2::theme_bw() +
        NULL

    # Scale-Location Plots
    p2 <- ggplot2::ggplot(data = std_resid_tbl, aes(sample = std_residuals)) +
        ggplot2::stat_qq(shape = 1, size = 3) + # open circles
        ggplot2::stat_qq_line() +
        ggplot2::theme_bw() +
        ggplot2::labs(
            title = "Normal Q-Q", # plot title
            x = "Theoretical Quantiles", # x-axis label
            y = "Standardized Residuals"
        ) +
        ggplot2::theme_bw() +
        NULL

    # Fitted vs. Standardized Residuals
    p3 <- ggplot2::ggplot(
        mod_fit,
        aes(x = mod_fit$fitted.values, sqrt(abs(std_resid)))
    ) +
        ggplot2::geom_point(na.rm = TRUE) +
        ggplot2::stat_smooth(formula = y ~ x, method = "loess", na.rm = TRUE) +
        ggplot2::labs(
            title = "Scale-Location",
            x = "Fitted values",
            y = expression(sqrt("|Standardized residuals|"))
        ) +
        ggplot2::theme_bw() +
        NULL

    # Residual vs. Leverage Plot
    p4 <- ggplot2::ggplot(
        mod_fit,
        aes(x = stats::lm.influence(mod_fit)$hat, std_resid)) +
        ggplot2::geom_point(aes(size = stats::cooks.distance(mod_fit)),
                            na.rm = TRUE) +
        ggplot2::stat_smooth(formula = y ~ x, method = "loess", na.rm = TRUE) +
        ggplot2::labs(
            title = "Residual vs. Leverage Plot",
            x = "Leverage",
            y = "Standardized Residuals"
        ) +
        ggplot2::scale_size_continuous("Cook's Distance", range=c(1,5)) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position="bottom") +
        NULL

    out <- list(p1 = p1, p2 = p2, p3 = p3, p4 = p4)
    base::return(out)
}

# devtools::load_all()
library(tidyverse)
set.seed(1243434)

# generate data
n <- 1e3
X_1 <- stats::rnorm(n, 0, 1)
X_2 <- stats::rnorm(n, 10, 20)
eps <- stats::rnorm(n, 0, 1)

# OLS data and model
y <- 2 + X_1 * 1 + X_2 * 5 + eps
lm_fit <- stats::lm(y ~ X_1 + X_2)

# Clear all existing plots - may throw a error if there are no plots to clear
# Uncomment the following line to clear plots
# dev.off()

# Check against the standard lm plot method
# Uncomment the following line to run the standard base lm plots
# plot(lm_fit)

# Clear all existing plots - may throw a error if there are no plots to clear
dev.off()

# Run our ggplot2 based diagnostics
mms_diag_plots <- get_lm_diag_plots(mod_fit = lm_fit)
mms_diag_plots

# Add in plot methods with
for (i in seq_along(mms_diag_plots)) {
    if (i == 1) {
        # For the first plot, don't ask the user for prompt
        par(ask = FALSE)
        print(mms_diag_plots[[i]])
    } else {
        # For subsequent plots, ask the user for prompts to display the plots
        # sequentially
        par(ask = TRUE)
        print(mms_diag_plots[[i]])
    }
    par(ask = FALSE)
}

# This is just a prototype - open for discussion
# plot.maars_lm <- function(mod_fit, ...){
#     mms_diag_plots <- get_lm_diag_plots(mod_fit = lm_fit)
#     for (i in seq_along(mms_diag_plots)) {
#         if (i == 1) {
#             # For the first plot, don't ask the user for prompt
#             par(ask = FALSE)
#             print(mms_diag_plots[[i]])
#         } else {
#             # For subsequent plots, ask the user for prompts to display the plots
#             # sequentially
#             par(ask = TRUE)
#             print(mms_diag_plots[[i]])
#         }
#         par(ask = FALSE)
#     }
# }
