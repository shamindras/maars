#' Generate \code{maars_lm, lm} object with estimates of the variance
#'
#' Generates an object of class \code{"maars_lm", "lm"} containing
#' estimates of the variance of the coefficients in the regression
#' model.
#'
#'
#' @param mod_fit An lm (OLS) object
#' @param boot_emp (list) In the case of empirical bootstrap the expected input
#'   is of the form #'   \code{list(B = 10, m = 100)}. Here the named
#'   element \code{m} is optional e.g. \code{list(B = 10)} is valid, or passed
#'   in as an explicit \code{NULL} e.g. \code{list(B = 10, m = NULL)}.
#'   Note that technically \code{B, m} should both be positive integers,
#'   but this assertion checking is handled explicitly in the
#'   \code{\link{comp_boot_emp}} function. So although passing
#'   in \code{list(B = -15, m = -20)} will pass this function without errors,
#'   these will be addressed explicitly in \code{\link{comp_boot_emp}} as
#'   invalid inputs.
#' @param boot_mul (list) : In the case of multiplier bootstrap the expected
#'   input is of the form \code{list(B = 10, weights_type = "rademacher")}.
#'   Here the named element \code{weights_type} is optional
#'   e.g. \code{list(B = 10)} is valid, or passed in as an explicit \code{NULL}
#'   e.g. \code{list(B = 10, weights_type = NULL)}.
#'   Note that technically \code{B} should be a positive integer, and
#'   \code{weights_type} should be a character vector
#'   (see \code{\link{comp_boot_mul}}), but this assertion checking is handled
#'   explicitly in the \code{\link{comp_boot_mul}} function. So although passing
#'   in \code{list(B = -15, m = "test")} will pass this function without errors,
#'   these will be addressed explicitly in \code{\link{comp_boot_mul}} as
#'   invalid inputs.
#' @param boot_res (list) : In the case of residual bootstrap the expected
#'   input is of the form \code{list(B = 10)}. Note that technically \code{B}
#'   should be a positive integer, but this assertion checking is handled
#'   explicitly in the \code{\link{comp_boot_res}} function. So although passing
#'   in \code{list(B = -15)} will pass this function without errors,
#'   these will be addressed explicitly in \code{\link{comp_boot_res}} as
#'   invalid inputs.
#'
#' @details The "maars_lm" object is basically an "lm" object with additional
#'   attributes (the additional estimates of the coefficients variance),
#'   which are stored within "var". For example, the estimates of the empirical
#'   bootstrap will be stored within "var$var_boot_emp".
#'   Each of the nested lists contains the following elements: the type of estimator of
#'   of the variance  (\code{var_type}); An abbreviated string representing the
#'   type of the estimator of the variance  (\code{var_type_abb}); the summary
#'   statistics of \code{mod_fit} based on this estimator of the variance
#'   (e.g., standard errors and p-values) (\code{var_summary}); the assumptions
#'   under which the estimator of the variance is consistent
#'   (\code{var_assumptions}); the covariance matrix for the coefficients
#'   estimates (\code{cov_mat}).
#'
#' @return A "maars_lm" object containing the estimates of the variance of the
#'   regression coefficients, including the sandwich and the variance
#'   returned by \code{stats::lm}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Simulate data from a linear model
#' set.seed(35542)
#' n <- 1e2
#' X <- stats::rnorm(n, 0, 1)
#' y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
#'
#' # Fit the linear model using OLS (ordinary least squares)
#' mod_fit <- stats::lm(y ~ X)
#'
#' # Run the multiplier bootstrap on the fitted (OLS) linear model
#' set.seed(162632)
#' out <- comp_var(mod_fit, boot_mul = list(B = 100, weights_type = "rademacher"))
#'
#' # print output
#' print(out)
#' print(out$var$var_boot_mul)
#' }
comp_var <- function(mod_fit,
                    boot_emp = NULL,
                    boot_res = NULL,
                    boot_mul = NULL) {
  # the following condition will need to be revised once we introduce glm
  if (all(c("maars_lm", "lm") %in% class(mod_fit))) {
    attr(mod_fit, "class") <- "lm"}

  out_var <- comp_mms_var(mod_fit, boot_emp, boot_res, boot_mul)
  mod_fit[['var']] <- out_var

  attr(mod_fit, "class") <- c("maars_lm", "lm")

  return(mod_fit)
}

#' Plot \code{maars_lm, lm} object
#'
#' @param x (\code{maars_lm, lm}) : A fitted \code{maars_lm, lm} OLS object
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(454354534)
#'
#' # generate data ----
#' n <- 1e3
#' X_1 <- stats::rnorm(n, 0, 1)
#' X_2 <- stats::rnorm(n, 10, 20)
#' eps <- stats::rnorm(n, 0, 1)
#'
#' # Let's generate data and fit a well-specified OLS data and model ----
#' y <- 2 + X_1 * 1 + X_2 * 5 + eps
#' lm_fit <- stats::lm(y ~ X_1 + X_2)
#' summary(lm_fit)
#'
#' # Fit our first maars_lm object i.e. comp_var1
#' comp_var1 <- comp_var(mod_fit = lm_fit,
#'                       boot_emp = list(B = 50, m = 200),
#'                       boot_res = NULL,
#'                       boot_mul = list(B = 60))
#'
#' # Plot our maars_lm object
#' plot(comp_var1)
#' }
plot.maars_lm <- function(x, ...){
  mms_diag_plots <- get_ols_diag_plots(mod_fit = x)
  for (i in base::seq_along(mms_diag_plots)) {
    if (i == 1) {
      # For the first plot, don't ask the user for prompt
      par(ask = FALSE)
      print(mms_diag_plots[[i]])
    } else {
      # For subsequent plots, ask the user for prompts to display
      # the plots sequentially
      par(ask = TRUE)
      print(mms_diag_plots[[i]])
    }
    par(ask = FALSE)
  }
}
