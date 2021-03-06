% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maars-lm.R
\name{comp_var}
\alias{comp_var}
\title{Generate \code{maars_lm, lm} object with estimates of the variance}
\usage{
comp_var(
  mod_fit,
  boot_emp = NULL,
  boot_sub = NULL,
  boot_res = NULL,
  boot_mul = NULL
)
}
\arguments{
\item{mod_fit}{An lm (OLS) object}

\item{boot_emp}{(list) In the case of empirical bootstrap the expected input
is of the form #'   \code{list(B = 10, m = 100)}. Here the named
element \code{m} is optional e.g. \code{list(B = 10)} is valid, or passed
in as an explicit \code{NULL} e.g. \code{list(B = 10, m = NULL)}.
Note that technically \code{B, m} should both be positive integers,
but this assertion checking is handled explicitly in the
\code{\link{comp_boot_emp}} function. So although passing
in \code{list(B = -15, m = -20)} will pass this function without errors,
these will be addressed explicitly in \code{\link{comp_boot_emp}} as
invalid inputs.}

\item{boot_sub}{(list) TODO: ADD}

\item{boot_res}{(list) : In the case of residual bootstrap the expected
input is of the form \code{list(B = 10)}. Note that technically \code{B}
should be a positive integer, but this assertion checking is handled
explicitly in the \code{\link{comp_boot_res}} function. So although passing
in \code{list(B = -15)} will pass this function without errors,
these will be addressed explicitly in \code{\link{comp_boot_res}} as
invalid inputs.}

\item{boot_mul}{(list) : In the case of multiplier bootstrap the expected
input is of the form \code{list(B = 10, weights_type = "rademacher")}.
Here the named element \code{weights_type} is optional
e.g. \code{list(B = 10)} is valid, or passed in as an explicit \code{NULL}
e.g. \code{list(B = 10, weights_type = NULL)}.
Note that technically \code{B} should be a positive integer, and
\code{weights_type} should be a character vector
(see \code{\link{comp_boot_mul}}), but this assertion checking is handled
explicitly in the \code{\link{comp_boot_mul}} function. So although passing
in \code{list(B = -15, m = "test")} will pass this function without errors,
these will be addressed explicitly in \code{\link{comp_boot_mul}} as
invalid inputs.}
}
\value{
A "maars_lm" object containing the estimates of the variance of the
regression coefficients, including the sandwich and the variance
returned by \code{stats::lm}.
}
\description{
Generates an object of class \code{"maars_lm", "lm"} containing
estimates of the variance of the coefficients in the regression
model.
}
\details{
The "maars_lm" object is basically an "lm" object with additional
attributes (the additional estimates of the coefficients variance),
which are stored within "var". For example, the estimates of the empirical
bootstrap will be stored within "var$var_boot_emp".
Each of the nested lists contains the following elements: the type of estimator of
of the variance  (\code{var_type}); An abbreviated string representing the
type of the estimator of the variance  (\code{var_type_abb}); the summary
statistics of \code{mod_fit} based on this estimator of the variance
(e.g., standard errors and p-values) (\code{var_summary}); the assumptions
under which the estimator of the variance is consistent
(\code{var_assumptions}); the covariance matrix for the coefficients
estimates (\code{cov_mat}).
}
\examples{
\dontrun{
# Simulate data from a linear model
set.seed(35542)
n <- 1e2
X <- stats::rnorm(n, 0, 1)
y <- 2 + X * 1 + stats::rnorm(n, 0, 1)

# Fit the linear model using OLS (ordinary least squares)
mod_fit <- stats::lm(y ~ X)

# Run the multiplier bootstrap on the fitted (OLS) linear model
set.seed(162632)
out <- comp_var(mod_fit, boot_mul = list(B = 100, weights_type = "rademacher"))

# print output
print(out)
print(out$var$var_boot_mul)
}
}
