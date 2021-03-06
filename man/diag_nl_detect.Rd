% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-diagnostics.R
\name{diag_nl_detect}
\alias{diag_nl_detect}
\title{Obtain the "nonlinearity detection" model diagnostic}
\usage{
diag_nl_detect(mod_fit, coef_rwgt)
}
\arguments{
\item{mod_fit}{An object of class \code{\link[stats]{lm}} or \code{\link[stats]{glm}} to fit on the data.
This object should contain the formula, the data, and, in case of
\code{\link[stats]{glm}}, the family.}

\item{coef_rwgt}{A tibble containing the number of the bootstrapped data set
(\code{b}), the size of each bootstrapped data set (\code{m}), the value of
the reweighting centers (\code{term_rwgt_center}) and of the reweighted
term (\code{term_rwgt}), and the estimates of the regression coefficients
(\code{term} and \code{estimate}). This tibble can be created via the
\code{\link{diag_fit_reg_rwgt}} function.}
}
\value{
A ggplot2 object which shows how the estimates of all coefficients
vary under reweighting of their own regressors.
The vertical axis represents the estimates of the coefficients under
reweighting of the one regressor, whose names appear in the panels
titles. The horizontal axis shows the values of the regressors. The grey
lines correspond to the traces of bootstrapped estimates forming the
"spaghetti plot". The black vertical lines indicate 95\% confidence
intervals computed via the percentile method for the estimates on the
bootstrapped data sets for the estimates based on each of the centers of
reweighting (\code{term_rwgt_center}). The black line in the middle
corresponds to the mean of the estimates and is approximately equal to the
estimates on the original data (under reweighting). The blue dashed lines
correspond exactly to the original estimate of the coefficients from
\code{mod_fit}.
}
\description{
\code{diag_nl_detect} returns the "nonlinearity detection" model
diagnostic as described in
\insertCite{@see @buja2019modelsasapproximationspart2;textual}{maars}.
This graphical tool provides insights into the marginal nonlinear behavior
of response surfaces for each of the regressors.
More specifically, based on the estimates under reweighting of the regressors
returned by \code{\link{diag_fit_reg_rwgt}} and specified in \code{coef_rwgt}, this
function shows how the estimate of each coefficient varies under
reweighting of its own regressor and compares it with the original
estimates from \code{mod_fit}.
}
\examples{
\dontrun{
set.seed(1332423)
# Get nonlinearity detection plot
n <- 1e3
X1 <- stats::rnorm(n, 0, 1)
X2 <- stats::rnorm(n, 0, 3)
y <- 2 + X1 + X2 * 0.3 + stats::rnorm(n, 0, 1)
reg_df <- tibble::tibble(y = y, X1 = X1, X2 = X2, n_obs = 1:length(X1))
mod_fit <- stats::lm(y ~ X1 + X2, reg_df)
ols_rwgt <- diag_fit_reg_rwgt(mod_fit, c("X1", "X2"), B = 300)

# Display the output
diag_nl_detect(mod_fit, ols_rwgt)
}
}
\references{
\insertAllCited{}
}
