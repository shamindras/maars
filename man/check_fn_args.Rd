% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-common.R
\name{check_fn_args}
\alias{check_fn_args}
\title{Check whether the arguments in the function are correctly specified}
\usage{
check_fn_args(n = NULL, B = NULL, m = NULL)
}
\arguments{
\item{n}{Sample size.}

\item{B}{Bootstrap repetitions or number of bootstrap samples to be drawn.}

\item{m}{Number of observations to be sampled with replacement from the
dataset for each bootstrap repetition.}
}
\value{
(logical) returns \code{TRUE} if the logical assertions are satisfied
}
\description{
\code{check_fn_args} is used to assess whether the arguments
are correctly specified and returns an error message if
they do not match the correct specification
}
\keyword{internal}
