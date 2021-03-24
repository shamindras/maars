#' This dataset contains information collected by the U.S Census Service
#' concerning housing in the area of Boston Mass.
#'
#' This dataset has 516 observations and 14 variables. It contains information
#' collected by the U.S Census Service oncerning housing in the area of
#' Boston Mass. It was obtained from the StatLib archive
#' \url{http://lib.stat.cmu.edu/datasets/boston}, and has been used
#' extensively throughout the literature to benchmark algorithms. Note that
#' the details of this dataset were sourced
#' from \url{https://www.cs.toronto.edu/~delve/data/boston/bostonDetail.html}.
#' The data was originally published in \insertCite{@see @harrison1978hedpricesdemandcleanairbhdata;textual}{maars}
#'
#' @importFrom Rdpack reprompt
#'
#' @references \insertAllCited{}
#'
#' @format A \code{tibble} with 516 rows and 14 variables:
#' \describe{
#'   \item{crim}{per capita crime rate by town}
#'   \item{zn}{proportion of residential land zoned for lots over 25,000 sq.ft.}
#'   \item{indus}{proportion of non-retail business acres per town.}
#'   \item{chas}{Charles River dummy variable (1 if tract bounds river; 0 otherwise)}
#'   \item{nox}{nitric oxides concentration (parts per 10 million)}
#'   \item{rm}{average number of rooms per dwelling}
#'   \item{age}{proportion of owner-occupied units built prior to 1940}
#'   \item{dis}{weighted distances to five Boston employment centres}
#'   \item{rad}{index of accessibility to radial highways}
#'   \item{tax}{full-value property-tax rate per $10,000}
#'   \item{ptratio}{pupil-teacher ratio by town}
#'   \item{b}{1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town}
#'   \item{lstat}{ lower status of the population}
#'   \item{medv}{Median value of owner-occupied homes in $1000's}
#' }
#' @source \url{http://lib.stat.cmu.edu/datasets/boston}
"boston_housing"
