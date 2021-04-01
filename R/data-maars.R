#' LA County Census tract data from \insertCite{@see @buja2019modelsasapproximationspart1;textual}{maars}
#'
#' This dataset has 505 observations, each representing a sampled metropolitan
#' LA County Census tract. It also has 7 numeric variables measuring different
#' quantities of interest in each tract. This dataset was used and sourced
#' from the paper \insertCite{@see @buja2019modelsasapproximationspart1;textual}{maars}.
#'
#' @importFrom Rdpack reprompt
#'
#' @references \insertAllCited{}
#'
#' @format A \code{tibble} with 505 rows and 7 variables:
#' \describe{
#'   \item{StreetTotal}{count, of the homeless people in each census tract}
#'   \item{MedianInc1000}{income, median income of households in each census tract}
#'   \item{PercVacant}{proportion, share of vacant lots in each census tract}
#'   \item{PercMinority}{proportion, share of non-Caucasian residents in each census tract}
#'   \item{PercResidential}{proportion, share of residential lots in each census tract}
#'   \item{PercCommercial}{proportion, share of commercial lots in each census tract}
#'   \item{PercIndustrial}{proportion, share of industrial lots in each census tract}
#' }
#' @source \url{http://www-stat.wharton.upenn.edu/~buja/STAT-961/Homeless_LA_by_Census_Tracts.csv}
"la_county"


#' This (corrected) dataset contains information collected by the U.S Census
#' Service concerning housing in the area of Boston Massachusetts.
#'
#' This dataset has 516 observations and 21 variables. It contains information
#' collected by the U.S Census Service concerning housing in the area of
#' Boston Mass. It was obtained from the StatLib archive
#' \url{http://lib.stat.cmu.edu/datasets/boston_corrected.txt}, and has been
#' used extensively throughout the literature to benchmark algorithms.
#' Note that the details of this dataset were sourced from
#' \url{https://geodacenter.github.io/data-and-lab/boston-housing/}.
#' The data was originally published in \insertCite{@see @harrison1978hedpricesdemandcleanairbhdata;textual}{maars}
#' and subsequently corrected in \insertCite{@see @gilley1996harrisonrubinfeldbhdata;textual}{maars}
#'
#' @importFrom Rdpack reprompt
#'
#' @references \insertAllCited{}
#'
#' @format A \code{tibble} with 516 rows and 21 variables:
#' \describe{
#'     \item{obs}{Sequential ID}
#'     \item{town}{A factor with levels given by town names}
#'     \item{town_no}{A numeric vector corresponding to TOWN}
#'     \item{tract}{A numeric vector of tract ID numbers}
#'     \item{lon}{A numeric vector of tract point longitudes in decimal degrees}
#'     \item{lat}{A numeric vector of tract point latitudes in decimal degrees}
#'     \item{medv}{A numeric vector of median values of owner-occupied housing in USD 1000}
#'     \item{cmedv}{A numeric vector of corrected median values of owner-occupied housing in USD 1000}
#'     \item{crim}{A numeric vector of per capita crime}
#'     \item{zn}{A numeric vector of proportions of residential land zoned for lots over 25000 sq. ft per town (constant for all Boston tracts)}
#'     \item{indus}{A numeric vector of proportions of non-retail business acres per town (constant for all Boston tracts)}
#'     \item{chas}{A factor with levels 1 if tract borders Charles River; 0 otherwise}
#'     \item{nox}{A numeric vector of nitric oxides concentration (parts per 10 million) per town}
#'     \item{rm}{A numeric vector of average numbers of rooms per dwelling}
#'     \item{age}{A numeric vector of proportions of owner-occupied units built prior to 1940}
#'     \item{dis}{A numeric vector of weighted distances to five Boston employment centers}
#'     \item{rad}{A numeric vector of an index of accessibility to radial highways per town (constant for all Boston tracts)}
#'     \item{tax}{A numeric vector full-value property-tax rate per USD 10,000 per town (constant for all Boston tracts)}
#'     \item{ptratio}{A numeric vector of pupil-teacher ratios per town (constant for all Boston tracts)}
#'     \item{b}{A numeric vector of 1000*(Bk - 0.63)^2 where Bk is the proportion of the black population}
#'     \item{lstat}{A numeric vector of percentage values of lower status population}
#' }
#' @source \url{http://lib.stat.cmu.edu/datasets/boston_corrected.txt}
"boston_housing"
