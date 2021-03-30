#' @description
#' To learn more about maars, start with the vignettes:
#' `browseVignettes(package = "maars")`
#' @keywords internal
#' @importFrom Rdpack reprompt
#' @importFrom rlang .data
"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## recomended per comments here: https://stackoverflow.com/questions/66816638/no-visible-binding-for-global-variable?noredirect=1#comment118198647_66816638
## previously used here: https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R#L15-L16
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
