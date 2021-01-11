#' A wrapper for the residual bootstrap of a fitted OLS regression model
#'
#' \code{comp_boot_res} is a wrapper for the empirical bootstrap of
#' a fitted \code{\link[stats]{lm}} model.
#'
#' @details The residual bootstrap consists of fitting the chosen statistical
#'   model (\code{mod_fit}) \code{B} times. Each of the \code{B} datasets
#'   consists of the original independent variables and of dependent variable
#'   given by the sum of the estimates of the original model and a
#'   bootstrap versions of the residuals.
#'
#' @param mod_fit An object of class \code{\link[stats]{lm}} to fit on the data.
#'   This object should contain the formula and the data.
#' @param B Bootstrap repetitions or number of bootstrap samples to be drawn.
#'
#' @return A tibble of the model's coefficients estimated (\code{term} and
#'   \code{estimate}) on the bootstrapped datasets,
#'   the size of the original dataset (\code{n}), and the number of the
#'   bootstrap repetition (\code{b}).
#'
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # Obtain estimates of the coefficients on bootstrapped versions of the dataset
#' set.seed(35542)
#' n <- 1e3
#' X <- stats::rnorm(n, 0, 1)
#' y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
#' lm_fit <- stats::lm(y ~ X)
#' out <- comp_boot_res(lm_fit, B = 100)
#'
#' print(out)
#' }
comp_boot_res <- function(mod_fit, B = 100) {
  assertthat::assert_that(all("lm" == class(mod_fit)),
    msg = glue::glue("mod_fit must only be of class lm")
  )
 checkargs(B=B)

  mod_res <- mod_fit$res
  mod_pred <- mod_fit$fitted.values
  n <- length(mod_res)
  data <- stats::model.frame(mod_fit)
  response_name <- as.character(formula(mod_fit)[2])

  out <- as.list(1:B) %>%
    purrr::map_df(
      ~ comp_cond_model(
        mod_fit = mod_fit,
        data = data %>% dplyr::mutate(!!sym(response_name) := mod_pred + sample(mod_res, n, replace = T))
      ),
      .id = "b"
    ) %>%
    tidyr::nest(data = c(term, estimate)) %>%
    dplyr::rename(boot_out = data) %>%
    dplyr::mutate(n = n) %>%
    dplyr::relocate(n)
  return(out)
}

