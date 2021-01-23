#' Create an empirical or multiplier bootstrap summary table
#'
#' \code{comp_bootstrap_summary} creates an OLS bootstrap summary table for
#' empirical bootstrap output or multiplier bootstrap output as generated from
#' \code{\link{comp_empirical_bootstrap}} or
#' \code{\link{comp_multiplier_bootstrap_var}}, respectively.
#'
#'
#' @param mod_fit An lm (OLS) object
#' @param boot_out A tibble of the bootstrap calculations from either
#'   \code{\link{comp_empirical_bootstrap}} or
#'   \code{\link{comp_multiplier_bootstrap_var}}, respectively.
#' @param boot_type A character specifying the bootstrap type. Must be "emp"
#'   for output from \code{\link{comp_empirical_bootstrap}} or "mult"
#'   for output from \code{\link{comp_multiplier_bootstrap_var}}, respectively.
#'
#' @return A summary statistics tibble for the bootstrap input.
#'
#' @export
#'
#' @importFrom rlang .data
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
#' coef_emp_boot <- comp_empirical_bootstrap(mod_fit = mod_fit, B = 1000)
#' out <- comp_bootstrap_summary(mod_fit, coef_emp_boot, 'emp')
#'
#' # print output
#' print(out)
#' }
comp_bootstrap_summary <- function(mod_fit, boot_out, boot_type) {
    assertthat::assert_that(all("lm" == class(mod_fit)) | any("glm" == class(mod_fit)),
                            msg = glue::glue("mod_fit must only be of class lm or glm"))
    assertthat::assert_that("tbl_df" %in% class(boot_out),
                            msg = glue::glue("boot_out must only be of class tibble"))
    assertthat::assert_that(boot_type %in% c("emp", "mult", "res"),
                            msg = glue::glue("boot_out must only be a character taking values either 'emp' or 'mult'"))

    if(boot_type %in% c("mult", "res")){
        boot_out <- boot_out %>% dplyr::mutate(m = nrow(model.frame(mod_fit)), n = m)
        }

    # Get model's estimate
    mod_out <- broom::tidy(x = mod_fit) %>%
        dplyr::rename(.data = .,
                      statistic = .data$statistic,
                      p.value = .data$p.value)
    mod_out_sel <- mod_out %>%
                    dplyr::select(.data = .,
                                  .data$term,
                                  .data$estimate)

    # Define colnames into separate variables, dplyr::rename does not like
    # this to be defined on the fly. TODO: Investigate a more elegant approach
    std.error.boot.colname <- stringr::str_c("std.error.boot", boot_type, sep = ".")
    p.value.boot.colname <- stringr::str_c("p.value.boot", boot_type, sep = ".")
    statistic.boot.colname <-  stringr::str_c("statistic.boot", boot_type, sep = ".")

    out <- boot_out %>%
        tidyr::unnest(data = .,
                      .data$boot_out) %>%
        dplyr::rename(.data = .,
                      estimate.boot = .data$estimate) %>%
        dplyr::left_join(x = .,
                         y = mod_out %>%
                                 dplyr::select(.data = ,
                                               .data$term, .data$estimate),
                         by = "term") %>%
        dplyr::group_by(.data = .,
                        .data$term,
                        .data$estimate) %>%
        dplyr::summarise(
              .data = .,
              std.error.boot = stats::sd(sqrt(.data$m / .data$n) * (.data$estimate.boot - mean(.data$estimate.boot))),
              p.value.boot = mean(abs(.data$estimate - .data$estimate.boot) > abs(.data$estimate))
            ) %>%
        dplyr::mutate(.data = .,
                      statistic.boot = .data$estimate / .data$std.error.boot)  %>%
        dplyr::left_join(x = .,
                         y = mod_out %>% dplyr::select(.data = .,
                                                      -.data$estimate),
                         by = "term") %>%
        # dplyr::left_join(mod_out, by = "term") %>%
        dplyr::select(.data = .,
                      .data$term,
                      .data$estimate,
                      .data$std.error,
                      .data$statistic,
                      .data$p.value,
                      .data$std.error.boot,
                      .data$statistic.boot,
                      .data$p.value.boot) %>%
        purrr::set_names(x = .,
                         nm = c("term", "estimate",
                                "std.error", "statistic", "p.value",
                                std.error.boot.colname,
                                statistic.boot.colname,
                                p.value.boot.colname)) %>%
        dplyr::arrange(.data = ., .data$term)
    return(out)
}
