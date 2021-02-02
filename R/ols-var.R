
get_summary <- function(mod_fit, boot_out, boot_type) {
    assertthat::assert_that(all("lm" == class(mod_fit)) | any("glm" == class(mod_fit)),
                            msg = glue::glue("mod_fit must only be of class lm or glm"))
    assertthat::assert_that("tbl_df" %in% class(boot_out),
                            msg = glue::glue("boot_out must only be of class tibble"))
    assertthat::assert_that(boot_type %in% c("emp", "mul", "res"),
                            msg = glue::glue("boot_out must only be a character taking values either 'emp' or 'mul'"))

    if(boot_type %in% c("mul", "res")){
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
                             dplyr::select(.data = .,
                                           .data$term, .data$estimate),
                         by = "term") %>%
        dplyr::group_by(.data = .,
                        .data$term,
                        .data$estimate) %>%
        dplyr::summarise(
            .data = .,
            std.error.boot = stats::sd(sqrt(.data$m / .data$n) * (.data$estimate.boot - mean(.data$estimate.boot))),
            p.value.boot = mean(abs(.data$estimate - .data$estimate.boot) > abs(.data$estimate)),
            .groups = 'keep'
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


#' Create an empirical or multiplier bootstrap summary table
#'
#' \code{comp_var} creates an OLS bootstrap summary table for
#' empirical bootstrap output or multiplier bootstrap output as generated from
#' \code{\link{comp_boot_emp}} or
#' \code{\link{comp_boot_mul}}, respectively.
#'
#'
#' @param mod_fit An lm (OLS) object
#' @param boot_out A tibble of the bootstrap calculations from either
#'   \code{\link{comp_boot_emp}} or
#'   \code{\link{comp_boot_mul}}, respectively.
#' @param boot_type A character specifying the bootstrap type. Must be "emp"
#'   for output from \code{\link{comp_boot_emp}} or "mult"
#'   for output from \code{\link{comp_boot_mul}}, respectively.
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
#' coef_emp_boot <- comp_boot_emp(mod_fit = mod_fit, B = 1000)
#' out <- comp_var(mod_fit, boot_mul = list(B = 100, weights_type = 'rademacher'))
#'
#' # print output
#' print(out)
#' }
comp_var <- function(mod_fit, boot_emp = NULL, boot_res = NULL, boot_mul = NULL) {
    assertthat::assert_that(all("lm" == class(mod_fit)) | any("glm" == class(mod_fit)),
                            msg = glue::glue("mod_fit must only be of class lm or glm"))

    out <- comp_sand_var(mod_fit)
    if(!is.null(boot_emp)){
        assertthat::assert_that('B' %in% names(boot_emp),
                                msg = glue::glue("boot_emp must contain a value for B"))
        # we should probably set m=sample size by default
        assertthat::assert_that('m' %in% names(boot_emp),
                                msg = glue::glue("boot_emp must contain a value for m"))
        boot_out_emp <- comp_boot_emp(mod_fit = mod_fit, B = boot_emp$B, m = boot_emp$m)
        tidy_emp <- get_summary(mod_fit = mod_fit, boot_out = boot_out_emp, boot_type = 'emp')
        out <- out %>% dplyr::left_join(tidy_emp,
                                        by = c('term', 'std.error', 'estimate',
                                               'statistic', 'p.value'))
    }
    if(!is.null(boot_res)){
        assertthat::assert_that('B' %in% names(boot_res),
                                msg = glue::glue("boot_res must contain a value for B"))
        boot_out_res <- comp_boot_res(mod_fit = mod_fit, B = boot_res$B)
        tidy_res <- get_summary(mod_fit = mod_fit, boot_out = boot_out_res, boot_type = 'res')
        out <- out %>% dplyr::left_join(tidy_res,
                                        by = c('term', 'std.error', 'estimate',
                                               'statistic', 'p.value'))
    }
    if(!is.null(boot_mul)){
        assertthat::assert_that('B' %in% names(boot_mul),
                                msg = glue::glue("boot_mul must contain a value for B"))
        assertthat::assert_that('weights_type' %in% names(boot_mul),
                                msg = glue::glue("boot_mul must contain a value for weights_type"))
        boot_out_mul <- comp_boot_mul(mod_fit = mod_fit, B = boot_mul$B,
                                      weights_type = boot_mul$weights_type)
        tidy_mul <- get_summary(mod_fit, boot_out = boot_out_mul, boot_type = 'mul')
        out <- out %>% dplyr::left_join(tidy_mul,
                                        by = c('term', 'std.error', 'estimate',
                                               'statistic', 'p.value'))
    }

    return(out)
}
