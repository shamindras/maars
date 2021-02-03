#' get_summary computes the statistics based on boot_out and boot_type
#'
#' \code{get_summary} returns a tibble containing the model's statistics
#'   based on the coefficients estimates (\code{boot_out}) obtained via
#'   \code{boot_type} bootstrap (e.g., empirical).
#'
#' @param mod_fit A \code{\link[stats]{lm}} (OLS) object.
#' @param boot_out A tibble of the model's coefficients estimated (\code{term}
#'   and \code{estimate}) on the bootstrapped datasets, the size of each
#'   bootstrapped dataset (\code{m}), the size of the original dataset
#'   (\code{n}), and the number of the bootstrap repetition (\code{b}).
#' @param boot_type A character specifying the bootstrap type. It can be be "emp"
#'   for output from \code{\link{comp_boot_emp}}, "mul"
#'   for output from \code{\link{comp_boot_mul}}, or
#'   "res" for output from \code{\link{comp_boot_res}}.
#'
#' @return A tibble containing the summary statistics for the model: terms,
#'   coefficients estimates, t-statistics, and p-values. These statistics are
#'   based on the output of the bootstrap passed in \code{boot_out}.
#'
#'
#' @keywords internal
#'
#' @importFrom Rdpack reprompt
#' @importFrom rlang .data
#'
#'
#' @return
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

    out <- boot_out %>%
        tidyr::unnest(.data$boot_out) %>%
        dplyr::rename(.data = .,
                      estimate.boot = .data$estimate) %>%
        dplyr::left_join(x = .,
                         y = broom::tidy(mod_fit) %>%
                             dplyr::select(.data = .,
                                           .data$term, .data$estimate),
                         by = "term") %>%
        dplyr::group_by(.data$term,
                        .data$estimate) %>%
        dplyr::summarise(
            .data = .,
            std.error = stats::sd(sqrt(.data$m / .data$n) * (.data$estimate.boot - mean(.data$estimate.boot))),
            p.value = mean(abs(.data$estimate - .data$estimate.boot) > abs(.data$estimate)),
            .groups = 'keep'
        ) %>%
        dplyr::mutate(statistic = .data$estimate / .data$std.error)  %>%
        dplyr::arrange(.data = ., .data$term) %>%
        dplyr::relocate(.data$statistic, .after = estimate)
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
#' @param boot_emp to add
#' @param boot_mul to add
#' @param boot_res to add
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

    # Compute the sandwich estimator by default
    out_sand <- comp_sand_var(mod_fit)

    # Initialize all other bootstrap variance computations to NULL values
    boot_out_emp <- NULL
    boot_out_mul <- NULL
    boot_out_res <- NULL

    # Override boostrap NULL bootstrap variance calculations if any of the
    # input values passed in are not NULL
    if(!is.null(boot_emp)){
        assertthat::assert_that('B' %in% names(boot_emp),
                                msg = glue::glue("boot_emp must contain a value for B"))
        # we should probably set m=sample size by default
        assertthat::assert_that('m' %in% names(boot_emp),
                                msg = glue::glue("boot_emp must contain a value for m"))
        boot_out_emp <- comp_boot_emp(mod_fit = mod_fit,
                                      B = boot_emp$B,
                                      m = boot_emp$m)
    }
    if(!is.null(boot_res)){
        assertthat::assert_that('B' %in% names(boot_res),
                                msg = glue::glue("boot_res must contain a value for B"))
        boot_out_res <- comp_boot_res(mod_fit = mod_fit, B = boot_res$B)
    }
    if(!is.null(boot_mul)){
        assertthat::assert_that('B' %in% names(boot_mul),
                                msg = glue::glue("boot_mul must contain a value for B"))
        assertthat::assert_that('weights_type' %in% names(boot_mul),
                                msg = glue::glue("boot_mul must contain a value for weights_type"))
        boot_out_mul <- comp_boot_mul(mod_fit = mod_fit,
                                      B = boot_mul$B,
                                      weights_type = boot_mul$weights_type)
    }

    # Combine all output into a single list of lists
    out <- list(var_sand = out_sand,
                var_boot_emp = boot_out_emp,
                var_boot_mul = boot_out_mul,
                var_boot_res = boot_out_res
                )
    return(out)
}
