#' Assertion Checks for individual \code{\link{comp_mms_var}} function bootstrap function inputs
#'
#' \code{check_fn_args_comp_mms_var_boot_ind} is used to assess whether the arguments
#' are correctly specified in \code{list} format and returns an error message if
#' they do not match the correct specification
#'
#' @param inp_list (list) : A list containing the relevant input parameters for
#'   empirical bootstrap (\code{\link{comp_boot_emp}}),
#'   residual bootstrap (\code{\link{comp_boot_res}}), and
#'   multiplier bootstrap (\code{\link{comp_boot_mul}}).
#'   In the case of empirical bootstrap the expected input is of the form
#'   \code{list(B = 10, m = 100)}. Here the named element \code{m} is optional
#'   e.g. \code{list(B = 10)} is valid, or passed in as an explicit \code{NULL}
#'   e.g. \code{list(B = 10, m = NULL)}. Note that technically \code{B, m}
#'   should both be positive integers, but this assertion checking is handled
#'   explicitly in the \code{\link{comp_boot_emp}} function. So although passing
#'   in \code{list(B = -15, m = -20)} will pass this function without errors,
#'   these will be addressed explicitly in \code{\link{comp_boot_emp}} as
#'   invalid inputs.
#'   In the case of residual bootstrap the expected input is of the form
#'   \code{list(B = 10)}. Note that technically \code{B}
#'   should be a positive integer, but this assertion checking is handled
#'   explicitly in the \code{\link{comp_boot_res}} function. So although passing
#'   in \code{list(B = -15)} will pass this function without errors,
#'   these will be addressed explicitly in \code{\link{comp_boot_res}} as
#'   invalid inputs.
#'   In the case of multiplier bootstrap the expected input is of the form
#'   \code{list(B = 10, weights_type = "rademacher")}. Here the named element
#'   \code{weights_type} is optional e.g. \code{list(B = 10)} is valid,
#'   or passed in as an explicit \code{NULL}
#'   e.g. \code{list(B = 10, weights_type = NULL)}.
#'   Note that technically \code{B} should be a positive integer, and
#'   \code{weights_type} should be a character vector
#'   (see \code{\link{comp_boot_mul}}), but this assertion checking is handled
#'   explicitly in the \code{\link{comp_boot_mul}} function. So although passing
#'   in \code{list(B = -15, m = "test")} will pass this function without errors,
#'   these will be addressed explicitly in \code{\link{comp_boot_mul}} as
#'   invalid inputs.
#' @param boot_type (character) : Can take one of the values
#'   \code{c('boot_emp', 'boot_sub', 'boot_res', 'boot_mul')}, which are for
#'   empirical, subsampling, residual, and multiplier bootstrap respectively.
#'
#' @return : A \code{TRUE} if assertions pass, or an error if there is an
#'   assertion failure.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Multiplier Bootstrap input list assertion checking
#'
#' # valid since the named arguments are B, weights_type
#' testthat::expect_true(check_fn_args_comp_mms_var_boot_ind(
#'   inp_list = list(B = 10, weights_type = "rademacher"),
#'   boot_type = "boot_mul"
#' ))
#'
#' # valid since the named arguments are B, weights_type i.e. weights_type is NULL
#' testthat::expect_true(check_fn_args_comp_mms_var_boot_ind(
#'   inp_list = list(B = 10, weights_type = NULL),
#'   boot_type = "boot_mul"
#' ))
#' }
check_fn_args_comp_mms_var_boot_ind <- function(inp_list, boot_type) {
  # Do input assertion checks on this function's inputs
  assertthat::assert_that(
    is.list(inp_list)
    && length(inp_list) > 0
    && length(purrr::compact(.x = inp_list)) > 0,
    msg = glue::glue("Input must be a non-empty non-NULL list")
  )

  assertthat::assert_that(
    boot_type %in% c("boot_emp", "boot_sub", "boot_res", "boot_mul"),
    msg = glue::glue("boot_type must be one of",
                     "['boot_emp', 'boot_sub', 'boot_res', 'boot_mul']",
                     .sep = " ")
  )

  # Empirical Bootstrap and subsampling
  if (boot_type == "boot_emp" | boot_type == "boot_sub") {
    assertthat::assert_that(
      length(inp_list) <= 2,
      msg = glue::glue("{boot_type} input list must have 1 or 2 elements",
        "currently it has",
        "{length(inp_list)} items",
        .sep = " "
      )
    )

    if (length(inp_list) == 1 || length(purrr::compact(.x = inp_list)) == 1) {
      assertthat::assert_that(
        all(names(purrr::compact(.x = inp_list)) == c("B")),
        msg = glue::glue("{boot_type} input list must only contain the ",
          "named element: B",
          .sep = " "
        )
      )
    } else if (length(inp_list) == 2) {
      assertthat::assert_that(all(sort(names(inp_list)) == sort(c("B", "m"))),
        msg = glue::glue("{boot_type} input list must only contain the ",
          "names ['B', 'm']",
          .sep = " "
        )
      )
    }
  }
  # Multiplier Bootstrap
  else if (boot_type == "boot_mul") {
    assertthat::assert_that(
      length(inp_list) <= 2,
      msg = glue::glue("{boot_type} input list must have 1 or 2 elements",
        "currently it has",
        "{length(inp_list)} items",
        .sep = " "
      )
    )

    if (length(inp_list) == 1 || length(purrr::compact(.x = inp_list)) == 1) {
      assertthat::assert_that(
        all(names(purrr::compact(.x = inp_list)) == c("B")),
        msg = glue::glue("{boot_type} input list must only contain the ",
          "named element: B",
          .sep = " "
        )
      )
    } else if (length(inp_list) == 2) {
      assertthat::assert_that(all(sort(names(inp_list)) == sort(c("B", "weights_type"))),
        msg = glue::glue("{boot_type} input list must only contain the ",
          "names ['B', 'weights_type']",
          .sep = " "
        )
      )
    }
  }
  # Residual Bootstrap
  else if (boot_type == "boot_res") {
    assertthat::assert_that(
      length(inp_list) == 1,
      msg = glue::glue("{boot_type} input list must have 1 element",
        "currently it has",
        "{length(inp_list)} items",
        .sep = " "
      )
    )

    assertthat::assert_that(
      all(names(purrr::compact(.x = inp_list)) == c("B")),
      msg = glue::glue("{boot_type} input list must only contain the ",
        "named element: B",
        .sep = " "
      )
    )
  } else {
    stop("An assertion error has occurred! Please review inputs and re-run.")
  }
}

#' Assertion Checks for all \code{\link{comp_mms_var}} function bootstrap function inputs
#'
#' \code{check_fn_args_comp_mms_var_boot} is used to assess whether the arguments
#' are correctly specified in \code{list} format and returns an error message if
#' they do not match the correct specification
#'
#' @param boot_emp (list) In the case of empirical bootstrap the expected input
#'   is of the form #'   \code{list(B = 10, m = 100)}. Here the named
#'   element \code{m} is optional e.g. \code{list(B = 10)} is valid, or passed
#'   in as an explicit \code{NULL} e.g. \code{list(B = 10, m = NULL)}.
#'   Note that technically \code{B, m} should both be positive integers,
#'   but this assertion checking is handled explicitly in the
#'   \code{\link{comp_boot_emp}} function. So although passing
#'   in \code{list(B = -15, m = -20)} will pass this function without errors,
#'   these will be addressed explicitly in \code{\link{comp_boot_emp}} as
#'   invalid inputs.
#' @param boot_sub (list) TODO: ADD
#' @param boot_res (list) : In the case of residual bootstrap the expected
#'   input is of the form \code{list(B = 10)}. Note that technically \code{B}
#'   should be a positive integer, but this assertion checking is handled
#'   explicitly in the \code{\link{comp_boot_res}} function. So although passing
#'   in \code{list(B = -15)} will pass this function without errors,
#'   these will be addressed explicitly in \code{\link{comp_boot_res}} as
#'   invalid inputs.
#' @param boot_mul (list) : In the case of multiplier bootstrap the expected
#'   input is of the form \code{list(B = 10, weights_type = "rademacher")}.
#'   Here the named element \code{weights_type} is optional
#'   e.g. \code{list(B = 10)} is valid, or passed in as an explicit \code{NULL}
#'   e.g. \code{list(B = 10, weights_type = NULL)}.
#'   Note that technically \code{B} should be a positive integer, and
#'   \code{weights_type} should be a character vector
#'   (see \code{\link{comp_boot_mul}}), but this assertion checking is handled
#'   explicitly in the \code{\link{comp_boot_mul}} function. So although passing
#'   in \code{list(B = -15, m = "test")} will pass this function without errors,
#'   these will be addressed explicitly in \code{\link{comp_boot_mul}} as
#'   invalid inputs.
#'
#' @return A summary statistics tibble for the bootstrap input.
#'
#' @return : A \code{TRUE} if assertions pass, or an error if there is an
#'   assertion failure.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Expect all assertions to pass
#' check_fn_args_comp_mms_var_boot(boot_emp = list(B = 1e4, m = 600),
#'                             boot_res = NULL,
#'                             boot_mul = NULL)
#' }
check_fn_args_comp_mms_var_boot <- function(boot_emp, boot_sub, boot_res, boot_mul) {

  # Override bootstrap NULL bootstrap variance calculations if any of the
  # input values passed in are not NULL
  if (!is.null(boot_emp)) {
    # Empirical Bootstrap: list format assertion checking
    check_fn_args_comp_mms_var_boot_ind(
      inp_list = boot_emp,
      boot_type = "boot_emp"
    )
  }
  if (!is.null(boot_sub)) {
    # Subsampling: list format assertion checking
    check_fn_args_comp_mms_var_boot_ind(
      inp_list = boot_sub,
      boot_type = "boot_sub"
    )
  }
  if (!is.null(boot_res)) {
    # Residual Bootstrap: list format assertion checking
    check_fn_args_comp_mms_var_boot_ind(
      inp_list = boot_res,
      boot_type = "boot_res"
    )
  }
  if (!is.null(boot_mul)) {
    # Multiplier Bootstrap: list format assertion checking
    check_fn_args_comp_mms_var_boot_ind(
      inp_list = boot_mul,
      boot_type = "boot_mul"
    )
  }
}

#' get_boot_summary computes the statistics based on boot_out and boot_type
#'
#' \code{get_boot_summary} returns a tibble containing the model's statistics
#'   based on the coefficients estimates (\code{boot_out}) obtained via
#'   \code{boot_type} bootstrap (e.g., empirical).
#'
#' @param mod_fit An "lm" (OLS) object.
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
#' @keywords internal
#'
#' @importFrom Rdpack reprompt
#' @importFrom rlang .data
#'
#'
#' @return
get_boot_summary <- function(mod_fit, boot_out, boot_type) {
  assertthat::assert_that(all("lm" == class(mod_fit)) | any("glm" == class(mod_fit)),
    msg = glue::glue("mod_fit must only be of class lm or glm")
  )
  assertthat::assert_that("tbl_df" %in% class(boot_out),
    msg = glue::glue("boot_out must only be of class tibble")
  )
  assertthat::assert_that(boot_type %in% c("emp", "mul", "res", "sub"),
    msg = glue::glue("boot_out must only be a character taking values",
    " either ['emp', 'mul', 'res', 'sub']")
  )

  if (boot_type %in% c("mul", "res")) {
    boot_out <- boot_out %>%
      dplyr::mutate(.data = .,
                    m = nrow(stats::model.frame(mod_fit)),
                    n = .data$m)
  }

  out <- boot_out %>%
    tidyr::unnest(.data$boot_out) %>%
    dplyr::rename(
      .data = .,
      estimate.boot = .data$estimate
    ) %>%
    dplyr::left_join(
      x = .,
      y = broom::tidy(mod_fit) %>%
        dplyr::select(
          .data = .,
          .data$term, .data$estimate
        ),
      by = "term"
    ) %>%
    dplyr::group_by(
      .data$term,
      .data$estimate
    ) %>%
    dplyr::summarize(
      .data = .,
      std.error = stats::sd(sqrt(.data$m / .data$n) * (.data$estimate.boot - mean(.data$estimate.boot))),
      p.value = mean(sqrt(.data$m/.data$n) * abs(.data$estimate - .data$estimate.boot) > abs(.data$estimate)),
      .groups = "keep"
    ) %>%
    dplyr::mutate(statistic = .data$estimate / .data$std.error) %>%
    dplyr::arrange(.data = ., .data$term) %>%
    dplyr::relocate(.data$statistic, .after = .data$estimate)
  return(out)
}

#' Generates list containing estimates for an individual variance type
#'
#' \code{get_mms_comp_var_ind} generates the final output list for an
#'   individual variance type. This is a helper function to be used in the
#'   following functions: \code{\link{comp_lm_var}}, \code{\link{comp_sand_var}},
#'   \code{\link{comp_boot_emp}}, \code{\link{comp_boot_mul}},
#'   \code{\link{comp_boot_res}}.
#'
#' @param var_type_abb (character) : The abbreviation used to classify the type
#'   of variance. The choice is one of the following
#'   \code{"lm", "sand", "emp", "sub", "res", "mul"}.
#' @param summary_tbl (tibble) : The summary table of standard errors for the
#'   type of variance.
#' @param cov_mat (matrix) : The covariance matrix for the type of variance.
#' @param B (integer) : The number of bootstrap replications.
#' @param m (integer) : The number of observations to be sampled with or without
#'   replacement from the dataset for each bootstrap repetition.
#' @param n (integer) : The number of observations from the original modeling
#'   dataset.
#' @param boot_out (tibble) : A tibble of the boostrap replication datasets
#'   used in \code{\link{comp_boot_mul}}, \code{\link{comp_boot_res}}. In all
#'   other variance types this should be set to \code{NULL}.
#' @param weights_type : The type of multiplier bootstrap weights to generate.
#'   Based on the \code{weighttype} option in the Stata \code{boottest} package,
#'   this can only take the following five prespecified values
#'   \code{"rademacher", "mammen", "webb", "std_gaussian", "gamma"}.
#'   For more details see the documentation for
#'   \code{\link{comp_boot_mul_wgt}}. The default value is "rademacher".
#'
#' @return (list) : A list containing essential computed variance output for the
#'   type of variance specified. The computed parameter types are the common
#'   output for the following functions: \code{\link{comp_lm_var}},
#'   \code{\link{comp_sand_var}}, \code{\link{comp_boot_emp}},
#'   \code{\link{comp_boot_mul}}, \code{\link{comp_boot_res}}.
#'
#' @keywords internal
#'
#' @export
get_mms_comp_var_ind <- function(var_type_abb,
                                 summary_tbl = NULL,
                                 cov_mat = NULL,
                                 B = NULL,
                                 m = NULL,
                                 n = NULL,
                                 weights_type = NULL,
                                 boot_out = NULL) {

  # Get the variance type abbreviation as a glue string
  var_type_abb <- glue::glue("{var_type_abb}")

  # Get the title only for the specific variance type
  var_title <- dplyr::case_when(
    var_type_abb == "lm" ~ "Well Specified Model",
    var_type_abb == "sand" ~ "Sandwich",
    var_type_abb == "emp" ~ "Empirical Bootstrap",
    var_type_abb == "sub" ~ "Subsampling",
    var_type_abb == "res" ~ "Residual Bootstrap",
    var_type_abb == "mul" ~ "Multiplier Bootstrap"
  ) %>%
    glue::as_glue(x = .)

  # Get the title only for the specific variance type
  var_type <- dplyr::case_when(
    var_type_abb == "lm" ~ "well_specified",
    var_type_abb == "sand" ~ "sand",
    var_type_abb == "emp" ~ "boot_emp",
    var_type_abb == "sub" ~ "boot_sub",
    var_type_abb == "res" ~ "boot_res",
    var_type_abb == "mul" ~ "boot_mul"
  ) %>%
    glue::as_glue(x = .)

  # Get the emoji only for the specific variance type
  var_emoji <- dplyr::case_when(
    var_type_abb == "lm" ~ "\U1F4C9\U1F4C8",
    var_type_abb == "sand" ~ "\U1F969\U1F35E", # \U1F96A
    var_type_abb == "emp" ~ "\U1F9EE\U1F45F",
    var_type_abb == "sub" ~ "\U1F9EE\U1F45F",
    var_type_abb == "res" ~ "\U2696\U1F45F",
    var_type_abb == "mul" ~ "\U274C\U1F45F" # \U2716
  ) %>%
    glue::as_glue(x = .)

  # Get the combined emoji: title for the specific variance type
  var_emoji_title <- glue::glue("{var_emoji}: {var_title}:")

  # Get the assumptions vector only for the specific variance type
  var_assumptions <- switch(var_type_abb,
    "lm" = {
      c(
        glue::glue("Observations are assumed to be independent"),
        glue::glue("Residuals are assumed to be homoscedastic"),
        glue::glue("Linearity of the conditional expectation is assumed")
      )
    },
    "sand" = {
      c(
        glue::glue("Observations are assumed to be independent")
      )
    },
    "emp" = {
      c(
        glue::glue("Observations are assumed to be independent",
          .sep = " "
        ),
        glue::glue("Parameters: B = {B}, m = {m}, n = {n}")
      )
    },
    "sub" = {
      c(
        glue::glue("Observations are assumed to be independent",
                   .sep = " "
        ),
        glue::glue("Parameters: B = {B}, m = {m}, n = {n}")
      )
    },
    "res" = {
      c(
        glue::glue("Observations are assumed to be independent"),
        glue::glue("Residuals are assumed to be homoscedastic"),
        glue::glue("Linearity of the conditional expectation is assumed"),
        glue::glue("Parameters: B = {B}")
      )
    },
    "mul" = {
      c(
        glue::glue("Observations are assumed to be independent",
          .sep = " "
        ),
        glue::glue("Parameters: B = {B}, weights = {weights_type}")
      )
    }
  )

  # Construct comp_var list only for the specific variance type
  out_list <- list(
    "var_type" = var_type,
    "var_type_abb" = var_type_abb,
    # "var_emoji" = var_emoji,
    # "var_title" = var_title,
    # "var_emoji_title" = var_emoji_title,
    "var_summary" = summary_tbl,
    "var_assumptions" = var_assumptions,
    "cov_mat" = cov_mat,
    "boot_out" = boot_out
  )

  return(out_list)
}

#' Generates list containing several estimates of the variance
#'
#' \code{comp_mms_var} returns a list containing the requested estimates of the
#' variance, together with the assumptions behind which these estimates are
#' consistent.
#'
#'
#' @param mod_fit An lm (OLS) object
#' @param boot_emp (list) In the case of empirical bootstrap the expected input
#'   is of the form #'   \code{list(B = 10, m = 100)}. Here the named
#'   element \code{m} is optional e.g. \code{list(B = 10)} is valid, or passed
#'   in as an explicit \code{NULL} e.g. \code{list(B = 10, m = NULL)}.
#'   Note that technically \code{B, m} should both be positive integers,
#'   but this assertion checking is handled explicitly in the
#'   \code{\link{comp_boot_emp}} function. So although passing
#'   in \code{list(B = -15, m = -20)} will pass this function without errors,
#'   these will be addressed explicitly in \code{\link{comp_boot_emp}} as
#'   invalid inputs.
#' @param boot_sub (list) TODO: ADD
#' @param boot_mul (list) : In the case of multiplier bootstrap the expected
#'   input is of the form \code{list(B = 10, weights_type = "rademacher")}.
#'   Here the named element \code{weights_type} is optional
#'   e.g. \code{list(B = 10)} is valid, or passed in as an explicit \code{NULL}
#'   e.g. \code{list(B = 10, weights_type = NULL)}.
#'   Note that technically \code{B} should be a positive integer, and
#'   \code{weights_type} should be a character vector
#'   (see \code{\link{comp_boot_mul}}), but this assertion checking is handled
#'   explicitly in the \code{\link{comp_boot_mul}} function. So although passing
#'   in \code{list(B = -15, m = "test")} will pass this function without errors,
#'   these will be addressed explicitly in \code{\link{comp_boot_mul}} as
#'   invalid inputs.
#' @param boot_res (list) : In the case of residual bootstrap the expected
#'   input is of the form \code{list(B = 10)}. Note that technically \code{B}
#'   should be a positive integer, but this assertion checking is handled
#'   explicitly in the \code{\link{comp_boot_res}} function. So although passing
#'   in \code{list(B = -15)} will pass this function without errors,
#'   these will be addressed explicitly in \code{\link{comp_boot_res}} as
#'   invalid inputs.
#'
#' @return A list containing the several types of variance estimates requested
#'   by the  user, including the sandwich and the the variance returned by
#'   \code{\link[stats]{lm}}.
#'
#' @keywords internal
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
#' out <- comp_mms_var(mod_fit,
#' boot_mul = list(B = 100, weights_type = "rademacher"),
#' boot_sub = list(B = 100, m = 50))
#'
#' # print output
#' print(out)
#' }
comp_mms_var <- function(mod_fit,
                         boot_emp = NULL,
                         boot_sub = NULL,
                         boot_res = NULL,
                         boot_mul = NULL) {
  # Assertion checking for mod_fit
  assertthat::assert_that(all("lm" == class(mod_fit))
                          || any("glm" == class(mod_fit)),
    msg = glue::glue("mod_fit must only be of class lm or glm")
  )

  # Assertion checking for empirical, multiplier, and residual bootstrap
  check_fn_args_comp_mms_var_boot(boot_emp = boot_emp,
                              boot_sub = boot_sub,
                              boot_res = boot_res,
                              boot_mul = boot_mul)

  # Compute the well specified lm standard errors by default
  out_well_specified <- comp_lm_var(mod_fit = mod_fit)

  # Compute the sandwich estimator by default
  out_sand <- comp_sand_var(mod_fit = mod_fit)

  # Initialize all other bootstrap variance computations to NULL values
  boot_out_emp <- NULL
  boot_out_sub <- NULL
  boot_out_mul <- NULL
  boot_out_res <- NULL

  # Override bootstrap NULL bootstrap variance calculations if any of the
  # input values passed in are not NULL
  if (!is.null(boot_emp)) {
    # Empirical Bootstrap: Extract parameters from list input
    if (length(purrr::compact(.x = boot_emp)) == 2) {
      B <- boot_emp[["B"]]
      m <- boot_emp[["m"]]
    } else if (length(purrr::compact(.x = boot_emp)) == 1) {
      B <- boot_emp[["B"]]
      m <- NULL
    } else {
      stop("An error has occurred in boot_emp input. Please check it and rerun")
    }

    # Run Empirical Bootstrap
    boot_out_emp <- comp_boot_emp(
      mod_fit = mod_fit,
      B = B,
      m = m,
      replace = TRUE
    )
  }


  if(!is.null(boot_sub)){
    if (length(purrr::compact(.x = boot_sub)) == 2) {
      B <- boot_sub[["B"]]
      m <- boot_sub[["m"]]
    } else if (length(purrr::compact(.x = boot_sub)) == 1) {
      B <- boot_sub[["B"]]
      m <- NULL
    } else {
      stop("An error has occurred in boot_sub input. Please check it and rerun")
    }

    # Run subsampling
    boot_out_sub <- comp_boot_emp(
      mod_fit = mod_fit,
      B = B,
      m = m,
      replace = FALSE
    )
  }


  if (!is.null(boot_res)) {
    # Residual Bootstrap: Extract parameters from list input
    if (length(purrr::compact(.x = boot_res)) == 1) {
      B <- boot_res[["B"]]
    } else {
      stop("An error has occurred in boot_res input. Please check it and rerun")
    }

    # Run Residual Bootstrap
    boot_out_res <- comp_boot_res(mod_fit = mod_fit, B = B)
  }


  if (!is.null(boot_mul)) {
    # Multiplier Bootstrap: Extract parameters from list input
    if (length(purrr::compact(.x = boot_mul)) == 2) {
      B <- boot_mul[["B"]]
      weights_type <- boot_mul[["weights_type"]]
    } else if (length(purrr::compact(.x = boot_mul)) == 1) {
      B <- boot_mul[["B"]]
      weights_type <- "rademacher"
    } else {
      stop("An error has occurred in boot_mul input. Please check it and rerun")
    }

    # Run Multiplier Bootstrap
    boot_out_mul <- comp_boot_mul(
      mod_fit = mod_fit,
      B = B,
      weights_type = weights_type
    )
  }


  # Combine all output into a single list of lists
  out <- list(
    var_well_specified = out_well_specified,
    var_sand = out_sand,
    var_boot_emp = boot_out_emp,
    var_boot_sub = boot_out_sub,
    var_boot_mul = boot_out_mul,
    var_boot_res = boot_out_res
  )
  return(out)
}
