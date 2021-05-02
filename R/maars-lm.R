#' Generate \code{maars_lm, lm} object with estimates of the variance
#'
#' Generates an object of class \code{"maars_lm", "lm"} containing
#' estimates of the variance of the coefficients in the regression
#' model.
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
#' @details The "maars_lm" object is basically an "lm" object with additional
#'   attributes (the additional estimates of the coefficients variance),
#'   which are stored within "var". For example, the estimates of the empirical
#'   bootstrap will be stored within "var$var_boot_emp".
#'   Each of the nested lists contains the following elements: the type of estimator of
#'   of the variance  (\code{var_type}); An abbreviated string representing the
#'   type of the estimator of the variance  (\code{var_type_abb}); the summary
#'   statistics of \code{mod_fit} based on this estimator of the variance
#'   (e.g., standard errors and p-values) (\code{var_summary}); the assumptions
#'   under which the estimator of the variance is consistent
#'   (\code{var_assumptions}); the covariance matrix for the coefficients
#'   estimates (\code{cov_mat}).
#'
#' @return A "maars_lm" object containing the estimates of the variance of the
#'   regression coefficients, including the sandwich and the variance
#'   returned by \code{stats::lm}.
#'
#' @export
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
#' out <- comp_var(mod_fit, boot_mul = list(B = 100, weights_type = "rademacher"))
#'
#' # print output
#' print(out)
#' print(out$var$var_boot_mul)
#' }
comp_var <- function(mod_fit,
                     boot_emp = NULL,
                     boot_sub = NULL,
                     boot_res = NULL,
                     boot_mul = NULL) {
  # the following condition will need to be revised once we introduce glm
  if (all(c("maars_lm", "lm") %in% class(mod_fit))) {
    attr(mod_fit, "class") <- "lm"
  }

  out_var <- comp_mms_var(mod_fit = mod_fit,
                          boot_emp = boot_emp,
                          boot_sub = boot_sub,
                          boot_res = boot_res,
                          boot_mul = boot_mul)
  mod_fit[["var"]] <- out_var

  class(mod_fit) <- c("maars_lm", "lm")

  return(mod_fit)
}

#' Print summary in the style of `lm`
#'
#' @param var_summary TODO: Add later
#' @param digits TODO: Add later
#'
#' @keywords internal
#'
#' @return TODO: Add later
#'
#' @examples
#' \dontrun{
#' # TODO: Add later
#' }
get_mms_summary_with_stars <- function(var_summary, digits) {
  out_summ <- var_summary %>%
    dplyr::mutate(.data = ., sig = stats::symnum(.data$p.value,
      corr = FALSE, na = FALSE,
      legend = FALSE,
      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
      symbols = c("***", "**", "*", ".", " ")
    )) %>%
    dplyr::mutate(.data = ., p.value = format.pval(.data$p.value, digits = 2)) %>%
    dplyr::rename(
      .data = .,
      Term = .data$term,
      Estimate = .data$estimate,
      `Std. Error` = .data$std.error,
      `t value` = .data$statistic,
      `Pr(>|t|)` = .data$p.value,
      `Signif.` = .data$sig
    )
  return(out_summ)
}

#' Print interleaved summary for a \code{maars_lm, lm} object, for a
#' specific variance type
#'
#' @param title TODO: Add later
#' @param summary TODO: Add later
#' @param assumptions TODO: Add later
#' @param digits TODO: Add later
#'
#' @keywords internal
#'
#' @return TODO: Add later
#'
#' @examples
#' \dontrun{
#' # TODO: Add later
#' }
get_mms_summary_split_cli <- function(title,
                                      summary,
                                      assumptions,
                                      digits = 3) {
  cli::cli_h1(cli::col_yellow(glue::glue("{title} Summary")))
  cli::cli_text("\n")

  summary_stats <- get_mms_summary_with_stars(var_summary = summary, digits = digits)
  cat("Coefficients:\n")
  print.data.frame(summary_stats, row.names = FALSE, digits = digits)

  cli::cli_h2(cli::col_green(glue::glue("{title} Assumptions")))
  cli::cli_li(assumptions)
}

#' Summary of \code{maars_lm} object
#'
#' Summary method for class "maars_lm".
#'
#' @param object A fitted "maars_lm" object.
#' @param sand (logical) : \code{TRUE} if sandwich estimator output is required,
#'   \code{FALSE} to exclude this output from the request.
#' @param boot_emp (logical) : \code{TRUE} if empirical bootstrap standard error
#'   output is required, \code{FALSE} to exclude this output from the request.
#' @param boot_sub (logical) : \code{TRUE} if subsampling standard error
#'   output is required, \code{FALSE} to exclude this output from the request.
#' @param boot_res (logical) : \code{TRUE} if residual bootstrap standard error
#'   output is required, \code{FALSE} to exclude this output from the request.
#' @param boot_mul (logical) : \code{TRUE} if multiplier bootstrap standard error
#'   output is required, \code{FALSE} to exclude this output from the request
#' @param well_specified (logical) : \code{TRUE} if lm standard errors.
#'   (well specified) output is required, \code{FALSE} to exclude this output
#'   from the request.
#' @param digits (integer) : Rounding digits used in some of the function's output.
#' @param ... Additional arguments.
#'
#' @method summary maars_lm
#' @export
summary.maars_lm <- function(object,
                             sand = NULL,
                             boot_emp = NULL,
                             boot_sub = NULL,
                             boot_mul = NULL,
                             boot_res = NULL,
                             well_specified = NULL,
                             digits = 3,
                             ...) {

  # Get the variance types the user has requested. This performs assertion
  # Checking, so if there is no error it will return the required names,
  # else it will throw an error
  req_var_nms <- check_fn_args_summary(
    mod_fit = object,
    sand = sand,
    boot_emp = boot_emp,
    boot_sub = boot_sub,
    boot_res = boot_res,
    boot_mul = boot_mul,
    well_specified = well_specified
  )

  # Filter the comp_mms_var output from the fitted maars_lm object for the
  # requested variance types from the user
  comp_var_ind_filt <- purrr::map(
    .x = req_var_nms,
    .f = ~ purrr::pluck(object$var, .x)
  )

  # Get emoji titles for all variance types
  all_emoji_titles <- purrr::map(
    .x = comp_var_ind_filt,
    .f = ~ fetch_mms_comp_var_attr(
      comp_var_ind = .x,
      req_type = "emoji_title"
    )
  )

  # Get all assumptions for all variance types
  all_assumptions <- purrr::map(
    .x = comp_var_ind_filt,
    .f = ~ fetch_mms_comp_var_attr(
      comp_var_ind = .x,
      req_type = "var_assumptions"
    )
  )

  # Get variance summaries for all variance types
  all_summaries <- purrr::map(
    .x = comp_var_ind_filt,
    .f = ~ fetch_mms_comp_var_attr(
      comp_var_ind = .x,
      req_type = "var_summary"
    )
  )

  # Add detailed assumptions for the well-specified linear model
  # This is to design the specific string at the bottom of the
  # standard out put of the summary.lm with F-statistics, p-value etc
  summ_lm <- stats::summary.lm(object)


  # Compute chi squared statistics under H0: all coefficients are equal to 0
  # based on the sandwich estimate of the variance
  coefs <- stats::coef(object)
  n <- nrow(stats::model.frame(object))
  d <- length(coefs)
  cov_mat <- purrr::pluck(object, "var", "var_sand", "cov_mat")
  # TODO: fix the following code because this case only deals with cases in which
  # users code the intercept as "(Intercept)" in the regression data or the
  # intercept is handled directly in lm
  if ("(Intercept)" %in% colnames(cov_mat)) {
    is_intercept <- (colnames(cov_mat) == "(Intercept)")
    cov_mat <- cov_mat[!is_intercept, !is_intercept]
    coefs <- coefs[!is_intercept]
    d <- d - 1
  }

  # Compute the statistic
  chi2_stat <- (t(coefs) %*%
    solve(cov_mat, coefs))
  # TODO: Do we need this following line?
  identical(stats::update(stats::formula(object), 0 ~ .), 0 ~ 1)
  chi2_p_value <- stats::pchisq(q = chi2_stat, df = d, lower.tail = FALSE)

  # Default output to be with 4 digits. Manually set by maars developers
  FMT_NUM_DIGITS <- 4

  # TODO: This "assumptions_lm" variable should perhaps be called
  #       "common_assumptions" or something similar, to make it more
  #       descriptive in the list output eventually returned by summary
  stats_lm <-
    c(
      glue::glue("Residual standard error:",
        "{formatC(signif(summ_lm$sigma, digits = FMT_NUM_DIGITS))}",
        "on",
        "{object$df.residual}",
        "degrees of freedom",
        .sep = " "
      ),
      glue::glue("Multiple R-squared:",
        "{formatC(summ_lm$r.squared, digits = FMT_NUM_DIGITS)},",
        "Adjusted R-squared:",
        "{formatC(summ_lm$adj.r.squared, digits = FMT_NUM_DIGITS)}",
        .sep = " "
      ),
      glue::glue("F-statistic:",
        "{formatC(summ_lm$fstatistic[1L], digits = FMT_NUM_DIGITS)}",
        "on",
        "{summ_lm$fstatistic[2L]} and {summ_lm$fstatistic[3L]} DF,",
        "p-value:",
        "{format.pval(stats::pf(summ_lm$fstatistic[1L],
                       summ_lm$fstatistic[2L],
                       summ_lm$fstatistic[3L],
                       lower.tail = FALSE))}",
        .sep = " "
      )
    )

  ## TODO: Need to decide how to display the statistics generated from lm vs.
  # those from sandwich
  stats_sand <-
    c(
      glue::glue("Multiple R-squared:",
        "{formatC(summ_lm$r.squared, digits = FMT_NUM_DIGITS)},",
        "Adjusted R-squared:",
        "{formatC(summ_lm$adj.r.squared, digits = FMT_NUM_DIGITS)}",
        .sep = " "
      ),
      glue::glue("Chi-squared statistic based on sandwich variance:",
        "{formatC(chi2_stat, digits = FMT_NUM_DIGITS)}",
        "on",
        "{d} DF,",
        "p-value:",
        "{format.pval(chi2_p_value, digits = 2)}",
        .sep = " "
      )
    )

  summary_out <- list(
    all_summaries = all_summaries,
    all_emoji_titles = all_emoji_titles,
    all_assumptions = all_assumptions,
    stats_lm = stats_lm,
    stats_sand = stats_sand,
    digits = digits
  )
  class(summary_out) <- "summary.maars_lm"
  return(summary_out)
}


#' Print summary of `maars_lm` object
#'
#' Calls \code{print.summary.maars_lm} on a \code{summary} of a `maars_lm` object.
#'
#' @param x A `maars_lm` object.
#' @param ... Additional arguments
#'
#' @method print summary.maars_lm
#' @export
print.summary.maars_lm <- function(x, ...) {

  # Get the printed summary table interleaved with assumptions
  # We just run an index over all_summaries, since it has the same
  # length as all_emoji_titles and all_assumptions and the same variance
  # type ordering
  purrr::iwalk(
    unname(purrr::pluck(x, "all_summaries")),
    ~ get_mms_summary_split_cli(
      title = purrr::pluck(x, "all_emoji_titles")[[.y]],
      summary = purrr::pluck(x, "all_summaries")[[.y]],
      assumptions = purrr::pluck(x, "all_assumptions")[[.y]],
      digits = purrr::pluck(x, "digits")
    )
  )
  cli::cli_h2(cli::col_blue(glue::glue("Signif. codes:")))
  cli::cli_li("0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")
  cli::cli_h2(cli::col_magenta(glue::glue("Summary statistics (sandwich):")))
  cli::cli_li(purrr::pluck(x, "stats_sand"))
}

# tidy.maars_lm <- function(x, ...){
#   warning(paste('The "tidy.maars_lm" method has not been implemented yet!',
#                  'The tidy method on the lm object will be returned.'))
#   NextMethod("tidy")
# }

#' Get assumptions for a \code{maars_lm, lm} object, for a specific
#' variance type
#'
#' @param title TODO: Add later
#' @param assumptions TODO: Add later
#'
#' @keywords internal
#'
#' @return TODO: Add later
#'
#' @examples
#' \dontrun{
#' # TODO: Add later
#' }
get_mms_assumptions_cli <- function(title, assumptions) {
  cli::cli_end()
  cli::cli_h1(cli::col_green(glue::glue("{title} Assumptions")))
  cli::cli_ul()
  cli::cli_li(assumptions)
  cli::cli_end()
}

#' Print method display for a \code{maars_lm, lm} object
#'
#' @param mod_fit TODO: Add later
#' @param all_emoji_titles TODO: Add later
#' @param all_assumptions TODO: Add later
#'
#' @keywords internal
#'
#' @return TODO: Add later
#'
#' @examples
#' \dontrun{
#' # TODO: Add later
#' }
get_mms_print_cli <- function(mod_fit,
                              all_emoji_titles,
                              all_assumptions) {
  cli::cli_end()
  cli::cli_h1(cli::col_yellow(glue::glue("Fitted OLS Model:")))
  mod_fit_lm <- mod_fit
  # Need this to UseNextMethod to print lm formula directly
  class(mod_fit_lm) <- c("lm")
  print(mod_fit_lm)
  purrr::iwalk(
    unname(all_emoji_titles),
    ~ get_mms_assumptions_cli(
      title = all_emoji_titles[[.y]],
      assumptions = all_assumptions[[.y]]
    )
  )
}

#' Print `maars_lm` object
#'
#' Calls \code{print} on a \code{\link[stats]{lm}} object.
#'
#' @param x A `maars_lm` object.
#' @param ... Additional arguments passed to `print.lm()` to print the
#'   object.
#'
#' @method print maars_lm
#' @export
print.maars_lm <- function(x, ...) {

  #  For print.maars_lm, this is different. We print everything that
  #  is available by default, not what variance types the user passes
  #  in. So just extract all the non-null value of our variance list
  #  output from the maars_lm, lm object
  req_var_nms <- x$var %>%
    purrr::compact(.x = .) %>%
    names(x = .)

  # Filter the comp_mms_var output from the fitted maars_lm object for the
  # requested variance types from the user
  comp_var_ind_filt <- req_var_nms %>%
    purrr::map(.x = ., .f = ~ purrr::pluck(x$var, .x))

  # Get emoji titles for all variance types
  all_emoji_titles <- purrr::map(
    .x = comp_var_ind_filt,
    .f = ~ fetch_mms_comp_var_attr(
      comp_var_ind = .x,
      req_type = "emoji_title"
    )
  )

  # Get all assumptions for all variance types
  all_assumptions <- purrr::map(.x = comp_var_ind_filt, .f = ~ fetch_mms_comp_var_attr(
    comp_var_ind = .x,
    req_type = "var_assumptions"
  ))

  get_mms_print_cli(
    mod_fit = x,
    all_emoji_titles = all_emoji_titles,
    all_assumptions = all_assumptions
  )
}

#' Convert an object to an object that can be handled by the "maars" package
#'
#' Several methods are provided to convert common objects
#' (such as "lm") into "maars" objects,
#' which can be used with the various functions in the package.
#'
#' @param x Object to be converted. See Methods section below for details on
#'   formatting of each input type.
#' @param ... Additional arguments passed to methods.
#'
#' @export
as.maars <- function(x, ...) {
  class(x) <- c("maars_lm", "lm")
  return(x)
}


#' @method as.maars lm
#' @describeIn as.maars The input object \code{x} must be of class
#'   "lm". The function returns an object of class
#'   ("maars_lm, "lm").
#'
#' @export
as.maars.lm <- function(x, ...) {
  UseMethod("as.maars")
}

#' Plot \code{maars_lm, lm} object
#'
#' @param x (\code{maars_lm, lm}) : A fitted \code{maars_lm, lm} OLS object
#' @param which (numeric vector) : if a subset of the plots is required, specify a
#'   subset of the numbers 1:6.
#'
#' @param ... Additional arguments passed to methods.
#'
#' @return TODO: Describe plots
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(454354534)
#'
#' # generate data ----
#' n <- 1e3
#' X_1 <- stats::rnorm(n, 0, 1)
#' X_2 <- stats::rnorm(n, 10, 20)
#' eps <- stats::rnorm(n, 0, 1)
#'
#' # Let's generate data and fit a well-specified OLS data and model ----
#' y <- 2 + X_1 * 1 + X_2 * 5 + eps
#' lm_fit <- stats::lm(y ~ X_1 + X_2)
#' summary(lm_fit)
#'
#' # Fit our first maars_lm object i.e. comp_var1
#' comp_var1 <- comp_var(
#'   mod_fit = lm_fit,
#'   boot_emp = list(B = 50, m = 200),
#'   boot_res = NULL,
#'   boot_mul = list(B = 60)
#' )
#'
#' # Plot our maars_lm object
#' plot(comp_var1)
#' }
plot.maars_lm <- function(x, which = NULL, ...) {

  # Reminder: p8 is not NULL only if one type of bootstrap estimates
  # are available
  mms_diag_plots <- get_plot(mod_fit = x)

  ### NEED TO FIX THIS ISSUE
  n_plots <- length(mms_diag_plots %>% purrr::keep(~ !is.null(.)))
  if (is.null(which)) which <- 1:n_plots
  assertthat::assert_that(is.numeric(which) & !any(which < 1) & !any(which > n_plots),
    msg = glue::glue("'which' must be in 1:{n_plots}")
  )

  for (i in which) {
    if (i == 1) {
      # For the first plot, don't ask the user for prompt
      graphics::par(ask = FALSE)
      print(mms_diag_plots[[i]])
    } else {
      # For subsequent plots, ask the user for prompts to display
      # the plots sequentially
      graphics::par(ask = TRUE)
      print(mms_diag_plots[[i]])
    }
    graphics::par(ask = FALSE)
  }
}

#' Print interleaved Confidence Interval summary for a \code{maars_lm, lm}
#' object, for a specific variance type
#'
#' @param title TODO: Add later
#' @param summary_confint TODO: Add later
#' @param digits TODO: Add later
#'
#' @keywords internal
#'
#' @return TODO: Add later
#'
#' @examples
#' \dontrun{
#' # TODO: Add later
#' }
get_mms_summary_confint_split_cli <- function(title,
                                              summary_confint,
                                              digits = 3) {
  cli::cli_end()
  cli::cli_h1(cli::col_yellow(glue::glue("{title} Confint Summary")))
  cli::cli_text("\n")
  cli::cli_end()
  print.data.frame(x = summary_confint, digits = digits)
  cli::cli_end()
}

#' Confidence interval of \code{maars_lm, lm} object
#'
#' \code{confint} method for an object of class \code{maars_lm, lm}.
#'
#' @param object A fitted "maars_lm" object.
#' @param parm (\code{NULL}) : a specification of which parameters are to be
#'   given confidence intervals, either a vector of numbers or a vector of
#'   names. If missing, all parameters are considered. Currently only allowed
#'   value is \code{NULL}.
#' @param level (double) : numeric value between 0 and 1 indicating the
#'   confidence level (e.g., 0.95)
#' @param sand (logical) : \code{TRUE} if sandwich estimator output is required,
#'   \code{FALSE} to exclude this output from the request.
#' @param boot_emp (logical) : \code{TRUE} if empirical bootstrap standard error
#'   output is required, \code{FALSE} to exclude this output from the request.
#' @param boot_sub (logical) : \code{TRUE} if subsampling standard error
#'   output is required, \code{FALSE} to exclude this output from the request.
#' @param boot_res (logical) : \code{TRUE} if residual bootstrap standard error
#'   output is required, \code{FALSE} to exclude this output from the request.
#' @param boot_mul (logical) : \code{TRUE} if multiplier bootstrap standard error
#'   output is required, \code{FALSE} to exclude this output from the request
#' @param well_specified (logical) : \code{TRUE} if lm standard errors.
#'   (well specified) output is required, \code{FALSE} to exclude this output
#'   from the request.
#' @param digits (integer) : Rounding digits used in some of the function's output.
#' @param level (numeric) : TODO: Add later
#' @param ... Additional arguments.
#'
#' @method confint maars_lm
#' @export
confint.maars_lm <- function(object,
                             parm = NULL,
                             level = 0.95,
                             # TODO: to be changed
                             sand = TRUE,
                             boot_emp = FALSE,
                             boot_sub = FALSE,
                             boot_res = FALSE,
                             boot_mul = FALSE,
                             well_specified = FALSE,
                             digits = 3,
                             ...) {

  # Check parm is NULL valued
  # TODO: Allow this to be a vector of numbers or a vector of names to filter
  #       for the term variable
  assertthat::assert_that(is.null(parm),
    msg = glue::glue("parm must be NULL valued")
  )

  assertthat::assert_that(level > 0 & level < 1,
    msg = glue::glue("level must be between 0 and 1")
  )

  # Get the variance types the user has requested. This performs assertion
  # Checking, so if there is no error it will return the required names,
  # else it will throw an error
  req_var_nms <- check_fn_args_summary(
    mod_fit = object,
    sand = sand,
    boot_emp = boot_emp,
    boot_sub = boot_sub,
    boot_res = boot_res,
    boot_mul = boot_mul,
    well_specified = well_specified
  )

  # Filter the comp_mms_var output from the fitted maars_lm object for the
  # requested variance types from the user
  comp_var_ind_filt <- purrr::map(
    .x = req_var_nms,
    .f = ~ purrr::pluck(object$var, .x)
  )

  # Get emoji titles for all variance types
  all_emoji_titles <- purrr::map(
    .x = comp_var_ind_filt,
    .f = ~ fetch_mms_comp_var_attr(
      comp_var_ind = .x,
      req_type = "emoji_title"
    )
  )

  # Modified summary table with the broom standard column names and
  # variance type column
  all_summary_mod <- purrr::map(
    .x = comp_var_ind_filt,
    .f = ~ fetch_mms_comp_var_attr(
      comp_var_ind = .x,
      req_type = "var_summary_mod"
    )
  )

  all_var_type_abb <- purrr::map(
    .x = comp_var_ind_filt,
    .f = ~ fetch_mms_comp_var_attr(
      comp_var_ind = .x,
      req_type = "var_type_abb"
    )
  )

  # Get fitted OLS residual degrees of freedom
  df_residual <- object[["df.residual"]]

  all_summary_confint <- all_summary_mod %>%
    purrr::map(
      .x = .,
      .f = ~ dplyr::mutate(
        .data = .,
        conf.low = .data$estimate +
          qt((1 - level) / 2, df_residual) * .data$std.error,
        conf.high = .data$estimate +
          qt(1 - (1 - level) / 2, df_residual) * .data$std.error
      )
    ) %>%
    purrr::set_names(x = ., nm = req_var_nms) %>%
    purrr::map(.x = ., .f = ~ dplyr::select(
      .data = dplyr::ungroup(.x),
      .data$term,
      .data$conf.low,
      .data$conf.high
    ))

  # Get the printed summary table interleaved with assumptions
  # We just run an index over all_summaries, since it has the same
  # length as all_emoji_titles and all_assumptions and the same variance
  # type ordering
  # TODO: As discussed, the output could just come from get_confint() rather
  #       than the above code, and we can get the all_summary_confint list as
  #       a purrr::group_split() on the var_type_abb of the get_confint() output.
  #       However, one must be careful to group_keys() first. See this relevant
  #       example which we should follow if we go down this route:
  #       https://stackoverflow.com/a/57275733/4687531
  purrr::iwalk(
    unname(all_emoji_titles),
    ~ get_mms_summary_confint_split_cli(
      title = all_emoji_titles[[.y]],
      summary_confint = all_summary_confint[[.y]],
      digits = digits
    )
  )
}
