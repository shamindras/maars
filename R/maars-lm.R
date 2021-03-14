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
                    boot_res = NULL,
                    boot_mul = NULL) {
  # the following condition will need to be revised once we introduce glm
  if (all(c("maars_lm", "lm") %in% class(mod_fit))) {
    attr(mod_fit, "class") <- "lm"}

  out_var <- comp_mms_var(mod_fit, boot_emp, boot_res, boot_mul)
  mod_fit[['var']] <- out_var

  attr(mod_fit, "class") <- c("maars_lm", "lm")

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
get_mms_summary_print_lm_style <- function(var_summary, digits) {
  out_summ <- var_summary %>%
    dplyr::mutate(sig = symnum(p.value, corr = FALSE, na = FALSE,
                               legend = FALSE,
                               cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                               symbols = c("***", "**", "*", ".", " ")))
  colnames(out_summ) <- c(
    'Term', 'Estimate', 'Std. Error ',
    't value', 'Pr(>|t|)', 'Significance'
  )

  cat('Coefficients:\n')
  print.data.frame(out_summ, row.names = FALSE, digits = digits)
  # name_width <- max(sapply(names(out_summ), nchar))
  # format(out_summ, width = name_width, justify = "centre", digits = digits)
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
  cli::cli_end()
  cli::cli_h1(cli::col_yellow(glue::glue("{title} Summary")))
  cli::cli_text("\n")
  cli::cli_end()
  get_mms_summary_print_lm_style(var_summary = summary, digits = digits)
  cli::cli_end()
  cli::cli_ul()
  cli::cli_h2(cli::col_green(glue::glue("{title} Assumptions")))
  cli::cli_ul()
  cli::cli_li(assumptions)
  cli::cli_end()
}


#' Print interleaved summary for a \code{maars_lm, lm} object, for all
#' variance types
#'
#' @param summary_joined TODO: Add later
#' @param digits TODO: Add later
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
get_mms_summary_joined_cli <- function(all_emoji_titles,
                                       all_assumptions,
                                       summary_joined,
                                       digits = 3) {
  cli::cli_end()
  cli::cli_h1(cli::col_yellow(glue::glue("Fitted OLS Model:")))
  print.data.frame(x = summary_joined, digits = digits)
  cli::cli_h1(cli::col_cyan(glue::glue("Assumptions")))
  purrr::iwalk(
    all_emoji_titles,
    ~ get_mms_assumptions_cli(
      title = all_emoji_titles[[.y]],
      assumptions = all_assumptions[[.y]]
    )
  )
  cli::cli_end()
}

#' Summary of `maars_lm` object
#'
#' Summary method for class "maars_lm".
#'
#' @param object A fitted "maars_lm" object.
#' @param sand (logical) : \code{TRUE} if sandwich estimator output is required,
#'   \code{FALSE} to exclude this output from the request.
#' @param boot_emp (logical) : \code{TRUE} if empirical bootstrap standard error
#'   output is required, \code{FALSE} to exclude this output from the request.
#' @param boot_res (logical) : \code{TRUE} if residual bootstrap standard error
#'   output is required, \code{FALSE} to exclude this output from the request.
#' @param boot_mul (logical) : \code{TRUE} if multiplier bootstrap standard error
#'   output is required, \code{FALSE} to exclude this output from the request
#' @param well_specified (logical) : \code{TRUE} if lm standard errors.
#'   (well specified) output is required, \code{FALSE} to exclude this output
#'   from the request.
#' @param digits (integer) : Rounding digits used in some of the function's output.
#' @param split (logical) : TODO: Add later
#' @param ... Additional arguments.
#'
#' @method summary maars_lm
#' @export
summary.maars_lm <- function(object,
                             sand = TRUE,
                             boot_emp = FALSE,
                             boot_res = FALSE,
                             boot_mul = FALSE,
                             well_specified = FALSE,
                             digits = 3,
                             split = TRUE,
                             ...) {

  # Get the variance types the user has requested. This performs assertion
  # Checking, so if there is no error it will return the required names,
  # else it will throw an error
  req_var_nms <- check_fn_args_summary(
    mod_fit = object,
    sand = sand,
    boot_emp = boot_emp,
    boot_res = boot_res,
    boot_mul = boot_mul,
    well_specified = well_specified
  )

  # Filter the comp_mms_var output from the fitted maars_lm object for the
  # requested variance types from the user
  comp_var_ind_filt <- req_var_nms %>%
    purrr::map(.x = ., ~ purrr::pluck(object$var, .x))

  # Get emoji titles for all variance types
  all_emoji_titles <- comp_var_ind_filt %>%
    purrr::map(.x = ., .f = ~ fetch_mms_comp_var_attr(
      comp_var_ind = .x,
      req_type = "emoji_title"
    ))

  # Get all assumptions for all variance types
  all_assumptions <- comp_var_ind_filt %>%
    purrr::map(.x = ., .f = ~ fetch_mms_comp_var_attr(
      comp_var_ind = .x,
      req_type = "var_assumptions"
    ))

  if(split){
    # Get variance summaries for all variance types
    all_summaries <- comp_var_ind_filt %>%
      purrr::map(.x = ., .f = ~ fetch_mms_comp_var_attr(
        comp_var_ind = .x,
        req_type = "var_summary"
      ))

    # Get the printed summary table interleaved with assumptions
    # We just run an index over all_summaries, since it has the same
    # length as all_emoji_titles and all_assumptions and the same variance
    # type ordering
    purrr::iwalk(
      all_summaries,
      ~ get_mms_summary_split_cli(
        title = all_emoji_titles[[.y]],
        summary = all_summaries[[.y]],
        assumptions = all_assumptions[[.y]],
        digits = digits
      )
    )
  } else{
    # Summary table with the abbreviated variance type as a suffix
    all_summaries_nmd_joined <- get_summary2(mod_fit = object,
                                             sand = sand,
                                             boot_emp = boot_emp,
                                             boot_res = boot_res,
                                             boot_mul = boot_mul,
                                             well_specified = well_specified,
                                             tidy = FALSE)

    # Get the printed joined summary table with assumptions
    get_mms_summary_joined_cli(all_emoji_titles,
                               all_assumptions,
                               summary_joined = all_summaries_nmd_joined,
                               digits = digits)
  }
  cli::cli_h3(cli::col_blue(glue::glue("Significance codes:")))
  cli::cli_text("\n")
  cli::cli_text("0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")

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
  # TODO: Get the formula manually and avoid this new "lm" class creation
  class(mod_fit_lm) <- c("lm")
  print(mod_fit_lm)
  cli::cli_text(glue::glue_collapse("\n\nClass:\n{class(mod_fit)}\n",
                                    sep = ", ",
                                    last = ""))
  purrr::iwalk(
    all_emoji_titles,
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

  # Get emoji titles for all variance types
  all_emoji_titles <- x$var %>%
    purrr::compact(.x = .) %>%
    purrr::map(.x = ., .f = ~ fetch_mms_comp_var_attr(
      comp_var_ind = .x,
      req_type = "emoji_title"
    ))

  # Get all assumptions for all variance types
  all_assumptions <- x$var %>%
    purrr::map(.x = ., .f = ~ fetch_mms_comp_var_attr(
      comp_var_ind = .x,
      req_type = "var_assumptions"
    ))

  get_mms_print_cli(mod_fit = x,
                    all_emoji_titles = all_emoji_titles,
                    all_assumptions = all_assumptions)

  # TODO: Revise the output of this code to perhaps just print the variance
  #       types available for the object

  # NextMethod("print")
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
  attr(x, "class") <- c("maars_lm", "lm")
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
#'
#' @param ... Additional arguments passed to methods.
#'
#' @return
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
#' comp_var1 <- comp_var(mod_fit = lm_fit,
#'                       boot_emp = list(B = 50, m = 200),
#'                       boot_res = NULL,
#'                       boot_mul = list(B = 60))
#'
#' # Plot our maars_lm object
#' plot(comp_var1)
#' }
plot.maars_lm <- function(x, ...){
  mms_diag_plots <- get_ols_diag_plots(mod_fit = x)
  for (i in base::seq_along(mms_diag_plots)) {
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

